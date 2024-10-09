//! A [Buffer] represents a single file or in memory text buffer open within the editor.
use crate::{
    config::ColorScheme,
    config_handle,
    dot::{find::find_forward_wrapping, Cur, Dot, LineRange, Range, TextObject},
    editor::{Action, ViewPort},
    exec::IterBoundedChars,
    fsys::InputFilter,
    ftype::{
        lex::{Token, TokenType, Tokenizer, Tokens},
        try_tokenizer_for_path,
    },
    key::Input,
    term::Style,
    util::relative_path_from,
    MAX_NAME_LEN, UNNAMED_BUFFER,
};
use ad_event::Source;
use std::{
    cmp::min,
    fs,
    io::{self, ErrorKind},
    path::{Path, PathBuf},
    time::SystemTime,
};
use tracing::debug;

mod buffers;
mod edit;
mod internal;

use edit::{Edit, EditLog, Kind, Txt};
pub use internal::{Chars, GapBuffer, IdxChars, Slice};

pub(crate) use buffers::Buffers;

pub(crate) const DEFAULT_OUTPUT_BUFFER: &str = "+output";
const HTTPS: &str = "https://";
const HTTP: &str = "http://";

// Used to inform the editor that further action needs to be taken by it after a Buffer has
// finished processing a given Action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ActionOutcome {
    SetClipboard(String),
    SetStatusMessage(String),
}

/// Buffer kinds control how each buffer interacts with the rest of the editor functionality
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BufferKind {
    /// A regular buffer that is backed by a file on disk.
    File(PathBuf),
    /// A directory buffer that is modifyable but cannot be saved
    Directory(PathBuf),
    /// An in-memory buffer that is not exposed through fsys
    Virtual(String),
    /// An in-memory buffer holding output from commands run within a given directory
    Output(String),
    /// A currently un-named buffer that can be converted to a File buffer when named
    Unnamed,
    /// State for an active mini-buffer
    MiniBuffer,
}

impl Default for BufferKind {
    fn default() -> Self {
        Self::Unnamed
    }
}

impl BufferKind {
    fn display_name(&self, cwd: &Path) -> String {
        match self {
            BufferKind::File(p) => relative_path_from(cwd, p).display().to_string(),
            BufferKind::Directory(p) => relative_path_from(cwd, p).display().to_string(),
            BufferKind::Virtual(s) => s.clone(),
            BufferKind::Output(s) => s.clone(),
            BufferKind::Unnamed => UNNAMED_BUFFER.to_string(),
            BufferKind::MiniBuffer => "".to_string(),
        }
    }

    /// The directory containing the file backing this buffer so long as it has kind `File`.
    fn dir(&self) -> Option<&Path> {
        match &self {
            BufferKind::File(p) => p.parent(),
            BufferKind::Directory(p) => Some(p.as_ref()),
            BufferKind::Output(s) => Path::new(s).parent(),
            _ => None,
        }
    }

    pub(crate) fn is_file(&self) -> bool {
        matches!(self, Self::File(_))
    }

    pub(crate) fn is_dir(&self) -> bool {
        matches!(self, Self::Directory(_))
    }

    /// The key for the +output buffer that output from command run from this buffer should be
    /// redirected to
    pub fn output_file_key(&self, cwd: &Path) -> String {
        let path = self.dir().unwrap_or(cwd);
        format!("{}/{DEFAULT_OUTPUT_BUFFER}", path.display())
    }

    fn try_kind_and_content_from_path(path: PathBuf) -> io::Result<(Self, String)> {
        match path.metadata() {
            Ok(m) if m.is_dir() => {
                let mut raw_entries = Vec::new();
                for entry in path.read_dir()? {
                    let p = entry?.path();
                    let mut s = p.strip_prefix(&path).unwrap_or(&p).display().to_string();
                    if p.metadata().map(|m| m.is_dir()).unwrap_or_default() {
                        s.push('/');
                    }
                    raw_entries.push(s);
                }
                raw_entries.sort_unstable();

                let mut raw = format!("{}\n\n..\n", path.display());
                raw.push_str(&raw_entries.join("\n"));

                Ok((Self::Directory(path), raw))
            }

            _ => {
                let mut raw = match fs::read_to_string(&path) {
                    Ok(contents) => contents,
                    Err(e) if e.kind() == ErrorKind::NotFound => String::new(),
                    Err(e) => return Err(e),
                };

                if raw.ends_with('\n') {
                    raw.pop();
                }

                Ok((Self::File(path), raw))
            }
        }
    }
}

/// Internal state for a text buffer backed by a file on disk
#[derive(Debug)]
pub struct Buffer {
    pub(crate) id: usize,
    pub(crate) kind: BufferKind,
    pub(crate) dot: Dot,
    pub(crate) xdot: Dot,
    pub(crate) txt: GapBuffer,
    pub(crate) rx: usize,
    pub(crate) row_off: usize,
    pub(crate) col_off: usize,
    pub(crate) last_save: SystemTime,
    pub(crate) dirty: bool,
    pub(crate) input_filter: Option<InputFilter>,
    edit_log: EditLog,
    tokenizer: Option<Tokenizer>,
}

impl Buffer {
    /// As the name implies, this method MUST be called with the full cannonical file path
    pub(super) fn new_from_canonical_file_path(id: usize, path: PathBuf) -> io::Result<Self> {
        let (kind, raw) = BufferKind::try_kind_and_content_from_path(path.clone())?;
        let tokenizer = try_tokenizer_for_path(&path, raw.lines().next());

        Ok(Self {
            id,
            kind,
            dot: Dot::default(),
            xdot: Dot::default(),
            txt: GapBuffer::from(raw),
            rx: 0,
            row_off: 0,
            col_off: 0,
            last_save: SystemTime::now(),
            dirty: false,
            edit_log: EditLog::default(),
            tokenizer,
            input_filter: None,
        })
    }

    pub(crate) fn state_changed_on_disk(&self) -> Result<bool, String> {
        fn inner(p: &Path, last_save: SystemTime) -> io::Result<bool> {
            let modified = p.metadata()?.modified()?;
            Ok(modified > last_save)
        }

        let path = match &self.kind {
            BufferKind::File(p) => p,
            _ => return Ok(false),
        };

        match inner(path, self.last_save) {
            Ok(modified) => Ok(modified),
            Err(e) if e.kind() == ErrorKind::NotFound => Ok(false),
            Err(e) => Err(format!("Error checking file state: {e}")),
        }
    }

    pub(crate) fn save_to_disk_at(&mut self, path: PathBuf, force: bool) -> String {
        if !self.dirty {
            return "Nothing to save".to_string();
        }

        if !force {
            match self.state_changed_on_disk() {
                Ok(false) => (),
                Ok(true) => return "File modified on disk, use :w! to force".to_string(),
                Err(s) => return s,
            }
        }

        let contents = self.contents();
        let n_lines = self.len_lines();
        let display_path = match path.canonicalize() {
            Ok(cp) => cp.display().to_string(),
            Err(_) => path.display().to_string(),
        };
        let n_bytes = contents.len();

        match fs::write(path, contents) {
            Ok(_) => {
                self.dirty = false;
                self.last_save = SystemTime::now();
                format!("\"{display_path}\" {n_lines}L {n_bytes}B written")
            }
            Err(e) => format!("Unable to save buffer: {e}"),
        }
    }

    pub(super) fn reload_from_disk(&mut self) -> String {
        let path = match &self.kind {
            BufferKind::File(p) | BufferKind::Directory(p) => p,
            _ => return "Buffer is not backed by a file on disk".to_string(),
        };

        debug!(id=%self.id, path=%path.as_os_str().to_string_lossy(), "reloading buffer state from disk");
        let raw = match BufferKind::try_kind_and_content_from_path(path.to_path_buf()) {
            Ok((_, raw)) => raw,
            Err(e) => return format!("Error reloading buffer: {e}"),
        };

        let n_chars = raw.len();
        self.txt = GapBuffer::from(raw);
        self.dot.clamp_idx(n_chars);
        self.edit_log.clear();
        self.dirty = false;
        self.last_save = SystemTime::now();

        let n_lines = self.txt.len_lines();
        let n_bytes = self.txt.len();
        debug!(%n_bytes, "reloaded buffer content");

        let display_path = match path.canonicalize() {
            Ok(cp) => cp.display().to_string(),
            Err(_) => path.display().to_string(),
        };

        format!("\"{display_path}\" {n_lines}L {n_bytes}B loaded")
    }

    pub(super) fn new_minibuffer() -> Self {
        Self {
            id: usize::MAX,
            kind: BufferKind::MiniBuffer,
            dot: Default::default(),
            xdot: Default::default(),
            txt: GapBuffer::from(""),
            rx: 0,
            row_off: 0,
            col_off: 0,
            last_save: SystemTime::now(),
            dirty: false,
            edit_log: Default::default(),
            tokenizer: None,
            input_filter: None,
        }
    }

    /// Create a new unnamed buffer with the given content
    pub fn new_unnamed(id: usize, content: &str) -> Self {
        Self {
            id,
            kind: BufferKind::Unnamed,
            dot: Dot::default(),
            xdot: Dot::default(),
            txt: GapBuffer::from(content),
            rx: 0,
            row_off: 0,
            col_off: 0,
            last_save: SystemTime::now(),
            dirty: false,
            edit_log: EditLog::default(),
            tokenizer: None,
            input_filter: None,
        }
    }

    /// Create a new virtual buffer with the given name and content.
    ///
    /// The buffer will not be included in the virtual filesystem and it will be removed when it
    /// loses focus.
    pub fn new_virtual(id: usize, name: impl Into<String>, content: impl Into<String>) -> Self {
        let mut content = content.into();
        if content.ends_with('\n') {
            content.pop();
        }

        Self {
            id,
            kind: BufferKind::Virtual(name.into()),
            dot: Dot::default(),
            xdot: Dot::default(),
            txt: GapBuffer::from(content),
            rx: 0,
            row_off: 0,
            col_off: 0,
            last_save: SystemTime::now(),
            dirty: false,
            edit_log: EditLog::default(),
            tokenizer: None,
            input_filter: None,
        }
    }

    /// Construct a new +output buffer with the given name which must be a valid output buffer name
    /// of the form '$dir/+output'.
    pub(super) fn new_output(id: usize, name: String, content: String) -> Self {
        Self {
            id,
            kind: BufferKind::Output(name),
            dot: Dot::default(),
            xdot: Dot::default(),
            txt: GapBuffer::from(content),
            rx: 0,
            row_off: 0,
            col_off: 0,
            last_save: SystemTime::now(),
            dirty: false,
            edit_log: EditLog::default(),
            tokenizer: None,
            input_filter: None,
        }
    }

    /// Short name for displaying in the status line
    pub fn display_name(&self, cwd: &Path) -> String {
        let s = self.kind.display_name(cwd);

        s[0..min(MAX_NAME_LEN, s.len())].to_string()
    }

    /// Absolute path of full name of a virtual buffer
    pub fn full_name(&self) -> &str {
        match &self.kind {
            BufferKind::File(p) => p.to_str().expect("valid unicode"),
            BufferKind::Directory(p) => p.to_str().expect("valid unicode"),
            BufferKind::Virtual(s) => s,
            BufferKind::Output(s) => s,
            BufferKind::Unnamed => UNNAMED_BUFFER,
            BufferKind::MiniBuffer => "*mini-buffer*",
        }
    }

    /// The directory containing the file backing this buffer so long as it has kind `File`.
    pub fn dir(&self) -> Option<&Path> {
        self.kind.dir()
    }

    /// The key for the +output buffer that output from command run from this buffer should be
    /// redirected to
    pub fn output_file_key(&self, cwd: &Path) -> String {
        self.kind.output_file_key(cwd)
    }

    /// Check whether or not this is an unnamed buffer
    pub fn is_unnamed(&self) -> bool {
        self.kind == BufferKind::Unnamed
    }

    /// The raw binary contents of this buffer
    pub fn contents(&self) -> Vec<u8> {
        let mut contents: Vec<u8> = self.txt.bytes();
        contents.push(b'\n');

        contents
    }

    /// The utf-8 string contents of this buffer
    pub fn str_contents(&self) -> String {
        let mut s = self.txt.to_string();
        s.push('\n');
        s
    }

    pub(crate) fn string_lines(&self) -> Vec<String> {
        self.txt
            .iter_lines()
            .map(|l| {
                let mut s = l.to_string();
                if s.ends_with('\n') {
                    s.pop();
                }
                s
            })
            .collect()
    }

    /// The contents of the current [Dot].
    pub fn dot_contents(&self) -> String {
        self.dot.content(self)
    }

    /// The address of the current [Dot].
    pub fn addr(&self) -> String {
        self.dot.addr(self)
    }

    /// The contents of the current xdot.
    ///
    /// This is a virtual dot that is only made use of through the filesystem interface.
    pub fn xdot_contents(&self) -> String {
        self.xdot.content(self)
    }

    /// The address of the current xdot.
    ///
    /// This is a virtual dot that is only made use of through the filesystem interface.
    pub fn xaddr(&self) -> String {
        self.xdot.addr(self)
    }

    /// The number of lines currently held in the buffer.
    #[inline]
    pub fn len_lines(&self) -> usize {
        self.txt.len_lines()
    }

    /// Whether or not the buffer is empty.
    ///
    /// # Note
    /// This does not always imply that the underlying buffer is zero sized, only that the visible
    /// contents are empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.txt.len_chars() == 0
    }

    pub(crate) fn debug_edit_log(&self) -> Vec<String> {
        self.edit_log.debug_edits(self)
    }

    /// Clamp the current viewport to include the [Dot].
    pub fn clamp_scroll(&mut self, screen_rows: usize, screen_cols: usize) {
        let (y, x) = self.dot.active_cur().as_yx(self);
        self.rx = self.rx_from_x(y, x);

        if y < self.row_off {
            self.row_off = y;
        }

        if y >= self.row_off + screen_rows {
            self.row_off = y - screen_rows + 1;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols {
            self.col_off = self.rx - screen_cols + 1;
        }
    }

    /// Set the current [ViewPort] while accounting for screen size.
    pub fn set_view_port(&mut self, vp: ViewPort, screen_rows: usize, screen_cols: usize) {
        let (y, _) = self.dot.active_cur().as_yx(self);

        self.row_off = match vp {
            ViewPort::Top => y,
            ViewPort::Center => y.saturating_sub(screen_rows / 2),
            ViewPort::Bottom => y.saturating_sub(screen_rows),
        };

        self.clamp_scroll(screen_rows, screen_cols);
    }

    pub(crate) fn rx_from_x(&self, y: usize, x: usize) -> usize {
        if y >= self.len_lines() {
            return 0;
        }

        let tabstop = config_handle!().tabstop;

        let mut rx = 0;
        for c in self.txt.line(y).chars().take(x) {
            if c == '\t' {
                rx += (tabstop - 1) - (rx % tabstop);
            }
            rx += 1;
        }

        rx
    }

    pub(crate) fn x_from_rx(&self, y: usize) -> usize {
        self.x_from_provided_rx(y, self.rx)
    }

    pub(crate) fn x_from_provided_rx(&self, y: usize, buf_rx: usize) -> usize {
        if self.is_empty() {
            return 0;
        }

        let mut rx = 0;
        let mut cx = 0;
        let tabstop = config_handle!().tabstop;

        for c in self.txt.line(y).chars() {
            if c == '\n' {
                break;
            }

            if c == '\t' {
                rx += (tabstop - 1) - (rx % tabstop);
            }
            rx += 1;

            if rx > buf_rx {
                break;
            }
            cx += 1;
        }

        cx
    }

    /// The line at the requested index returned as a [Slice].
    pub fn line(&self, y: usize) -> Option<Slice<'_>> {
        if y >= self.len_lines() {
            None
        } else {
            Some(self.txt.line(y))
        }
    }

    /// The render representation of a given line, truncated to fit within the
    /// available screen space.
    /// This includes tab expansion but not any styling that might be applied,
    /// trailing \r\n or screen clearing escape codes.
    /// If a dot range is provided then the character offsets used will be adjusted
    /// to account for expanded tab characters, returning None if self.col_off would
    /// mean that the requested range is not currently visible.
    pub(crate) fn raw_rline_unchecked(
        &self,
        y: usize,
        lpad: usize,
        screen_cols: usize,
        dot_range: Option<(usize, usize)>,
    ) -> (String, Option<(usize, usize)>) {
        let max_chars = screen_cols - lpad;
        let tabstop = config_handle!().tabstop;
        let mut rline = Vec::with_capacity(max_chars);
        // Iterating over characters not bytes as we need to account for multi-byte utf8
        let line = self.txt.line(y);
        let mut it = line.chars().skip(self.col_off);

        let mut update_dot = dot_range.is_some();
        let (mut start, mut end) = dot_range.unwrap_or_default();

        if update_dot && self.col_off > end {
            update_dot = false; // we're past the requested range
        } else {
            start = start.saturating_sub(self.col_off);
            end = end.saturating_sub(self.col_off);
        }

        while rline.len() <= max_chars {
            match it.next() {
                Some('\n') | None => break,
                Some('\t') => {
                    if rline.len() < start {
                        start += tabstop;
                    }
                    if rline.len() < end {
                        end = end.saturating_add(tabstop);
                    }
                    rline.append(&mut [' '].repeat(tabstop));
                }
                Some(c) => rline.push(c),
            }
        }

        rline.truncate(max_chars); // noop if max_chars > rline.len()
        let n_chars = rline.len();
        let s = rline.into_iter().collect();

        if update_dot {
            start = min(start, n_chars);
            end = min(end, n_chars);
            (s, Some((start, end)))
        } else {
            (s, None)
        }
    }

    /// The render representation of a given line, truncated to fit within the
    /// available screen space.
    /// This includes tab expansion and any styling that might be applied but not
    /// trailing \r\n or screen clearing escape codes.
    pub(crate) fn styled_rline_unchecked(
        &self,
        y: usize,
        lpad: usize,
        screen_cols: usize,
        cs: &ColorScheme,
    ) -> String {
        let dot_range = self.dot.line_range(y, self).map(|lr| match lr {
            // LineRange is an inclusive range so we need to insert after `end` if its
            // not the end of the line
            LineRange::Partial { start, end, .. } => (start, end + 1),
            LineRange::FromStart { end, .. } => (0, end + 1),
            LineRange::ToEnd { start, .. } => (start, usize::MAX),
            LineRange::Full { .. } => (0, usize::MAX),
        });

        let (rline, dot_range) = self.raw_rline_unchecked(y, lpad, screen_cols, dot_range);

        let raw_tks = match &self.tokenizer {
            Some(t) => t.tokenize(&rline),
            None => Tokens::Single(Token {
                ty: TokenType::Default,
                s: &rline,
            }),
        };

        let tks = match dot_range {
            Some((start, end)) => raw_tks.with_highlighted_dot(start, end),
            None => match raw_tks {
                Tokens::Single(tk) => vec![tk],
                Tokens::Multi(tks) => tks,
            },
        };

        let mut buf = String::new();
        for tk in tks.into_iter() {
            buf.push_str(&tk.render(cs));
        }

        buf.push_str(&Style::Bg(cs.bg).to_string());

        buf
    }

    pub(crate) fn sign_col_dims(&self) -> (usize, usize) {
        let w_lnum = n_digits(self.len_lines());
        let w_sgncol = w_lnum + 2;

        (w_lnum, w_sgncol)
    }

    /// If the current dot is a cursor rather than a range, expand it to a sensible range.
    ///
    /// This is modeled after (but not identical to) the behaviour in acme's `expand` function
    /// found in look.c
    pub(crate) fn expand_cur_dot(&mut self) {
        let current_index = match self.dot {
            Dot::Cur { c: Cur { idx } } => idx,
            Dot::Range { .. } => return,
        };

        if let Some(dot) = self.try_expand_known(current_index) {
            self.dot = dot;
            return;
        }

        // Expand until we hit non-alphanumeric characters on each sides
        let (mut from, mut to) = (current_index, current_index);
        for (i, ch) in self.iter_between(current_index, self.txt.len_chars()) {
            if !(ch == '_' || ch.is_alphanumeric()) {
                break;
            }
            to = i;
        }

        for (i, ch) in self.rev_iter_between(current_index, 0) {
            if !(ch == '_' || ch.is_alphanumeric()) {
                break;
            }
            from = i;
        }

        self.dot = Dot::from_char_indices(from, to);
    }

    /// Try to be smart about expanding from the current cursor position to something that we
    /// understand how to parse:
    ///   - file path with addr (some/path:addr)
    ///   - file path
    ///   - url
    fn try_expand_known(&self, current_index: usize) -> Option<Dot> {
        let (mut from, mut to) = (current_index, current_index);
        let mut colon: Option<usize> = None;
        let n_chars = self.txt.len_chars();

        let is_file_char = |ch: char| ch.is_alphanumeric() || "._-+/:@".contains(ch);
        let is_addr_char = |ch: char| "+-/$.#,;?".contains(ch);
        let is_url_char = |ch: char| "?&=".contains(ch);
        let has_url_prefix = |i: usize| {
            let http = i > 4 && i + 3 <= n_chars && self.txt.slice(i - 4, i + 3) == HTTP;
            let https = i > 5 && i + 3 <= n_chars && self.txt.slice(i - 5, i + 3) == HTTPS;

            http || https
        };

        // Start by expanding to cover things that are candidates for being file names (optionally
        // with a following address) or URLs
        for (i, ch) in self.iter_between(current_index, self.txt.len_chars()) {
            if !is_file_char(ch) {
                break;
            }
            if ch == ':' && !has_url_prefix(i) {
                colon = Some(i);
                break;
            }
            to = i;
        }

        for (i, ch) in self.rev_iter_between(current_index, 0) {
            if !(is_file_char(ch) || is_url_char(ch) || is_addr_char(ch)) {
                break;
            }
            if colon.is_none() && ch == ':' && !has_url_prefix(i) {
                colon = Some(i);
            }
            from = i;
        }

        // Now grab the address if we had a trailing colon
        if let Some(ix) = colon {
            to = ix;
            for (_, ch) in self.iter_between(ix + 1, self.txt.len_chars()) {
                if ch.is_whitespace() || "()[]{}<>;".contains(ch) {
                    break;
                }
                to += 1;
            }
        }

        let dot_content = self.txt.slice(from, to + 1).to_string();

        // If dot looks like a URL then expand until whitespace and strip trailing punctuation
        if dot_content.starts_with(HTTP) || dot_content.starts_with(HTTPS) {
            if to < self.txt.len_chars() {
                for (_, ch) in self.iter_between(to + 1, self.txt.len_chars()) {
                    if ch.is_whitespace() || "()[]{}<>;".contains(ch) {
                        break;
                    }
                    to += 1;
                }
            }

            if dot_content.ends_with('.') {
                to -= 1;
            }

            return Some(Dot::from_char_indices(from, to));
        }

        let dot = Dot::from_char_indices(from, to);

        // If dot up until ':' is a file then return the entire dot
        let fname = match dot_content.split_once(':') {
            Some((fname, _)) => fname,
            None => &dot_content,
        };

        let path = Path::new(fname);
        if path.is_absolute() && path.exists() {
            return Some(dot);
        } else if let Some(dir) = self.dir() {
            if dir.join(path).exists() {
                return Some(dot);
            }
        }

        // Not a file or a URL
        None
    }

    #[inline]
    fn set_rx_from_screen_rows(&mut self, x: usize) {
        self.rx = self.rx_from_screen_rows(x);
    }

    #[inline]
    pub(crate) fn rx_from_screen_rows(&self, x: usize) -> usize {
        let (_, w_sgncol) = self.sign_col_dims();
        x.saturating_sub(1).saturating_sub(w_sgncol)
    }

    pub(crate) fn cur_from_screen_coords(&self, y: usize, rx: usize) -> Cur {
        let y = min(y + self.row_off, self.len_lines()).saturating_sub(1);
        let mut cur = Cur::from_yx(y, self.x_from_provided_rx(y, rx), self);

        cur.clamp_idx(self.txt.len_chars());

        cur
    }

    pub(crate) fn set_dot_from_screen_coords_if_outside_current_range(
        &mut self,
        x: usize,
        y: usize,
    ) {
        self.set_rx_from_screen_rows(x);
        let mouse_cur = self.cur_from_screen_coords(y, self.rx);
        if !self.dot.contains(&mouse_cur) {
            self.set_dot_from_screen_coords(x, y);
        }
    }

    pub(crate) fn set_dot_from_screen_coords(&mut self, x: usize, y: usize) {
        self.set_rx_from_screen_rows(x);
        self.dot = Dot::Cur {
            c: self.cur_from_screen_coords(y, self.rx),
        };
    }

    pub(crate) fn extend_dot_to_screen_coords(&mut self, x: usize, y: usize) {
        let mut r = self.dot.as_range();
        self.set_rx_from_screen_rows(x);
        let c = self.cur_from_screen_coords(y, self.rx);
        r.set_active_cursor(c);

        let mut dot = Dot::Range { r };
        dot.clamp_idx(self.txt.len_chars());
        self.dot = dot;
    }

    pub(crate) fn scroll_up(&mut self, screen_rows: usize) {
        let c = self.dot.active_cur();
        let (y, x) = c.as_yx(self);
        if self.row_off > 0 && y == self.row_off + screen_rows - 1 {
            self.dot.set_active_cur(Cur::from_yx(y - 1, x, self));
        }

        // clamp scroll is called when we render so no need to run it here as well
        self.row_off = self.row_off.saturating_sub(1);
    }

    pub(crate) fn scroll_down(&mut self) {
        let c = self.dot.active_cur();
        let (y, x) = c.as_yx(self);
        if y == self.row_off && self.row_off < self.txt.len_lines() - 1 {
            self.dot.set_active_cur(Cur::from_yx(y + 1, x, self));
            self.dot.clamp_idx(self.txt.len_chars());
        }

        // clamp scroll is called when we render so no need to run it here as well
        self.row_off += 1;
    }

    pub(crate) fn append(&mut self, s: String) {
        let dot = self.dot;
        self.set_dot(TextObject::BufferEnd, 1);
        self.handle_action(Action::InsertString { s });
        self.dot = dot;
    }

    /// The error result of this function is an error string that should be displayed to the user
    pub(crate) fn handle_action(&mut self, a: Action) -> Option<ActionOutcome> {
        match a {
            Action::Delete => {
                let (c, deleted) = self.delete_dot(self.dot, Some(Source::Fsys));
                self.dot = Dot::Cur { c };
                self.dot.clamp_idx(self.txt.len_chars());
                return deleted.map(ActionOutcome::SetClipboard);
            }
            Action::InsertChar { c } => {
                let (c, deleted) = self.insert_char(self.dot, c, Some(Source::Fsys));
                self.dot = Dot::Cur { c };
                self.dot.clamp_idx(self.txt.len_chars());
                return deleted.map(ActionOutcome::SetClipboard);
            }
            Action::InsertString { s } => {
                let (c, deleted) = self.insert_string(self.dot, s, Some(Source::Fsys));
                self.dot = Dot::Cur { c };
                self.dot.clamp_idx(self.txt.len_chars());
                return deleted.map(ActionOutcome::SetClipboard);
            }

            Action::Redo => return self.redo(),
            Action::Undo => return self.undo(),

            Action::DotCollapseFirst => self.dot = self.dot.collapse_to_first_cur(),
            Action::DotCollapseLast => self.dot = self.dot.collapse_to_last_cur(),
            Action::DotExtendBackward(tobj, count) => self.extend_dot_backward(tobj, count),
            Action::DotExtendForward(tobj, count) => self.extend_dot_forward(tobj, count),
            Action::DotFlip => self.dot.flip(),
            Action::DotSet(t, count) => self.set_dot(t, count),

            Action::RawInput { i } => return self.handle_raw_input(i),

            _ => (),
        }

        None
    }

    fn handle_raw_input(&mut self, k: Input) -> Option<ActionOutcome> {
        let (match_indent, expand_tab, tabstop) = {
            let conf = config_handle!();
            (conf.match_indent, conf.expand_tab, conf.tabstop)
        };

        match k {
            Input::Return => {
                let prefix = if match_indent {
                    let cur = self.dot.first_cur();
                    let y = self.txt.char_to_line(cur.idx);
                    let line = self.txt.line(y).to_string();
                    line.find(|c: char| !c.is_whitespace())
                        .map(|ix| line.split_at(ix).0.to_string())
                } else {
                    None
                };

                let (c, deleted) = self.insert_char(self.dot, '\n', Some(Source::Keyboard));
                let c = match prefix {
                    Some(s) => self.insert_string(Dot::Cur { c }, s, None).0,
                    None => c,
                };

                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }

            Input::Tab => {
                let (c, deleted) = if expand_tab {
                    self.insert_string(self.dot, " ".repeat(tabstop), Some(Source::Keyboard))
                } else {
                    self.insert_char(self.dot, '\t', Some(Source::Keyboard))
                };

                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }

            Input::Char(ch) => {
                let (c, deleted) = self.insert_char(self.dot, ch, Some(Source::Keyboard));
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }

            Input::Arrow(arr) => self.set_dot(TextObject::Arr(arr), 1),

            _ => (),
        }

        None
    }

    /// Set dot and clamp to ensure it is within bounds
    pub(crate) fn set_dot(&mut self, t: TextObject, n: usize) {
        for _ in 0..n {
            t.set_dot(self);
        }
        self.dot.clamp_idx(self.txt.len_chars());
    }

    /// Extend dot foward and clamp to ensure it is within bounds
    fn extend_dot_forward(&mut self, t: TextObject, n: usize) {
        for _ in 0..n {
            t.extend_dot_forward(self);
        }
        self.dot.clamp_idx(self.txt.len_chars());
    }

    /// Extend dot backward and clamp to ensure it is within bounds
    fn extend_dot_backward(&mut self, t: TextObject, n: usize) {
        for _ in 0..n {
            t.extend_dot_backward(self);
        }
        self.dot.clamp_idx(self.txt.len_chars());
    }

    pub(crate) fn new_edit_log_transaction(&mut self) {
        self.edit_log.new_transaction()
    }

    fn undo(&mut self) -> Option<ActionOutcome> {
        match self.edit_log.undo() {
            Some(edits) => {
                self.edit_log.paused = true;
                for edit in edits.into_iter() {
                    self.apply_edit(edit);
                }
                self.edit_log.paused = false;
                self.dirty = !self.edit_log.is_empty();
                None
            }
            None => Some(ActionOutcome::SetStatusMessage(
                "Nothing to undo".to_string(),
            )),
        }
    }

    fn redo(&mut self) -> Option<ActionOutcome> {
        match self.edit_log.redo() {
            Some(edits) => {
                self.edit_log.paused = true;
                for edit in edits.into_iter() {
                    self.apply_edit(edit);
                }
                self.edit_log.paused = false;
                None
            }
            None => Some(ActionOutcome::SetStatusMessage(
                "Nothing to redo".to_string(),
            )),
        }
    }

    fn apply_edit(&mut self, Edit { kind, cur, txt }: Edit) {
        let new_cur = match (kind, txt) {
            (Kind::Insert, Txt::Char(c)) => self.insert_char(Dot::Cur { c: cur }, c, None).0,
            (Kind::Insert, Txt::String(s)) => self.insert_string(Dot::Cur { c: cur }, s, None).0,
            (Kind::Delete, Txt::Char(_)) => self.delete_dot(Dot::Cur { c: cur }, None).0,
            (Kind::Delete, Txt::String(s)) => {
                let start_idx = cur.idx;
                let end_idx = (start_idx + s.chars().count()).saturating_sub(1);
                let end = Cur { idx: end_idx };
                self.delete_dot(
                    Dot::Range {
                        r: Range::from_cursors(cur, end, true),
                    }
                    .collapse_null_range(),
                    None,
                )
                .0
            }
        };

        self.dot = Dot::Cur { c: new_cur };
    }

    /// Only files get marked as dirty to ensure that they are prompted for saving before being
    /// closed.
    fn mark_dirty(&mut self) {
        self.dirty = self.kind.is_file();
    }

    /// Returns true if a filter was present and the notification was sent
    pub(crate) fn notify_load(&self) -> bool {
        match self.input_filter.as_ref() {
            Some(f) => {
                let (ch_from, ch_to) = self.dot.as_char_indices();
                let txt = self.dot.content(self);
                f.notify_load(Source::Mouse, ch_from, ch_to, &txt);
                true
            }
            None => false,
        }
    }

    /// Returns true if a filter was present and the notification was sent
    pub(crate) fn notify_execute(&self) -> bool {
        match self.input_filter.as_ref() {
            Some(f) => {
                let (ch_from, ch_to) = self.dot.as_char_indices();
                let txt = self.dot.content(self);
                f.notify_execute(Source::Mouse, ch_from, ch_to, &txt);
                true
            }
            None => false,
        }
    }

    fn insert_char(&mut self, dot: Dot, ch: char, source: Option<Source>) -> (Cur, Option<String>) {
        let (cur, deleted) = match dot {
            Dot::Cur { c } => (c, None),
            Dot::Range { r } => self.delete_range(r, source),
        };

        let idx = cur.idx;
        self.txt.insert_char(idx, ch);

        if let (Some(source), Some(f)) = (source, self.input_filter.as_ref()) {
            f.notify_insert(source, idx, idx + 1, &ch.to_string());
        }

        self.edit_log.insert_char(cur, ch);
        self.mark_dirty();

        (Cur { idx: idx + 1 }, deleted)
    }

    fn insert_string(
        &mut self,
        dot: Dot,
        s: String,
        source: Option<Source>,
    ) -> (Cur, Option<String>) {
        let (mut cur, deleted) = match dot {
            Dot::Cur { c } => (c, None),
            Dot::Range { r } => self.delete_range(r, source),
        };

        // Inserting an empty string should not be recorded as an edit (and is
        // a no-op for the content of self.txt) but we support it as inserting
        // an empty string while dot is a range has the same effect as a delete.
        if !s.is_empty() {
            let idx = cur.idx;
            let len = s.chars().count();
            self.txt.insert_str(idx, &s);

            if let (Some(source), Some(f)) = (source, self.input_filter.as_ref()) {
                f.notify_insert(source, idx, idx + len, &s);
            }

            self.edit_log.insert_string(cur, s);
            cur.idx += len;
        }

        self.mark_dirty();

        (cur, deleted)
    }

    fn delete_dot(&mut self, dot: Dot, source: Option<Source>) -> (Cur, Option<String>) {
        let (cur, deleted) = match dot {
            Dot::Cur { c } => (self.delete_cur(c, source), None),
            Dot::Range { r } => self.delete_range(r, source),
        };

        (cur, deleted)
    }

    fn delete_cur(&mut self, cur: Cur, source: Option<Source>) -> Cur {
        let idx = cur.idx;
        if idx < self.txt.len_chars() {
            let ch = self.txt.char(idx);
            self.txt.remove_char(idx);

            if let (Some(source), Some(f)) = (source, self.input_filter.as_ref()) {
                f.notify_delete(source, idx, idx + 1);
            }

            self.edit_log.delete_char(cur, ch);
            self.mark_dirty();
        }

        cur
    }

    fn delete_range(&mut self, r: Range, source: Option<Source>) -> (Cur, Option<String>) {
        let (from, to) = if r.start.idx != r.end.idx {
            (r.start.idx, min(r.end.idx + 1, self.txt.len_chars()))
        } else {
            return (r.start, None);
        };

        let s = self.txt.slice(from, to).to_string();
        self.txt.remove_range(from, to);

        if let (Some(source), Some(f)) = (source, self.input_filter.as_ref()) {
            f.notify_delete(source, from, to);
        }

        self.edit_log.delete_string(r.start, s.clone());
        self.mark_dirty();

        (r.start, Some(s))
    }

    pub(crate) fn find_forward(&mut self, s: &str) {
        if let Some(dot) = find_forward_wrapping(&s, self) {
            self.dot = dot;
        }
    }
}

fn n_digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut digits = 0;
    while n != 0 {
        digits += 1;
        n /= 10;
    }

    digits
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::key::Arrow;
    use edit::tests::{del_c, del_s, in_c, in_s};
    use simple_test_case::test_case;
    use std::env;

    #[test_case(0, 1; "n0")]
    #[test_case(5, 1; "n5")]
    #[test_case(10, 2; "n10")]
    #[test_case(13, 2; "n13")]
    #[test_case(731, 3; "n731")]
    #[test_case(930, 3; "n930")]
    #[test]
    fn n_digits_works(n: usize, digits: usize) {
        assert_eq!(n_digits(n), digits);
    }

    const LINE_1: &str = "This is a test";
    const LINE_2: &str = "involving multiple lines";

    pub fn buffer_from_lines(lines: &[&str]) -> Buffer {
        let mut b = Buffer::new_unnamed(0, "");
        let s = lines.join("\n");

        for c in s.chars() {
            b.handle_action(Action::InsertChar { c });
        }

        b
    }

    fn simple_initial_buffer() -> Buffer {
        buffer_from_lines(&[LINE_1, LINE_2])
    }

    #[test]
    fn simple_insert_works() {
        let b = simple_initial_buffer();
        let c = Cur::from_yx(1, LINE_2.len(), &b);
        let lines = b.string_lines();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], LINE_2);
        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(
            b.edit_log.edits,
            vec![vec![in_s(0, &format!("{LINE_1}\n{LINE_2}"))]]
        );
    }

    #[test]
    fn insert_with_moving_dot_works() {
        let mut b = Buffer::new_unnamed(0, "");

        // Insert from the start of the buffer
        for c in "hello w".chars() {
            b.handle_action(Action::InsertChar { c });
        }

        // move back to insert a character inside of the text we already have
        b.handle_action(Action::DotSet(TextObject::Arr(Arrow::Left), 2));
        b.handle_action(Action::InsertChar { c: ',' });

        // move forward to the end of the line to finish inserting
        b.handle_action(Action::DotSet(TextObject::LineEnd, 1));
        for c in "orld!".chars() {
            b.handle_action(Action::InsertChar { c });
        }

        // inserted characters should be in the correct positions
        assert_eq!(b.txt.to_string(), "hello, world!");
    }

    #[test]
    fn insert_char_w_range_dot_works() {
        let mut b = simple_initial_buffer();
        b.handle_action(Action::DotSet(TextObject::Line, 1));
        b.handle_action(Action::InsertChar { c: 'x' });

        let lines = b.string_lines();
        assert_eq!(lines.len(), 2);

        let c = Cur::from_yx(1, 1, &b);
        assert_eq!(b.dot, Dot::Cur { c });

        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], "x");
        assert_eq!(
            b.edit_log.edits,
            vec![vec![
                in_s(0, &format!("{LINE_1}\n{LINE_2}")),
                del_s(LINE_1.len() + 1, LINE_2),
                in_c(LINE_1.len() + 1, 'x'),
            ]]
        );
    }

    #[test]
    fn move_forward_at_end_of_buffer_is_fine() {
        let mut b = Buffer::new_unnamed(0, "");
        b.handle_raw_input(Input::Arrow(Arrow::Right));

        let c = Cur { idx: 0 };
        assert_eq!(b.dot, Dot::Cur { c });
    }

    #[test]
    fn delete_in_empty_buffer_is_fine() {
        let mut b = Buffer::new_unnamed(0, "");
        b.handle_action(Action::Delete);
        let c = Cur { idx: 0 };
        let lines = b.string_lines();

        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0], "");
        assert!(b.edit_log.edits.is_empty());
    }

    #[test]
    fn simple_delete_works() {
        let mut b = simple_initial_buffer();
        b.handle_action(Action::DotSet(TextObject::Arr(Arrow::Left), 1));
        b.handle_action(Action::Delete);

        let c = Cur::from_yx(1, LINE_2.len() - 1, &b);
        let lines = b.string_lines();

        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], "involving multiple line");
        assert_eq!(
            b.edit_log.edits,
            vec![vec![
                in_s(0, &format!("{LINE_1}\n{LINE_2}")),
                del_c(LINE_1.len() + 24, 's')
            ]]
        );
    }

    #[test]
    fn delete_range_works() {
        let mut b = simple_initial_buffer();
        b.handle_action(Action::DotSet(TextObject::Line, 1));
        b.handle_action(Action::Delete);

        let c = Cur::from_yx(1, 0, &b);
        let lines = b.string_lines();

        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], "");
        assert_eq!(
            b.edit_log.edits,
            vec![vec![
                in_s(0, &format!("{LINE_1}\n{LINE_2}")),
                del_s(LINE_1.len() + 1, "involving multiple lines")
            ]]
        );
    }

    #[test]
    fn delete_undo_works() {
        let mut b = simple_initial_buffer();
        let original_lines = b.string_lines();
        b.new_edit_log_transaction();

        b.handle_action(Action::DotExtendBackward(TextObject::Word, 1));
        b.handle_action(Action::Delete);

        b.set_dot(TextObject::BufferStart, 1);
        b.handle_action(Action::DotExtendForward(TextObject::Word, 1));
        b.handle_action(Action::Delete);

        b.handle_action(Action::Undo);

        let lines = b.string_lines();

        assert_eq!(lines, original_lines);
    }

    fn c(idx: usize) -> Cur {
        Cur { idx }
    }

    #[test]
    fn undo_string_insert_works() {
        let initial_content = "foo foo foo\n";
        let mut b = Buffer::new_unnamed(0, initial_content);

        b.insert_string(Dot::Cur { c: c(0) }, "bar".to_string(), None);
        b.handle_action(Action::Undo);

        assert_eq!(b.string_lines(), vec!["foo foo foo", ""]);
    }

    #[test]
    fn undo_string_delete_works() {
        let initial_content = "foo foo foo\n";
        let mut b = Buffer::new_unnamed(0, initial_content);

        let r = Range::from_cursors(c(0), c(2), true);
        b.delete_dot(Dot::Range { r }, None);
        b.handle_action(Action::Undo);

        assert_eq!(b.string_lines(), vec!["foo foo foo", ""]);
    }

    #[test]
    fn undo_string_insert_and_delete_works() {
        let initial_content = "foo foo foo\n";
        let mut b = Buffer::new_unnamed(0, initial_content);

        let r = Range::from_cursors(c(0), c(2), true);
        b.delete_dot(Dot::Range { r }, None);
        b.insert_string(Dot::Cur { c: c(0) }, "bar".to_string(), None);

        assert_eq!(b.string_lines(), vec!["bar foo foo", ""]);

        b.handle_action(Action::Undo);
        b.handle_action(Action::Undo);

        assert_eq!(b.string_lines(), vec!["foo foo foo", ""]);
    }

    #[test_case("simple line", None, 0, "simple line", None; "simple line no dot")]
    #[test_case("simple line", Some((1, 5)), 0, "simple line", Some((1, 5)); "simple line partial")]
    #[test_case("simple line", Some((0, usize::MAX)), 0, "simple line", Some((0, 11)); "simple line full")]
    #[test_case("simple line", Some((0, 2)), 4, "le line", None; "scrolled past dot")]
    #[test_case("simple line", Some((0, 9)), 4, "le line", Some((0, 5)); "scrolled updating dot")]
    #[test_case("\twith tabs", Some((3, usize::MAX)), 0, "    with tabs", Some((7, 13)); "with tabs")]
    #[test_case("\twith tabs", Some((0, usize::MAX)), 0, "    with tabs", Some((0, 13)); "with tabs full")]
    #[test]
    fn raw_line_unchecked_updates_dot_correctly(
        line: &str,
        dot_range: Option<(usize, usize)>,
        col_off: usize,
        expected_line: &str,
        expected_dot_range: Option<(usize, usize)>,
    ) {
        let mut b = Buffer::new_unnamed(0, line);
        b.col_off = col_off;

        let (line, dot_range) = b.raw_rline_unchecked(0, 0, 200, dot_range);

        assert_eq!(line, expected_line);
        assert_eq!(dot_range, expected_dot_range);
    }

    // Tests are executed from the root of the crate so existing file paths are relative to there
    #[test_case("foo", None; "unknown format")]
    #[test_case("someFunc()", None; "camel case function call")]
    #[test_case("some_func()", None; "snake case function call")]
    #[test_case("not_a_real_file.rs", None; "file that does not exist")]
    #[test_case("README.md", Some("README.md"); "file that exists")]
    #[test_case("README.md:12,19", Some("README.md:12,19"); "file that exists with addr")]
    #[test_case("README.md:12:19", Some("README.md:12:19"); "file that exists with addr containing colon")]
    #[test_case("/usr/bin/sh", Some("/usr/bin/sh"); "file that exists abs path")]
    #[test_case("/usr/bin/sh:12-+#", Some("/usr/bin/sh:12-+#"); "file that exists abs path with addr")]
    #[test_case("http://example.com", Some("http://example.com"); "http url")]
    #[test_case("http://example.com/some/path", Some("http://example.com/some/path"); "http url with path")]
    #[test_case("http://example.com?foo=1", Some("http://example.com?foo=1"); "http url with query string")]
    #[test_case("http://example.com?foo=1&bar=2", Some("http://example.com?foo=1&bar=2"); "http url with multi query string")]
    #[test]
    fn try_expand_known_works(s: &str, expected: Option<&str>) {
        let cwd = env::current_dir().unwrap().display().to_string();
        // Check with surrounding whitespace and delimiters
        for (l, r) in [(" ", " "), ("(", ")"), ("[", "]"), ("<", ">"), ("{", "}")] {
            let b = Buffer::new_output(
                0,
                format!("{cwd}/+output"),
                format!("abc_123 {l}{s}{r}\tmore text"),
            );

            // Check with the initial cursor position being at any offset within the target
            for i in 0..s.len() {
                let dot = b.try_expand_known(9 + i);
                let maybe_content = dot.map(|d| d.content(&b));
                assert_eq!(
                    maybe_content.as_deref(),
                    expected,
                    "failed at offset={i} with lr=({l:?}, {r:?})"
                )
            }
        }
    }
}
