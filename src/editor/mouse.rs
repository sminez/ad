//! Handling for acme-style mouse interactions with the editor.
use crate::{
    config_handle,
    dot::{Dot, Range},
    editor::{Action, Editor},
    fsys::LogEvent,
    key::{MouseButton, MouseEvent, MouseEventKind, MouseMod},
    system::System,
    ui::{Border, SCRATCH_ID},
};
use ad_event::Source;
use std::time::Instant;

/// Number of milliseconds between successive mouse inputs under which we enable fast scrolling.
const FAST_SCROLL_MS: u128 = 10;
/// Number of rows to scroll per mouse wheel event when fast scrolling is enabled.
const FAST_SCROLL_ROWS: usize = 5;

/// Transient state that we hold to track the last mouse click we saw while
/// we wait for it to be released or if the buffer changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Click {
    Text {
        /// The button being held down
        btn: MouseButton,
        /// The current state of the dot associated with this click. This is updated
        /// by Hold events and matches the buffer Dot for Left clicks. For Right and
        /// Middle clicks this is a separate selection that is used on release.
        selection: Range,
        cut_handled: bool,
        paste_handled: bool,
    },
    ResizeColumn {
        last_x: usize,
    },
    ResizeWindow {
        last_y: usize,
    },
}

impl Click {
    pub(super) fn text(btn: MouseButton, selection: Range) -> Self {
        Self::Text {
            btn,
            selection,
            cut_handled: false,
            paste_handled: false,
        }
    }
}

impl<S> Editor<S>
where
    S: System,
{
    /// When scrolling with the mouse wheel we support two scroll rates based on the interval
    /// between successive mouse clicks. If the delta between clicks is under [FAST_SCROLL_MS] we
    /// move into fast scrolling and advance by [FAST_SCROLL_ROWS] per mouse wheel event rather
    /// than individual rows.
    ///
    /// We _could_ be tracking whether or not the last click received was the same mouse wheel
    /// event in order to avoid false positives when the user is clicking and scrolling at the same
    /// time but in practice this doesn't seem to be required due to the short interval we are
    /// using for detecting fast scrolling.
    fn scroll_rows(&self, last_click_time: Instant) -> usize {
        let delta = (self.last_click_time - last_click_time).as_millis();
        if delta < FAST_SCROLL_MS {
            FAST_SCROLL_ROWS
        } else {
            1
        }
    }

    /// The outcome of a mouse event depends on any prior mouse state that is being held:
    ///   - Left   (click+release):      set Cur Dot at the location of the click
    ///   - Left   (click+hold+release): set Range Dot from click->release with click active
    ///   - Right  (click+release):      Load Dot (expanding current dot from Cur if needed)
    ///   - Right  (click+hold+release): Load click->release
    ///   - Middle (click+release):      Execute Dot (expanding current dot from Cur if needed)
    ///   - Middle (click+hold+release): Execute click->release
    ///
    ///   -> For Execute actions, if the current Dot is a Range then it is used as an
    ///      argument to the command being executed
    ///
    /// The chording behaviour we implement follows that of acme: http://acme.cat-v.org/mouse
    ///   - Hold Left + click Right:    Paste into selection
    ///   - Hold Left + click Middle:   Delete selection (cut)
    ///   - Hold Right + click either:  cancel Load
    ///   - Hold Middle + click either: cancel Execute
    pub(super) fn handle_mouse_event(&mut self, MouseEvent { k, m, b, x, y }: MouseEvent) {
        use MouseButton::*;
        use MouseEventKind::*;
        use MouseMod::*;

        let last_click_time = self.last_click_time;
        self.last_click_time = Instant::now();

        match (k, m, b) {
            (Press, NoMod, Left) => {
                // Left clicking while Right or Middle is held is always a cancel
                if self.held_click.is_some() {
                    self.held_click = None;
                    return;
                }

                // Check border hits first as they don't attempt to modify the focus state in the
                // same way that `set_dot_from_screen_coords` does.
                if let Some(border) = self.layout.border_at_coords(x, y) {
                    match border {
                        Border::Vertical { col_idx } => {
                            self.layout.focus_column_for_resize(col_idx);
                            self.held_click = Some(Click::ResizeColumn { last_x: x });
                        }
                        Border::Horizontal { col_idx, win_idx } => {
                            self.layout
                                .focus_column_and_window_for_resize(col_idx, win_idx);
                            self.held_click = Some(Click::ResizeWindow { last_y: y });
                        }
                    }
                    return;
                }

                let click_in_active_buffer = self.layout.set_dot_from_screen_coords(x, y);
                let b = self.layout.active_buffer_mut();
                if !click_in_active_buffer && b.id != SCRATCH_ID {
                    _ = self.tx_fsys.send(LogEvent::Focus(b.id));
                }

                if self.last_click_was_left && click_in_active_buffer {
                    let delta = (self.last_click_time - last_click_time).as_millis();
                    if delta < config_handle!(self).double_click_ms as u128 {
                        b.try_expand_delimited();
                        return;
                    }
                }

                self.held_click = Some(Click::text(Left, b.dot.as_range()));
                self.last_click_was_left = true;
            }

            (Press, NoMod, Right) => self.handle_right_or_middle_click(true, x, y),
            (Press, Alt, Right) => self.handle_right_or_middle_click(true, x, y),

            (Press, NoMod, Middle) | (Press, Ctrl, Left) => {
                self.handle_right_or_middle_click(false, x, y)
            }

            (Hold, _, _) => match &mut self.held_click {
                Some(Click::Text {
                    btn,
                    selection,
                    cut_handled,
                    paste_handled,
                }) => {
                    if *btn == Left && (*cut_handled || *paste_handled) {
                        return;
                    }

                    match self.layout.try_active_cur_from_screen_coords(x, y) {
                        Some(cur) => selection.set_active_cursor(cur),
                        None => return,
                    }

                    if *btn == Left {
                        self.layout.active_buffer_mut().dot = Dot::from(*selection);
                    }
                }

                Some(Click::ResizeColumn { last_x }) => {
                    let delta = x as i16 - *last_x as i16;
                    if delta != 0 {
                        self.layout.resize_active_column_against_next(delta);
                        *last_x = x;
                    }
                }

                Some(Click::ResizeWindow { last_y }) => {
                    let delta = y as i16 - *last_y as i16;
                    if delta != 0 {
                        self.layout.resize_active_window_against_next(delta);
                        *last_y = y;
                    }
                }

                None => (),
            },

            (Press, _, WheelUp) => {
                self.last_click_was_left = false;
                self.layout
                    .scroll_view(x, y, true, self.scroll_rows(last_click_time));
            }

            (Press, _, WheelDown) => {
                self.last_click_was_left = false;
                self.layout
                    .scroll_view(x, y, false, self.scroll_rows(last_click_time));
            }

            (Release, m, b) => {
                if let Some(Click::Text { btn, .. }) = self.held_click
                    && btn == Left
                    && (b == Right || b == Middle)
                {
                    return; // paste and cut are handled on click
                }

                let held = match self.held_click.take() {
                    Some(held) => held,
                    None => return,
                };

                // Only Text clicks need further processing on release
                let (btn, mut selection, cut_handled, paste_handled) = match held {
                    Click::Text {
                        btn,
                        selection,
                        cut_handled,
                        paste_handled,
                    } => (btn, selection, cut_handled, paste_handled),
                    Click::ResizeColumn { .. } | Click::ResizeWindow { .. } => return,
                };

                if btn == Left && (cut_handled || paste_handled) {
                    return;
                }

                // Support releasing the mouse over a different window as actioning the selection
                // as it was present in the active buffer
                if let Some(cur) = self.layout.try_active_cur_from_screen_coords(x, y) {
                    selection.set_active_cursor(cur);
                }

                match btn {
                    Left | WheelUp | WheelDown => (),
                    Right | Middle => {
                        self.handle_right_or_middle_release(btn == Right, selection, m == Alt)
                    }
                }
            }

            _ => (),
        }
    }

    #[inline]
    fn handle_right_or_middle_click(&mut self, is_right: bool, x: usize, y: usize) {
        use MouseButton::*;

        self.last_click_was_left = false;

        match &mut self.held_click {
            Some(Click::Text {
                btn,
                selection,
                cut_handled,
                paste_handled,
            }) => {
                // Mouse chords execute on the click of the second button rather than the release
                if *btn == Left {
                    if is_right && !*paste_handled {
                        *paste_handled = true;
                        self.paste_from_clipboard(Source::Mouse);
                    } else if !is_right && !*cut_handled {
                        *selection = self.layout.active_buffer().dot.as_range();
                        *cut_handled = true;
                        self.forward_action_to_active_buffer(Action::Delete, Source::Mouse);
                    }
                } else if (is_right && *btn == Middle) || (!is_right && *btn == Right) {
                    self.held_click = None;
                }
            }

            Some(_) => {
                // ResizeColumn or ResizeWindow - cancel on other button press
                self.held_click = None;
            }

            None => {
                let btn = if is_right { Right } else { Middle };
                let (id, cur) = self.layout.focus_cur_from_screen_coords(x, y);
                _ = self.tx_fsys.send(LogEvent::Focus(id));
                self.held_click = Some(Click::text(btn, Range::from_cursors(cur, cur, false)));
            }
        };
    }

    #[inline]
    fn handle_right_or_middle_release(
        &mut self,
        is_right: bool,
        selection: Range,
        load_in_new_window: bool,
    ) {
        if selection.start != selection.end {
            // In the case where the click selection is a range we Load/Execute it directly.
            // For Middle clicks, if there is also a range dot in the buffer then that is
            // used as an argument to the command being executed.
            if is_right {
                self.layout.active_buffer_mut().dot = Dot::from(selection);
                self.default_load_dot(Source::Mouse, load_in_new_window);
            } else {
                let dot = self.layout.active_buffer().dot;
                self.layout.active_buffer_mut().dot = Dot::from(selection);

                if dot.is_range() {
                    // Execute as if the click selection was dot then reset dot
                    let arg = dot.content(self.layout.active_buffer()).trim().to_string();
                    self.default_execute_dot(Some((dot.as_range(), arg)), Source::Mouse);
                    self.layout.active_buffer_mut().dot = dot;
                } else {
                    self.default_execute_dot(None, Source::Mouse);
                }
            }
        } else {
            // In the case where the click selection was a Cur rather than a Range we
            // set the buffer dot to the click location if it is outside of the current buffer
            // dot (and allow smart expand to handle generating the selection) before we Load/Execute
            if !self.layout.active_buffer().dot.contains(&selection.start) {
                self.layout.active_buffer_mut().dot = Dot::from(selection.start);
            }

            if is_right {
                self.default_load_dot(Source::Mouse, load_in_new_window);
            } else {
                self.default_execute_dot(None, Source::Mouse);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dot::{Cur, Range},
        editor::EditorMode,
        fsys::InputFilter,
        key::{MouseButton::*, MouseEvent, MouseEventKind::*, MouseMod::*},
        log::LogBuffer,
    };
    use ad_event::{FsysEvent, Kind, Source};
    use simple_test_case::test_case;
    use std::{io, sync::mpsc::channel};

    #[derive(Debug, Default)]
    struct TestSystem {
        clipboard: String,
    }

    impl System for TestSystem {
        fn set_clipboard(&mut self, s: &str) -> io::Result<()> {
            self.clipboard = s.to_string();

            Ok(())
        }

        fn read_clipboard(&self) -> io::Result<String> {
            Ok(self.clipboard.clone())
        }

        fn store_child_handle(&mut self, _: &str, _: std::process::Child) {}
        fn running_children(&self) -> Vec<String> {
            vec![]
        }
        fn cleanup_child(&mut self, _: u32) {}
        fn kill_child(&mut self, _: usize) {}
    }

    fn r(start: usize, end: usize, start_active: bool) -> Range {
        Range {
            start: Cur { idx: start },
            end: Cur { idx: end },
            start_active,
        }
    }

    // NOTE: All mouse tests use 1-indexed terminal coordinates

    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        "some",
        "some text to test with",
        "X",
        &[];
        "left click drag selection complete"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Right, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
        ],
        None,
        "some",
        "some text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::LoadBody, 0, 3, "some"),
        ];
        "right click drag selection complete"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
        ],
        None,
        "some",
        "some text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::ExecuteBody, 0, 3, "some"),
        ];
        "middle click drag selection complete"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        Some(Click::text(Left, r(0, 3, false))),
        "some",
        "some text to test with",
        "X",
        &[];
        "left click drag selection without release"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Right, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Right, x: 7, y: 1 },
        ],
        Some(Click::text(Right, r(0, 3, false))),
        "t",  // default dot position
        "some text to test with",
        "X",
        &[];
        "right click drag selection without release"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Middle, x: 7, y: 1 },
        ],
        Some(Click::text(Middle, r(0, 3, false))),
        "t",  // default dot position
        "some text to test with",
        "X",
        &[];
        "middle click drag selection without release"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 4, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 4, y: 1 },
        ],
        None,
        "some",
        "some text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::LoadBody, 0, 3, "some"),
        ];
        "right click expand in existing selection"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 4, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 4, y: 1 },
        ],
        None,
        "some",
        "some text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::ExecuteBody, 0, 3, "some"),
        ];
        "middle click expand in existing selection"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 9, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 12, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 12, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
        ],
        None,
        "text",
        "some text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::ChordedArgument, 5, 8, "text"),
            FsysEvent::new(Source::Mouse, Kind::ExecuteBody, 0, 3, "some"),
        ];
        "middle click with dot arg"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        " ",
        " text to test with",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
        ];
        "chord cut"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        " ",
        "X text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
        ];
        "chord paste"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        " ",
        "some text to test with",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 4, "some"),
        ];
        "chord cut then paste"
    )]
    // not 100% sure this is correct at the moment in terms of what the
    // selection and clipboard end up as
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        "t",
        "Xtext to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 1, 2, " "),
        ];
        "chord paste then cut"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        " ",
        " text to test with",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
        ];
        "repeated chord cut"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
        ],
        None,
        " ",
        "X text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
        ];
        "repeated chord paste"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 3, y: 1 },
        ],
        None,
        " ",
        "X text to test with",
        "X",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
            FsysEvent::new(Source::Mouse, Kind::InsertBody, 0, 1, "X"),
        ];
        "motion after chord paste is ignored"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Left, x: 2, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 2, y: 1 },
        ],
        None,
        " ",
        " text to test with",
        "some",
        &[
            FsysEvent::new(Source::Mouse, Kind::DeleteBody, 0, 4, ""),
        ];
        "motion after chord cut is ignored"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Right, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
        ],
        None,
        "t",
        "some text to test with",
        "X",
        &[];
        "right click cancel with left"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Right, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
        ],
        None,
        "t",
        "some text to test with",
        "X",
        &[];
        "right click cancel with middle"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
        ],
        None,
        "t",
        "some text to test with",
        "X",
        &[];
        "middle click cancel with left"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Middle, x: 3, y: 1 },
            MouseEvent { k: Hold, m: NoMod, b: Middle, x: 7, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Right, x: 7, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Middle, x: 7, y: 1 },
        ],
        None,
        "t",
        "some text to test with",
        "X",
        &[];
        "middle click cancel with right"
    )]
    #[test_case(
        &[
            MouseEvent { k: Press, m: NoMod, b: Left, x: 9, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 9, y: 1 },
            MouseEvent { k: Press, m: NoMod, b: Left, x: 9, y: 1 },
            MouseEvent { k: Release, m: NoMod, b: Left, x: 9, y: 1 },
        ],
        None,
        "text",
        "some text to test with",
        "X",
        &[];
        "double left click"
    )]
    #[test]
    fn mouse_interactions_work(
        evts: &[MouseEvent],
        click: Option<Click>,
        dot: &str,
        content: &str,
        clipboard: &str,
        fsys_events: &[FsysEvent],
    ) {
        let mut ed = Editor::new_with_system(
            Default::default(),
            Default::default(),
            EditorMode::Headless,
            LogBuffer::default(),
            TestSystem {
                clipboard: "X".to_string(),
            },
        );
        ed.update_window_size(100, 80); // Needed in order to keep clicks in bounds
        ed.layout
            .open_virtual("test", "some text to test with", false);
        ed.layout.active_buffer_mut().dot = Dot::Cur { c: Cur { idx: 5 } };

        // attach an input filter so we can intercept load and execute events
        let (tx, rx) = channel();
        let filter = InputFilter::new(tx);
        ed.layout
            .try_set_input_filter(ed.active_buffer_id(), filter);

        for evt in evts.iter() {
            ed.handle_mouse_event(*evt);
        }

        let recvd_fsys_events: Vec<_> = rx.try_iter().collect();
        let b = ed.layout.active_buffer();

        assert_eq!(ed.held_click, click, "click");
        assert_eq!(b.dot.content(b), dot, "dot content");
        assert_eq!(b.str_contents(), content, "buffer content");
        assert_eq!(ed.system.clipboard, clipboard, "clipboard content");
        assert_eq!(fsys_events, &recvd_fsys_events, "fsys events");
    }

    // TODO:
    //   - test that each mouse click and scroll informs fsys that the appropriate
    //     window is focused
    //   - test that focus events aren't sent when they aren't needed

    fn editor_with_layout(n_cols: usize, n_wins: usize) -> Editor<TestSystem> {
        let mut ed = Editor::new_with_system(
            Default::default(),
            Default::default(),
            EditorMode::Headless,
            LogBuffer::default(),
            TestSystem::default(),
        );
        ed.update_window_size(80, 100);
        ed.layout.open_virtual("test", "test content", false);

        for _ in 1..n_cols {
            ed.layout.new_column();
        }
        for _ in 1..n_wins {
            ed.layout.new_window();
        }

        ed
    }

    #[test_case(5; "drag right grows first column")]
    #[test_case(-5; "drag left shrinks first column")]
    #[test]
    fn resize_column_drag(delta: i16) {
        let mut ed = editor_with_layout(2, 1);
        let initial_sizes = ed.layout.column_widths();
        let border_x = initial_sizes[0] + 1;

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: Left,
            x: border_x,
            y: 10,
        });
        assert!(matches!(ed.held_click, Some(Click::ResizeColumn { .. })));

        let target_x = (border_x as i16 + delta) as usize;
        ed.handle_mouse_event(MouseEvent {
            k: Hold,
            m: NoMod,
            b: Left,
            x: target_x,
            y: 10,
        });
        ed.handle_mouse_event(MouseEvent {
            k: Release,
            m: NoMod,
            b: Left,
            x: target_x,
            y: 10,
        });

        assert!(ed.held_click.is_none());

        let final_sizes = ed.layout.column_widths();
        assert_eq!(
            final_sizes[0] as i16,
            initial_sizes[0] as i16 + delta,
            "first column"
        );
        assert_eq!(
            final_sizes[1] as i16,
            initial_sizes[1] as i16 - delta,
            "second column"
        );
    }

    #[test_case(5; "drag down grows first window")]
    #[test_case(-5; "drag up shrinks first window")]
    #[test]
    fn resize_window_drag(delta: i16) {
        let mut ed = editor_with_layout(1, 2);
        let initial_sizes = ed.layout.window_heights();
        let border_y = initial_sizes[0] + 1;

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: Left,
            x: 10,
            y: border_y,
        });
        assert!(matches!(ed.held_click, Some(Click::ResizeWindow { .. })));

        let target_y = (border_y as i16 + delta) as usize;
        ed.handle_mouse_event(MouseEvent {
            k: Hold,
            m: NoMod,
            b: Left,
            x: 10,
            y: target_y,
        });
        ed.handle_mouse_event(MouseEvent {
            k: Release,
            m: NoMod,
            b: Left,
            x: 10,
            y: target_y,
        });

        assert!(ed.held_click.is_none());

        let final_sizes = ed.layout.window_heights();
        assert_eq!(
            final_sizes[0] as i16,
            initial_sizes[0] as i16 + delta,
            "first window"
        );
        assert_eq!(
            final_sizes[1] as i16,
            initial_sizes[1] as i16 - delta,
            "second window"
        );
    }

    #[test]
    fn resize_sets_correct_focus() {
        let mut ed = editor_with_layout(3, 1);

        ed.layout.focus_column_for_resize(0);
        assert_eq!(ed.layout.cols_before_focus(), 0, "initially on column 0");

        let widths = ed.layout.column_widths();
        let border_x = widths[0] + widths[1] + 2;

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: Left,
            x: border_x,
            y: 10,
        });

        assert_eq!(ed.layout.cols_before_focus(), 1, "focus moved to column 1");
    }

    #[test]
    fn click_on_border_without_drag_is_noop() {
        let mut ed = editor_with_layout(2, 1);
        let initial_sizes = ed.layout.column_widths();
        let border_x = initial_sizes[0] + 1;

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: Left,
            x: border_x,
            y: 10,
        });
        ed.handle_mouse_event(MouseEvent {
            k: Release,
            m: NoMod,
            b: Left,
            x: border_x,
            y: 10,
        });

        let final_sizes = ed.layout.column_widths();
        assert_eq!(initial_sizes, final_sizes, "sizes unchanged");
    }

    #[test_case(Right; "right click")]
    #[test_case(Middle; "middle click")]
    #[test]
    fn non_left_click_on_border_is_noop(btn: MouseButton) {
        let mut ed = editor_with_layout(2, 1);
        let initial_sizes = ed.layout.column_widths();
        let border_x = initial_sizes[0] + 1;

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: btn,
            x: border_x,
            y: 10,
        });
        ed.handle_mouse_event(MouseEvent {
            k: Release,
            m: NoMod,
            b: btn,
            x: border_x,
            y: 10,
        });

        assert!(ed.held_click.is_none());

        let final_sizes = ed.layout.column_widths();
        assert_eq!(initial_sizes, final_sizes, "sizes unchanged");
    }

    #[test]
    fn resize_cancelled_by_right_click() {
        let mut ed = editor_with_layout(2, 1);
        let initial_sizes = ed.layout.column_widths();

        let border_x = initial_sizes[0] + 1;

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: Left,
            x: border_x,
            y: 10,
        });
        assert!(matches!(ed.held_click, Some(Click::ResizeColumn { .. })));

        ed.handle_mouse_event(MouseEvent {
            k: Press,
            m: NoMod,
            b: Right,
            x: border_x,
            y: 10,
        });
        assert!(ed.held_click.is_none(), "resize cancelled");

        let final_sizes = ed.layout.column_widths();
        assert_eq!(initial_sizes, final_sizes, "sizes unchanged after cancel");
    }
}
