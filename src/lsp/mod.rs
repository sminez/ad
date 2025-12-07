//! Built-in minimal LSP support for ad
//!
//! See the LSP spec for details of semantics:
//!   <https://microsoft.github.io/language-server-protocol/specification>
use crate::{
    buffer::{Buffer, Buffers},
    config::{FtypeConfig, LspConfig, ftype_config_for_path_and_first_line},
    die,
    editor::{Action, MbSelect},
    input::Event,
    lsp::{
        capabilities::{Capabilities, PositionEncoding},
        client::{LspClient, LspMessage},
        messages::{
            Diagnostic, Diagnostics, LspNotification, LspRequest, NotificationHandler,
            OpenDocument, PendingLspRequest, PreparedLspNotification, PreparedLspRequest,
            RequestHandler, txt_doc_id,
        },
        rpc::{Message, Notification, Request, RequestId, Response},
    },
    util::ReadOnlyLock,
};
use lsp_types::{NumberOrString, Uri, notification as notif, request as req, request::Initialize};
use std::{
    collections::HashMap,
    path::Path,
    sync::{
        Arc, RwLock,
        mpsc::{Receiver, Sender, channel},
    },
    thread::{sleep, spawn},
    time::Duration,
};
use tracing::{debug, error, warn};

mod capabilities;
mod client;
mod messages;
mod rpc;

pub use capabilities::Coords;

const LSP_FILE: &str = "+lsp";

#[derive(Debug)]
pub(crate) enum PreparedMessage {
    Request(Box<dyn PreparedLspRequest>),
    Notification(Box<dyn PreparedLspNotification>),
}

#[derive(Debug)]
pub(crate) enum Req {
    Start {
        ftype: String,
        cmd: String,
        args: Vec<String>,
        init_opts: Option<serde_json::Value>,
        root: String,
        open_docs: Vec<OpenDocument>,
    },
    Stop {
        lsp_id: usize,
    },
    Prepared(PreparedMessage),
    Message(LspMessage),
}

#[derive(Debug)]
pub struct LspManagerHandle {
    tx_req: Sender<Req>,
    capabilities: ReadOnlyLock<HashMap<String, (usize, Capabilities)>>,
    diagnostics: ReadOnlyLock<HashMap<Uri, Vec<Diagnostic>>>,
    configs: HashMap<String, FtypeConfig>,
    autostart: bool,
}

impl LspManagerHandle {
    pub(crate) fn new_stubbed(tx_req: Sender<Req>) -> Self {
        Self {
            tx_req,
            capabilities: Default::default(),
            diagnostics: Default::default(),
            configs: Default::default(),
            autostart: false,
        }
    }

    fn send_req(&self, req: impl PreparedLspRequest) {
        let msg = PreparedMessage::Request(Box::new(req));
        if let Err(e) = self.tx_req.send(Req::Prepared(msg)) {
            die!("LSP manager died: {e}")
        }
    }

    fn send_notification(&self, notif: impl PreparedLspNotification) {
        let msg = PreparedMessage::Notification(Box::new(notif));
        if let Err(e) = self.tx_req.send(Req::Prepared(msg)) {
            die!("LSP manager died: {e}")
        }
    }

    /// Will return None if there is no active client with recorded capabilities for the
    /// given language.
    fn lsp_id_and_encoding_for(&self, b: &Buffer) -> Option<(usize, PositionEncoding)> {
        let (ftype, _) = &self.config_for_buffer(b)?;

        self.capabilities
            .read()
            .unwrap()
            .get(ftype.as_str())
            .map(|(id, caps)| (*id, caps.position_encoding))
    }

    fn lsp_id_and_encoding_for_ftype(&self, ftype: &str) -> Option<(usize, PositionEncoding)> {
        self.capabilities
            .read()
            .unwrap()
            .get(ftype)
            .map(|(id, caps)| (*id, caps.position_encoding))
    }

    fn config_for_path_and_first_line(
        &self,
        path: &Path,
        first_line: &str,
    ) -> Option<(&String, &LspConfig)> {
        ftype_config_for_path_and_first_line(path, first_line, &self.configs)
            .and_then(|(name, c)| c.lsp.as_ref().map(|lsp| (name, lsp)))
    }

    fn config_for_buffer(&self, b: &Buffer) -> Option<(&String, &LspConfig)> {
        let first_line = b.line(0).map(|l| l.to_string()).unwrap_or_default();

        self.config_for_path_and_first_line(b.path()?, &first_line)
    }

    fn start_req_for_buf(&self, bs: &Buffers) -> Option<Req> {
        let b = bs.active();
        let (ftype, config) = self.config_for_buffer(b)?;
        let root = config.root_for_buffer(b)?.to_str()?.to_owned();
        let open_docs: Vec<_> = bs
            .iter()
            .flat_map(|b| match self.config_for_buffer(b) {
                Some((bftype, _)) if bftype == ftype => Some(OpenDocument {
                    ftype: ftype.to_owned(),
                    path: b.full_name().to_owned(),
                    content: b.str_contents(),
                }),
                _ => None,
            })
            .collect();

        Some(Req::Start {
            ftype: ftype.to_owned(),
            cmd: config.command.clone(),
            args: config.args.clone(),
            init_opts: config.init_opts.clone(),
            root,
            open_docs,
        })
    }

    pub fn start_client(&self, bs: &Buffers) -> Option<&'static str> {
        match self.start_req_for_buf(bs) {
            Some(req) => {
                debug!("starting LSP server");
                if let Err(e) = self.tx_req.send(req) {
                    die!("LSP manager died: {e}")
                }
                None
            }

            None => Some("no LSP available for buffer"),
        }
    }

    pub fn stop_client(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("stopping LSP server {lsp_id}");
            if let Err(e) = self.tx_req.send(Req::Stop { lsp_id }) {
                die!("LSP manager died: {e}")
            }
        };
    }

    pub fn show_server_capabilities(&self, b: &Buffer) -> Option<(&'static str, String)> {
        let (ftype, _) = &self.config_for_buffer(b)?;
        let txt = self
            .capabilities
            .read()
            .unwrap()
            .get(ftype.as_str())?
            .1
            .as_pretty_json()?;

        Some((LSP_FILE, txt))
    }

    pub fn show_diagnostics(&self, b: &Buffer) -> Action {
        if b.dirty {
            // give diagnostics a chance to update
            self.document_changed(b);
            sleep(Duration::from_millis(300));
        }

        debug!("showing LSP diagnostics");
        let guard = self.diagnostics.read().unwrap();
        let mut diags: Vec<Diagnostic> = guard.values().flatten().cloned().collect();
        diags.sort_unstable();

        Action::MbSelect(Diagnostics(diags).into_selector())
    }

    /// Notify an attached LSP server that a document has been opened.
    ///
    /// If the `editor.lsp_autostart` config value is true then attempt to start the server if one
    /// is not already running.
    pub fn document_opened(&self, bs: &Buffers) {
        let b = bs.active();
        let ftype = match self.config_for_buffer(b) {
            Some((ftype, _)) => ftype,
            None => return,
        };

        match self.lsp_id_and_encoding_for_ftype(ftype) {
            Some((lsp_id, _)) => {
                debug!("sending LSP textDocument/didOpen ({lsp_id})");
                let path = b.full_name().to_string();
                let content = b.str_contents();

                self.send_notification(notif::DidOpenTextDocument::data(
                    lsp_id,
                    (ftype.clone(), path, content),
                ));
            }

            None if self.autostart => {
                self.start_client(bs);
            }

            None => (),
        }
    }

    pub fn document_closed(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didClose ({lsp_id})");
            let path = b.full_name().to_string();

            self.send_notification(notif::DidCloseTextDocument::data(lsp_id, path));
        }
    }

    pub fn document_changed(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didChange ({lsp_id})");
            let path = b.full_name().to_string();
            let content = b.str_contents();
            let version = b.next_edit_version() as i32;

            self.send_notification(notif::DidChangeTextDocument::data(
                lsp_id,
                (path, content, version),
            ));
        }
    }

    pub fn document_saved(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            debug!("sending LSP textDocument/didSave ({lsp_id})");
            let path = b.full_name().to_string();

            self.send_notification(notif::DidSaveTextDocument::data(lsp_id, path));
        }
    }

    pub fn goto_declaration(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/declaration ({lsp_id})");
            self.send_req(req::GotoDeclaration::data(lsp_id, enc.buffer_pos(b), ()));
        }
    }

    pub fn goto_definition(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/definition ({lsp_id})");
            self.send_req(req::GotoDefinition::data(lsp_id, enc.buffer_pos(b), ()));
        }
    }

    pub fn goto_type_definition(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/typeDefinition ({lsp_id})");
            self.send_req(req::GotoTypeDefinition::data(lsp_id, enc.buffer_pos(b), ()));
        }
    }

    pub fn hover(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/hover ({lsp_id})");
            self.send_req(req::HoverRequest::data(lsp_id, enc.buffer_pos(b), ()));
        }
    }

    pub fn completion(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/completion ({lsp_id})");
            let pos = enc.buffer_pos(b);
            self.send_req(req::Completion::data(lsp_id, pos.clone(), pos));
        }
    }

    pub fn find_references(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/references ({lsp_id})");
            self.send_req(req::References::data(lsp_id, enc.buffer_pos(b), ()));
        }
    }

    pub fn format(&self, b: &Buffer) {
        if let Some((lsp_id, _)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/formatting ({lsp_id})");
            self.send_req(req::Formatting::data(
                lsp_id,
                (txt_doc_id(b.full_name()), b.tabstop() as u32),
                (),
            ));
        }
    }

    pub fn prepare_rename(&self, b: &Buffer) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/prepareRename ({lsp_id})");
            let pos = enc.buffer_pos(b);
            self.send_req(req::PrepareRenameRequest::data(lsp_id, pos, ()));
        }
    }

    pub fn rename(&self, b: &Buffer, new_name: String) {
        if let Some((lsp_id, enc)) = self.lsp_id_and_encoding_for(b) {
            if b.dirty {
                self.document_changed(b);
            }

            debug!("sending LSP textDocument/rename ({lsp_id})");
            let pos = enc.buffer_pos(b);
            self.send_req(req::Rename::data(
                lsp_id,
                (pos.clone(), new_name),
                (pos, b.id),
            ));
        }
    }
}

#[derive(Debug)]
pub struct LspManager {
    clients: HashMap<usize, LspClient>,
    // ftype -> (lspID, server capabilities)
    capabilities: Arc<RwLock<HashMap<String, (usize, Capabilities)>>>,
    // (lspID, ReqID) -> in-flight requests we need a response for
    pending: HashMap<(usize, RequestId), Box<dyn PendingLspRequest>>,
    // lspID -> map of progress token -> title
    progress_tokens: HashMap<usize, HashMap<NumberOrString, String>>,
    diagnostics: Arc<RwLock<HashMap<Uri, Vec<Diagnostic>>>>,
    pub(super) tx_req: Sender<Req>,
    tx_events: Sender<Event>,
    next_id: usize,
}

impl LspManager {
    pub fn spawn(
        configs: HashMap<String, FtypeConfig>,
        tx_events: Sender<Event>,
        autostart: bool,
    ) -> LspManagerHandle {
        let (tx_req, rx_req) = channel();
        let manager = Self {
            clients: Default::default(),
            capabilities: Default::default(),
            pending: Default::default(),
            progress_tokens: Default::default(),
            diagnostics: Default::default(),
            tx_req: tx_req.clone(),
            tx_events,
            next_id: 0,
        };

        let capabilities = ReadOnlyLock::new(manager.capabilities.clone());
        let diagnostics = ReadOnlyLock::new(manager.diagnostics.clone());
        spawn(move || manager.run(rx_req));

        LspManagerHandle {
            tx_req,
            capabilities,
            diagnostics,
            configs,
            autostart,
        }
    }

    fn run(mut self, rx_req: Receiver<Req>) {
        for r in rx_req.into_iter() {
            match r {
                Req::Start {
                    ftype,
                    cmd,
                    args,
                    init_opts,
                    root,
                    open_docs,
                } => self.start_client(ftype, cmd, args, init_opts, root, open_docs),
                Req::Stop { lsp_id } => self.stop_client(lsp_id),
                Req::Prepared(p) => self.handle_prepared_message(p),
                Req::Message(LspMessage { lsp_id, msg }) => match msg {
                    Message::Request(r) => self.handle_request(lsp_id, r),
                    Message::Response(r) => self.handle_response(lsp_id, r),
                    Message::Notification(n) => self.handle_notification(lsp_id, n),
                },
            }
        }
    }

    fn handle_prepared_message(&mut self, msg: PreparedMessage) {
        match msg {
            PreparedMessage::Request(mut req) => req.send(self),
            PreparedMessage::Notification(mut notif) => notif.send(self),
        }
    }

    fn handle_request(&mut self, lsp_id: usize, req: Request) {
        use lsp_types::request as req;

        RequestHandler {
            lsp_id,
            r: Some(req),
            man: self,
        }
        .handle::<req::WorkDoneProgressCreate>()
        .log_unhandled();
    }

    fn handle_response(&mut self, lsp_id: usize, res: Response) {
        let mut p = match self.pending.remove(&(lsp_id, res.id())) {
            Some(p) => p,
            None => {
                warn!("LSP - got response for unknown request: {res:?}");
                return;
            }
        };

        if let Some(actions) = p.handle(res, self)
            && self.tx_events.send(Event::Actions(actions)).is_err()
        {
            error!("LSP - sender actions channel closed: exiting");
        }
    }

    pub fn handle_notification(&mut self, lsp_id: usize, n: Notification) {
        use lsp_types::notification as notif;

        NotificationHandler {
            lsp_id,
            n: Some(n),
            man: self,
        }
        .handle::<notif::Progress>()
        .handle::<notif::PublishDiagnostics>()
        .log_unhandled();
    }

    pub(super) fn progress_tokens(&mut self, lsp_id: usize) -> &mut HashMap<RequestId, String> {
        self.progress_tokens.entry(lsp_id).or_default()
    }

    fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;

        id
    }

    fn send_status(&self, message: impl Into<String>) {
        _ = self.tx_events.send(Event::Action(Action::SetStatusMessage {
            message: message.into(),
        }));
    }

    #[inline]
    fn report_error(&self, message: impl Into<String>) {
        let message = message.into();
        error!("{message}");
        self.send_status(message);
    }

    fn start_client(
        &mut self,
        ftype: String,
        cmd: String,
        args: Vec<String>,
        init_opts: Option<serde_json::Value>,
        root: String,
        open_bufs: Vec<OpenDocument>,
    ) {
        let lsp_id = self.next_id();
        match LspClient::new(lsp_id, &cmd, args, self.tx_req.clone()) {
            Ok(client) => self.clients.insert(lsp_id, client),
            Err(e) => {
                return self.report_error(format!("failed to start LSP server: {e}"));
            }
        };

        Initialize::send(lsp_id, (root, init_opts), (ftype, open_bufs), self);
        self.send_status("LSP server started");
    }

    fn stop_client(&mut self, lsp_id: usize) {
        use lsp_types::{notification::Exit, request::Shutdown};

        Shutdown::send(lsp_id, (), (), self);
        Exit::send(lsp_id, (), self);

        match self.clients.remove(&lsp_id) {
            Some(client) => client.join(),
            None => self.report_error("no attached LSP server"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Pos {
    pub(crate) file: String,
    pub(crate) line: u32,
    pub(crate) character: u32,
}

impl Pos {
    fn new(file: impl Into<String>, line: u32, character: u32) -> Self {
        Self {
            file: file.into(),
            line,
            character,
        }
    }
}
