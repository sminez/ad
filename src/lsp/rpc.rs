//! A lightweight wrapper around the JSON RPC message protocol required for
//! communicating with LSP servers.
//!
//! The implementation here is heavily inspired by the lsp-server module found in
//! rust-analyzer: <https://github.com/rust-lang/rust-analyzer/tree/master/lib/lsp-server>
use lsp_types::NumberOrString;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::Value;
use std::{
    borrow::Cow,
    io::{self, BufRead, Write},
};

pub type RequestId = NumberOrString;

fn invalid_data(error: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, error)
}

macro_rules! invalid_data {
    ($($tt:tt)*) => (invalid_data(format!($($tt)*)))
}

/// The various message types supported in JSON rpc
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

impl Message {
    pub fn read(r: &mut impl BufRead) -> io::Result<Option<Self>> {
        match read_msg(r)? {
            Some(bytes) => match serde_json::from_slice(&bytes) {
                Ok(msg) => Ok(Some(msg)),
                Err(e) => Err(invalid_data!("malformed LSP message: {e:?}")),
            },
            None => Ok(None),
        }
    }

    pub fn write(self, w: &mut impl Write) -> io::Result<()> {
        let data = serde_json::to_vec(&JsonRpc {
            jsonrpc: "2.0",
            msg: self,
        })?;

        write!(w, "Content-Length: {}\r\n\r\n", data.len())?;
        w.write_all(&data)?;

        return w.flush();

        // Serde structs

        #[derive(Serialize)]
        struct JsonRpc {
            jsonrpc: &'static str,
            #[serde(flatten)]
            msg: Message,
        }
    }
}

fn read_msg(r: &mut dyn BufRead) -> io::Result<Option<Vec<u8>>> {
    let mut content_length: Option<usize> = None;
    let mut buf = String::new();

    // Consume all headers from an incoming message and parse the content length.
    loop {
        buf.clear();
        if r.read_line(&mut buf)? == 0 {
            return Ok(None);
        }
        if !buf.ends_with("\r\n") {
            return Err(invalid_data!("malformed header: {buf:?}"));
        } else if buf == "\r\n" {
            break; // end of headers
        }
        match buf.trim().split_once(": ") {
            Some((h, v)) if h.eq_ignore_ascii_case("Content-Length") => {
                content_length = Some(v.parse().map_err(invalid_data)?);
            }
            Some(_) => (), // ignored header
            None => return Err(invalid_data!("malformed header: {buf:?}")),
        }
    }

    match content_length {
        None => Err(invalid_data!("no Content-Length header")),
        Some(len) => {
            let mut buf = buf.into_bytes();
            buf.resize(len, 0);
            r.read_exact(&mut buf)?;

            Ok(Some(buf))
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Notification {
    pub method: Cow<'static, str>,
    #[serde(default = "Value::default")]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Request {
    pub id: RequestId,
    pub method: Cow<'static, str>,
    #[serde(default = "Value::default")]
    #[serde(skip_serializing_if = "Value::is_null")]
    pub params: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Response {
    Error { id: RequestId, error: ResponseError },
    Result { id: RequestId, result: Value },
}

impl Response {
    pub fn id(&self) -> RequestId {
        match self {
            Self::Result { id, .. } => id.clone(),
            Self::Error { id, .. } => id.clone(),
        }
    }

    pub fn null_resp(id: RequestId) -> Self {
        Self::Result {
            id,
            result: Value::Null,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResponseError {
    pub code: ErrorCode,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    // Defined by JSON RPC:
    ParseError,
    InvalidRequest,
    MethodNotFound,
    InvalidParams,
    InternalError,
    ServerErrorStart,
    ServerErrorEnd,

    /// Error code indicating that a server received a notification or
    /// request before the server has received the `initialize` request.
    ServerNotInitialized,
    Unknown,

    // Defined by the protocol:
    /// The client has canceled a request and a server has detected
    /// the cancel.
    RequestCancelled,

    /// The server detected that the content of a document got
    /// modified outside normal conditions. A server should
    /// NOT send this error code if it detects a content change
    /// in it unprocessed messages. The result even computed
    /// on an older state might still be useful for the client.
    ///
    /// If a client decides that a result is not of any use anymore
    /// the client should cancel the request.
    ContentModified,

    /// The server cancelled the request. This error code should
    /// only be used for requests that explicitly support being
    /// server cancellable.
    ///
    /// @since 3.17.0
    ServerCancelled,

    /// A request failed but it was syntactically correct, e.g the
    /// method name was known and the parameters were valid. The error
    /// message should contain human readable information about why
    /// the request failed.
    ///
    /// @since 3.17.0
    RequestFailed,

    /// Catch-all for unknown custom error codes from servers
    Custom(i32),
}

impl Serialize for ErrorCode {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let val = match self {
            Self::ParseError => -32700,
            Self::InvalidRequest => -32600,
            Self::MethodNotFound => -32601,
            Self::InvalidParams => -32602,
            Self::InternalError => -32603,
            Self::ServerErrorStart => -32099,
            Self::ServerErrorEnd => -32000,
            Self::ServerNotInitialized => -32002,
            Self::Unknown => -32001,
            Self::RequestCancelled => -32800,
            Self::ContentModified => -32801,
            Self::ServerCancelled => -32802,
            Self::RequestFailed => -32803,
            Self::Custom(i) => *i,
        };

        serializer.serialize_i32(val)
    }
}

impl<'de> Deserialize<'de> for ErrorCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let val = match i32::deserialize(deserializer)? {
            -32700 => Self::ParseError,
            -32600 => Self::InvalidRequest,
            -32601 => Self::MethodNotFound,
            -32602 => Self::InvalidParams,
            -32603 => Self::InternalError,
            -32099 => Self::ServerErrorStart,
            -32000 => Self::ServerErrorEnd,
            -32002 => Self::ServerNotInitialized,
            -32001 => Self::Unknown,
            -32800 => Self::RequestCancelled,
            -32801 => Self::ContentModified,
            -32802 => Self::ServerCancelled,
            -32803 => Self::RequestFailed,
            i => Self::Custom(i),
        };

        Ok(val)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use simple_test_case::test_case;
    use std::io::BufReader;

    fn rpc(val: serde_json::Value) -> String {
        let s = serde_json::to_string(&val).unwrap();
        let len = s.len();

        format!("Content-Length: {len}\r\n\r\n{s}")
    }

    #[test_case(
        rpc(json!({ "id": 1, "method": "foo", "params": {"foo": "bar"} })),
        Message::Request(Request {
            id: RequestId::Number(1),
            method: Cow::Borrowed("foo"),
            params: json!({"foo": "bar"}),
        });
        "simple request"
    )]
    #[test_case(
        rpc(json!({ "id": 1, "result": {"foo": "bar"} })),
        Message::Response(Response::Result {
            id: RequestId::Number(1),
            result: json!({"foo": "bar"}),
        });
        "simple successful response"
    )]
    #[test_case(
        rpc(json!({
            "id": 1,
            "error": {
                "code": -32600,
                "message": "invalid",
                "data": {"foo": "bar"}
            }
        })),
        Message::Response(Response::Error {
            id: RequestId::Number(1),
            error: ResponseError {
                code: ErrorCode::InvalidRequest,
                message: "invalid".to_owned(),
                data: Some(json!({"foo": "bar"})),
            }
        });
        "simple error response with data"
    )]
    #[test_case(
        rpc(json!({
            "id": 1,
            "error": {
                "code": -32600,
                "message": "invalid",
            }
        })),
        Message::Response(Response::Error {
            id: RequestId::Number(1),
            error: ResponseError {
                code: ErrorCode::InvalidRequest,
                message: "invalid".to_owned(),
                data: None,
            }
        });
        "simple error response without data"
    )]
    #[test_case(
        rpc(json!({ "method": "foo", "params": {"foo": "bar"} })),
        Message::Notification(Notification {
            method: Cow::Borrowed("foo"),
            params: json!({"foo": "bar"}),
        });
        "simple notification"
    )]
    #[test]
    fn read_message(s: String, expected: Message) {
        let m = Message::read(&mut BufReader::new(s.as_bytes()))
            .unwrap()
            .unwrap();

        assert_eq!(m, expected);
    }

    #[test_case(ErrorCode::ParseError; "parse error")]
    #[test_case(ErrorCode::InvalidRequest; "invalid request")]
    #[test_case(ErrorCode::MethodNotFound; "method not found")]
    #[test_case(ErrorCode::InvalidParams; "invalid params")]
    #[test_case(ErrorCode::InternalError; "internal error")]
    #[test_case(ErrorCode::ServerErrorStart; "server error start")]
    #[test_case(ErrorCode::ServerErrorEnd; "server error end")]
    #[test_case(ErrorCode::ServerNotInitialized; "server not initialized")]
    #[test_case(ErrorCode::Unknown; "unknown")]
    #[test_case(ErrorCode::RequestCancelled; "request cancelled")]
    #[test_case(ErrorCode::ContentModified; "content modified")]
    #[test_case(ErrorCode::ServerCancelled; "server cancelled")]
    #[test_case(ErrorCode::RequestFailed; "request failed")]
    #[test_case(ErrorCode::Custom(42); "custom")]
    #[test]
    fn error_code_serde_round_trip(e: ErrorCode) {
        let s = serde_json::to_string(&e).unwrap();
        let parsed: ErrorCode = serde_json::from_str(&s).unwrap();

        assert_eq!(parsed, e);
    }
}
