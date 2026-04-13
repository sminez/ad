use crate::sansio::client::{Error, Result};
use std::collections::HashMap;

/// A test case to be run for a given client implementation.
pub(crate) type TestCase = Vec<Step>;

/// A single step in a client-behavior test.
#[derive(Debug)]
pub(crate) enum Step {
    Connect {
        uname: &'static str,
        aname: &'static str,
        res: Result<()>,
    },
    Walk {
        path: &'static str,
        res: Result<u32>,
    },
    AssertState {
        next_fid: u32,
        fids: HashMap<String, u32>,
    },
}

impl Step {
    fn connect_valid() -> Self {
        Step::Connect {
            uname: "uname",
            aname: "/",
            res: Ok(()),
        }
    }

    fn connect(uname: &'static str, aname: &'static str, res: Result<()>) -> Self {
        Step::Connect { uname, aname, res }
    }

    fn walk(path: &'static str, res: Result<u32>) -> Self {
        Step::Walk { path, res }
    }

    fn assert_state(next_fid: u32, fids: &[(&str, u32)]) -> Self {
        Self::AssertState {
            next_fid,
            fids: fids.iter().map(|(k, v)| (k.to_string(), *v)).collect(),
        }
    }
}

/// Helper for making assertions around client errors which can't implement partial eq due to
/// wrapping io::Error.
#[macro_export]
macro_rules! assert_9p_client_result {
    ($tag:expr, $i:expr, $actual:expr, $res:expr) => {
        match ($actual, $res) {
            (Ok(l), Ok(r)) => assert_eq!(l, r, "(step {}) {}", $i, $tag),
            (Err(l), Err(r)) => assert_eq!(l.to_string(), r.to_string(), "(step {}) {}", $i, $tag),
            (l, r) => panic!("(step {}) {} expected {r:?}, got {l:?}", $i, $tag),
        }
    };
}

/// Helper for generating shared behavioural tests of both sync and tokio client implementations.
#[macro_export]
macro_rules! generate_client_test_suite {
    ($mode:ident, $run_one:ident) => {
        generate_client_test_suite!(
            @cases $mode, $run_one;
            connect_to_known_aname_succeeds,
            connect_to_unknown_aname_errors,
            walk_same_path_doesnt_alter_next_fid,
            walk_to_known_file_succeeds,
            walk_to_root_succeeds,
            walk_to_unknown_entry_errors,
        );
    };

    (@cases sync, $run_one:ident; $($case:ident),+ $(,)?) => {
        $(
            #[test]
            fn $case() { $run_one($crate::test_utils::client_cases::$case()); }
        )+
    };

    (@cases tokio, $run_one:ident; $($case:ident),+ $(,)?) => {
        $(
            #[tokio::test]
            async fn $case() { $run_one($crate::test_utils::client_cases::$case()).await; }
        )+
    };
}

// Test cases for use with the generate_client_test_suite macro above

pub(crate) fn connect_to_known_aname_succeeds() -> TestCase {
    vec![Step::connect_valid(), Step::assert_state(1, &[("/", 0)])]
}

pub(crate) fn connect_to_unknown_aname_errors() -> TestCase {
    vec![
        Step::connect(
            "user",
            "unknown",
            Err(Error::Rerror {
                ename: "unknown root directory".to_string(),
            }),
        ),
        Step::assert_state(1, &[("/", 0)]),
    ]
}

pub(crate) fn walk_to_known_file_succeeds() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk("/hello", Ok(1)),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn walk_to_root_succeeds() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk("/", Ok(0)),
        // shouldn't alter next_fid
        Step::assert_state(1, &[("/", 0)]),
    ]
}

pub(crate) fn walk_same_path_doesnt_alter_next_fid() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk("/hello", Ok(1)),
        // fid=1 now bound for /hello
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
        Step::walk("/hello", Ok(1)),
        // should have read from cache
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn walk_to_unknown_entry_errors() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk(
            "/missing",
            Err(Error::Rerror {
                ename: "unknown file".into(),
            }),
        ),
        Step::assert_state(2, &[("/", 0)]),
    ]
}
