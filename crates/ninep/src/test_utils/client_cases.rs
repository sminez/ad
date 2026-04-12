use crate::{
    sansio::client::{Error, MSIZE, Result},
    test_utils::ROOT_QID,
};

/// A test case to be run for a given client implementation.
pub(crate) type TestCase = Vec<Step>;

/// A single step in a client-behavior test.
#[derive(Debug)]
pub(crate) enum Step {
    Connect {
        uname: String,
        aname: String,
        res: Result<()>,
    },
    AssertState {
        msize: u32,
        next_fid: u32,
        fids: Vec<(String, u32)>,
    },
}

/// Helper for making assertions around client errors which can't implement partial eq due to
/// wrapping io::Error.
#[macro_export]
macro_rules! assert_9p_client_result {
    ($i:expr, $actual:expr, $res:expr) => {
        match ($actual, $res) {
            (Ok(()), Ok(())) => (),
            (Err(l), Err(r)) => assert_eq!(l.to_string(), r.to_string(), "step {}", $i),
            (l, r) => panic!("(step {}) expected {r:?}, got {l:?}", $i),
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
    vec![
        Step::Connect {
            uname: "user".to_string(),
            aname: "/".to_string(),
            res: Ok(()),
        },
        Step::AssertState {
            msize: MSIZE,
            next_fid: 1,
            fids: vec![("/".to_string(), ROOT_QID as u32)],
        },
    ]
}

pub(crate) fn connect_to_unknown_aname_errors() -> TestCase {
    vec![
        Step::Connect {
            uname: "user".to_string(),
            aname: "unknown".to_string(),
            res: Err(Error::Rerror {
                ename: "unknown root directory".to_string(),
            }),
        },
        Step::AssertState {
            msize: MSIZE,
            next_fid: 1,
            fids: vec![("/".to_string(), ROOT_QID as u32)],
        },
    ]
}
