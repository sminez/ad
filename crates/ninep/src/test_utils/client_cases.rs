use crate::{sansio::client::MSIZE, test_utils::ROOT_QID};

/// A test case to be run for a given client implementation.
pub(crate) type TestCase = Vec<Step>;

/// A single step in a client-behavior test.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Step {
    Connect {
        uname: String,
        aname: String,
    },
    AssertState {
        msize: u32,
        next_fid: u32,
        fids: Vec<(String, u32)>,
    },
}

/// Helper for generating shared behavioural tests of both sync and tokio client implementations.
#[macro_export]
macro_rules! generate_client_test_suite {
    ($mode:ident, $run_one:ident) => {
        generate_client_test_suite!(
            @cases $mode, $run_one;
            connect_succeeds,
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

pub(crate) fn connect_succeeds() -> TestCase {
    vec![
        Step::Connect {
            uname: "user".to_string(),
            aname: "/".to_string(),
        },
        Step::AssertState {
            msize: MSIZE,
            next_fid: 1,
            fids: vec![("/".to_string(), ROOT_QID as u32)],
        },
    ]
}
