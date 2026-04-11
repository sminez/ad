use crate::{
    fs::Mode,
    sansio::{
        protocol::{DEFAULT_MSIZE, Qid, Rdata, Tdata},
        server::{AFID_NO_AUTH, E_NO_VERSION_MESSAGE, SUPPORTED_VERSION},
    },
    test_utils::{Call, ROOT_QID},
};

/// A test case to be run against a 9p server.
pub(crate) type TestCase = Vec<Step>;

/// A single step within a larger test case.
///
/// Corresponds to an action to take and possibly a paired assertion.
#[derive(Debug)]
#[expect(clippy::large_enum_variant)]
pub(crate) enum Step {
    Request { req: Tdata, resp: Rdata },
    AssertCalls { calls: Vec<Call> },
    CloseStream,
}

impl Step {
    fn version_req(msize: u32) -> Step {
        Step::Request {
            req: Tdata::Version {
                msize,
                version: SUPPORTED_VERSION.to_string(),
            },
            resp: Rdata::Version {
                msize,
                version: SUPPORTED_VERSION.to_string(),
            },
        }
    }

    fn attach_req() -> Step {
        Step::Request {
            req: Tdata::Attach {
                fid: 0,
                afid: AFID_NO_AUTH,
                uname: "user".to_string(),
                aname: "/".to_string(),
            },
            resp: Rdata::Attach {
                aqid: Qid {
                    ty: Mode::DIR.bits(),
                    version: 0,
                    path: ROOT_QID,
                },
            },
        }
    }
}

/// Helper for generating shared behavioural tests of both the sync and tokio server
/// implementations.
#[macro_export]
macro_rules! generate_test_suite {
    // Public entrypoint used by sync/tokio test modules.
    //
    // As new cases are added to the suite below, they MUST be added here in order to actually be
    // run as part of the sync/tokio test suites.
    ($mode:ident, $run_one:ident) => {
        generate_test_suite!(
            @cases $mode, $run_one;
            // Test cases
            version_sets_negotiated_msize,
            attach_before_version_returns_error,
            attach_after_version_succeeds,
        );
    };

    // generate sync test suite
    (@cases sync, $run_one:ident; $($case:ident),+ $(,)?) => {
        $(
            #[test]
            fn $case() { $run_one!($crate::test_utils::cases::$case()); }
        )+
    };

    // generate tokio test suite
    (@cases tokio, $run_one:ident; $($case:ident),+ $(,)?) => {
        $(
            #[tokio::test]
            async fn $case() { $run_one!($crate::test_utils::cases::$case()); }
        )+
    };
}

// Test cases for use with the generate_test_suite macro above

pub(crate) fn version_sets_negotiated_msize() -> TestCase {
    vec![Step::version_req(DEFAULT_MSIZE / 2)]
}

pub(crate) fn attach_before_version_returns_error() -> TestCase {
    vec![
        Step::Request {
            req: Tdata::Attach {
                fid: 0,
                afid: AFID_NO_AUTH,
                uname: "user".to_string(),
                aname: "/".to_string(),
            },
            resp: Rdata::Error {
                ename: E_NO_VERSION_MESSAGE.to_string(),
            },
        },
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn attach_after_version_succeeds() -> TestCase {
    vec![
        Step::version_req(DEFAULT_MSIZE / 2),
        Step::attach_req(),
        Step::AssertCalls { calls: vec![] },
    ]
}
