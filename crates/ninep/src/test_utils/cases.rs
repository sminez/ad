use crate::{
    fs::{FileMeta, Mode, Perm, Stat},
    sansio::{
        protocol::{DEFAULT_MSIZE, Data, NineP, Qid, RawStat, Rdata, Tdata},
        server::{
            AFID_NO_AUTH, ClientId, E_ALREADY_ATTACHED, E_CREATE_NON_DIR, E_ILLEGAL_CREATE_NAME,
            E_NO_VERSION_MESSAGE, E_UNKNOWN_FID, E_UNKNOWN_FILE, SUPPORTED_VERSION,
        },
    },
    test_utils::{
        BLOCKED_CONTENT, BLOCKED_QID, CREATED_QID, Call, HELLO_CONTENT, HELLO_QID, ROOT_QID,
        SUBDIR_QID, TEST_IOUNIT,
    },
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
    fn version_req() -> Step {
        Step::Request {
            req: Tdata::Version {
                msize: DEFAULT_MSIZE,
                version: SUPPORTED_VERSION.to_string(),
            },
            resp: Rdata::Version {
                msize: DEFAULT_MSIZE,
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

    fn walk_req(fid: u32, new_fid: u32, wnames: Vec<&str>, wqids: Vec<Qid>) -> Step {
        Step::Request {
            req: Tdata::Walk {
                fid,
                new_fid,
                wnames: wnames.into_iter().map(str::to_string).collect(),
            },
            resp: Rdata::Walk { wqids },
        }
    }

    fn err(req: Tdata, msg: &str) -> Step {
        Step::Request {
            req,
            resp: Rdata::Error {
                ename: msg.to_string(),
            },
        }
    }
}

fn dir_qid(path: u64) -> Qid {
    Qid {
        ty: Mode::DIR.bits(),
        version: 0,
        path,
    }
}

fn file_qid(path: u64) -> Qid {
    Qid {
        ty: Mode::FILE.bits(),
        version: 0,
        path,
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
            duplicate_attach_returns_error,
            version_while_attached_clunks_all_open_fids,
            connection_close_clunks_all_open_fids,
            flush_returns_rflush,
            walk_to_known_child_returns_qids,
            walk_first_element_missing_returns_error,
            walk_partial_returns_partial_qids,
            walk_unknown_fid_returns_error,
            clunk_known_fid_returns_rclunk_and_calls_serve9p,
            clunk_unknown_fid_returns_error,
            open_known_fid_returns_ropen,
            open_unknown_fid_returns_error,
            read_file_returns_data,
            read_dir_returns_serialized_stats,
            write_to_file_returns_byte_count,
            write_with_oversized_offset_returns_error,
            stat_known_fid_returns_rstat,
            stat_unknown_fid_returns_error,
            create_in_directory_returns_rcreate,
            create_with_dot_name_returns_error,
            create_on_non_directory_returns_error,
            remove_known_fid_returns_rremove,
            blocked_read_delivers_response_later,
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
    vec![Step::Request {
        req: Tdata::Version {
            msize: DEFAULT_MSIZE / 2,
            version: SUPPORTED_VERSION.to_string(),
        },
        resp: Rdata::Version {
            msize: DEFAULT_MSIZE / 2,
            version: SUPPORTED_VERSION.to_string(),
        },
    }]
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
        Step::version_req(),
        Step::attach_req(),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn duplicate_attach_returns_error() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(
            Tdata::Attach {
                fid: 1,
                afid: AFID_NO_AUTH,
                uname: "user".to_string(),
                aname: "/".to_string(),
            },
            E_ALREADY_ATTACHED,
        ),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn version_while_attached_clunks_all_open_fids() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Open { fid: 1, mode: 0 },
            resp: Rdata::Open {
                qid: file_qid(HELLO_QID),
                iounit: TEST_IOUNIT,
            },
        },
        Step::version_req(),
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::open(ClientId(0), HELLO_QID, Mode::new(0), "user"),
                Call::clunk(ClientId(0), ROOT_QID),
                Call::clunk(ClientId(0), HELLO_QID),
            ],
        },
    ]
}

pub(crate) fn connection_close_clunks_all_open_fids() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Open { fid: 1, mode: 0 },
            resp: Rdata::Open {
                qid: file_qid(HELLO_QID),
                iounit: TEST_IOUNIT,
            },
        },
        Step::CloseStream,
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::open(ClientId(0), HELLO_QID, Mode::new(0), "user"),
                Call::clunk(ClientId(0), ROOT_QID),
                Call::clunk(ClientId(0), HELLO_QID),
            ],
        },
    ]
}

pub(crate) fn flush_returns_rflush() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::Request {
            req: Tdata::Flush { old_tag: 123 },
            resp: Rdata::Flush {},
        },
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn walk_to_known_child_returns_qids() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::AssertCalls {
            calls: vec![Call::walk(ClientId(0), ROOT_QID, "hello", "user")],
        },
    ]
}

pub(crate) fn walk_first_element_missing_returns_error() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(
            Tdata::Walk {
                fid: 0,
                new_fid: 1,
                wnames: vec!["missing".to_string()],
            },
            E_UNKNOWN_FILE,
        ),
        Step::AssertCalls {
            calls: vec![Call::walk(ClientId(0), ROOT_QID, "missing", "user")],
        },
    ]
}

pub(crate) fn walk_partial_returns_partial_qids() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["subdir", "missing"], vec![dir_qid(SUBDIR_QID)]),
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "subdir", "user"),
                Call::walk(ClientId(0), SUBDIR_QID, "missing", "user"),
            ],
        },
    ]
}

pub(crate) fn walk_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(
            Tdata::Walk {
                fid: 99,
                new_fid: 1,
                wnames: vec!["hello".to_string()],
            },
            E_UNKNOWN_FID,
        ),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn clunk_known_fid_returns_rclunk_and_calls_serve9p() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Clunk { fid: 1 },
            resp: Rdata::Clunk {},
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::clunk(ClientId(0), HELLO_QID),
            ],
        },
    ]
}

pub(crate) fn clunk_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(Tdata::Clunk { fid: 99 }, E_UNKNOWN_FID),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn open_known_fid_returns_ropen() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Open { fid: 1, mode: 0 },
            resp: Rdata::Open {
                qid: file_qid(HELLO_QID),
                iounit: TEST_IOUNIT,
            },
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::open(ClientId(0), HELLO_QID, Mode::new(0), "user"),
            ],
        },
    ]
}

pub(crate) fn open_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(Tdata::Open { fid: 99, mode: 0 }, E_UNKNOWN_FID),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn read_file_returns_data() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Read {
                fid: 1,
                offset: 0,
                count: 5,
            },
            resp: Rdata::Read {
                data: Data(HELLO_CONTENT[..5].to_vec()),
            },
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::read(ClientId(0), HELLO_QID, 0, 5, "user"),
            ],
        },
    ]
}

pub(crate) fn read_dir_returns_serialized_stats() -> TestCase {
    let s1: RawStat = Stat::stub(FileMeta::file("hello", HELLO_QID)).into();
    let s2: RawStat = Stat::stub(FileMeta::dir("subdir", SUBDIR_QID)).into();
    let mut buf = s1.write_9p_bytes().unwrap();
    buf.extend(s2.write_9p_bytes().unwrap());

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::Request {
            req: Tdata::Read {
                fid: 0,
                offset: 0,
                count: 4096,
            },
            resp: Rdata::Read { data: Data(buf) },
        },
        Step::AssertCalls {
            calls: vec![Call::read_dir(ClientId(0), ROOT_QID, "user")],
        },
    ]
}

pub(crate) fn write_to_file_returns_byte_count() -> TestCase {
    let payload = b"abc".to_vec();

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Write {
                fid: 1,
                offset: 0,
                data: Data(payload.clone()),
            },
            resp: Rdata::Write { count: 3 },
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::write(ClientId(0), HELLO_QID, 0, payload, "user"),
            ],
        },
    ]
}

pub(crate) fn write_with_oversized_offset_returns_error() -> TestCase {
    let offset = u64::from(u32::MAX) + 1;

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Write {
                fid: 1,
                offset,
                data: Data(vec![1]),
            },
            resp: Rdata::Error {
                ename: format!("offset too large: {offset} > {}", u32::MAX),
            },
        },
        Step::AssertCalls {
            calls: vec![Call::walk(ClientId(0), ROOT_QID, "hello", "user")],
        },
    ]
}

pub(crate) fn stat_known_fid_returns_rstat() -> TestCase {
    let stat: RawStat = Stat::stub(FileMeta::file("hello", HELLO_QID)).into();
    let size = stat.n_bytes() as u16;

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Stat { fid: 1 },
            resp: Rdata::Stat { size, stat },
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::stat(ClientId(0), HELLO_QID, "user"),
            ],
        },
    ]
}

pub(crate) fn stat_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(Tdata::Stat { fid: 99 }, E_UNKNOWN_FID),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn create_in_directory_returns_rcreate() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::Request {
            req: Tdata::Create {
                fid: 0,
                name: "new.txt".to_string(),
                perm: perm.bits(),
                mode: 0,
            },
            resp: Rdata::Create {
                qid: file_qid(CREATED_QID),
                iounit: TEST_IOUNIT,
            },
        },
        Step::AssertCalls {
            calls: vec![Call::create(
                ClientId(0),
                ROOT_QID,
                "new.txt",
                perm,
                Mode::new(0),
                "user",
            )],
        },
    ]
}

pub(crate) fn create_with_dot_name_returns_error() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::err(
            Tdata::Create {
                fid: 0,
                name: ".".to_string(),
                perm: perm.bits(),
                mode: 0,
            },
            E_ILLEGAL_CREATE_NAME,
        ),
        Step::AssertCalls { calls: vec![] },
    ]
}

pub(crate) fn create_on_non_directory_returns_error() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::err(
            Tdata::Create {
                fid: 1,
                name: "child".to_string(),
                perm: perm.bits(),
                mode: 0,
            },
            E_CREATE_NON_DIR,
        ),
        Step::AssertCalls {
            calls: vec![Call::walk(ClientId(0), ROOT_QID, "hello", "user")],
        },
    ]
}

pub(crate) fn remove_known_fid_returns_rremove() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["hello"], vec![file_qid(HELLO_QID)]),
        Step::Request {
            req: Tdata::Remove { fid: 1 },
            resp: Rdata::Remove {},
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "hello", "user"),
                Call::remove(ClientId(0), HELLO_QID, "user"),
            ],
        },
    ]
}

pub(crate) fn blocked_read_delivers_response_later() -> TestCase {
    vec![
        Step::version_req(),
        Step::attach_req(),
        Step::walk_req(0, 1, vec!["blocked"], vec![file_qid(BLOCKED_QID)]),
        Step::Request {
            req: Tdata::Read {
                fid: 1,
                offset: 0,
                count: 4096,
            },
            resp: Rdata::Read {
                data: Data(BLOCKED_CONTENT.to_vec()),
            },
        },
        Step::AssertCalls {
            calls: vec![
                Call::walk(ClientId(0), ROOT_QID, "blocked", "user"),
                Call::read(ClientId(0), BLOCKED_QID, 0, 4096, "user"),
            ],
        },
    ]
}
