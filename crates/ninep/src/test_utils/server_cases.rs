use crate::{
    fs::{FileMeta, FileType, Mode, Perm, Stat},
    sansio::{
        protocol::{DEFAULT_MSIZE, NineP, Qid, RawStat, Rdata, Tdata},
        server::{
            AFID_NO_AUTH, ClientId, E_ALREADY_ATTACHED, E_CREATE_NON_DIR, E_ILLEGAL_CREATE_NAME,
            E_NO_VERSION_MESSAGE, E_PERMISSION_DENIED, E_UNKNOWN_FID, E_UNKNOWN_FILE,
            E_WALK_OPEN_FID, SUPPORTED_VERSION,
        },
    },
    test_utils::{
        BLOCKED_CONTENT, BLOCKED_QID, CREATED_QID, Call, HELLO_CONTENT, HELLO_QID, ROOT_QID,
        SUBDIR_QID, TEST_IOUNIT, dir_qid, file_qid,
    },
};

/// A test case to be run against a 9p server.
pub(crate) type TestCase = Vec<Step>;

/// A single step within a larger test case.
///
/// Corresponds to an action to take and possibly a paired assertion.
#[derive(Debug)]
pub(crate) enum Step {
    /// Blocking request -> assert expected response
    Request { tag: u16, req: Tdata, resp: Rdata },
    /// Submit request without waiting for response
    Send { tag: u16, req: Tdata },
    /// Assert expected response
    Receive { tag: u16, resp: Rdata },
    /// Assert state of TestFs method calls
    AssertCalls { calls: Vec<Call> },
    /// Close client connection stream
    CloseStream,
}

impl Step {
    fn assert_calls(calls: &[Call]) -> Step {
        Self::AssertCalls {
            calls: calls.into(),
        }
    }

    fn req(tag: u16, req: Tdata, resp: Rdata) -> Step {
        Step::Request { tag, req, resp }
    }

    fn snd(tag: u16, req: Tdata) -> Step {
        Step::Send { tag, req }
    }

    fn rcv(tag: u16, resp: Rdata) -> Step {
        Step::Receive { tag, resp }
    }

    fn version_req(tag: u16) -> Step {
        Step::req(
            tag,
            Tdata::version(DEFAULT_MSIZE, SUPPORTED_VERSION),
            Rdata::version(DEFAULT_MSIZE, SUPPORTED_VERSION),
        )
    }

    fn attach_req(tag: u16) -> Step {
        Step::req(
            tag,
            Tdata::attach(0, AFID_NO_AUTH, "owner", "/"),
            Rdata::attach(Qid {
                ty: FileType::DIRECTORY.bits(),
                version: 0,
                path: ROOT_QID,
            }),
        )
    }

    fn walk_req(tag: u16, fid: u32, new_fid: u32, wnames: &[&str], wqids: &[Qid]) -> Step {
        let wnames: Vec<String> = wnames.iter().map(|s| s.to_string()).collect();
        Step::req(tag, Tdata::walk(fid, new_fid, wnames), Rdata::walk(wqids))
    }

    fn open_req(tag: u16, fid: u32, mode: Mode, qid: u64) -> Step {
        Step::req(
            tag,
            Tdata::open(fid, mode.bits()),
            Rdata::open(file_qid(qid), TEST_IOUNIT),
        )
    }

    fn open_dir_req(tag: u16, fid: u32, mode: Mode, qid: u64) -> Step {
        Step::req(
            tag,
            Tdata::open(fid, mode.bits()),
            Rdata::open(dir_qid(qid), TEST_IOUNIT),
        )
    }

    fn err(tag: u16, req: Tdata, msg: impl Into<String>) -> Step {
        Step::req(tag, req, Rdata::error(msg))
    }
}

/// Helper for generating shared behavioural tests of both the sync and tokio server
/// implementations.
#[macro_export]
macro_rules! generate_server_test_suite {
    // Public entrypoint used by sync/tokio test modules.
    //
    // As new cases are added to the suite below, they MUST be added here in order to actually be
    // run as part of the sync/tokio test suites.
    ($mode:ident, $run_one:ident) => {
        generate_server_test_suite!(
            @cases $mode, $run_one;
            // Test cases
            attach_after_version_succeeds,
            attach_before_version_returns_error,
            blocked_read_delivers_response_later,
            clunk_known_fid_returns_rclunk_and_calls_serve9p,
            clunk_unknown_fid_returns_error,
            connection_close_clunks_all_open_fids,
            create_in_directory_returns_rcreate,
            create_masks_permissions_before_call,
            create_on_non_directory_returns_error,
            create_with_dot_name_returns_error,
            create_with_double_dot_name_returns_error,
            duplicate_attach_returns_error,
            flush_returns_rflush,
            flush_pending_request_calls_filesystem_flush,
            flush_waits_for_blocked_read,
            open_known_fid_returns_ropen,
            open_unknown_fid_returns_error,
            read_dir_returns_serialized_stats,
            read_file_returns_data,
            remove_known_fid_returns_rremove,
            stat_known_fid_returns_rstat,
            stat_unknown_fid_returns_error,
            version_sets_negotiated_msize,
            version_while_attached_clunks_all_open_fids,
            walk_first_element_missing_returns_error,
            walk_partial_returns_partial_qids,
            walk_open_fid_returns_error_after_create,
            walk_open_fid_returns_error_after_open,
            walk_to_known_child_returns_qids,
            walk_unknown_fid_returns_error,
            write_to_directory_returns_error,
            write_to_file_returns_byte_count,
            write_with_oversized_offset_returns_error,
        );
    };

    // generate sync test suite
    (@cases sync, $run_one:ident; $($case:ident),+ $(,)?) => {
        $(
            #[test]
            fn $case() { $run_one($crate::test_utils::server_cases::$case()); }
        )+
    };

    // generate tokio test suite
    (@cases tokio, $run_one:ident; $($case:ident),+ $(,)?) => {
        $(
            #[tokio::test]
            async fn $case() { $run_one($crate::test_utils::server_cases::$case()).await; }
        )+
    };
}

// Test cases for use with the generate_server_test_suite macro above

pub(crate) fn version_sets_negotiated_msize() -> TestCase {
    vec![Step::Request {
        tag: 0,
        req: Tdata::version(DEFAULT_MSIZE / 2, SUPPORTED_VERSION),
        resp: Rdata::version(DEFAULT_MSIZE / 2, SUPPORTED_VERSION),
    }]
}

pub(crate) fn attach_before_version_returns_error() -> TestCase {
    vec![
        Step::err(
            0,
            Tdata::attach(0, AFID_NO_AUTH, "owner", "/"),
            E_NO_VERSION_MESSAGE,
        ),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn attach_after_version_succeeds() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn duplicate_attach_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(
            2,
            Tdata::attach(1, AFID_NO_AUTH, "owner", "/"),
            E_ALREADY_ATTACHED,
        ),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn version_while_attached_clunks_all_open_fids() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::open_req(3, 1, Mode::READ, HELLO_QID),
        Step::version_req(4),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::READ, "owner"),
            Call::clunk(ClientId(0), ROOT_QID),
            Call::clunk(ClientId(0), HELLO_QID),
        ]),
    ]
}

pub(crate) fn connection_close_clunks_all_open_fids() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::open_req(3, 1, Mode::READ, HELLO_QID),
        Step::CloseStream,
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::READ, "owner"),
            Call::clunk(ClientId(0), ROOT_QID),
            Call::clunk(ClientId(0), HELLO_QID),
        ]),
    ]
}

pub(crate) fn flush_returns_rflush() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::req(2, Tdata::flush(123), Rdata::flush()),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn flush_waits_for_blocked_read() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["blocked"], &[file_qid(BLOCKED_QID)]),
        Step::open_req(3, 1, Mode::READ, BLOCKED_QID),
        Step::req(
            4,
            Tdata::read(1, 0, 4096),
            Rdata::read(BLOCKED_CONTENT.to_vec()),
        ),
        Step::req(5, Tdata::flush(3), Rdata::flush()),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "blocked", "owner"),
            Call::stat(ClientId(0), BLOCKED_QID, "owner"),
            Call::open(ClientId(0), BLOCKED_QID, Mode::READ, "owner"),
            Call::read(ClientId(0), BLOCKED_QID, 0, 4096, "owner"),
        ]),
    ]
}

pub(crate) fn flush_pending_request_calls_filesystem_flush() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["blocked"], &[file_qid(BLOCKED_QID)]),
        Step::open_req(3, 1, Mode::READ, BLOCKED_QID),
        Step::snd(3, Tdata::read(1, 0, 4096)),
        Step::snd(4, Tdata::flush(3)),
        Step::rcv(3, Rdata::read(BLOCKED_CONTENT.to_vec())),
        Step::rcv(4, Rdata::flush()),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "blocked", "owner"),
            Call::stat(ClientId(0), BLOCKED_QID, "owner"),
            Call::open(ClientId(0), BLOCKED_QID, Mode::READ, "owner"),
            Call::read(ClientId(0), BLOCKED_QID, 0, 4096, "owner"),
            Call::flush(ClientId(0), 3),
        ]),
    ]
}

pub(crate) fn walk_to_known_child_returns_qids() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::assert_calls(&[Call::walk(ClientId(0), ROOT_QID, "hello", "owner")]),
    ]
}

pub(crate) fn walk_first_element_missing_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(
            2,
            Tdata::walk(0, 1, &["missing".to_string()]),
            E_UNKNOWN_FILE,
        ),
        Step::assert_calls(&[Call::walk(ClientId(0), ROOT_QID, "missing", "owner")]),
    ]
}

pub(crate) fn walk_partial_returns_partial_qids() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["subdir", "missing"], &[dir_qid(SUBDIR_QID)]),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "subdir", "owner"),
            Call::walk(ClientId(0), SUBDIR_QID, "missing", "owner"),
        ]),
    ]
}

pub(crate) fn walk_open_fid_returns_error_after_open() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::req(
            3,
            Tdata::open(1, 0),
            Rdata::open(file_qid(HELLO_QID), TEST_IOUNIT),
        ),
        Step::err(4, Tdata::walk(1, 2, &[]), E_WALK_OPEN_FID),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::READ, "owner"),
        ]),
    ]
}

pub(crate) fn walk_open_fid_returns_error_after_create() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::req(
            2,
            Tdata::create(0, "new.txt", perm.bits(), 0),
            Rdata::create(file_qid(CREATED_QID), TEST_IOUNIT),
        ),
        Step::err(3, Tdata::walk(0, 1, &[]), E_WALK_OPEN_FID),
        Step::assert_calls(&[
            Call::stat(ClientId(0), ROOT_QID, "owner"),
            Call::create(
                ClientId(0),
                ROOT_QID,
                "new.txt",
                Perm::OWNER_READ,
                Mode::READ,
                "owner",
            ),
        ]),
    ]
}

pub(crate) fn walk_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(2, Tdata::walk(99, 1, &["hello".to_string()]), E_UNKNOWN_FID),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn clunk_known_fid_returns_rclunk_and_calls_serve9p() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::req(3, Tdata::clunk(1), Rdata::clunk()),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::clunk(ClientId(0), HELLO_QID),
        ]),
    ]
}

pub(crate) fn clunk_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(2, Tdata::clunk(99), E_UNKNOWN_FID),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn open_known_fid_returns_ropen() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::open_req(3, 1, Mode::READ, HELLO_QID),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::READ, "owner"),
        ]),
    ]
}

pub(crate) fn open_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(2, Tdata::open(99, 0), E_UNKNOWN_FID),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn read_file_returns_data() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::open_req(3, 1, Mode::READ, HELLO_QID),
        Step::req(
            4,
            Tdata::read(1, 0, 5),
            Rdata::read(HELLO_CONTENT[..5].to_vec()),
        ),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::READ, "owner"),
            Call::read(ClientId(0), HELLO_QID, 0, 5, "owner"),
        ]),
    ]
}

pub(crate) fn read_dir_returns_serialized_stats() -> TestCase {
    let s1: RawStat = Stat::stub(FileMeta::file("hello", HELLO_QID, Perm::empty())).into();
    let s2: RawStat = Stat::stub(FileMeta::dir("subdir", SUBDIR_QID, Perm::empty())).into();
    let mut buf = s1.write_9p_bytes().unwrap();
    buf.extend(s2.write_9p_bytes().unwrap());

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::open_dir_req(2, 0, Mode::READ, ROOT_QID),
        Step::req(3, Tdata::read(0, 0, 4096), Rdata::read(buf)),
        Step::assert_calls(&[
            Call::stat(ClientId(0), ROOT_QID, "owner"),
            Call::open(ClientId(0), ROOT_QID, Mode::READ, "owner"),
            Call::read_dir(ClientId(0), ROOT_QID, "owner"),
        ]),
    ]
}

pub(crate) fn write_to_file_returns_byte_count() -> TestCase {
    let payload = b"abc".to_vec();

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::open_req(3, 1, Mode::WRITE, HELLO_QID),
        Step::req(4, Tdata::write(1, 0, payload.clone()), Rdata::write(3)),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::WRITE, "owner"),
            Call::write(ClientId(0), HELLO_QID, 0, payload, "owner"),
        ]),
    ]
}

pub(crate) fn write_with_oversized_offset_returns_error() -> TestCase {
    let offset = u64::from(u32::MAX) + 1;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::open_req(3, 1, Mode::WRITE, HELLO_QID),
        Step::err(
            4,
            Tdata::write(1, offset, vec![1]),
            format!("offset too large: {offset} > {}", u32::MAX),
        ),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
            Call::open(ClientId(0), HELLO_QID, Mode::WRITE, "owner"),
        ]),
    ]
}

pub(crate) fn stat_known_fid_returns_rstat() -> TestCase {
    let stat: RawStat = Stat::stub(FileMeta::file("hello", HELLO_QID, Perm::empty())).into();
    let size = stat.n_bytes() as u16;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::req(3, Tdata::stat(1), Rdata::stat(size, stat)),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::stat(ClientId(0), HELLO_QID, "owner"),
        ]),
    ]
}

pub(crate) fn stat_unknown_fid_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(2, Tdata::stat(99), E_UNKNOWN_FID),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn create_in_directory_returns_rcreate() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::req(
            2,
            Tdata::create(0, "new.txt", perm.bits(), 0),
            Rdata::create(file_qid(CREATED_QID), TEST_IOUNIT),
        ),
        Step::assert_calls(&[
            Call::stat(ClientId(0), ROOT_QID, "owner"),
            Call::create(
                ClientId(0),
                ROOT_QID,
                "new.txt",
                Perm::OWNER_READ,
                Mode::READ,
                "owner",
            ),
        ]),
    ]
}

pub(crate) fn create_masks_permissions_before_call() -> TestCase {
    let requested = Perm::OWNER_READ
        | Perm::OWNER_WRITE
        | Perm::GROUP_READ
        | Perm::GROUP_WRITE
        | Perm::OTHER_READ
        | Perm::OTHER_WRITE;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::req(
            2,
            Tdata::create(0, "masked.txt", requested.bits(), 0),
            Rdata::create(file_qid(CREATED_QID), TEST_IOUNIT),
        ),
        Step::assert_calls(&[
            Call::stat(ClientId(0), ROOT_QID, "owner"),
            Call::create(
                ClientId(0),
                ROOT_QID,
                "masked.txt",
                Perm::OWNER_READ,
                Mode::READ,
                "owner",
            ),
        ]),
    ]
}

pub(crate) fn create_with_dot_name_returns_error() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(
            2,
            Tdata::create(0, ".", perm.bits(), 0),
            E_ILLEGAL_CREATE_NAME,
        ),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn create_with_double_dot_name_returns_error() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(
            2,
            Tdata::create(0, "..", perm.bits(), 0),
            E_ILLEGAL_CREATE_NAME,
        ),
        Step::assert_calls(&[]),
    ]
}

pub(crate) fn create_on_non_directory_returns_error() -> TestCase {
    let perm = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ;

    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::err(
            3,
            Tdata::create(1, "child", perm.bits(), 0),
            E_CREATE_NON_DIR,
        ),
        Step::assert_calls(&[Call::walk(ClientId(0), ROOT_QID, "hello", "owner")]),
    ]
}

pub(crate) fn remove_known_fid_returns_rremove() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["hello"], &[file_qid(HELLO_QID)]),
        Step::req(3, Tdata::remove(1), Rdata::remove()),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "hello", "owner"),
            Call::remove(ClientId(0), HELLO_QID, "owner"),
            Call::clunk(ClientId(0), HELLO_QID),
        ]),
    ]
}

pub(crate) fn blocked_read_delivers_response_later() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::walk_req(2, 0, 1, &["blocked"], &[file_qid(BLOCKED_QID)]),
        Step::open_req(3, 1, Mode::READ, BLOCKED_QID),
        Step::req(
            4,
            Tdata::read(1, 0, 4096),
            Rdata::read(BLOCKED_CONTENT.to_vec()),
        ),
        Step::assert_calls(&[
            Call::walk(ClientId(0), ROOT_QID, "blocked", "owner"),
            Call::stat(ClientId(0), BLOCKED_QID, "owner"),
            Call::open(ClientId(0), BLOCKED_QID, Mode::READ, "owner"),
            Call::read(ClientId(0), BLOCKED_QID, 0, 4096, "owner"),
        ]),
    ]
}

pub(crate) fn write_to_directory_returns_error() -> TestCase {
    vec![
        Step::version_req(0),
        Step::attach_req(1),
        Step::err(3, Tdata::open(0, Mode::WRITE.bits()), E_PERMISSION_DENIED),
        Step::assert_calls(&[Call::stat(ClientId(0), ROOT_QID, "owner")]),
    ]
}
