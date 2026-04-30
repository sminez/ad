use crate::{
    fs::{Mode, Perm, Qid, Stat},
    sansio::{
        client::{Error, Result},
        server::{E_PERMISSION_DENIED, E_UNKNOWN_FID, E_UNKNOWN_FILE, E_UNKNOWN_ROOT},
    },
    test_utils::{HELLO_QID, SUBDIR_QID, SUBFILE_QID, perm_file_stat},
};
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
    Clunk {
        fid: u32,
        res: Result<()>,
    },
    Create {
        dir: &'static str,
        name: &'static str,
        perms: Perm,
        mode: Mode,
        res: Result<()>,
    },
    Walk {
        path: &'static str,
        res: Result<u32>,
    },
    Read {
        path: &'static str,
        res: Result<&'static str>,
    },

    ReadFrom {
        path: &'static str,
        offset: u64,
        count: u32,
        res: Result<Vec<u8>>,
    },

    ReadDir {
        path: &'static str,
        res: Result<Vec<Stat>>,
    },
    Remove {
        path: &'static str,
        res: Result<()>,
    },
    Write {
        path: &'static str,
        offset: u64,
        content: &'static [u8],
        res: Result<usize>,
    },
    AssertState {
        next_fid: u32,
        fids: HashMap<String, u32>,
    },
}

impl Step {
    fn connect_valid() -> Self {
        Step::Connect {
            uname: "owner",
            aname: "/",
            res: Ok(()),
        }
    }

    fn connect(uname: &'static str, aname: &'static str, res: Result<()>) -> Self {
        Step::Connect { uname, aname, res }
    }

    fn clunk(fid: u32, res: Result<()>) -> Self {
        Step::Clunk { fid, res }
    }

    fn create(
        dir: &'static str,
        name: &'static str,
        perms: Perm,
        mode: Mode,
        res: Result<()>,
    ) -> Self {
        Self::Create {
            dir,
            name,
            perms,
            mode,
            res,
        }
    }

    fn walk(path: &'static str, res: Result<u32>) -> Self {
        Step::Walk { path, res }
    }

    fn read(path: &'static str, res: Result<&'static str>) -> Self {
        Step::Read { path, res }
    }

    fn read_from(path: &'static str, offset: u64, count: u32, res: Result<Vec<u8>>) -> Self {
        Step::ReadFrom {
            path,
            offset,
            count,
            res,
        }
    }

    fn read_dir(path: &'static str, res: Result<Vec<Stat>>) -> Self {
        Step::ReadDir { path, res }
    }

    fn remove(path: &'static str, res: Result<()>) -> Self {
        Step::Remove { path, res }
    }

    fn write(path: &'static str, offset: u64, content: &'static [u8], res: Result<usize>) -> Self {
        Step::Write {
            path,
            offset,
            content,
            res,
        }
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
            clunk_open_file_clears_fid_cache,
            clunk_unknown_file_errors,
            connect_to_known_aname_succeeds,
            connect_to_unknown_aname_errors,
            create_dir_succeeds,
            create_file_succeeds,
            read_from_beyond_eof_is_empty,
            read_from_known_file_works,
            read_from_partial_to_eof_works,
            read_from_unknown_file_errors,
            read_known_file_works,
            read_root_dir_works,
            read_subdir_works,
            read_unknown_dir_errors,
            read_unknown_file_errors,
            remove_clears_fid_cache,
            repeated_read_works,
            walk_dot_is_root,
            walk_empty_path_is_root,
            walk_same_path_doesnt_alter_next_fid,
            walk_to_known_file_succeeds,
            walk_to_root_succeeds,
            walk_to_unknown_entry_errors,
            write_without_permission_fails,
            write_with_permission_succeeds,
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
        Step::connect("owner", "unknown", Err(Error::r(E_UNKNOWN_ROOT))),
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

// From the spec:
//   It is legal for nwname to be zero, in which case newfid will represent the same file as fid
//   and the walk will usually succeed; this is equivalent to walking to dot. The rest of this
//   discussion assumes nwname is greater than zero.
pub(crate) fn walk_empty_path_is_root() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk("", Ok(0)),
        // shouldn't alter next_fid
        Step::assert_state(1, &[("/", 0)]),
    ]
}

pub(crate) fn walk_dot_is_root() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk(".", Ok(0)),
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
        Step::walk("/missing", Err(Error::r(E_UNKNOWN_FILE))),
        Step::assert_state(2, &[("/", 0)]),
    ]
}

pub(crate) fn clunk_open_file_clears_fid_cache() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk("/hello", Ok(1)),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
        Step::clunk(1, Ok(())),
        Step::assert_state(2, &[("/", 0)]),
    ]
}

pub(crate) fn clunk_unknown_file_errors() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::clunk(42, Err(Error::r(E_UNKNOWN_FID))),
        Step::assert_state(1, &[("/", 0)]),
    ]
}

pub(crate) fn read_known_file_works() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read("/hello", Ok("hello world")),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn read_from_known_file_works() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_from("/hello", 6, 5, Ok(b"world".to_vec())),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn read_from_partial_to_eof_works() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_from("/hello", 8, 10, Ok(b"rld".to_vec())),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn read_from_beyond_eof_is_empty() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_from("/hello", 64, 10, Ok(vec![])),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn read_from_unknown_file_errors() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_from("/not/a/known/file", 0, 8, Err(Error::r(E_UNKNOWN_FILE))),
        Step::assert_state(2, &[("/", 0)]),
    ]
}

pub(crate) fn repeated_read_works() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read("/hello", Ok("hello world")),
        Step::read("/hello", Ok("hello world")),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn read_unknown_file_errors() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read("/not/a/known/file", Err(Error::r(E_UNKNOWN_FILE))),
        Step::assert_state(2, &[("/", 0)]),
    ]
}

pub(crate) fn read_root_dir_works() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_dir(
            "/",
            Ok(vec![
                Stat::stub(Qid::file(HELLO_QID), "hello"),
                Stat::stub(Qid::dir(SUBDIR_QID), "subdir"),
            ]),
        ),
        Step::assert_state(1, &[("/", 0)]),
    ]
}

pub(crate) fn read_subdir_works() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_dir(
            "/subdir",
            Ok(vec![
                Stat::stub(Qid::file(SUBFILE_QID), "subfile"),
                perm_file_stat(),
            ]),
        ),
        // should have walked to the subdir
        Step::assert_state(2, &[("/", 0), ("/subdir", 1)]),
    ]
}

pub(crate) fn read_unknown_dir_errors() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::read_dir("/not-a-dir", Err(Error::r(E_UNKNOWN_FILE))),
        Step::assert_state(2, &[("/", 0)]),
    ]
}

pub(crate) fn write_with_permission_succeeds() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::write("/hello", 0, b"data", Ok(4)),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn write_without_permission_fails() -> TestCase {
    assert!(
        !Stat::stub(Qid::file(0), "")
            .perms
            .contains(Perm::OTHER_WRITE),
        "write perms not expected to include 'other'"
    );

    vec![
        Step::connect("not-owner", "/", Ok(())),
        Step::write("/hello", 0, b"data", Err(Error::r(E_PERMISSION_DENIED))),
        // Should still have walked to the file and cached the fid
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
    ]
}

pub(crate) fn create_file_succeeds() -> TestCase {
    let perms = Perm::FILE | Perm::OWNER_READ | Perm::OWNER_WRITE;

    vec![
        Step::connect_valid(),
        Step::create("", "new.txt", perms, Mode::READ, Ok(())),
        Step::assert_state(2, &[("/", 0), ("/new.txt", 1)]),
    ]
}

pub(crate) fn create_dir_succeeds() -> TestCase {
    let perms = Perm::DIRECTORY | Perm::OWNER_READ | Perm::OWNER_WRITE;

    vec![
        Step::connect_valid(),
        Step::create("", "new-dir", perms, Mode::READ, Ok(())),
        Step::assert_state(2, &[("/", 0), ("/new-dir", 1)]),
    ]
}

pub(crate) fn remove_clears_fid_cache() -> TestCase {
    vec![
        Step::connect_valid(),
        Step::walk("/hello", Ok(1)),
        Step::assert_state(2, &[("/", 0), ("/hello", 1)]),
        Step::remove("/hello", Ok(())),
        Step::assert_state(2, &[("/", 0)]),
    ]
}
