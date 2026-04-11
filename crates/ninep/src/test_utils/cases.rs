use crate::{
    sansio::protocol::{Rdata, Tdata},
    test_utils::Call,
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
