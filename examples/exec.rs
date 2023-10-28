// A minimal example of the exec language functionality
use ad::exec::{Error, Program};
use ropey::Rope;
use std::{env::args, fs};

fn main() -> Result<(), Error> {
    let args: Vec<String> = args().collect();
    let s = fs::read_to_string(&args[2]).expect("unable to open file");
    let mut r = Rope::from_str(&s);
    let mut prog = Program::try_parse(&args[1], r.len_chars())?;

    prog.execute(&mut r)
}
