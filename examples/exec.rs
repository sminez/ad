// A minimal example of the exec language functionality
use ad::exec::{Error, Program};
use ropey::Rope;
use std::{
    env, fs,
    io::{self, Write},
};

fn main() -> Result<(), Error> {
    let mut args = env::args().skip(1);
    let mut prog = Program::try_parse(&args.next().expect("no program provided"))?;
    let mut buf = vec![];

    for path in args {
        let s = fs::read_to_string(&path).expect("unable to open file");
        prog.execute(&mut Rope::from_str(&s), &path, &mut buf)?;
    }

    io::stdout().write_all(&buf).unwrap();

    Ok(())
}
