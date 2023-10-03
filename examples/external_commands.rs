use ad::util::pipe_through_command;

fn main() {
    let s = pipe_through_command("wc", std::iter::empty::<&str>(), "this is a string").unwrap();
    println!("OUTPUT: '{s}'");
}
