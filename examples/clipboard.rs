use ad::util::{read_clipboard, set_clipboard};

fn main() {
    println!("{}", read_clipboard().unwrap());
    set_clipboard("this is a test").unwrap();
    std::thread::sleep(std::time::Duration::from_millis(300));
    println!("{}", read_clipboard().unwrap());
}
