fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=data/plumbing_tests");
    println!("cargo::rerun-if-changed=data/help-template.txt");
}
