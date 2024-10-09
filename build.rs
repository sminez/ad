use std::{env, fs, path::Path};

// brings in built_in_commands
include!("src/editor/built_in_commands.rs");

fn main() {
    gen_help_docs();

    println!("cargo::rerun-if-changed=build.rs");
    println!("cargo::rerun-if-changed=data/plumbing_tests");
    println!("cargo::rerun-if-changed=data/help-template.txt");
    println!("cargo::rerun-if-changed=src/editor/built_in_commands.rs");
}

/// Pull in data from the ad crate itself to auto-generate the docs on the functionality
/// available in the editor.
fn gen_help_docs() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("help.txt");
    let help_template = include_str!("data/help-template.txt");
    let help_txt = help_template.replace("{{BUILT_IN_COMMANDS}}", &commands_section());

    fs::write(&dest_path, help_txt).unwrap();
}

fn commands_section() -> String {
    let commands = built_in_commands();
    let mut buf = Vec::with_capacity(commands.len());

    for (cmds, desc) in commands.into_iter() {
        buf.push((cmds.join(" | "), desc));
    }

    let w_max = buf.iter().map(|(s, _)| s.len()).max().unwrap();
    let mut s = String::new();

    for (cmds, desc) in buf.into_iter() {
        s.push_str(&format!("{:width$} -- {desc}\n", cmds, width = w_max));
    }

    s
}
