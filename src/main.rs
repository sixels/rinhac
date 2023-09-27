use std::{
    io::{self, Write},
    process::Command,
};

use rinhac::{ast, compiler::Rinhac};

fn main() {
    let ast_file_path = std::env::args()
        .nth(1)
        .unwrap_or(String::from("/var/rinha/source.rinha.json"));
    let ast_file = std::fs::File::open(ast_file_path).expect("failed to open file");

    let ast: ast::File = serde_json::from_reader(&ast_file).expect("failed to parse ast file");

    Rinhac::compile(ast);

    Command::new("cc")
        .arg("-o")
        .arg("output.out")
        .arg("output.o")
        .arg("./core/librinha_core.so")
        .output()
        .expect("failed to link file");
    let out = Command::new("./output.out")
        .output()
        .expect("failed to run file");

    io::stdout().write_all(&out.stdout).unwrap();
    io::stderr().write_all(&out.stderr).unwrap();
}
