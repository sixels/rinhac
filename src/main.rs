#![recursion_limit = "9999999999999999999"]

use std::{
    io::{self, BufReader, Write},
    process::Command,
};

use rinhac::{ast, compiler::Rinhac};
use serde::Deserialize;

fn main() {
    let ast_file_path = std::env::args()
        .nth(1)
        .unwrap_or(String::from("/var/rinha/source.rinha.json"));
    let ast_file = std::fs::File::open(ast_file_path).expect("failed to open file");

    let mut dese = serde_json::Deserializer::from_reader(ast_file);
    dese.disable_recursion_limit();
    let ast = ast::File::deserialize(&mut dese).expect("failed to parse ast file");

    // dese.disable_recursion_limit();
    // let ast: ast::File = serde_json::from_reader(dese)

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
