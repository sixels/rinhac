use rinhac::{ast, compiler::Rinhac};

fn main() {
    let ast_file_path = std::env::args().nth(1).expect("no file given");
    let ast_file = std::fs::File::open(ast_file_path).expect("failed to open file");

    let ast: ast::File = serde_json::from_reader(&ast_file).expect("failed to parse ast file");

    Rinhac::compile(ast);
}
