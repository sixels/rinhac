use inkwell::{basic_block::BasicBlock, values::IntValue};

use crate::compiler::Compiler;

use super::core::CoreFunction;

#[allow(clippy::type_complexity)]
pub fn build_switch<'ctx>(
    compiler: &Compiler<'_, 'ctx>,
    value: IntValue<'ctx>,
    match_map: &[(usize, &dyn Fn(&Compiler<'_, 'ctx>))],
    merge_block: BasicBlock<'ctx>,
) {
    let scope = compiler.scope.borrow();

    // create blocks for each match
    let blocks = match_map
        .iter()
        .map(|(expected, _)| {
            let expected_tag = compiler.context.i64_type().const_int(*expected as _, false);
            (
                expected_tag,
                compiler.context.prepend_basic_block(merge_block, "bb"),
            )
        })
        .collect::<Vec<_>>();
    // create a fallback block
    let else_block = compiler.context.prepend_basic_block(merge_block, "else");

    // create the switch instruction
    compiler.builder.build_switch(value, else_block, &blocks);

    drop(scope);

    // build the blocks
    for ((_, codegen), (_, block)) in match_map.iter().zip(blocks.into_iter()) {
        compiler.builder.position_at_end(block);
        codegen(compiler);
        if block.get_terminator().is_none() {
            compiler.builder.build_unconditional_branch(merge_block);
        }
    }

    compiler.builder.position_at_end(else_block);
    compiler
        .builder
        .build_call(compiler.core_functions.get(CoreFunction::Panic), &[], "");
    compiler.builder.build_unreachable();
}
