use inkwell::values::PointerValue;

use super::ValueTypeHint;

#[derive(Debug, Clone, Copy)]
pub struct Tuple<'ctx> {
    pub first_ptr: PointerValue<'ctx>,
    pub second_ptr: PointerValue<'ctx>,
    pub first_ty: ValueTypeHint,
    pub second_ty: ValueTypeHint,
}
