/// # Safety
///
/// `data` must be a pointer to a valid UTF-8 string otherwise the behavior is undefined.
#[no_mangle]
pub unsafe fn __rinha_print_str(data: *const u8, len: usize) {
    if data.is_null() {
        println!()
    }
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let string = std::str::from_utf8(slice).unwrap();
    println!("{string}");
}

#[no_mangle]
pub fn __rinha_print_int(value: i32) {
    println!("{value}");
}

#[no_mangle]
pub fn __rinha_print_bool(value: bool) {
    println!("{value}");
}
