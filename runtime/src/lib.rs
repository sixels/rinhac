#[no_mangle]
extern "C" fn __rinha_rt_print_str(data: *const u8, len: usize) {
    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let string = std::str::from_utf8(slice).unwrap();
    println!("{string}");
}

#[no_mangle]
extern "C" fn __rinha_rt_print_int(value: i32) {
    println!("{value}");
}

#[no_mangle]
extern "C" fn __rinha_rt_print_bool(value: bool) {
    println!("{value}");
}
