#![no_std]
#![no_main]
#![feature(global_asm, llvm_asm)]

#[macro_use]
mod console;

use core::panic::PanicInfo;

global_asm!(include_str!("entry.asm"));

unsafe fn putchar(c: u8) {
    core::ptr::write_volatile(0xfe000000 as *mut u32, c as u32);
}

#[no_mangle]
pub unsafe extern "C" fn rust_main() -> ! {
    println!("Hello world from Rust on Violet!");
    loop {
        llvm_asm!("" :::: "volatile");
    }
}

#[panic_handler]
fn on_panic(info: &PanicInfo) -> ! {
    loop {}
}
