#![no_std]
#![no_main]
#![feature(global_asm, llvm_asm)]

#[macro_use]
mod console;

mod quicksort;

use core::panic::PanicInfo;

global_asm!(include_str!("entry.asm"));

#[no_mangle]
pub static mut SOME_N: u64 = 424242424242;

#[no_mangle]
pub static mut SOME_M: u32 = 200;

#[no_mangle]
pub static mut SOME_BITS: &'static [u8] = &[0x12, 0x34, 0x56, 0x78];

#[no_mangle]
pub static mut DIV_A: u32 = 231;

#[no_mangle]
pub static mut DIV_A_S: i32 = 231;

#[no_mangle]
pub static mut DIV_B: u32 = 40;

#[no_mangle]
pub static mut DIV_B_S: i32 = 40;

#[no_mangle]
pub static mut QS_ARR: [u32; 1000] = [0; 1000];

unsafe fn putchar(c: u8) {
    core::ptr::write_volatile(0xfe000000 as *mut u32, c as u32);
}

unsafe fn cycle() -> u64 {
    let low = core::ptr::read_volatile(0xfe000010 as *mut u32);
    let high = core::ptr::read_volatile(0xfe000014 as *mut u32);
    (low as u64) | ((high as u64) << 32)
}

#[no_mangle]
pub unsafe extern "C" fn rust_main() -> ! {
    // Comment cycle() out for co-simulation
    /*
    let start = cycle();
    println!("Hello world from Rust on Violet.");
    let end = cycle();
    println!("Previous println took {} cycles.", end - start);
    */

    println!("Some const 1: {}", SOME_N);
    println!("Some const 2: {}", SOME_M);

    println!("Mul A*B: {}", DIV_A * DIV_B);

    println!("UDiv A/B: {}", DIV_A / DIV_B);
    println!("URem A/B: {}", DIV_A % DIV_B);

    println!("SDiv A/B: {}", DIV_A_S / DIV_B_S);
    println!("SRem A/B: {}", DIV_A_S % DIV_B_S);

    println!("SDiv -A/B: {}", -DIV_A_S / DIV_B_S);
    println!("SRem -A/B: {}", -DIV_A_S % DIV_B_S);

    println!("SDiv A/-B: {}", DIV_A_S / -DIV_B_S);
    println!("SRem A/-B: {}", DIV_A_S % -DIV_B_S);

    println!("SDiv -A/-B: {}", -DIV_A_S / -DIV_B_S);
    println!("SRem -A/-B: {}", -DIV_A_S % -DIV_B_S);

    populate_qs_arr();

    run_profile(|| {
        quicksort::quicksort(&mut QS_ARR);
    });

    run_profile(|| {
        for _ in 0..1000 {
            let x = DIV_A / DIV_B;
            llvm_asm!("" :: "r" (x) :: "volatile");
        }
    });

    run_profile(|| {
        for i in 0..1000 {
            if i % 2 == 0 {
                llvm_asm!("mul x0, x0, x0" :::: "volatile");
            } else {
                llvm_asm!("nop" :::: "volatile");
            }
        }
    });

    loop {
        llvm_asm!("" :::: "volatile");
    }
}

unsafe fn populate_qs_arr() {
    for (i, v) in QS_ARR.iter_mut().enumerate() {
        *v = (i as u32) % 30;
    }
}

fn run_profile<F: FnOnce() -> R, R>(f: F) -> R {
    let start = rdcycle();
    let instret_start = rdinstret();
    let brhit_start = rd_branch_hits();
    let brmiss_start = rd_branch_misses();

    let ret = f();

    let end = rdcycle();
    let instret_end = rdinstret();
    let brhit_end = rd_branch_hits();
    let brmiss_end = rd_branch_misses();
    println!("cycles: {}", end - start);
    println!("instret: {}", instret_end - instret_start);
    println!("branch hits/misses: {}/{}", brhit_end - brhit_start, brmiss_end - brmiss_start);
    ret
}

fn rdcycle() -> u64 {
    unsafe {
        read_long_counter(|| {
            let x: u32;
            llvm_asm!("rdcycle $0" : "=r" (x));
            x
        }, || {
            let x: u32;
            llvm_asm!("rdcycleh $0" : "=r" (x));
            x
        })
    }
}

fn rdinstret() -> u64 {
    unsafe {
        read_long_counter(|| {
            let x: u32;
            llvm_asm!("rdinstret $0" : "=r" (x));
            x
        }, || {
            let x: u32;
            llvm_asm!("rdinstreth $0" : "=r" (x));
            x
        })
    }
}

fn rd_branch_hits() -> u64 {
    unsafe {
        read_long_counter(|| {
            let x: u32;
            llvm_asm!("csrr $0, 0xc03" : "=r" (x));
            x
        }, || {
            let x: u32;
            llvm_asm!("csrr $0, 0xc83" : "=r" (x));
            x
        })
    }
}

fn rd_branch_misses() -> u64 {
    unsafe {
        read_long_counter(|| {
            let x: u32;
            llvm_asm!("csrr $0, 0xc04" : "=r" (x));
            x
        }, || {
            let x: u32;
            llvm_asm!("csrr $0, 0xc84" : "=r" (x));
            x
        })
    }
}

fn read_long_counter(low_f: fn() -> u32, high_f: fn() -> u32) -> u64 {
    loop {
        let high1 = high_f();
        let low = low_f();
        let high2 = high_f();
        if high1 == high2 {
            return ((high1 as u64) << 32) | (low as u64);
        }
    }
}

#[panic_handler]
fn on_panic(info: &PanicInfo) -> ! {
    loop {}
}
