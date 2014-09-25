extern crate native;
extern crate libc;
use libc::{c_void, LPVOID, DWORD};
use libc::types::os::arch::extra::LPWSTR;

extern "system" {
    fn FormatMessageW(flags: DWORD,
                      lpSrc: LPVOID,
                      msgId: DWORD,
                      langId: DWORD,
                      buf: LPWSTR,
                      nsize: DWORD,
                      args: *const c_void)
                      -> DWORD;

    fn GetLastError() -> u32;
}

fn test() {
    let mut buf: [u16, ..50] = [0, ..50];
    let ret = unsafe {
        FormatMessageW(0x1000, 0 as *mut c_void, 1, 0x400,
                       buf.as_mut_ptr(), buf.len() as u32, 0 as *const c_void)
    };
    assert!(ret != 0);
    if ret == 0 {
        let err = unsafe { GetLastError() };
        println!("err: {}", err);
    }
    let s = std::str::from_utf16(buf);
    println!("{}", s);
    fail!();
}
fn main() {
    if cfg!(spawn) {
        spawn(proc() {
            test();
        });
    } else {
        test();
    }
}

#[start]
pub fn start(argc: int, argv: *const *const u8) -> int {
    if cfg!(green) {
        //green::start(argc, argv, rustuv::event_loop, main)
        0
    } else if cfg!(raw) {
        main();
        0
    } else {
        native::start(argc, argv, main)
    }
}