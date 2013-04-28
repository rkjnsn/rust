// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Logging

pub mod rustrt {
    use libc;

    pub extern {
        unsafe fn rust_log_console_on();
        unsafe fn rust_log_console_off();
        unsafe fn rust_log_str(level: u32,
                               string: *libc::c_char,
                               size: libc::size_t);
    }
}

/// Turns on logging to stdout globally
pub fn console_on() {
    unsafe {
        rustrt::rust_log_console_on();
    }
}

/**
 * Turns off logging to stdout globally
 *
 * Turns off the console unless the user has overridden the
 * runtime environment's logging spec, e.g. by setting
 * the RUST_LOG environment variable
 */
pub fn console_off() {
    unsafe {
        rustrt::rust_log_console_off();
    }
}

#[cfg(notest)]
#[lang="log_type"]
pub fn log_type<T>(level: u32, object: &T) {
    use cast::transmute;
    use io;
    use libc;
    use repr;
    use vec;
    use rt;
    use rt::local_services::unsafe_try_borrow_local_services;

    let bytes = do io::with_bytes_writer |writer| {
        repr::write_repr(writer, object);
    };

    match rt::context() {
        rt::OldTaskContex => {
            unsafe {
                let len = bytes.len() as libc::size_t;
                rustrt::rust_log_str(level, transmute(vec::raw::to_ptr(bytes)), len);
            }
        }
        _ => {
            // XXX: Bad allocation
            let msg = bytes.to_str();
            newsched_log_str(msg);
        }
    }
}

fn newsched_log_str(msg: ~str) {
    unsafe {
        let local = unsafe_try_borrow_local_services();
        match local {
            Some(local) => local.logger.log_str(msg),
            None => {
                use io::WriterUtil;

                let dbg = ::libc::STDERR_FILENO as ::io::fd_t;
                dbg.write_str(s);
                dbg.write_str("\n");
                dbg.flush();
            }
        }
    }
}
