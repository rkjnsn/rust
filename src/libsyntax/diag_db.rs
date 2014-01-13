// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! The diagnostic database.
//!
//! Extended information about Rust diagnostics is included in the
//! diag_db.md file and can be loaded at runtime with the `load`
//! function.

use std::cell::RefCell;
use std::hashmap::HashMap;

/// Load the database of extended diagnostic descriptions
pub fn load() -> DiagnosticDb {
    DiagnosticDb::new(~[load_raw])
}

pub fn load_raw() -> ~[RawInfo] {
    ~[include!("diag_db.md")]
}

pub type RawInfo = (&'static str, &'static str, &'static str);
pub type Initializer = fn() -> ~[RawInfo];

#[deriving(Clone)]
pub struct DiagnosticInfo {
    code: &'static str,
    msg: &'static str,
    desc: &'static str
}

pub struct DiagnosticDb {
    state: RefCell<DiagnosticDbState>
}

pub enum DiagnosticDbState {
    Uninitialized(~[Initializer]),
    Initialized(HashMap<&'static str, DiagnosticInfo>)
}

impl DiagnosticDb {
    pub fn new(initializers: ~[Initializer]) -> DiagnosticDb {
        DiagnosticDb {
            state: RefCell::new(Uninitialized(initializers))
        }
    }

    fn get_map<T>(&self, f: |&mut HashMap<&'static str, DiagnosticInfo>| -> T) -> T {
        let mut new_map;
        {
            let mut state = self.state.borrow_mut();
            match *state.get() {
                Uninitialized(ref initializers) => {
                    let mut map = HashMap::new();
                    for initr in initializers.iter() {
                        let raw_info = (*initr)();
                        for &(code, msg, desc) in raw_info.iter() {
                            let info = DiagnosticInfo { code: code, msg: msg, desc: desc };
                            map.insert(code, info);
                        }
                    }
                    new_map = Some(map);
                }
                Initialized(ref mut map) => {
                    return f(map);
                }
            }
        }

        match new_map {
            Some(new_map) => {
                self.state.set(Initialized(new_map));
                return self.get_map(f);
            }
            None => unreachable!()
        }
    }

    pub fn get_info(&self, code: &str) -> Option<DiagnosticInfo> {
        self.get_map(|map| {
            match map.find_equiv(&code) {
                Some(&info) => Some(info),
                None => None
            }
        })
    }
}

impl DiagnosticInfo {
    /// Returns a markdown-formatted explanation of the diagnostic
    pub fn format(&self) -> ~str {
        format!("\\# {}: {}\n\n{}", self.code, self.msg, self.desc.trim())
    }
}

/// Print extended information about a single diagnostic code to the console.
/// Returns false if the DB contains no information about the code.
pub fn explain_diagnostic(db: &DiagnosticDb, code: &str) -> bool {
    match db.get_info(code) {
        Some(info) => {
            println!("\n{}\n", info.format())
            true
        }
        None => false
    }
}

pub fn explain_diag_help() {
    println!("\nRust includes extended documentation about some compiler errors\n\
             that explain in greater depth what the errors means, present examples,\n\
             and suggestions for how to fix them.\n\
             \n\
             Each Rust error message has a corresponding code. When emitted by\n\
             a tool the code will be included in square brackets, like `[A0001]`. If\n\
             the error has additional documentation the code will be appended with\n\
             an asterisk, as in `[A0002*]`.\n\
             \n\
             To view the extended documentation, run `rustc --explain A0002`, relpacing\n\
             'A0002' with your error code.\n\
             \n\
             The extend and quality of extended error documentation depends on user\n\
             contributions. To learn how to improve Rust's error documentation visit\n\
             http://github.com/mozilla/rust/wiki/Note-extended-diagnostics.\n");
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn smoke_test() {
        fn load() -> ~[RawInfo] {
            ~[("A0000", "message", "description")]
        }

        let db = DiagnosticDb::new(~[load]);
        let info = db.get_info("A0000");
        let text = info.unwrap().format();
        assert!(text.contains("A0000"));
        assert!(text.contains("message"));
        assert!(text.contains("description"));
    }
}
