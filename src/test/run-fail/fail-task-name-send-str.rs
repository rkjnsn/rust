// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// error-pattern:task 'send name' failed at 'test'

fn main() {
    let (port, chan) = Chan::new();
    let mut t = ::std::task::task();
    t.name("send name".to_send_str());
    do t.spawn {
        fail!("test");
        chan.send(());
    }
    port.recv();
}
