// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! The diagnostic registry
//!
//! All diagnostic codes must be registered here. To add a new
//! diagnostic code just go to the end of the file and add a new
//! line with a code that is one greater than the previous.

// B errors are used only by ext/

reg_diag!(B0000)
reg_diag!(B0001)
reg_diag!(B0002)
reg_diag!(B0003)
reg_diag!(B0004)
reg_diag!(B0005)
reg_diag!(B0006)
reg_diag!(B0007)
reg_diag!(B0008)
reg_diag!(B0009)
reg_diag!(B0010)
reg_diag!(B0011)
reg_diag!(B0012)
reg_diag!(B0013)
reg_diag!(B0014)
reg_diag!(B0015)
reg_diag!(B0016)
reg_diag!(B0017)

// C errors are everything else
