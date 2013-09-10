// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
 * Simple metrics gathering and reporting
 *
 * This is inspired from https://github.com/rcrowley/go-metrics and
 * https://github.com/codahale/metrics.
 */

use std::local_data;
use std::hashmap::HashMap;
use std::container::MutableMap;


condition! {
    pub wrong_metric_type : ~str -> ();
}

/// A group of metrics with a common prefix, using TLS for storage.
///
/// Use `metrics!(M)` to declare a static `Metrics` value named `M`,
/// prefixed by the current module path. The TLS map containing each metric
/// associated with `M` will be populated automatically in response to
/// method calls on the static `M`, such as `M.inc_counter("foo", 1)`.
///
/// Declaring a static `Metrics` value this way incurs zero runtime cost
/// until actual metrics are recorded in it.
pub struct Metrics {
    priv prefix: &'static str,
}

impl Metrics {

    /// Borrows a `Counter` temporarily with a given `name`, scoped under
    /// the `prefix` of this `Metrics` instance, and runs callback `f` on
    /// that `Counter`. Creates the `Counter` if it does not exist. Raises
    /// `wrong_metric_type` if the name is already in use for some
    /// non-`Counter` metric.
    pub fn with_counter(&self, name: &str, f: &fn(&mut Counter)) {
        do tls_add_or_modify(self.prefix, name,
                             || Counter(Counter::new())) |m| {
            match *m {
                Counter(ref mut c) => f(c),
                _ => wrong_metric_type::cond.raise(name.to_owned())
            }
        }
    }

    /// Convenience method to increment a `Counter` as would be
    /// borrowed using `with_counter`.
    pub fn inc_counter(&self, name: &str, n: i64) {
        do self.with_counter(name) |c| {
            c.inc(n)
        }
    }
}


// Private enum for storing multiple metric types in TLS.
enum Metric {
    Counter(Counter),
    Meter(Meter),
    Timer(Timer)
}

// TLS interface
static tls_key: local_data::Key<HashMap<~str,~HashMap<~str, Metric>>> = &local_data::Key;

fn tls_with_registry(f: &fn(reg: &mut HashMap<~str, ~HashMap<~str, Metric>>)) {
    let new = do local_data::get_mut(tls_key) |r| {
        match r {
            None => true,
            Some(r) => { f(r); false }
        }
    };
    if new {
        let mut h = HashMap::new();
        f(&mut h);
        local_data::set(tls_key, h)
    }
}

fn tls_with_prefix(prefix: &str, f: &fn(mg: &mut ~HashMap<~str, Metric>)) {
    do tls_with_registry |reg| {
        let new = match reg.find_mut_equiv(&prefix) {
            None => true,
            Some(mg) => { f(mg); false }
        };
        if new {
            let mut mg = ~HashMap::new();
            f(&mut mg);
            reg.insert(prefix.to_owned(), mg);
        }
    }
}

fn tls_add_or_modify(prefix: &str,
                     name: &str,
                     create: &fn() -> Metric,
                     existing: &fn(m: &mut Metric)) {
    do tls_with_prefix(prefix) |mg| {
        let new = match mg.find_mut_equiv(&name) {
            None => true,
            Some(m) => { existing(m); false }
        };
        if new {
            let mut m = create();
            existing(&mut m);
            mg.insert(name.to_owned(), m);
        }
    }
}

/// A counter which increments and decrements over time
pub struct Counter {
    priv val: i64
}

// xxx: wrapping
impl Counter {
    /// Create a new counter
    pub fn new   ()      -> Counter      { Counter { val: 0 } }
    /// Get the value of the counter
    pub fn get   (&self) -> i64          { self.val }
    /// Set the counter to 0
    pub fn reset (&mut self)             { self.val = 0; }
    /// Increment the counter by `value`
    pub fn inc   (&mut self, value: i64) { self.val += value; }
    /// Decrement the counter by `value`
    pub fn dec   (&mut self, value: i64) { self.val -= value; }
}

/// A metric that measures the rate at which some event is occurring.
/// Stores exponentially-weighted moving averages.
pub struct Meter {
    priv ignore: ()
}

/// Combines a Histogram of an event's duration with a Meter
/// of the rate of occurrence.
pub struct Timer {
    priv ignore: ()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_counter() {
        let mut c = Counter::new();
        assert_eq!(c.get(), 0);
        c.inc(5);
        assert_eq!(c.get(), 5);
        c.dec(2);
        assert_eq!(c.get(), 3);
        c.reset();
        assert_eq!(c.get(), 0);
    }

    #[test]
    fn test_tls() {

        metrics!(M);

        M.inc_counter("counter", 1);
        M.inc_counter("counter", 1);
        M.inc_counter("counter", 1);

        do M.with_counter("counter") |c| {
            assert_eq!(c.get(), 3);
        }
    }
}
