// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use comm::{GenericPort, GenericChan, Peekable};
use clone::Clone;
use option::{Option, Some, None};
use ops::Drop;
use rt::comm::{SharedChan, Port, stream};
use unstable::atomics::{AtomicUint, SeqCst};

// FIXME #7026: Would prefer this to be an enum
pub struct JoinLatch {
    priv parent: Option<ParentLink>,
    priv child: Option<ChildLink>,
    closed: bool,
}

struct SharedState {
    count: AtomicUint,
    child_success: bool
}

struct ParentLink {
    shared: *mut SharedState,
    chan: SharedChan<Message>
}

struct ChildLink {
    shared: ~SharedState,
    port: Port<Message>,
    chan: SharedChan<Message>,
}

enum Message {
    Tombstone(~JoinLatch),
    ChildrenTerminated
}

impl JoinLatch {
    fn new_root() -> ~JoinLatch {
        ~JoinLatch {
            parent: None,
            child: None,
            closed: false
        }
    }

    fn new_child(&mut self) -> ~JoinLatch {
        rtassert!(!self.closed);

        if self.child.is_none() {
            // This is the first time spawning a child
            let shared = ~SharedState {
                count: AtomicUint::new(1),
                child_success: true
            };
            let (port, chan) = stream();
            let chan = SharedChan::new(chan);
            let child = ChildLink {
                shared: shared,
                port: port,
                chan: chan
            };
            self.child = Some(child);
        }

        let child_link: &mut ChildLink = self.child.get_mut_ref();
        let shared_state: *mut SharedState = &mut *child_link.shared;

        child_link.shared.count.fetch_add(1, SeqCst);

        ~JoinLatch {
            parent: Some(ParentLink {
                shared: shared_state,
                chan: child_link.chan.clone()
            }),
            child: None,
            closed: false
        }
    }

    // XXX: Should not require ~self
    fn release(~self, local_success: bool) {
        rtassert!(!self.closed);

        let mut this = self;
        let mut child_success = true;
        let mut children_done = false;

        if this.child.is_some() {
            rtdebug!("waiting for children");
            let child_link: &mut ChildLink = this.child.get_mut_ref();
            let shared: &mut SharedState = &mut *child_link.shared;

            let last_count = shared.count.fetch_sub(1, SeqCst);
            rtdebug!("child count before sub %u", last_count);
            if last_count == 1 {
                child_link.chan.send(ChildrenTerminated)
            }

            // Wait for messages from children
            loop {
                if child_link.port.peek() {
                    match child_link.port.recv() {
                        Tombstone(t) => {
                            t.release(true);
                        },
                        ChildrenTerminated => {
                            children_done = true;
                            break;
                        }
                    }
                } else {
                    break
                }
            }

            let count = shared.count.load(SeqCst);
            assert!(count == 0);
            // self_count is the acquire-read barrier
            child_success = shared.child_success;
        }

        let total_success = local_success && child_success;

        rtassert!(this.parent.is_some());

        rtdebug!("releasing parent");
        unsafe {
            {
                let parent_link: &mut ParentLink = this.parent.get_mut_ref();
                let shared: *mut SharedState = parent_link.shared;

                if !total_success {
                    // parent_count is the write-wait barrier
                    (*shared).child_success = false;
                }
            }

            if children_done {
                let parent_link: &mut ParentLink = this.parent.get_mut_ref();
                let shared: *mut SharedState = parent_link.shared;
                let last_count = (*shared).count.fetch_sub(1, SeqCst);
                rtdebug!("count before parent sub %u", last_count);
                if last_count == 1 {
                    parent_link.chan.send(ChildrenTerminated);
                    this.closed = true;
                }
            } else {
                let chan = {
                    let parent_link: &mut ParentLink = this.parent.get_mut_ref();
                    parent_link.chan.clone()
                };
                chan.send(Tombstone(this));
            }
        }
    }

    // XXX: Should not require ~self
    fn wait(~self, local_success: bool) -> bool {
        rtassert!(!self.closed);

        let mut this = self;
        let mut child_success = true;

        if this.child.is_some() {
            rtdebug!("waiting for children");
            let child_link: &mut ChildLink = this.child.get_mut_ref();
            let shared: &mut SharedState = &mut *child_link.shared;

            let last_count = shared.count.fetch_sub(1, SeqCst);
            rtdebug!("child count before sub %u", last_count);
            if last_count == 1 {
                child_link.chan.send(ChildrenTerminated)
            }

            // Wait for messages from children
            loop {
                match child_link.port.recv() {
                    Tombstone(t) => {
                        t.wait(true);
                    }
                    ChildrenTerminated => break
                }
            }

            let count = shared.count.load(SeqCst);
            assert!(count == 0);
            // self_count is the acquire-read barrier
            child_success = shared.child_success;
        }

        let total_success = local_success && child_success;

        if this.parent.is_some() {
            rtdebug!("releasing parent");
            unsafe {
                let parent_link: &mut ParentLink = this.parent.get_mut_ref();
                let shared: *mut SharedState = parent_link.shared;

                if !total_success {
                    // parent_count is the write-wait barrier
                    (*shared).child_success = false;
                }

                let last_count = (*shared).count.fetch_sub(1, SeqCst);
                rtdebug!("count before parent sub %u", last_count);
                if last_count == 1 {
                    parent_link.chan.send(ChildrenTerminated);
                }
            }
        }

        this.closed = true;

        return total_success;
    }
}

impl Drop for JoinLatch {
    fn finalize(&self) {
        rtassert!(self.closed);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use cell::Cell;
    use iter::Times;
    use rt::test::*;

    #[test]
    fn success_immediately() {
        do run_in_newsched_task {
            let mut latch = JoinLatch::new_root();

            let child_latch = latch.new_child();
            let child_latch = Cell(child_latch);
            do spawntask_immediately {
                let child_latch = child_latch.take();
                assert!(child_latch.wait(true));
            }

            assert!(latch.wait(true));
        }
    }

    #[test]
    fn success_later() {
        do run_in_newsched_task {
            let mut latch = JoinLatch::new_root();

            let child_latch = latch.new_child();
            let child_latch = Cell(child_latch);
            do spawntask_later {
                let child_latch = child_latch.take();
                assert!(child_latch.wait(true));
            }

            assert!(latch.wait(true));
        }
    }

    #[test]
    fn mt_success() {
        do run_in_mt_newsched_task {
            let mut latch = JoinLatch::new_root();

            for 10.times {
                let child_latch = latch.new_child();
                let child_latch = Cell(child_latch);
                do spawntask_random {
                    let child_latch = child_latch.take();
                    assert!(child_latch.wait(true));
                }
            }

            assert!(latch.wait(true));
        }
    }

    #[test]
    fn mt_failure() {
        do run_in_mt_newsched_task {
            let mut latch = JoinLatch::new_root();

            let spawn = |status| {
                let child_latch = latch.new_child();
                let child_latch = Cell(child_latch);
                do spawntask_random {
                    let child_latch = child_latch.take();
                    child_latch.wait(status);
                }
            };

            for 10.times { spawn(true) }
            spawn(false);
            for 10.times { spawn(true) }

            assert!(!latch.wait(true));
        }
    }

    #[test]
    fn mt_multi_level_success() {
        do run_in_mt_newsched_task {
            let mut latch = JoinLatch::new_root();

            fn child(latch: &mut JoinLatch, i: int) {
                let child_latch = latch.new_child();
                let child_latch = Cell(child_latch);
                do spawntask_random {
                    let mut child_latch = child_latch.take();
                    if i != 0 {
                        child(&mut *child_latch, i - 1);
                        child_latch.wait(true);
                    } else {
                        child_latch.wait(true);
                    }
                }
            }

            child(&mut *latch, 10);

            assert!(latch.wait(true));
        }
    }

    #[test]
    fn mt_multi_level_failure() {
        do run_in_mt_newsched_task {
            let mut latch = JoinLatch::new_root();

            fn child(latch: &mut JoinLatch, i: int) {
                let child_latch = latch.new_child();
                let child_latch = Cell(child_latch);
                do spawntask_random {
                    let mut child_latch = child_latch.take();
                    if i != 0 {
                        child(&mut *child_latch, i - 1);
                        child_latch.wait(false);
                    } else {
                        child_latch.wait(true);
                    }
                }
            }

            child(&mut *latch, 10);

            assert!(!latch.wait(true));
        }
    }
}
