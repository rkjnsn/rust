// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use option::{Option, Some, None};
use ops::Drop;
use rt::local::Local;
use rt::sched::{Scheduler, Coroutine};
use unstable::atomics::{AtomicUint, AtomicOption, SeqCst};

pub struct JoinLatch {
    priv parent: Option<*mut (AtomicUint, AtomicOption<Coroutine>, bool)>,
    priv child: Option<~(AtomicUint, AtomicOption<Coroutine>, bool)>,
    closed: bool,
}

impl JoinLatch {
    fn new_root() -> JoinLatch {
        JoinLatch {
            parent: None,
            child: None,
            closed: false
        }
    }

    fn new_child(&mut self) -> JoinLatch {
        rtassert!(!self.closed);

        if self.child.is_none() {
            // This is the first time spawning a child
            self.child = Some(~(AtomicUint::new(1), AtomicOption::empty(), true));
        }

        match *self.child.get_mut_ref() {
            ~(ref mut count, _, _) => {
                count.fetch_add(1, SeqCst);
            }
        }

        let child_ptr: *mut (AtomicUint, AtomicOption<Coroutine>, bool)
            = &mut **self.child.get_mut_ref();

        JoinLatch {
            parent: Some(child_ptr),
            child: None,
            closed: false
        }
    }

    fn release(&mut self, local_success: bool) -> bool {
        rtassert!(!self.closed);

        let mut child_success = true;

        if self.child.is_some() {
            rtdebug!("waiting for children");
            let child_ptr: *mut (AtomicUint, AtomicOption<Coroutine>, bool)
                = &mut **self.child.get_mut_ref();

            // Wait for children
            let sched = Local::take::<Scheduler>();
            do sched.deschedule_running_task_and_then |sched, task| {
                unsafe {
                    match *child_ptr {
                        (ref mut self_count, ref mut self_task, _) => {
                            assert!(self_task.swap(task, SeqCst).is_none());
                            let last_count = self_count.fetch_sub(1, SeqCst);
                            rtdebug!("child count before sub %u", last_count);
                            if last_count == 1 {
                                let task = self_task.take(SeqCst).unwrap();
                                sched.enqueue_task(task);
                            }
                        }
                    }
                }
            }

            unsafe {
                match *child_ptr {
                    (ref mut self_count, _, ref mut child_success_ptr) => {
                        let count = self_count.load(SeqCst);
                        assert!(count == 0);
                        // self_count is the acquire-read barrier
                        child_success = *child_success_ptr;
                    }
                }
            }
        }

        let total_success = local_success && child_success;

        if self.parent.is_some() {
            rtdebug!("releasing parent");
            unsafe {
                match **self.parent.get_mut_ref() {
                    (ref mut parent_count,
                     ref mut parent_task,
                     ref mut peer_success) => {
                        if !total_success {
                            // parent_count is the write-release barrier
                            *peer_success = false;
                        }

                        let last_count = parent_count.fetch_sub(1, SeqCst);
                        rtdebug!("count before parent sub %u", last_count);
                        if last_count == 1 {
                            let parent_task = parent_task.take(SeqCst);
                            let parent_task = parent_task.unwrap();
                            let sched = Local::take::<Scheduler>();
                            sched.schedule_task(parent_task);
                        }
                    }
                }
            }
        }

        self.closed = true;

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
                let mut child_latch = child_latch.take();
                assert!(child_latch.release(true));
            }

            assert!(latch.release(true));
        }
    }

    #[test]
    fn success_later() {
        do run_in_newsched_task {
            let mut latch = JoinLatch::new_root();

            let child_latch = latch.new_child();
            let child_latch = Cell(child_latch);
            do spawntask_later {
                let mut child_latch = child_latch.take();
                assert!(child_latch.release(true));
            }

            assert!(latch.release(true));
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
                    let mut child_latch = child_latch.take();
                    assert!(child_latch.release(true));
                }
            }

            assert!(latch.release(true));
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
                    let mut child_latch = child_latch.take();
                    child_latch.release(status);
                }
            };

            for 10.times { spawn(true) }
            spawn(false);
            for 10.times { spawn(true) }

            assert!(!latch.release(true));
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
                        child(&mut child_latch, i - 1);
                        child_latch.release(true);
                    } else {
                        child_latch.release(true);
                    }
                }
            }

            child(&mut latch, 10);

            assert!(latch.release(true));
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
                        child(&mut child_latch, i - 1);
                        child_latch.release(false);
                    } else {
                        child_latch.release(true);
                    }
                }
            }

            child(&mut latch, 10);

            assert!(!latch.release(true));
        }
    }
}
