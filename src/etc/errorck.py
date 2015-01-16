# Copyright 2015 The Rust Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution and at
# http://rust-lang.org/COPYRIGHT.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

# Digs error codes out of files named 'diagnostics.rs' across
# the tree, and ensures thare are no duplicates.

import os, re

src_dir = sys.argv[1]

errcode_map = { }

for (dirpath, dirnames, filenames) in os.walk(src_dir):

    if "src/test" in dirpath or "src/llvm" in dirpath:
        # Short circuit for fast
        continue

    for filename in filenames:
        if filename != "diagnostics.rs":
            continue

        path = os.path.join(dirpath, filename)
        line_num = 1
        with open(path, 'r') as f:
            for line in f:
                
                p = re.compile("(E\d\d\d\d)")
                m = p.search(line)
                if not m is None:
                    errcode = m.group(1)

                    new_record = [(errcode, path, line_num, line)]
                    existing = errcode_map.get(errcode)
                    if existing is not None:
                        # This is a dupe
                        errcode_map[errcode] = existing + new_record
                    else:
                        errcode_map[errcode] = new_record

                line_num += 1

errors = False
for errcode in errcode_map:
    entries = errcode_map[errcode]
    if len(entries) > 1:
        print "error: duplicate error code " + errcode
        for entry in entries:
            print entries[1] + ": " + entries[2]
            print entries[3]
        errors = True

print str(len(errcode_map)) + " error codes"

if errors:
    sys.exit(1)
