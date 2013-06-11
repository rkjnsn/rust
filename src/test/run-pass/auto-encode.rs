// xfail-fast

// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use EBReader = self::ebml::reader;
use EBWriter = self::ebml::writer;
use std::io;
use self::serialize::{Decodable, Encodable};

struct Point {x: uint, }

impl <__D: serialize::Decoder> serialize::Decodable<__D>
    for Point {
    pub fn decode(__arg_0: &mut __D) -> Point {
        __arg_0.read_struct("Point", 1u,
                            |_d| {
                                Point{x:
                                          _d.read_struct_field("x", 0u, |_d| {
                                                           serialize::Decodable::decode(_d)
                                          }),} })
    }
}

impl <__E: serialize::Encoder> serialize::Encodable<__E>
    for Point {
    pub fn encode(&self, __arg_0: &mut __E) {
        match *self {
            Point{x: ref __self_0_0} =>
            __arg_0.emit_struct("Point", 1u, |_e| {
                _e.emit_struct_field("x", 0u, |_e| {
                    __self_0_0.encode(_e) });
            })
        }
    }
}

pub fn main() {
    let a1 = &Point {x: 3u, };
    let bytes = do io::with_bytes_writer |wr| {
        let mut ebml_w = EBWriter::Encoder(wr);
        a1.encode(&mut ebml_w)
    };
    let d = EBReader::Doc(@bytes);
    let mut decoder = EBReader::Decoder(d);
    let _a2: Point = Decodable::decode(&mut decoder);
}

pub mod serialize {

    pub trait Encoder {
        fn emit_uint(&mut self, v: uint);

        fn emit_struct(&mut self, name: &str, len: uint, f: &fn(&mut Self));
        fn emit_struct_field(&mut self,
                             f_name: &str,
                             f_idx: uint,
                             f: &fn(&mut Self));

    }

    pub trait Decoder {
        fn read_uint(&mut self) -> uint;

        fn read_struct<T>(&mut self,
                          s_name: &str,
                          len: uint,
                          f: &fn(&mut Self) -> T)
            -> T;
        fn read_struct_field<T>(&mut self,
                                f_name: &str,
                                f_idx: uint,
                                f: &fn(&mut Self) -> T)
            -> T;

    }

    pub trait Encodable<S:Encoder> {
        fn encode(&self, s: &mut S);
    }

    pub trait Decodable<D:Decoder> {
        fn decode(d: &mut D) -> Self;
    }

    impl<S:Encoder> Encodable<S> for uint {
        fn encode(&self, s: &mut S) {
            s.emit_uint(*self)
        }
    }

    impl<D:Decoder> Decodable<D> for uint {
        fn decode(d: &mut D) -> uint {
            d.read_uint()
        }
    }

}

pub mod ebml {

    // Simple Extensible Binary Markup Language (ebml) reader and writer on a
    // cursor model. See the specification here:
    //     http://www.matroska.org/technical/specs/rfc/index.html

    // Common data structures
    struct EbmlTag {
        id: uint,
        size: uint,
    }

    struct EbmlState {
        ebml_tag: EbmlTag,
        tag_pos: uint,
        data_pos: uint,
    }

    pub struct Doc {
        data: @~[u8],
        start: uint,
        end: uint,
    }

    pub struct TaggedDoc {
        tag: uint,
        doc: Doc,
    }

    pub enum EbmlEncoderTag {
        EsUint,     // 0
        EsU64,      // 1
        EsU32,      // 2
        EsU16,      // 3
        EsU8,       // 4
        EsInt,      // 5
        EsI64,      // 6
        EsI32,      // 7
        EsI16,      // 8
        EsI8,       // 9
        EsBool,     // 10
        EsChar,     // 11
        EsStr,      // 12
        EsF64,      // 13
        EsF32,      // 14
        EsFloat,    // 15
        EsEnum,     // 16
        EsEnumVid,  // 17
        EsEnumBody, // 18
        EsVec,      // 19
        EsVecLen,   // 20
        EsVecElt,   // 21
        EsMap,      // 22
        EsMapLen,   // 23
        EsMapKey,   // 24
        EsMapVal,   // 25

        EsOpaque,

        EsLabel, // Used only when debugging
    }
    // --------------------------------------

    pub mod reader {
        use super::*;

        use serialize;

        use std::io;

        // ebml reading

        struct Res {
            val: uint,
            next: uint
        }

        pub fn vuint_at(data: &[u8], start: uint) -> Res {
            let a = data[start];
            if a & 0x80u8 != 0u8 {
                return Res {val: (a & 0x7fu8) as uint, next: start + 1u};
            }
            if a & 0x40u8 != 0u8 {
                return Res {val: ((a & 0x3fu8) as uint) << 8u |
                                (data[start + 1u] as uint),
                            next: start + 2u};
            }
            if a & 0x20u8 != 0u8 {
                return Res {val: ((a & 0x1fu8) as uint) << 16u |
                                (data[start + 1u] as uint) << 8u |
                                (data[start + 2u] as uint),
                            next: start + 3u};
            }
            if a & 0x10u8 != 0u8 {
                return Res {val: ((a & 0x0fu8) as uint) << 24u |
                                (data[start + 1u] as uint) << 16u |
                                (data[start + 2u] as uint) << 8u |
                                (data[start + 3u] as uint),
                            next: start + 4u};
            }
            fail!("vint too big");
        }

        pub fn Doc(data: @~[u8]) -> Doc {
            Doc { data: data, start: 0u, end: data.len() }
        }

        pub fn doc_at(data: @~[u8], start: uint) -> TaggedDoc {
            let elt_tag = vuint_at(*data, start);
            let elt_size = vuint_at(*data, elt_tag.next);
            let end = elt_size.next + elt_size.val;
            TaggedDoc {
                tag: elt_tag.val,
                doc: Doc { data: data, start: elt_size.next, end: end }
            }
        }

        pub fn doc_as_u64(d: Doc) -> u64 {
            assert_eq!(d.end, d.start + 8u);
            io::u64_from_be_bytes(*d.data, d.start, 8u)
        }

        pub struct Decoder {
            priv parent: Doc,
            priv pos: uint,
        }

        pub fn Decoder(d: Doc) -> Decoder {
            Decoder {
                parent: d,
                pos: d.start
            }
        }

        impl Decoder {

            fn next_doc(&mut self, exp_tag: EbmlEncoderTag) -> Doc {
                debug!(". next_doc(exp_tag=%?)", exp_tag);
                if self.pos >= self.parent.end {
                    fail!("no more documents in current node!");
                }
                let TaggedDoc { tag: r_tag, doc: r_doc } =
                    doc_at(self.parent.data, self.pos);
                self.pos = r_doc.end;
                r_doc
            }

        }

        impl serialize::Decoder for Decoder {
            fn read_uint(&mut self) -> uint {
                let v = doc_as_u64(self.next_doc(EsUint));
                error!("v: %u", v as uint);
                let max = ::std::uint::max_value as u64;
                error!("m: %u", max as uint);
                if v > max {
                    fail!("uint %? too large for this architecture", v);
                }
                v as uint
            }



            fn read_struct<T>(&mut self,
                              name: &str,
                              _: uint,
                              f: &fn(&mut Decoder) -> T)
                -> T {
                debug!("read_struct(name=%s)", name);
                f(self)
            }

            fn read_struct_field<T>(&mut self,
                                    name: &str,
                                    idx: uint,
                                    f: &fn(&mut Decoder) -> T)
                -> T {
                debug!("read_struct_field(name=%?, idx=%u)", name, idx);
                f(self)
            }

        }
    }

    pub mod writer {
        use super::*;

        use std::io;

        // ebml writing
        pub struct Encoder {
            writer: @io::Writer,
            priv size_positions: ~[uint],
        }

        fn write_sized_vuint(w: @io::Writer, n: uint, size: uint) {
            match size {
                1u => w.write(&[0x80u8 | (n as u8)]),
                2u => w.write(&[0x40u8 | ((n >> 8_u) as u8), n as u8]),
                3u => w.write(&[0x20u8 | ((n >> 16_u) as u8), (n >> 8_u) as u8,
                                n as u8]),
                4u => w.write(&[0x10u8 | ((n >> 24_u) as u8), (n >> 16_u) as u8,
                                (n >> 8_u) as u8, n as u8]),
                _ => fail!("vint to write too big: %?", n)
            };
        }

        fn write_vuint(w: @io::Writer, n: uint) {
            if n < 0x7f_u { write_sized_vuint(w, n, 1u); return; }
            if n < 0x4000_u { write_sized_vuint(w, n, 2u); return; }
            if n < 0x200000_u { write_sized_vuint(w, n, 3u); return; }
            if n < 0x10000000_u { write_sized_vuint(w, n, 4u); return; }
            fail!("vint to write too big: %?", n);
        }

        pub fn Encoder(w: @io::Writer) -> Encoder {
            let size_positions: ~[uint] = ~[];
            Encoder {
                writer: w,
                size_positions: size_positions
            }
        }

        impl Encoder {
            pub fn wr_tagged_bytes(&mut self, tag_id: uint, b: &[u8]) {
                write_vuint(self.writer, tag_id);
                write_vuint(self.writer, b.len());
                self.writer.write(b);
            }

            pub fn wr_tagged_u64(&mut self, tag_id: uint, v: u64) {
                do io::u64_to_be_bytes(v, 8u) |v| {
                    self.wr_tagged_bytes(tag_id, v);
                }
            }
        }

        impl ::serialize::Encoder for Encoder {

            fn emit_uint(&mut self, v: uint) {
                self.wr_tagged_u64(EsUint as uint, v as u64);
            }

            fn emit_struct(&mut self, _: &str, _len: uint, f: &fn(&mut Encoder)) {
                f(self)
            }

            fn emit_struct_field(&mut self,
                                 _name: &str,
                                 _: uint,
                                 f: &fn(&mut Encoder)) {
                f(self)
            }

        }
    }
}