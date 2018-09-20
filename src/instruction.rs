use register::LowRegisterIdent;
use register::LowRegisterOrI8Ident;
use register::RegisterIdent;
use register::Zero;
use label::NumLabel;

//use std::collections::BitVec;
use bit_vec::BitVec;
use register::Immediate8;
use register::Immediate11;

/// The naming convention will specify both non-s and s variants.
/// Instructions without any suffixes will typically operate on registers, while those that operate on immediate values will have a suffix saying so.
// TODO what about directives?
// TODO could this be represented as bytecode? Just an into/from -> [u8; 4] w/ a statemachine to read it?
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Instruction {
    Nop,
    Add(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    Adds(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    AddImmediate8(LowRegisterIdent, Immediate8),
    AddsImmediate8(LowRegisterIdent, Immediate8),
    Sub(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    Subs(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    SubImmediate8(LowRegisterIdent, Immediate8),
    SubsImmediate8(LowRegisterIdent, Immediate8),
    Rsbs(LowRegisterIdent, LowRegisterIdent, Zero),
    Mov(LowRegisterIdent, LowRegisterIdent),
    Movs(LowRegisterIdent, LowRegisterIdent),
    MovImmediate8(LowRegisterIdent, Immediate8),
    MovsImmediate8(LowRegisterIdent, Immediate8),
    Cmp(LowRegisterIdent, LowRegisterIdent),
    B(Immediate11)
}



impl Into<BitVec> for Instruction {
    fn into(self) -> BitVec {
        use Instruction::*;
        match self {
            Nop => {
                const NOP_OP: u8 = 0b1011_1111;
                BitVec::from_bytes(&[NOP_OP, 0])
            },
            AddImmediate8(src_dest, immediate) => {
                const ADD_OP: u8 = 0b00110_000; // first 5
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[ADD_OP, 0]);
                src_dest.encode_to_bitvector(&mut bv, 5);
                immediate.encode_to_bitvector(&mut bv, 8);
                return bv
            }
            B(immediate_11) => {
                const B_OP: u8 = 0b11100_000; // first 5
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[B_OP, 0]);
                immediate_11.encode_to_bitvector(&mut bv, 5);
                return bv
            }
//            Add()
           _ => unimplemented!()
        }
    }
}

impl From<BitVec> for Instruction {
    fn from(bv: BitVec) -> Self {
//        let vv: &[u8] = bv.to_bytes().as_slice();
        use util::first_bits_match;
        if first_bits_match(bv.to_bytes().as_slice()[0], 0b11100_000, 5 ) {
            return Instruction::B(Immediate11::decode_from_bitvector(&bv, 5))
        }
        unimplemented!()
    }
}

#[test]
fn encode_decode_b() {
    let bv: BitVec<u32>  = Instruction::B(Immediate11(23)).into();
    assert_eq!(bv.to_bytes()[0], 0b1110_0000);
    assert_eq!(bv.to_bytes()[1], 0b000_10111);
    let i: Instruction = bv.into();

    assert_eq!(i, Instruction::B(Immediate11(23)));
}