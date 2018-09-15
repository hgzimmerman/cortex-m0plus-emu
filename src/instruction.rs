use register::LowRegisterIdent;
use register::LowRegisterOrI8Ident;
use register::RegisterIdent;
use register::Zero;
use label::NumLabel;

//use std::collections::BitVec;
use bit_vec::BitVec;
use register::Immediate8;

/// The naming convention will specify both non-s and s variants.
/// Instructions without any suffixes will typically operate on registers, while those that operate on immediate values will have a suffix saying so.
// TODO what about directives?
// TODO could this be represented as bytecode? Just an into/from -> [u8; 4] w/ a statemachine to read it?
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    Nop,
    Add(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    Adds(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    AddImmediate8(LowRegisterIdent, Immediate8),
    AddsImmediate8(LowRegisterIdent, Immediate8),
    Sub(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    Subs(LowRegisterIdent, LowRegisterIdent, LowRegisterIdent),
    Rsbs(LowRegisterIdent, LowRegisterIdent, Zero),
    Mov(LowRegisterIdent, LowRegisterIdent),
    Movs(LowRegisterIdent, LowRegisterIdent),
    MovImmediate8(LowRegisterIdent, Immediate8),
    MovsImmediate8(LowRegisterIdent, Immediate8),
    Cmp(LowRegisterIdent, LowRegisterIdent),
    B(NumLabel)
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
                const ADD_OP: u8 = 0b00110_000;
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[ADD_OP, 0]);
                src_dest.encode_to_bitvector(&mut bv, 5);
                immediate.encode_to_bitvector(&mut bv, 8);
                return bv
            }
//            Add()
           _ => unimplemented!()
        }
    }
}

impl From<BitVec> for Instruction {
    fn from(_: BitVec) -> Self {
        unimplemented!()
    }
}