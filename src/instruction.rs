use register::LowRegisterIdent;
use register::LowRegisterOrI8Ident;
use register::RegisterIdent;
use register::Zero;
use label::NumLabel;

//use std::collections::BitVec;
use bit_vec::BitVec;
use register::Immediate8;
use register::Immediate11;

use register::ByteCodeEncodable;
use std::convert::TryFrom;
use std::convert::TryInto;

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
            Movs(dest, rhs) => {
                // Encoding T2
                const MOVS_OP: u8 = 0;
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[MOVS_OP, 0]);
                rhs.encode_to_bitvector(&mut bv, 10);
                dest.encode_to_bitvector(&mut bv, 13);
                bv
            }
            MovsImmediate8(dest, immediate) => {
                // Encoding T2
                const MOVS_IMM_OP: u8 = 0b00100_000;
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[MOVS_IMM_OP, 0]);
                dest.encode_to_bitvector(&mut bv, 5);
                immediate.encode_to_bitvector(&mut bv, 8);
                bv
            }
            Adds(dest, lhs, rhs) => {
                const ADDS_REG_OP: u8 = 0b0001100_0;
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[ADDS_REG_OP, 0]);
                rhs.encode_to_bitvector(&mut bv, 7);
                lhs.encode_to_bitvector(&mut bv, 10);
                dest.encode_to_bitvector(&mut bv,13);
                return bv
            }
            AddImmediate8(src_dest, immediate) => {
                const ADD_OP: u8 = 0b00110_000; // first 5
                let mut bv: BitVec<u32> = BitVec::from_bytes(&[ADD_OP, 0]);
                src_dest.encode_to_bitvector(&mut bv, 5);
                immediate.encode_to_bitvector(&mut bv, 8);
                return bv
            }
            AddsImmediate8(src_dest, immediate) => {
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

impl TryFrom<BitVec> for Instruction {
    type Error = ();

    fn try_from(bv: BitVec) -> Result<Self, ()> {
        use util::first_bits_match;

        let byte_0: u8 = bv.to_bytes().as_slice()[0];
        let byte_1: u8 = bv.to_bytes().as_slice()[1];
        let instruction: Instruction
        = if first_bits_match(byte_0, 0b11100_000, 5 ) {
            Instruction::B(Immediate11::decode_from_bitvector(&bv, 5))
        } else if first_bits_match(byte_0,0, 8) && first_bits_match(byte_1, 0, 2){
            Instruction::Movs(
                LowRegisterIdent::decode_from_bitvector(&bv, 13),
                LowRegisterIdent::decode_from_bitvector(&bv, 10),
            )
        } else if first_bits_match(byte_0, 0b00100_000, 5){
            Instruction::MovsImmediate8(
                LowRegisterIdent::decode_from_bitvector(&bv, 5),
                Immediate8::decode_from_bitvector(&bv, 8)
            )
        } else if first_bits_match(byte_0, 0b1011_1111, 8) {
            Instruction::Nop
        } else if first_bits_match(byte_0, 0b00110_000, 5) {
            Instruction::AddsImmediate8(
                LowRegisterIdent::decode_from_bitvector(&bv, 5),
                Immediate8::decode_from_bitvector(&bv, 8)
            )
        } else if first_bits_match(byte_0, 0b0001_100_0, 7) {
            Instruction::Adds(
                LowRegisterIdent::decode_from_bitvector(&bv, 13),
                LowRegisterIdent::decode_from_bitvector(&bv, 10),
                LowRegisterIdent::decode_from_bitvector(&bv, 7),
            )
        } else {
            return Err(())
        };

        Ok(instruction)

    }
}

#[test]
fn encode_decode_b() {
    let bv: BitVec<u32>  = Instruction::B(Immediate11(23)).into();
    assert_eq!(bv.to_bytes()[0], 0b1110_0000);
    assert_eq!(bv.to_bytes()[1], 0b000_10111);
    let i: Instruction = bv.try_into().unwrap();

    assert_eq!(i, Instruction::B(Immediate11(23)));
}

#[test]
fn encode_decode_adds_immediate() {
    let bv: BitVec<u32>  = Instruction::AddsImmediate8(LowRegisterIdent::R3,Immediate8(23)).into();
    let i: Instruction = bv.try_into().unwrap();

    assert_eq!(i, Instruction::AddsImmediate8(LowRegisterIdent::R3,Immediate8(23)));
}

#[test]
fn encode_decode_adds() {
    let bv: BitVec<u32>  = Instruction::Adds(LowRegisterIdent::R3,LowRegisterIdent::R2, LowRegisterIdent::R1).into();
    let i: Instruction = bv.try_into().unwrap();

    assert_eq!(i, Instruction::Adds(LowRegisterIdent::R3,LowRegisterIdent::R2, LowRegisterIdent::R1));
}
#[test]
fn encode_decode_movs_immediate() {
    let bv: BitVec<u32>  = Instruction::MovsImmediate8(LowRegisterIdent::R3,Immediate8(39)).into();
    let i: Instruction = bv.try_into().unwrap();
    assert_eq!(i, Instruction::MovsImmediate8(LowRegisterIdent::R3,Immediate8(39)));
}
