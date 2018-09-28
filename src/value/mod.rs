use bit_vec::BitVec;
use word::Word;

/// Allows a value to be encoded and decoded to a part of ByteCode.
pub trait ByteCodeEncodable {
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize);
    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self;
}

/// Extends a given value with 0s on the left of its given "length" to make a 32 bit Word.
pub trait ZeroExtendable {
    fn zero_extend(&self) -> Word;
}



#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Zero;

pub mod low_register_ident;
pub mod register_ident;
pub mod immediate3;
pub mod immediate5;
pub mod immediate8;
pub mod immediate11;