use value::ByteCodeEncodable;
use bit_vec::BitVec;
use value::register_ident::RegisterIdent;

/// A restricted set of registers that can be represented by 3 bits when serialized.
/// This is a common set of registers to work with due to being able to fit 3 in 9 bits
/// or one and a meaningfully sized immediate.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LowRegisterIdent {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7
}

impl ByteCodeEncodable for LowRegisterIdent {
    /// This sets bits in a bitvector to encode which low register to be represented
    #[inline(always)]
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
        let mut bit_1: bool = false;
        let mut bit_2: bool = false;
        let mut bit_3: bool = false;
        use self::LowRegisterIdent::*;
        match self {
            R0 => {},
            R1 => {bit_3 = true},
            R2 => {bit_2 = true},
            R3 => {bit_2 = true; bit_3 = true},
            R4 => {bit_1 = true},
            R5 => {bit_1 = true; bit_3 = true},
            R6 => {bit_1 = true; bit_2 = true},
            R7 => {bit_1 = true; bit_2 = true; bit_3 = true}
        }
        bit_vec.set(offset, bit_1);
        bit_vec.set(offset + 1, bit_2);
        bit_vec.set(offset + 2, bit_3)
    }

    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self {
        let bits: (bool, bool, bool) = (bit_vec.get(offset).expect("Should be inbounds"),
                                        bit_vec.get(offset + 1).expect("Should be inbounds"),
                                        bit_vec.get(offset + 2).expect("Should be inbounds"));

        use self::LowRegisterIdent::*;
        match bits {
            (false, false, false) => R0,
            (false, false, true) => R1,
            (false, true, false) => R2,
            (false, true, true) => R3,
            (true, false, false) => R4,
            (true, false, true) => R5,
            (true, true, false) => R6,
            (true, true, true) => R7
        }

    }
}

impl Into<RegisterIdent> for LowRegisterIdent {
    fn into(self) -> RegisterIdent {
        match self {
            LowRegisterIdent::R0 => RegisterIdent::R0,
            LowRegisterIdent::R1 => RegisterIdent::R1,
            LowRegisterIdent::R2 => RegisterIdent::R2,
            LowRegisterIdent::R3 => RegisterIdent::R3,
            LowRegisterIdent::R4 => RegisterIdent::R4,
            LowRegisterIdent::R5 => RegisterIdent::R5,
            LowRegisterIdent::R6 => RegisterIdent::R6,
            LowRegisterIdent::R7 => RegisterIdent::R7
        }
    }
}
