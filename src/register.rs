use label::NumLabel;
use bit_vec::BitVec;


pub trait ByteCodeEncodable {
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize);
    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self;
}

/// Superset of all register combinations
#[derive(Clone, Copy, Debug)]
pub enum RegisterIdent {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    StackRegister,
    LinkRegister,
    ProgramCounter,
    /// Abstract other thing
    Word(u32)
//    Ident(NumLabel)
}

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
        use LowRegisterIdent::*;
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

        use LowRegisterIdent::*;
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Immediate8(pub u8);
impl ByteCodeEncodable for Immediate8 {
    /// Sets bits in a bitvector to encode the value of the immediate 8.
    /// The offset specifies where to start placing bits in the bit_vec.
    #[inline(always)]
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
        let bv = BitVec::from_bytes(&[self.0]);
        (0..8_usize).for_each(|index| {
            bit_vec.set(index + offset, bv.get(index ).expect(&format!("Should be inbounds {}", index + offset)));
        });
    }
    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self {
        let mut target = Immediate8(0);
        (0..8_usize).for_each(|index| {
            if let Some(bit) = bit_vec.get(index + offset) {
                if bit {
                    target.0 |= ::util::u8_bit_select_mask(7 - index as u8)
                }
            }
        });
        target
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Immediate5(pub u8);
impl ByteCodeEncodable for Immediate5 {
    /// Sets bits in a bitvector to encode the value of the immediate 8.
    /// The offset specifies where to start placing bits in the bit_vec.
    #[inline(always)]
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
        let bv = BitVec::from_bytes(&[self.0]);
        (0..5_usize).for_each(|index| {
            bit_vec.set(index + offset, bv.get(index + 3 ).expect(&format!("Should be inbounds {}", index + offset)));
        });
    }
    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self {
        let mut target = Immediate5(0);
        (0..5_usize).for_each(|index| {
            if let Some(bit) = bit_vec.get(index + offset) {
                if bit {
                    target.0 |= ::util::u8_bit_select_mask(4 - index as u8)
                }
            }
        });
        target
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Immediate11(pub u16);
impl ByteCodeEncodable for Immediate11 {
    /// Sets bits in a bitvector to encode the value of the immediate 11.
    #[inline(always)]
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
        let bv = BitVec::from_bytes(&::util::u16_to_u8array(self.0));
        (0..11_usize).for_each(|index| {
            let value = bv
                .get(index + 5 )
                .expect(&format!("Should be inbounds {}", index + offset));
            bit_vec.set(index + offset, value)
        });
    }
    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self {
        let mut target = Immediate11(0);
        (0..11_usize).for_each(|index| {
            if let Some(bit) = bit_vec.get(index + offset) {
                if bit {
                    target.0 |= ::util::u16_bit_select_mask(10 - index as u8)
                }
            }
        });
        target
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
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LowRegisterOrI8Ident {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    /// A byte is allowed to be used
    I8(Immediate8),
//    Ident(NumLabel)
}

impl Into<RegisterIdent> for LowRegisterOrI8Ident {
    fn into(self) -> RegisterIdent {
        match self {
            LowRegisterOrI8Ident::R0 => RegisterIdent::R0,
            LowRegisterOrI8Ident::R1 => RegisterIdent::R1,
            LowRegisterOrI8Ident::R2 => RegisterIdent::R2,
            LowRegisterOrI8Ident::R3 => RegisterIdent::R3,
            LowRegisterOrI8Ident::R4 => RegisterIdent::R4,
            LowRegisterOrI8Ident::R5 => RegisterIdent::R5,
            LowRegisterOrI8Ident::R6 => RegisterIdent::R6,
            LowRegisterOrI8Ident::R7 => RegisterIdent::R7,
            LowRegisterOrI8Ident::I8(byte) => RegisterIdent::Word(byte.0 as u32),
//            LowRegisterOrI8Ident::Ident(label) => RegisterIdent::Ident(label)

        }
    }
}

impl Into<LowRegisterOrI8Ident> for LowRegisterIdent {
    fn into(self) -> LowRegisterOrI8Ident {
        match self {
            LowRegisterIdent::R0 => LowRegisterOrI8Ident::R0,
            LowRegisterIdent::R1 => LowRegisterOrI8Ident::R1,
            LowRegisterIdent::R2 => LowRegisterOrI8Ident::R2,
            LowRegisterIdent::R3 => LowRegisterOrI8Ident::R3,
            LowRegisterIdent::R4 => LowRegisterOrI8Ident::R4,
            LowRegisterIdent::R5 => LowRegisterOrI8Ident::R5,
            LowRegisterIdent::R6 => LowRegisterOrI8Ident::R6,
            LowRegisterIdent::R7 => LowRegisterOrI8Ident::R7,
        }
    }
}

impl Into<LowRegisterOrI8Ident> for Immediate8 {
    fn into(self) -> LowRegisterOrI8Ident {
        LowRegisterOrI8Ident::I8(self)
    }
}

impl From<u8> for Immediate8 {
    fn from(byte: u8) -> Self {
        Immediate8(byte)
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LowRegisterOrI5Ident {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    /// A byte is allowed to be used
    I5(Immediate5),
}

impl Into<RegisterIdent> for LowRegisterOrI5Ident {
    fn into(self) -> RegisterIdent {
        match self {
            LowRegisterOrI5Ident::R0 => RegisterIdent::R0,
            LowRegisterOrI5Ident::R1 => RegisterIdent::R1,
            LowRegisterOrI5Ident::R2 => RegisterIdent::R2,
            LowRegisterOrI5Ident::R3 => RegisterIdent::R3,
            LowRegisterOrI5Ident::R4 => RegisterIdent::R4,
            LowRegisterOrI5Ident::R5 => RegisterIdent::R5,
            LowRegisterOrI5Ident::R6 => RegisterIdent::R6,
            LowRegisterOrI5Ident::R7 => RegisterIdent::R7,
            LowRegisterOrI5Ident::I5(byte) => RegisterIdent::Word(byte.0 as u32),

        }
    }
}

impl Into<LowRegisterOrI5Ident> for LowRegisterIdent {
    fn into(self) -> LowRegisterOrI5Ident {
        match self {
            LowRegisterIdent::R0 => LowRegisterOrI5Ident::R0,
            LowRegisterIdent::R1 => LowRegisterOrI5Ident::R1,
            LowRegisterIdent::R2 => LowRegisterOrI5Ident::R2,
            LowRegisterIdent::R3 => LowRegisterOrI5Ident::R3,
            LowRegisterIdent::R4 => LowRegisterOrI5Ident::R4,
            LowRegisterIdent::R5 => LowRegisterOrI5Ident::R5,
            LowRegisterIdent::R6 => LowRegisterOrI5Ident::R6,
            LowRegisterIdent::R7 => LowRegisterOrI5Ident::R7,
        }
    }
}

impl Into<LowRegisterOrI5Ident> for Immediate5 {
    fn into(self) -> LowRegisterOrI5Ident {
        LowRegisterOrI5Ident::I5(self)
    }
}

// TODO this technically should be able to fail. switch to Try_From
impl From<u8> for Immediate5 {
    fn from(byte: u8) -> Self {
        Immediate5(byte)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Zero;