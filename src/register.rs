use label::NumLabel;
use bit_vec::BitVec;

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

#[derive(Clone, Copy, Debug)]
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

impl LowRegisterIdent {
    /// This sets bits in a bitvector to encode which low register to be represented
    #[inline(always)]
    pub fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
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
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Immediate8(pub u8);
impl Immediate8 {
    /// Sets bits in a bitvector to encode the value of the immediate 8.
    #[inline(always)]
    pub fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
        let bv = BitVec::from_bytes(&[self.0]);
        bv.into_iter().enumerate().for_each(|(index,bit)| {
            bit_vec.set(index + offset, bit);
        })
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
#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub struct Zero;