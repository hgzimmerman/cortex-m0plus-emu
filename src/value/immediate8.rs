use value::ZeroExtendable;
use word::Word;
use bit_vec::BitVec;
use value::ByteCodeEncodable;
use value::register_ident::RegisterIdent;
use value::low_register_ident::LowRegisterIdent;

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
impl ZeroExtendable for Immediate8 {
    fn zero_extend(&self) -> Word {
        Word(self.0 as u32)
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
