use value::low_register_ident::LowRegisterIdent;
use value::register_ident::RegisterIdent;
use bit_vec::BitVec;
use value::ByteCodeEncodable;

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

