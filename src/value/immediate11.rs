use bit_vec::BitVec;
use word::Word;
use value::ZeroExtendable;
use value::ByteCodeEncodable;

/// 11 bits number as represented in an instruction.
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
impl ZeroExtendable for Immediate11 {
    fn zero_extend(&self) -> Word {
        Word(self.0 as u32)
    }
}