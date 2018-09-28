use value::ByteCodeEncodable;
use bit_vec::BitVec;
use value::ZeroExtendable;
use word::Word;

/// 3 bit number as represented in an instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Immediate3(pub u8);
impl ByteCodeEncodable for Immediate3 {
    /// Sets bits in a bitvector to encode the value of the immediate 8.
    /// The offset specifies where to start placing bits in the bit_vec.
    #[inline(always)]
    fn encode_to_bitvector(&self, bit_vec: &mut BitVec<u32>, offset: usize) {
        let bv = BitVec::from_bytes(&[self.0]);
        (0..3_usize).for_each(|index| {
            bit_vec.set(index + offset, bv.get(index + 5 ).expect(&format!("Should be inbounds {}", index + offset)));
        });
    }
    fn decode_from_bitvector(bit_vec: &BitVec<u32>, offset: usize) -> Self {
        let mut target = Immediate3(0);
        (0..3_usize).for_each(|index| {
            if let Some(bit) = bit_vec.get(index + offset) {
                if bit {
                    target.0 |= ::util::u8_bit_select_mask(2 - index as u8) // TODO??? check for correctness
                }
            }
        });
        target
    }
}
impl ZeroExtendable for Immediate3 {
    fn zero_extend(&self) -> Word {
        Word(self.0 as u32)
    }
}