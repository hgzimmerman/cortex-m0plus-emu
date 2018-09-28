use std::ops::Add;
use std::u32;
use std::ops::Sub;
use std::ops::Shl;
use std::ops::Shr;

#[derive(Default,Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Word(pub u32);

impl Word {
    pub fn set(&mut self, value: u32) {
        self.0 = value;
    }
    pub fn set_word(&mut self, word: Word){
        self.0 = word.0
    }

    pub fn is_negative(&self) -> bool {
        self.0 & Self::MASK_31 > 0
    }

    pub fn not(self) -> Word {
        Word(self.0 ^ u32::MAX)
    }

    /// An add with the ability to carry in a value.
    /// This allows things like multi-word additions.
    pub fn add_with_carry(self, rhs: Word, carry_in: bool) -> OpResult {
        let carry_bit = if carry_in {
            1
        } else {
            0
        };

        let word = Word(u32::wrapping_add(self.0, rhs.0 + carry_bit));
        let operands_same_sign = self.is_negative() && rhs.is_negative();
        let overflow = word.is_negative() != operands_same_sign;
        let carry = self.0 as u64 + rhs.0 as u64 > u32::MAX as u64;
        OpResult {
            carry,
            overflow,
            word,
        }
    }

    /// Shifts '1's into leftmost bits
    pub fn arithmetic_shift_right(self, rhs: Word) -> LogicalOpResult {
        let rhs: u32 = rhs.0;
        let mut word = Word(self.0 >> rhs % 32);

        let (carry_mask, shift_ones_mask): (u32, u32) = if rhs > 31 {
            (0, u32::MAX)
        } else if rhs > 1 {
            let m = 2u32.pow(rhs);
            (m, (m-1).reverse_bits())
        } else {
            (rhs, rhs.reverse_bits())
        };

        let carry: bool = self.0 & carry_mask != 0;
        // emulate shifting in '1's
        word.0 |= shift_ones_mask;

        LogicalOpResult {
            word,
            carry,
        }
    }

    // TODO this is duplicated in the machine, I think it really should belong here.
    /// Leftmost bit
    pub const MASK_31: u32 = 0x80_00_00_00;
    /// Second-leftmost bit
    pub const MASK_30: u32 = 0x40_00_00_00;
    /// Third-leftmost bit
    pub const MASK_29: u32 = 0x20_00_00_00;
    /// Fourth leftmost bit
    pub const MASK_28: u32 = 0x10_00_00_00;
}


/// The result of an arithmetic operation
pub struct OpResult {
    /// Carry out flag
    pub carry: bool,
    /// Overflow flag
    pub overflow: bool,
    /// The result of the operation
    pub word: Word
}

pub struct LogicalOpResult {
    /// Carry out flag
    pub carry: bool,
    /// The result of the operation
    pub word: Word
}

impl Add<Word> for Word {
    type Output = OpResult;

    fn add(self, rhs: Word) -> <Self as Add<Word>>::Output {
        self.add_with_carry(rhs, false)
    }
}

#[test]
fn add_carry() {
    let w1 = Word(u32::MAX);
    let w2 = Word(1);

    let r = w1 + w2;
    assert_eq!(r.word, Word(0));
    assert!(r.carry);
    assert!(!r.overflow);
}

#[test]
fn add_overflow() {
    let w1 = Word(u32::MAX >> 1);
    let w2 = Word(u32::MAX >> 1);

    let r = w1 + w2;
    assert_eq!(r.word, Word(u32::MAX - 1));
    assert!(!r.carry);
    assert!(r.overflow);
}
impl Sub<Word> for Word {
    type Output = OpResult;

    fn sub(self, rhs: Word) -> <Self as Sub<Word>>::Output {
        self.add_with_carry(rhs.not(), true)
    }
}


#[test]
fn sub_overflow() {
    let w1 = Word(0);
    let w2 = Word(1);

    let r = w1 - w2;
    assert_eq!(r.word, Word(u32::MAX));
    assert!(!r.carry);
    assert!(r.overflow);
}

#[test]
fn sub_carry() {
    let w1 = Word(10);
    let w2 = Word(1);

    let r = w1 - w2;
    assert_eq!(r.word, Word(9));
    assert!(r.carry);
    assert!(!r.overflow);
}

/// Logical shift
impl Shl<Word> for Word {
    type Output = LogicalOpResult;

    fn shl(self, rhs: Word) -> <Self as Shl<Word>>::Output {
        let rhs: u32 = rhs.0;
        let word = Word(self.0 << rhs);

        let mask: u32 = if rhs > 1 {
            2u32.pow(rhs)
        } else if rhs == 1 {
            1
        } else {
            0
        }.reverse_bits();

        let carry = self.0 & mask != 0;

        LogicalOpResult {
            word,
            carry,
        }
    }
}

#[test]
fn shift_left_no_carry() {
    let w = Word(1);
    let r = w << Word(1);
    assert_eq!(r.word, Word(2));
    assert!(!r.carry);
}

#[test]
fn shift_left_carry() {
    let w = Word(u32::MAX);
    let r = w << Word(1);
    assert_eq!(r.word, Word(u32::MAX - 1));
    assert!(r.carry);
}

/// Logical shift
impl Shr<Word> for Word {
    type Output = LogicalOpResult;

    fn shr(self, rhs: Word) -> <Self as Shr<Word>>::Output {
        let rhs: u32 = rhs.0;
        let word = Word(self.0 >> rhs);

        let mask: u32 = if rhs > 1 {
            2u32.pow(rhs)
        } else if rhs == 1 {
            1
        } else {
            0
        };

        let carry = self.0 & mask != 0;

        LogicalOpResult {
            word,
            carry,
        }
    }
}

#[test]
fn shift_right_no_carry() {
    let w = Word(2);
    let r = w >> Word(1);
    assert_eq!(r.word, Word(1));
    assert!(!r.carry);
}

#[test]
fn shift_right_carry() {
    let w = Word(1);
    let r = w >> Word(1);
    assert_eq!(r.word, Word(0));
    assert!(r.carry);
}

#[test]
fn arithmetic_shift_right() {
    let w = Word(1);
    let r = w.arithmetic_shift_right(Word(1));
    assert_eq!(r.word, Word(0b10000000_00000000_00000000_00000000));
    assert!(r.carry);
}

#[test]
fn arithmetic_shift_right_twice() {
    let w = Word(1);
    let r = w.arithmetic_shift_right(Word(2));
    assert_eq!(r.word, Word(0b11000000_00000000_00000000_00000000));
    assert!(!r.carry);
}
#[test]
fn arithmetic_shift_right_31() {
    let w = Word(1);
    let r = w.arithmetic_shift_right(Word(31));
    assert_eq!(r.word, Word(u32::MAX - 1));
    assert!(!r.carry);
}
#[test]
fn arithmetic_shift_right_32() {
    let w = Word(1);
    let r = w.arithmetic_shift_right(Word(32));
    assert_eq!(r.word, Word(u32::MAX));
    assert!(!r.carry);
}

