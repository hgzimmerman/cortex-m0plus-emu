pub fn u16_to_u8array(v: u16) -> [u8; 2] {
    let v = v.swap_bytes();
    unsafe {
        ::std::mem::transmute(v)
    }
}

#[test]
fn  u16_to_u8array_test() {
    let u_sixteen: u16 = 20;
    let aaah = u16_to_u8array(u_sixteen);
    assert_eq!(aaah, [0, 20])
}

pub fn u32_to_u8array(v: u32) -> [u8; 4] {
    let v = v.swap_bytes();
    unsafe {
        ::std::mem::transmute(v)
    }
}

#[test]
fn u32_to_u8array_test() {
    let n: u32 = 20;
    let aaah = u32_to_u8array(n);
    assert_eq!(aaah, [0, 0, 0, 20])
}


/// sets the bit of the specified index to 1 (indexed 0-15 right to left)
pub fn u16_bit_select_mask(index: u8) -> u16 {
    2u16.pow(index as u32)
}

pub fn u8_bit_select_mask(index: u8) -> u8 {
    2u8.pow(index as u32)
}

// TODO this may be refactored for performance reasons in the future
pub fn first_bits_match(value: u8, mask: u8, num_bits: u8) -> bool {
    const ONE_MASK: u8 = 0b1000_0000;
    const TWO_MASK: u8 = 0b1100_0000;
    const THREE_MASK: u8 = 0b1110_0000;
    const FOUR_MASK: u8 = 0b1111_0000;
    const FIVE_MASK: u8 = 0b1111_1000;
    const SIX_MASK: u8 = 0b1111_1100;
    const SEVEN_MASK: u8 = 0b1111_1110;
    const EIGHT_MASK: u8 = 0b1111_1111;

    let (amended_value, amended_mask): (u8, u8) = match num_bits {
        0 => return true,
        1 => (value & ONE_MASK, mask & ONE_MASK),
        2 => (value & TWO_MASK, mask & TWO_MASK),
        3 => (value & THREE_MASK, mask & THREE_MASK),
        4 => (value & FOUR_MASK, mask & FOUR_MASK),
        5 => (value & FIVE_MASK, mask & FIVE_MASK),
        6 => (value & SIX_MASK, mask & SIX_MASK),
        7 => (value & SEVEN_MASK, mask & SEVEN_MASK),
        8 => (value, mask),
        _ => panic!("Programmer error, should not care about bit index > 8")
    };
    let amended_mask= amended_mask ^ EIGHT_MASK;

    amended_value ^ amended_mask == EIGHT_MASK

}

#[test]
fn first_bits_match_test() {
    assert!(first_bits_match(3, 1, 4));
}

#[test]
fn first_bits_match_test_1() {
    assert!(!first_bits_match(0b1010_1001, 0b1011_1111, 4));
}

#[test]
fn first_bits_match_test_2() {
    assert!(first_bits_match(0b1111_1001, 0b1111_1111, 5));
}

#[test]
fn first_bits_match_test_3() {
    assert!(first_bits_match(0b1011_1001, 0b1011_1001, 8));
}

#[test]
fn first_bits_match_test_4() {
    assert!(!first_bits_match(0b1111_1001, 0b1011_1001, 3));
}
