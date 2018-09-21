#![feature(reverse_bits)]

extern crate bit_vec;

use std::u32;
mod instruction;
mod register;
mod label;
mod word;
mod util;
use instruction::Instruction;
use register::*;
use label::NumLabel;
use word::Word;
use bit_vec::BitVec;
use std::ops::Index;
use word::OpResult;


const INSTRUCTIONS_SIZE: usize = 256;

#[derive(Clone,)]
pub struct Machine {
    // Low registers
    r0: Word,
    r1: Word,
    r2: Word,
    r3: Word,
    r4: Word,
    r5: Word,
    r6: Word,
    r7: Word,
    // high registers
    r8: Word,
    r9: Word,
    r10: Word,
    r11: Word,
    r12: Word,
    /// Also known as R13
    stack_pointer: Word, // points to an area of memory that contains data used for scratch space/ stack information.
    /// Also known as R14
    link_register: Word, // holds address of the current address +1 when BL (Branch+Link) is called, so BX can restore it.
    /// Also known as R15
    program_counter: Word, // counts number of instructions run
    /// Also known as R16
    program_status_register: Word,
    // Memory regions
    ram: Box<[u8; 256]>, // TODO, determine how much memory this should have
    rom_consts: Box<[u32; 256]>, // TODO likewise
    rom_instructions: Box<[u8; 256]>, // TODO likewise; Q: should there be a bit-representation for instructions? A: Yeet
}

impl Default for Machine {
    fn default() -> Self {
        Machine {
            r0: Default::default(),
            r1: Default::default(),
            r2: Default::default(),
            r3: Default::default(),
            r4: Default::default(),
            r5: Default::default(),
            r6: Default::default(),
            r7: Default::default(),
            r8: Default::default(),
            r9: Default::default(),
            r10: Default::default(),
            r11: Default::default(),
            r12: Default::default(),
            stack_pointer: Default::default(),
            link_register: Default::default(),
            program_counter: Default::default(),
            program_status_register: Default::default(),
            ram: Box::new([0; 256]),
            rom_consts: Box::new([0; 256]),
            rom_instructions: Box::new([0; INSTRUCTIONS_SIZE]),
        }
    }
}

impl Index<usize> for Machine {
    type Output = u32;

    fn index(&self, index: usize) -> &<Self as Index<usize>>::Output {
        unimplemented!()
    }
}

impl Machine {

    pub fn load_instructions(&mut self, instructions: &[Instruction]) {
        for (index, instruction) in instructions.iter().enumerate() {
            let bv: BitVec<u32> = (*instruction).into();
            let mut bytes = bv.to_bytes();
            // each instruction must be 4 bytes long (32 bit word instructions) even though
            // the instruction content may fit in 16 bits.
            while bytes.len() < 4 {
                bytes.push(0)
            }


            self.rom_instructions[index * 4] = bytes[0];
            self.rom_instructions[index * 4 + 1] = bytes[1];
            self.rom_instructions[index * 4 + 2] = bytes[2];
            self.rom_instructions[index * 4 + 3] = bytes[3];

        }
    }


    pub fn run(&mut self) {
        loop {
            // Read 4 bytes (one word) into a bitvec.
            let byte1 = self.rom_instructions[self.program_counter.0 as usize];
            self.program_counter.0 += 1;
            let byte2 = self.rom_instructions[self.program_counter.0 as usize];
            self.program_counter.0 += 1;
            let byte3 = self.rom_instructions[self.program_counter.0 as usize];
            self.program_counter.0 += 1;
            let byte4 = self.rom_instructions[self.program_counter.0 as usize];
            self.program_counter.0 += 1;
            let bv = BitVec::from_bytes(&[byte1, byte2, byte3, byte4]);

            let instruction= Instruction::from(bv); // TODO, convert to try_from
            self.process_instruction(&instruction)
        }
    }


    fn process_instruction(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Nop => {},
            Instruction::Add(dest, lhs, rhs) => self.add(dest, lhs, &(*rhs).into(), false),
            Instruction::Adds(dest, lhs, rhs) => self.add(dest, lhs, &(*rhs).into(), true),
            Instruction::AddImmediate8(src_dest, immediate) => self.add(src_dest, src_dest, &(*immediate).into(), false),
            Instruction::AddsImmediate8(src_dest, immediate) => self.add(src_dest, src_dest, &(*immediate).into(), true),
            Instruction::Sub(dest, lhs, rhs) => self.sub(dest, lhs, &(*rhs).into(), false),
            Instruction::Subs(dest, lhs, rhs) => self.sub(dest, lhs, &(*rhs).into(), true),
            Instruction::SubImmediate8(src_dest, immediate) => self.sub(src_dest, src_dest, &(*immediate).into(), false),
            Instruction::SubsImmediate8(src_dest, immediate) => self.sub(src_dest, src_dest, &(*immediate).into(), true),
            Instruction::Rsbs(dest, src, _zero) => self.rsb(dest, src),
            Instruction::Mov(dest, src) => self.mov(dest, &(*src).into(), false),
            Instruction::Movs(dest, src) => self.mov(dest, &(*src).into(), true),
            Instruction::MovImmediate8(dest, immediate) => self.mov(dest, &(*immediate).into(), false),
            Instruction::MovsImmediate8(dest, immediate) => self.mov(dest, &(*immediate).into(), true),
            Instruction::Cmp(basis, compare) => self.cmp(basis, &(*compare).into()),
            Instruction::B(address) => self.branch_11(address),
        }
    }

    /// Generic implementation of ADD instruction
    fn add(&mut self, dest: &LowRegisterIdent, lhs: &LowRegisterIdent, rhs: &LowRegisterOrI8Ident, s: bool) {
        let lhs_register =  self.get_value((*lhs).into());
        let rhs_register = self.get_value((*rhs).into());
        let r = lhs_register + rhs_register;

        self.get_register_mut((*dest).into()).set_word(r.word);

        if s {
           self.set_psr_all(&r)
        }
    }

    /// Implementation of compare
    fn cmp(&mut self, basis: &LowRegisterIdent, compare: &LowRegisterOrI8Ident) {
        let basis_value = self.get_value((*basis).into());
        let compare_value = self.get_value((*compare).into());
        let value: u32 = basis_value.0 - compare_value.0;

        self.set_psr_zero( value == 0);
        self.set_psr_negative(value & Self::MASK_31 > 0);
    }

    /// Implementation of subtraction
    fn sub(&mut self, dest: &LowRegisterIdent, lhs: &LowRegisterIdent, rhs: &LowRegisterOrI8Ident, s: bool) {
        let lhs_register =  self.get_value((*lhs).into());
        let rhs_register = self.get_value((*rhs).into());
        let r = lhs_register - rhs_register;
        self.get_register_mut((*dest).into()).set_word(r.word);

        if s {
            self.set_psr_all(&r)
        }
    }

    /// Generic implementation of MOV instruction
    fn mov(&mut self, dest: &LowRegisterIdent, src: &LowRegisterOrI8Ident, s: bool) {
        let source_value: u32 = self.get_value((*src).into()).0;
        self.get_register_mut((*dest).into()).set(source_value);
        if s {
            self.set_psr_negative(source_value & Word::MASK_31 > 0 );
            self.set_psr_zero(source_value == 0)
        }
    }

    /// Reverse subtract
    /// Always sets flags
    fn rsb(&mut self, dest: &LowRegisterIdent, src: &LowRegisterIdent) {
        let src_register = self.get_value((*src).into());
        {
            let dest_register = self.get_register_mut((*dest).into());
            dest_register.set(u32::wrapping_sub(0, src_register.0));
        }

        let dest_register = self.get_value((*dest).into());
        self.set_psr_zero(dest_register.0 == 0);
        self.set_psr_negative(dest_register.0 & Word::MASK_31 > 0);
    }

    fn branch_11(&mut self, address: &Immediate11) {
        let address: u32 = address.0 as u32;
        self.program_counter.0 = address;
    }


    /// Gets the value of a register, this will act independently of the stored value.
    /// It is non-mutating.
    fn get_value(&self, ident: RegisterIdent) -> Word {
        match ident {
            RegisterIdent::R0 => self.r0,
            RegisterIdent::R1 => self.r1,
            RegisterIdent::R2 => self.r2,
            RegisterIdent::R3 => self.r3,
            RegisterIdent::R4 => self.r4,
            RegisterIdent::R5 => self.r5,
            RegisterIdent::R6 => self.r6,
            RegisterIdent::R7 => self.r7,
            RegisterIdent::R8 => self.r8,
            RegisterIdent::R9 => self.r9,
            RegisterIdent::R10 => self.r10,
            RegisterIdent::R11 => self.r11,
            RegisterIdent::R12 => self.r12,
            RegisterIdent::StackRegister => self.stack_pointer,
            RegisterIdent::LinkRegister => self.link_register,
            RegisterIdent::ProgramCounter => self.program_counter,
            RegisterIdent::Word(word) => Word(word),
//            RegisterIdent::Ident(label_index) => Word(self.rom_consts[label_index.0 as usize])
        }
    }

    /// Gets a mutable reference to the register, so that it may be modified directly.
    fn get_register_mut(&mut self, ident: RegisterIdent) -> &mut Word {
        match ident {
            RegisterIdent::R0 => &mut self.r0,
            RegisterIdent::R1 => &mut self.r1,
            RegisterIdent::R2 => &mut self.r2,
            RegisterIdent::R3 => &mut self.r3,
            RegisterIdent::R4 => &mut self.r4,
            RegisterIdent::R5 => &mut self.r5,
            RegisterIdent::R6 => &mut self.r6,
            RegisterIdent::R7 => &mut self.r7,
            RegisterIdent::R8 => &mut self.r8,
            RegisterIdent::R9 => &mut self.r9,
            RegisterIdent::R10 => &mut self.r10,
            RegisterIdent::R11 => &mut self.r11,
            RegisterIdent::R12 => &mut self.r12,
            RegisterIdent::StackRegister => &mut self.stack_pointer,
            RegisterIdent::LinkRegister => &mut self.link_register,
            RegisterIdent::ProgramCounter => &mut self.program_counter,
            RegisterIdent::Word(_word) => panic!(),
//            RegisterIdent::Ident(_label_index) => panic!()
        }
    }

    /// Leftmost bit
    const MASK_31: u32 = 0x80_00_00_00;
    /// Second-leftmost bit
    const MASK_30: u32 = 0x40_00_00_00;

    const MASK_29: u32 = 0x20_00_00_00;

    const MASK_28: u32 = 0x10_00_00_00;

    const N_MASK: u32 = Self::MASK_31;
    const Z_MASK: u32 = Self::MASK_30;
    const C_MASK: u32 = Self::MASK_29;
    const O_MASK: u32 = Self::MASK_28;


    const FULL_MASK: u32 = 0xFF_FF_FF_FF;


    fn set_psr_all(&mut self, op_result: &OpResult) {
        self.set_psr_carry(op_result.carry);
        self.set_psr_overflow(op_result.overflow);
        self.set_psr_zero(op_result.word.0 == 0);
        self.set_psr_negative(op_result.word.0 & Word::MASK_31 > 0)
    }


    /// The previous result was negative
    #[inline(always)]
    fn psr_negative(&self) -> bool {
        self.program_status_register.0 & Self::N_MASK > 0
    }

    #[inline(always)]
    fn psr_zero(&self) -> bool {
        self.program_status_register.0 & Self::Z_MASK > 0
    }

    #[inline(always)]
    fn psr_carry(&self) -> bool {
        self.program_status_register.0 & Self::C_MASK > 0
    }

    #[inline(always)]
    fn psr_overflow(&self) -> bool {
        self.program_status_register.0 & Self::O_MASK > 0
    }

    #[inline(always)]
    fn set_psr_negative(&mut self, value: bool) {
        if value {
            self.program_status_register.0 |= Self::N_MASK // set the bit to 1
        } else {
            self.program_status_register.0 &= Self::N_MASK ^ Self::FULL_MASK // set the bit to 0
        }
    }

    #[inline(always)]
    fn set_psr_zero(&mut self, value: bool) {
        if value {
            self.program_status_register.0 |= Self::Z_MASK
        } else {
            self.program_status_register.0 &= Self::Z_MASK ^ Self::FULL_MASK
        }
    }

    #[inline(always)]
    fn set_psr_carry(&mut self, value: bool) {
        if value {
            self.program_status_register.0 |= Self::C_MASK
        } else {
            self.program_status_register.0 &= Self::C_MASK ^ Self::FULL_MASK
        }
    }

    #[inline(always)]
    fn set_psr_overflow(&mut self, value: bool) {
        if value {
            self.program_status_register.0 |= Self::O_MASK
        } else {
            self.program_status_register.0 &= Self::O_MASK ^ Self::FULL_MASK
        }
    }

}

#[test]
fn movs_works() {
    let mut machine = Machine::default();
    let mov = Instruction::MovsImmediate8(LowRegisterIdent::R0, Immediate8(20));
    machine.process_instruction(&mov);

    assert_eq!(machine.r0.0 , 20);
    assert!(!machine.psr_negative());
    assert!(!machine.psr_carry());
    assert!(!machine.psr_overflow());
    assert!(!machine.psr_zero());
}

#[test]
fn movs_zero() {
    let mut machine = Machine::default();
    let mov = Instruction::MovsImmediate8(LowRegisterIdent::R0, Immediate8(0));
    machine.process_instruction(&mov);

    assert_eq!(machine.r0.0 , 0);
    assert!(!machine.psr_negative());
    assert!(!machine.psr_carry());
    assert!(!machine.psr_overflow());
    assert!(machine.psr_zero());
}

#[test]
fn adds_works() {
    let mut machine = Machine::default();
    const LHS: u8 = 20;
    const RHS: u8 = 15;
    let mov = Instruction::MovsImmediate8(LowRegisterIdent::R0, LHS.into());
    let adds = Instruction::AddsImmediate8(LowRegisterIdent::R0, Immediate8(RHS));
    machine.process_instruction(&mov);
    machine.process_instruction(&adds);

    assert_eq!(machine.r0.0 , (LHS + RHS) as u32);
    assert!(!machine.psr_negative());
    assert!(!machine.psr_carry());
    assert!(!machine.psr_overflow());
    assert!(!machine.psr_zero());
}

#[test]
fn adds_carry() {
    let mut machine = Machine::default();
    machine.r0 = Word(u32::MAX); // Set the R0 to be 1111_..._1111
    const RHS: u8 = 15;
    let adds = Instruction::AddsImmediate8(LowRegisterIdent::R0, Immediate8(RHS));
    machine.process_instruction(&adds);

    assert_eq!(machine.r0.0 , (RHS - 1) as u32);
    assert!(!machine.psr_negative());
    assert!(machine.psr_carry());
    assert!(!machine.psr_overflow());
    assert!(!machine.psr_zero());
}

#[test]
fn adds_overflow() {
    let mut machine = Machine::default();
    machine.r0 = Word(u32::MAX >> 1); // Set the R0 to be 1111_..._1111
    const RHS: u8 = 15;
    let adds = Instruction::AddsImmediate8(LowRegisterIdent::R0, Immediate8(RHS));
    machine.process_instruction(&adds);

//    assert_eq!(machine.r0.0 , (RHS - 1) as u32);
    assert!(machine.psr_negative());
    assert!(!machine.psr_carry());
    assert!(machine.psr_overflow());
    assert!(!machine.psr_zero());
}



#[test]
fn subs_works() {
    let mut machine = Machine::default();
    const LHS: u8 = 15;
    const RHS: u8 = 20;
    let mov = Instruction::MovsImmediate8(LowRegisterIdent::R0, LHS.into());
    let subs = Instruction::SubsImmediate8(LowRegisterIdent::R0, Immediate8(RHS));

    machine.process_instruction(&mov);
    machine.process_instruction(&subs);


    assert_eq!(machine.r0.0 as i32 , (u32::wrapping_sub(LHS as u32, RHS as u32)) as i32);
    assert!(machine.psr_negative());
    assert!(machine.psr_overflow());
    assert!(!machine.psr_carry());
    assert!(!machine.psr_zero());
}

#[test]
fn basic_branch() {
    let mut machine = Machine::default();
    let i11 = Immediate11(24);

    machine.process_instruction(&Instruction::B(i11));
    assert_eq!(machine.program_counter.0, 24)
}
