use register::LowRegisterIdent;
use register::LowRegisterOrI8Ident;

// TODO what about directives?
// TODO could this be represented as bytecode? Just an into/from -> [u8; 4] w/ a statemachine to read it?
#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    NOP,
    ADD(LowRegisterIdent, LowRegisterIdent, LowRegisterOrI8Ident),
    ADDS(LowRegisterIdent, LowRegisterIdent, LowRegisterOrI8Ident),
    MOV(LowRegisterIdent, LowRegisterOrI8Ident),
    MOVS(LowRegisterIdent, LowRegisterOrI8Ident)
}