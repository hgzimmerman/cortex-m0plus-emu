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
}