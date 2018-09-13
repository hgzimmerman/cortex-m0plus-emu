use label::NumLabel;

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
    Word(u32),
    Ident(NumLabel)
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
    I8(u8),
    /// A label may be used to reference another var
    Ident(NumLabel)
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
            LowRegisterOrI8Ident::I8(byte) => RegisterIdent::Word(byte as u32),
            LowRegisterOrI8Ident::Ident(label) => RegisterIdent::Ident(label)

        }
    }
}