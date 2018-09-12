#[derive(Default,Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Word(pub u32);

impl Word {
    pub fn set(&mut self, value: u32) {
        self.0 = value;
    }
}