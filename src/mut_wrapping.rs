pub trait MutatingWrappingAdd {
    fn mut_wrapping_add_signed(&mut self, other: isize);
}

impl MutatingWrappingAdd for u16 {
    fn mut_wrapping_add_signed(&mut self, other: isize) {
        *self = self.wrapping_add_signed(other as i16);
    }
}

impl MutatingWrappingAdd for u8 {
    fn mut_wrapping_add_signed(&mut self, other: isize) {
        *self = self.wrapping_add_signed(other as i8);
    }
}
