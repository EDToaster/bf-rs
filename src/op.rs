use std::fmt::Debug;

type LoopId = usize;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Add(isize),
    Move(isize),

    StartLoop(LoopId),
    EndLoop(LoopId),
    In,
    Out,

    Set(u8),

    OffsetAdd(isize, isize),
    OffsetSet(isize, u8),
    /// Copy offset and multiplier
    Copy(isize, isize),
}

impl Debug for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Add(a) => {
                let a = if *a > 0 {
                    write!(f, "+")?;
                    *a
                } else {
                    write!(f, "-")?;
                    -a
                };
                if a > 1 {
                    write!(f, "{a}")?;
                }
            }
            Op::Move(a) => {
                let a = if *a > 0 {
                    write!(f, ">")?;
                    *a
                } else {
                    write!(f, "<")?;
                    -a
                };
                if a > 1 {
                    write!(f, "{a}")?;
                }
            }
            Op::StartLoop(_) => write!(f, "[")?,
            Op::EndLoop(_) => write!(f, "]")?,
            Op::In => write!(f, ",")?,
            Op::Out => write!(f, ".")?,
            Op::Set(a) => write!(f, "={a}")?,
            Op::Copy(o, m) => write!(f, "({o}*{m})")?,
            Op::OffsetAdd(o, a) => write!(f, "({o}+={a})")?,
            Op::OffsetSet(o, a) => write!(f, "_")?,
        }
        Ok(())
    }
}

pub trait OpSequence {
    fn vec(self) -> Vec<Op>;
}

impl OpSequence for Op {
    fn vec(self) -> Vec<Op> {
        vec![self]
    }
}

impl OpSequence for Vec<Op> {
    fn vec(self) -> Vec<Op> {
        self
    }
}
