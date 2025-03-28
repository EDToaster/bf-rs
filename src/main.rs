use std::{
    fmt::Debug,
    fs,
    io::{self, Read, Write},
    path::PathBuf,
};

use mut_wrapping::MutatingWrappingAdd;
use op::{Op, OpSequence};
use parser::{Parser, Wrapped, any};

mod mut_wrapping;
mod op;
mod parser;

#[derive(clap::Parser, Debug)]
#[command(version, about)]
struct Args {
    #[arg(short, long)]
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = <Args as clap::Parser>::parse();
    let text = fs::read_to_string(args.file)?;
    let mut prog = parse_program(&text);
    let mut opt = true;
    macro_rules! opt {
        ($func:expr) => {
            prog = optimize(&prog, $func, &mut opt);
        };
    }

    while opt {
        opt = false;
        opt!(parse_fused_add_move());
        opt!(parse_offset_add);
        opt!(parse_copies());
        opt!(parse_set_zero_adds());
    }

    resolve_jumps(&mut prog);
    // debug_program(&prog);
    execute(&prog);
    Ok(())
}

fn parse_program(text: &str) -> Vec<Op> {
    let mut prog = vec![];

    for c in text.bytes() {
        let op = match c {
            b'+' => Some(Op::Add(1)),
            b'-' => Some(Op::Add(-1)),
            b'>' => Some(Op::Move(1)),
            b'<' => Some(Op::Move(-1)),
            b'[' => Some(Op::StartLoop(0)),
            b']' => Some(Op::EndLoop(0)),
            b'.' => Some(Op::Out),
            b',' => Some(Op::In),
            _ => None,
        };

        if let Some(op) = op {
            prog.push(op);
        }
    }
    prog
}

fn resolve_jumps(prog: &mut [Op]) {
    let mut stack: Vec<(usize, &mut usize)> = vec![];

    for (i, op) in prog.iter_mut().enumerate() {
        match op {
            Op::StartLoop(a) => stack.push((i, a)),
            Op::EndLoop(end_offset) => {
                let (start_i, start_offset) = stack.pop().unwrap();
                *start_offset = i - start_i;
                *end_offset = i - start_i;
            }
            _ => {}
        }
    }
}

fn execute(prog: &[Op]) {
    // circular buffer
    let mut m = [0u8; 1 << u16::BITS];
    let mut r = 0u16;

    let mut p = 0usize;

    while p < prog.len() {
        let next: isize = match prog[p] {
            Op::Add(a) => {
                m[r as usize].mut_wrapping_add_signed(a);
                1
            }
            Op::Move(a) => {
                r.mut_wrapping_add_signed(a);
                1
            }
            Op::StartLoop(o) => {
                if m[r as usize] > 0 {
                    1
                } else {
                    o as isize + 1
                }
            }
            Op::EndLoop(o) => -(o as isize),
            Op::In => {
                let val = std::io::stdin().bytes().next().unwrap_or(Ok(0)).unwrap();
                m[r as usize] = val;
                1
            }
            Op::Out => {
                print!("{}", m[r as usize] as char);
                io::stdout().flush().expect("Failure on flush!");
                1
            }
            Op::Set(a) => {
                m[r as usize] = a;
                1
            }
            Op::Copy(offset, mult) => {
                m[r.wrapping_add_signed(offset as i16) as usize]
                    .mut_wrapping_add_signed((m[r as usize] as isize) * mult);
                1
            }
            Op::OffsetAdd(offset, a) => {
                m[r.wrapping_add_signed(offset as i16) as usize].mut_wrapping_add_signed(a);
                1
            }
            Op::OffsetSet(offset, a) => {
                m[r.wrapping_add_signed(offset as i16) as usize] = a;
                1
            }
        };

        p = p.wrapping_add_signed(next);
    }
}

/// Parse >>> into r+=3 and +++ into m[r]+=3
fn parse_fused_add_move() -> impl Parser<Op, Op> {
    or![
        token!(Op::Add(a), a)
            .repeat(2..)
            .map(|v| Op::Add(v.iter().sum())),
        token!(Op::Move(a), a)
            .repeat(2..)
            .map(|v| Op::Move(v.iter().sum())),
    ]
}

/// Parse sequences like >>>++<<=0< into m[r+3] += 2 and m[r+1] = 0
fn parse_offset_add(input: &[Op]) -> Option<(Vec<Op>, &[Op])> {
    let mut input = input;
    let mut offset = 0;

    let mut ops = vec![];

    loop {
        if let Some((shift, new_input)) = token!(Op::Move(a), a).parse(input) {
            input = new_input;
            offset += shift;
            if offset == 0 {
                // end!
                break;
            }
        } else if offset != 0 {
            // complete the sequence!
            ops.push(Op::Move(offset));
            break;
        } else {
            return None;
        }

        // if offset isn't 0, then there needs to be an add here
        let (op, new_input) = or![
            token!(Op::Add(a), Op::OffsetAdd(offset, a)),
            token!(Op::Set(a), Op::OffsetSet(offset, a))
        ]
        .parse(input)?;
        input = new_input;
        ops.push(op);
    }
    Some((ops, input))
}

/// Parse optimization of [-], [->>>++<<<], and [>>>++<<<-]
fn parse_copies() -> impl Parser<Op, Vec<Op>> {
    let body = or![
        (
            token!(Op::Add(-1)),
            token!(Op::OffsetAdd(o, a), (o, a)).star(),
        )
            .map(|(_, adds)| adds),
        (
            token!(Op::OffsetAdd(o, a), (o, a)).star(),
            token!(Op::Add(-1)),
        )
            .map(|(adds, _)| adds),
    ]
    .flat_map(|copies| {
        let mut ops: Vec<Op> = vec![];

        for (offset, mult) in copies {
            ops.push(Op::Copy(offset, mult));
        }

        ops.push(Op::Set(0));

        Some(ops)
    });

    (token!(Op::StartLoop(_)), body, token!(Op::EndLoop(_))).map(|(_, e, _)| e)
}

/// Parse [-]++++ as =4
fn parse_set_zero_adds() -> impl Parser<Op, Op> {
    (token!(Op::Set(0)), token!(Op::Add(a), a)).map(|(_, a)| Op::Set(a as u8))
}

fn optimize<P, E: OpSequence>(prog: &[Op], optimizer: P, opt_flag: &mut bool) -> Vec<Op>
where
    P: Parser<Op, E> + 'static,
    E: Debug,
{
    let mut opt = vec![];

    let parser = optimizer
        // .inspect(|e| println!("Produced {e:?}"))
        .map(OpSequence::vec)
        .map(|e| (true, e))
        .or(any.map(OpSequence::vec).map(|e| (false, e)));

    for (optimized, es) in parser.iter(prog) {
        *opt_flag |= optimized;
        opt.extend(es);
    }

    if prog.len() != opt.len() {
        println!(
            "Optimized {} instructions down to {}",
            prog.len(),
            opt.len()
        );
    }

    opt
}

fn debug_program(prog: &[Op]) {
    for op in prog {
        match op {
            Op::Add(a) => {
                if *a > 0 {
                    print!("+")
                } else {
                    print!("-")
                }
            }
            Op::Move(a) => {
                if *a > 0 {
                    print!(">")
                } else {
                    print!("<")
                }
            }
            Op::StartLoop(_) => print!("["),
            Op::EndLoop(_) => print!("]"),
            Op::In => print!(","),
            Op::Out => print!("."),
            Op::Set(_) => print!("="),
            Op::OffsetAdd(_, _) => print!("$"),
            Op::OffsetSet(_, _) => print!("_"),
            Op::Copy(_, _) => print!("%"),
        }
    }
    println!("");
}
