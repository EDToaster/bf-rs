use std::{marker::PhantomData, ops::RangeBounds};

// type alias...
pub trait ClosureParser<T, E>: Fn(&[T]) -> Option<(E, &[T])> + Copy {}
impl<B: Fn(&[T]) -> Option<(E, &[T])> + Copy, T, E> ClosureParser<T, E> for B {}

pub trait Parser<T, E>: Sized + Copy {
    fn parse<'a>(&self, input: &'a [T]) -> Option<(E, &'a [T])>;

    fn or<P>(self, other: P) -> impl ClosureParser<T, E>
    where
        P: Parser<T, E>,
    {
        move |input| self.parse(input).or_else(|| other.parse(input))
    }

    fn map<F, E1>(self, mapping: F) -> impl ClosureParser<T, E1>
    where
        F: Fn(E) -> E1 + Copy,
    {
        move |input| self.parse(input).map(|(e, t)| (mapping(e), t))
    }

    fn inspect<F>(self, inspection: F) -> impl Parser<T, E>
    where
        F: Fn(&E) -> () + Copy,
    {
        self.map(move |e| {
            inspection(&e);
            e
        })
    }

    fn flat_map<F, E1>(self, mapping: F) -> impl ClosureParser<T, E1>
    where
        F: Fn(E) -> Option<E1> + Copy,
    {
        move |input| {
            self.parse(input)
                .and_then(|(e, rest)| mapping(e).map(|e| (e, rest)))
        }
    }

    fn emit<E1: Copy>(self, emit: E1) -> impl Parser<T, E1> {
        self.map(move |_| emit)
    }

    fn repeat<R>(self, range: R) -> impl ClosureParser<T, Vec<E>>
    where
        R: RangeBounds<usize> + 'static,
    {
        let range = (range.start_bound().cloned(), range.end_bound().cloned());

        move |mut input| {
            let mut res = vec![];

            while let Some((e, new_input)) = self.parse(input) {
                res.push(e);
                input = new_input;
            }

            if range.contains(&res.len()) {
                Some((res, input))
            } else {
                None
            }
        }
    }

    fn plus(self) -> impl Parser<T, Vec<E>> {
        self.repeat(1..)
    }

    fn star(self) -> impl Parser<T, Vec<E>> {
        self.repeat(0..)
    }

    fn maybe(self) -> impl ClosureParser<T, Option<E>> {
        move |input| {
            if let Some((e, rest)) = self.parse(input) {
                Some((Some(e), rest))
            } else {
                Some((None, input))
            }
        }
    }

    fn iter(self, input: &[T]) -> ParserIter<Self, T, E> {
        ParserIter::new(self, input)
    }
}

impl<T, E, P: ClosureParser<T, E>> Parser<T, E> for P {
    fn parse<'a>(&self, input: &'a [T]) -> Option<(E, &'a [T])> {
        self(input)
    }
}

#[derive(Clone, Copy)]
pub struct Wrapped<F>(pub F);

impl<T, E, F> Parser<T, E> for Wrapped<F>
where
    T: Copy,
    F: Fn(T) -> Option<E> + Copy,
{
    fn parse<'a>(&self, input: &'a [T]) -> Option<(E, &'a [T])> {
        if let Some((tok, rest)) = input.split_first() {
            if let Some(e) = self.0(*tok) {
                return Some((e, rest));
            }
        }
        None
    }
}

impl<T, E1, E2, A: Parser<T, E1>, B: Parser<T, E2>> Parser<T, (E1, E2)> for (A, B) {
    fn parse<'a>(&self, input: &'a [T]) -> Option<((E1, E2), &'a [T])> {
        self.0.parse(input).and_then(|(e1, rest)| {
            if let Some((e2, rest)) = self.1.parse(rest) {
                Some(((e1, e2), rest))
            } else {
                None
            }
        })
    }
}

impl<T, E1, E2, E3, A: Parser<T, E1>, B: Parser<T, E2>, C: Parser<T, E3>> Parser<T, (E1, E2, E3)>
    for (A, B, C)
{
    fn parse<'a>(&self, input: &'a [T]) -> Option<((E1, E2, E3), &'a [T])> {
        self.0.parse(input).and_then(|(e1, rest)| {
            self.1
                .parse(rest)
                .and_then(|(e2, rest)| self.2.parse(rest).map(|(e3, rest)| ((e1, e2, e3), rest)))
        })
    }
}

pub fn any<T>(input: &[T]) -> Option<(T, &[T])>
where
    T: Copy,
{
    input.split_first().map(|(t, rest)| (*t, rest))
}

#[macro_export]
macro_rules! token {
    ($pattern:pat, $body:expr) => {
        Wrapped(move |t| match t {
            $pattern => Some($body),
            _ => None,
        })
    };
    ($pattern:pat) => {
        Wrapped(move |t| match t {
            $pattern => Some(t),
            _ => None,
        })
    };
}

pub struct ParserIter<'a, P, T, E> {
    parser: P,
    input: &'a [T],
    _phantom: PhantomData<E>,
}

impl<'a, P, T, E> ParserIter<'a, P, T, E> {
    pub fn new(parser: P, input: &'a [T]) -> Self {
        Self {
            parser,
            input,
            _phantom: PhantomData,
        }
    }
}

impl<'a, P, T, E> Iterator for ParserIter<'a, P, T, E>
where
    P: Parser<T, E>,
{
    type Item = E;
    fn next(&mut self) -> Option<Self::Item> {
        let (e, rest) = self.parser.parse(&self.input)?;
        self.input = rest;
        Some(e)
    }
}
