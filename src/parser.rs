use std::{marker::PhantomData, ops::RangeBounds};

// type alias...
pub trait ClosureParser<T, E>: Fn(&[T]) -> Option<(E, &[T])> + Copy {}
impl<P: Fn(&[T]) -> Option<(E, &[T])> + Copy, T, E> ClosureParser<T, E> for P {}
impl<P: ClosureParser<T, E>, T, E> Parser<T, E> for P {
    fn parse(self, input: &[T]) -> Option<(E, &[T])> {
        self(input)
    }
}

pub trait Parser<T, E>: Sized + Copy {
    fn parse(self, input: &[T]) -> Option<(E, &[T])>;

    fn or<P>(self, other: P) -> impl ClosureParser<T, E>
    where
        P: Parser<T, E>,
    {
        move |input| self.parse(input).or_else(|| other.parse(input))
    }

    fn and<P, E1>(self, other: P) -> impl ClosureParser<T, (E, E1)>
    where
        P: Parser<T, E1>,
    {
        move |input| {
            self.parse(input)
                .and_then(|(e, rest)| other.parse(rest).map(|(e1, rest)| ((e, e1), rest)))
        }
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

    fn maybe(self) -> impl Parser<T, Option<E>>
    where
        E: Copy,
    {
        self.map(Some).or(emit(None))
    }

    fn not(self) -> impl ClosureParser<T, ()> {
        move |input| match self.parse(input) {
            Some(_) => None,
            None => Some(((), input)),
        }
    }

    fn iter(self, input: &[T]) -> ParserIter<Self, T, E> {
        ParserIter::new(self, input)
    }
}

#[derive(Clone, Copy)]
pub struct Wrapped<F>(pub F);

impl<T, E, F> Parser<T, E> for Wrapped<F>
where
    T: Copy,
    F: Fn(T) -> Option<E> + Copy,
{
    fn parse(self, input: &[T]) -> Option<(E, &[T])> {
        if let Some((tok, rest)) = input.split_first() {
            if let Some(e) = self.0(*tok) {
                return Some((e, rest));
            }
        }
        None
    }
}

// Define parsers for tuples up to 50 values.
eval_macro::eval! {
    fn format_nested_params(i: usize) -> String {
        (2..=i).fold("e1".to_string(), |acc, j| format!("({acc}, e{j})"))
    }

    for size in 1..=10 {
        let emit_types = (1..=size)
            .map(|i| format!("E{i}"))
            .collect::<Vec<_>>()
            .join(", ");
        let emit_value_types = (1..=size)
            .map(|i| format!("e{i}"))
            .collect::<Vec<_>>()
            .join(", ");
        let parser_params = (1..=size)
            .map(|i| format!("P{i}: Parser<T, E{i}>"))
            .collect::<Vec<_>>()
            .join(", ");
        let parser_list = (1..=size)
            .map(|i| format!("P{i}"))
            .collect::<Vec<_>>()
            .join(", ");

        let and_list = (1..size).map(|i| format!(".and(self.{i})")).collect::<String>();

        let mapper_params = format_nested_params(size);


        output! {
            impl<T, {{emit_types}}, {{parser_params}}> Parser<T, ({{emit_types}},)> for ({{parser_list}},) {
                fn parse(self, input: &[T]) -> Option<(({{emit_types}},), &[T])> {
                    self.0{{and_list}}
                        .map(|{{mapper_params}}| ({{emit_value_types}},))
                        .parse(input)
                }
            }
        }
    }
}

// Simple parsers

pub fn any<T>(input: &[T]) -> Option<(T, &[T])>
where
    T: Copy,
{
    input.split_first().map(|(t, rest)| (*t, rest))
}

fn emit<T, E: Copy>(emit: E) -> impl ClosureParser<T, E> {
    move |input| Some((emit.clone(), input))
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

// Combinators

#[macro_export]
macro_rules! or {
    ($parser:expr $(,)?) => {
        $parser
    };
    ($parser:expr, $($rest:expr),* $(,)?) => {
        $parser.or(or!($($rest),*))
    }
}

// Iterator impl

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
