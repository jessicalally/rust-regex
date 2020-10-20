use self::{Atom::*, ClassMember::*, Quantifier::*, Term::*};
use crate::lexer::{Lexemes, Lexemes::*};
use std::iter::Peekable;
use std::slice::Iter;

pub type Expr = Vec<Term>;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    TAtom(Atom),
    TOp(Quantifier),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    AtomExpr(Expr),
    AtomCh(char),
    CharClass(Vec<ClassMember>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClassMember {
    Ch(char),
    Range(char, char),
    Caret(Box<[ClassMember]>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Quantifier {
    Plus(Atom),
    Star(Atom),
    Question(Atom),
    Invert(Atom),
}

const ASCII_MIN: char = ' ';
const ASCII_MAX: char = '~';
const WORD_CHARS: [ClassMember; 4] = [Ch('_'), Range('a', 'z'), Range('A', 'Z'), Range('0', '9')];

/// Parses a single meta-character, converting it to ASCII characters
///
/// # Arguments
///
/// * 'c': the meta-character
/// * 'class_members': a mutable vector that contains all the parsed members of the current character class
fn parse_meta_characters(
    c: char,
    class_members: &mut Vec<ClassMember>,
) -> Result<(), &'static str> {
    match c {
        '.' => {
            class_members.push(Ch('\t'));
            class_members.push(Range(ASCII_MIN, ASCII_MAX));
        }
        'b' => class_members.push(Ch('\n')),
        's' => {
            class_members.push(Ch(' '));
            class_members.push(Ch('\t'));
        }
        'w' => class_members.append(&mut WORD_CHARS.to_vec()),
        'd' => class_members.push(Range('0', '9')),
        'B' => class_members.push(Caret(Box::new([Ch('\n')]))),
        'S' => class_members.push(Caret(Box::new([Ch(' '), Ch('\t')]))),
        'W' => class_members.push(Caret(Box::new(WORD_CHARS))),
        'D' => class_members.push(Caret(Box::new([Range('0', '9')]))),
        _ => return Err("Invalid meta character"),
    }

    Ok(())
}

/// Parses a range of characters
///
/// # Arguments
///
/// * 'lexemes': an iterator through the regex lexemes
/// * 'class_members': a mutable vector that contains all the parsed members of the current character class
fn parse_range(
    lexemes: &mut Peekable<Iter<'_, Lexemes>>,
    class_members: &mut Vec<ClassMember>,
) -> Result<(), &'static str> {
    if let Some(Ch(lower)) = class_members.pop() {
        if let Some(Char(upper)) = lexemes.next() {
            class_members.push(Range(lower, *upper));
            return Ok(());
        }
    }

    Err("Invalid range expression")
}

/// Parses a single member of a character class
///
/// # Arguments
///
/// * 'lexeme': the current lexemes being parsed
/// * 'lexemes': an iterator through the regex lexemes
/// * 'class_members': a mutable vector that contains all the parsed members of the current character class
fn parse_class_member(
    lexeme: &Lexemes,
    lexemes: &mut Peekable<Iter<'_, Lexemes>>,
    class_members: &mut Vec<ClassMember>,
) -> Result<(), &'static str> {
    match lexeme {
        Quantifier('-') => parse_range(lexemes, class_members)?,
        Char(c) => class_members.push(Ch(*c)),
        Meta(c) => parse_meta_characters(*c, class_members)?,
        _ => return Err("Invalid class member"),
    }

    Ok(())
}

/// Parses a character class
///
/// # Arguments
///
/// * 'lexemes': an iterator through the regex lexemes
fn parse_character_class(lexemes: &mut Peekable<Iter<'_, Lexemes>>) -> Result<Atom, &'static str> {
    let mut class_members = Vec::new();

    while let Some(lexeme) = lexemes.next() {
        match lexeme {
            RSquare => {
                if let Some(Quantifier('^')) = lexemes.peek() {
                    lexemes.next();
                    return Ok(CharClass(vec![Caret(class_members.into_boxed_slice())]));
                }

                return Ok(CharClass(class_members));
            }
            _ => parse_class_member(lexeme, lexemes, &mut class_members)?,
        }
    }

    Err("Character class does not contain a closing bracket")
}

/// Parses an atom of the regex string, e.g. a character, meta-character, or square bracket
///
/// # Arguments
///
/// * 'lexemes': an iterator through the regex lexemes
fn parse_atom(lexemes: &mut Peekable<Iter<'_, Lexemes>>) -> Result<Atom, &'static str> {
    if let Some(lexeme) = lexemes.next() {
        match lexeme {
            LSquare => return parse_character_class(lexemes),
            LRound => return Ok(AtomExpr(parse_expression(lexemes)?)),
            Char(c) => return Ok(AtomCh(*c)),
            Meta(c) => {
                let mut members = Vec::new();
                parse_meta_characters(*c, &mut members)?;
                return Ok(CharClass(members));
            }
            Quantifier(_) => return Err("An quantifier must be preceeded by another atom"),
            _ => return Err("Lexeme is not an atom"),
        };
    }

    Err("No lexemes present")
}

/// Parses a regex quantifier, e.g. +, *, ? or ^
///
/// # Arguments
///
/// * 'lexemes': an iterator through the regex lexemes
fn parse_quantifier<'a>(
    lexemes: &mut Peekable<Iter<'_, Lexemes>>,
) -> Result<Box<dyn Fn(Atom) -> Quantifier + 'a>, &'static str> {
    // PRE: the next lexeme is an quantifier
    match lexemes.next().unwrap() {
        Quantifier('+') => Ok(Box::new(Plus)),
        Quantifier('*') => Ok(Box::new(Star)),
        Quantifier('?') => Ok(Box::new(Question)),
        Quantifier('^') => Ok(Box::new(Invert)),
        _ => Err("Lexeme is not an quantifier"),
    }
}

/// Parses a regex term, which may be either an atom, or an atom with a quantifier  
///
/// # Arguments
///
/// * 'lexemes': an iterator through the regex lexemes
fn parse_term(lexemes: &mut Peekable<Iter<'_, Lexemes>>) -> Result<Term, &'static str> {
    let atom = parse_atom(lexemes)?;

    if let Some(Quantifier(_)) = lexemes.peek() {
        let op = parse_quantifier(lexemes)?;
        return Ok(TOp(op(atom)));
    }

    Ok(TAtom(atom))
}

/// Parses a regex expression
///
/// # Arguments
///
/// * 'lexemes': an iterator through the regex lexemes
fn parse_expression(lexemes: &mut Peekable<Iter<'_, Lexemes>>) -> Result<Expr, &'static str> {
    let mut expr: Vec<Term> = Vec::new();

    while let Some(lexeme) = lexemes.peek() {
        match lexeme {
            RRound => {
                lexemes.next();
                return Ok(expr);
            }
            _ => expr.push(parse_term(lexemes)?),
        }
    }

    Ok(expr)
}

/// Parses the whole lexed regex pattern
///
/// # Arguments
///
/// * 'lexemes': a slice that contains all the lexemes of the regex pattern
pub fn parse(lexemes: &[Lexemes]) -> Result<Expr, &'static str> {
    let mut lexeme_iter = lexemes.iter().peekable();
    let expr = parse_expression(&mut lexeme_iter)?;

    if lexeme_iter.next().is_none() {
        return Ok(expr);
    }

    Err("Invalid regex expression")
}

#[cfg(test)]
mod parser_tests {
    use crate::lexer::lex;
    use crate::parser::*;

    #[test]
    fn test_parse_class_member() {
        let mut class_members = vec![];
        parse_class_member(
            &Char('a'),
            &mut vec![].iter().peekable(),
            &mut class_members,
        )
        .expect("Failure with parsing char");
        assert_eq!(class_members, vec![Ch('a')]);

        let mut class_members = vec![];
        parse_class_member(
            &Char('A'),
            &mut vec![Quantifier('-'), Char('Z')].iter().peekable(),
            &mut class_members,
        )
        .expect("Failure with parsing range");
        assert_eq!(class_members, vec![Ch('A')]);
    }

    #[test]
    fn test_parse_character_class() {
        let result =
            parse_character_class(&mut vec![Char('a'), Char('b'), RSquare].iter().peekable())
                .expect("Failure with char");
        assert_eq!(result, CharClass(vec![Ch('a'), Ch('b')]));

        let result = parse_character_class(
            &mut vec![Char('A'), Quantifier('-'), Char('Z'), RSquare]
                .iter()
                .peekable(),
        )
        .expect("Failure with range");
        assert_eq!(result, CharClass(vec![Range('A', 'Z')]));

        let result = parse_character_class(
            &mut vec![
                Char('A'),
                Quantifier('-'),
                Char('Z'),
                Char('a'),
                Quantifier('-'),
                Char('z'),
                RSquare,
            ]
            .iter()
            .peekable(),
        )
        .expect("Failure with multiple ranges");
        assert_eq!(result, CharClass(vec![Range('A', 'Z'), Range('a', 'z')]));
    }

    #[test]
    fn test_parse_atom() {
        let result = parse_atom(&mut vec![Char('a'), Char('b')].iter().peekable())
            .expect("Failure parsing Char atom");
        assert_eq!(result, AtomCh('a'));

        let result = parse_atom(
            &mut vec![LSquare, Char('A'), Quantifier('-'), Char('Z'), RSquare]
                .iter()
                .peekable(),
        )
        .expect("Failure parsing character class");
        assert_eq!(result, CharClass(vec![Range('A', 'Z')]));

        let result = parse_atom(
            &mut vec![
                LSquare,
                Char('A'),
                Quantifier('-'),
                Char('Z'),
                Char('a'),
                Quantifier('-'),
                Char('z'),
                RSquare,
            ]
            .iter()
            .peekable(),
        )
        .expect("Failure parsing character class with multiple ranges");
        assert_eq!(result, CharClass(vec![Range('A', 'Z'), Range('a', 'z')]));
    }

    #[test]
    fn test_parse_term() {
        let lexemes = vec![Char('a'), Char('b')];
        let result =
            parse_term(&mut lexemes.iter().peekable()).expect("Failure parsing character term");
        assert_eq!(result, TAtom(AtomCh('a')));

        let lexemes = vec![
            LSquare,
            Char('A'),
            Quantifier('-'),
            Char('Z'),
            RSquare,
            Quantifier('+'),
        ];
        let result = parse_term(&mut lexemes.iter().peekable())
            .expect("Failure parsing term with plus quantifier");
        assert_eq!(result, TOp(Plus(CharClass(vec![Range('A', 'Z')]))));

        let lexemes = vec![
            LSquare,
            Char('A'),
            Quantifier('-'),
            Char('Z'),
            Char('a'),
            Quantifier('-'),
            Char('z'),
            RSquare,
            Quantifier('?'),
        ];
        let result = parse_term(&mut lexemes.iter().peekable())
            .expect("Failure parsing term with question quantifier");
        assert_eq!(
            result,
            TOp(Question(CharClass(vec![Range('A', 'Z'), Range('a', 'z')])))
        );
    }

    #[test]
    fn test_parse() {
        let mut lexemes =
            lex(&String::from("yee+t")).expect("Failure lexing string with plus quantifier");
        let result = parse(&mut lexemes).expect("Failure parsing lexemes with plus quantifier");
        assert_eq!(
            result,
            vec![
                TAtom(AtomCh('y')),
                TAtom(AtomCh('e')),
                TOp(Plus(AtomCh('e'))),
                TAtom(AtomCh('t'))
            ]
        );

        let mut lexemes = lex(&String::from(
            "(mailto:)?[\\w\\-\\.]+'[\\w\\-]+(.[A-Za-z]+)+",
        ))
        .expect("Failure lexing string with subexpression");
        let result = parse(&mut lexemes).expect("Failure parsing lexemes with subexpression");
        assert_eq!(
            result,
            vec![
                TOp(Question(AtomExpr(vec![
                    TAtom(AtomCh('m')),
                    TAtom(AtomCh('a')),
                    TAtom(AtomCh('i')),
                    TAtom(AtomCh('l')),
                    TAtom(AtomCh('t')),
                    TAtom(AtomCh('o')),
                    TAtom(AtomCh(':'))
                ]))),
                TOp(Plus(CharClass(vec![
                    Ch('_'),
                    Range('a', 'z'),
                    Range('A', 'Z'),
                    Range('0', '9'),
                    Ch('-'),
                    Range('!', '~')
                ]))),
                TAtom(AtomCh('\'')),
                TOp(Plus(CharClass(vec![
                    Ch('_'),
                    Range('a', 'z'),
                    Range('A', 'Z'),
                    Range('0', '9'),
                    Ch('-')
                ]))),
                TOp(Plus(AtomExpr(vec![
                    TAtom(AtomCh('.')),
                    TOp(Plus(CharClass(vec![Range('A', 'Z'), Range('a', 'z')])))
                ])))
            ]
        );
    }
}
