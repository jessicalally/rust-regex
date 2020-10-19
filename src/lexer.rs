use self::Lexemes::*;
use std::str::Chars;

const META_CHARS: &str = ".wbdsWBDS";

#[derive(Debug, PartialEq, Clone)]
pub enum Lexemes {
    Char(char),
    Meta(char),
    Quantifier(char),
    LSquare,
    LRound,
    RSquare,
    RRound,
}

/// Lexes an individual character class. The only special characters or metacharacters inside a character class
/// are the closing bracket ], the backslash \, the caret ^, and the hyphen -.
/// If we have a caret that is not the first character in a character class, it should be treated as a regular
/// character.
/// # Arguments
/// * regex - A string slice holding the regex string
fn lex_class(regex: &mut Chars, lexemes: &mut Vec<Lexemes>) -> Result<(), &'static str> {
    let mut char_iter = regex.enumerate();
    let mut inverted = false;

    while let Some((i, c)) = char_iter.next() {
        match c {
            '-' => lexemes.push(Quantifier(c)),
            '^' => {
                if i == 0 {
                    inverted = true;
                } else {
                    lexemes.push(Char(c))
                }
            }

            ']' => {
                lexemes.push(RSquare);

                if inverted {
                    lexemes.push(Quantifier('^'));
                }

                return Ok(());
            }

            '\\' => {
                if let Some((_, c)) = char_iter.next() {
                    if META_CHARS.contains(c) {
                        lexemes.push(Meta(c));
                    } else {
                        lexemes.push(Char(c));
                    }
                } else {
                    return Err("Invalid character class");
                }
            }
            _ => lexemes.push(Char(c)),
        }
    }

    Err("Invalid character class")
}

/// Lexes the inputted regex string
///
/// # Arguments
/// * 'regex' - A string slice holding the regex string
pub fn lex(regex: &str) -> Result<Vec<Lexemes>, &'static str> {
    let mut lexemes = Vec::new();
    let mut char_iter = regex.chars();

    while let Some(c) = char_iter.next() {
        match c {
            '[' => {
                lexemes.push(LSquare);
                lex_class(&mut char_iter, &mut lexemes)?;
            }

            ']' => {
                return Err(
                    "Regex string contains closing bracket without matching opening bracket",
                )
            }

            '+' | '*' | '?' | '-' => lexemes.push(Quantifier(c)),
            '(' => lexemes.push(LRound),
            ')' => lexemes.push(RRound),
            '\\' => match char_iter.next() {
                Some(c) => {
                    if META_CHARS.contains(c) {
                        lexemes.push(Meta(c));
                    } else {
                        lexemes.push(Char(c));
                    }
                }
                None => return Err("Invalid metacharacter syntax"),
            },
            _ => lexemes.push(Char(c)),
        }
    }

    Ok(lexemes)
}

#[cfg(test)]
mod lexer_tests {
    use crate::lexer::*;

    #[test]
    fn test_lex_class() {
        let mut regex = "hello!]".chars();
        let mut lexemes = vec![];
        assert_eq!(lex_class(&mut regex, &mut lexemes), Ok(()));
        assert_eq!(regex.collect::<String>(), "");
        assert_eq!(
            lexemes,
            vec![
                Char('h'),
                Char('e'),
                Char('l'),
                Char('l'),
                Char('o'),
                Char('!'),
                RSquare
            ]
        );

        let mut regex = "A-Za-z]".chars();
        let mut lexemes = vec![];
        assert_eq!(lex_class(&mut regex, &mut lexemes), Ok(()));
        assert_eq!(regex.collect::<String>(), "");
        assert_eq!(
            lexemes,
            vec![
                Char('A'),
                Quantifier('-'),
                Char('Z'),
                Char('a'),
                Quantifier('-'),
                Char('z'),
                RSquare
            ]
        );

        let mut regex = "A-Z_]world".chars();
        let mut lexemes = vec![];
        assert_eq!(lex_class(&mut regex, &mut lexemes), Ok(()));
        assert_eq!(regex.collect::<String>(), "world");
        assert_eq!(
            lexemes,
            vec![Char('A'), Quantifier('-'), Char('Z'), Char('_'), RSquare]
        );
    }

    #[test]
    fn test_lex() {
        let lexemes = lex("hello!");
        assert_eq!(
            lexemes,
            Ok(vec![
                Char('h'),
                Char('e'),
                Char('l'),
                Char('l'),
                Char('o'),
                Char('!')
            ])
        );

        let lexemes = lex("yee+t");
        assert_eq!(
            lexemes,
            Ok(vec![
                Char('y'),
                Char('e'),
                Char('e'),
                Quantifier('+'),
                Char('t')
            ])
        );

        let lexemes = lex("[\\w\\-.]");
        assert_eq!(
            lexemes,
            Ok(vec![LSquare, Meta('w'), Char('-'), Char('.'), RSquare])
        );

        let lexemes = lex("(mailto:)?[\\w\\-.]+@[\\w\\-]+(.[A-Za-z]+)+");
        assert_eq!(
            lexemes,
            Ok(vec![
                LRound,
                Char('m'),
                Char('a'),
                Char('i'),
                Char('l'),
                Char('t'),
                Char('o'),
                Char(':'),
                RRound,
                Quantifier('?'),
                LSquare,
                Meta('w'),
                Char('-'),
                Char('.'),
                RSquare,
                Quantifier('+'),
                Char('@'),
                LSquare,
                Meta('w'),
                Char('-'),
                RSquare,
                Quantifier('+'),
                LRound,
                Char('.'),
                LSquare,
                Char('A'),
                Quantifier('-'),
                Char('Z'),
                Char('a'),
                Quantifier('-'),
                Char('z'),
                RSquare,
                Quantifier('+'),
                RRound,
                Quantifier('+')
            ])
        );
    }
}
