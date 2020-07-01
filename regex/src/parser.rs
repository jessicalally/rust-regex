use crate::lexer::{Lexemes, Lexemes::*};
use crate::parser::{ClassMember::*, Atom::*, Operation::*, Node::*};

type Expr = Vec<Node>;

#[derive(Debug, PartialEq)]
pub enum Node {
    TAtom(Atom),
    TOp(Operation)
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    AtomExpr(Expr),
    AtomCh(char),
    CharClass(Vec<ClassMember>),
}

#[derive(Debug, PartialEq)]
pub enum ClassMember {
    Ch(char),
    Range(char, char)
}

#[derive(Debug, PartialEq)]
pub enum Operation {
  Plus(Atom),
  Multiply(Atom),
  Question(Atom),
  Invert(Atom)
}

fn parse_range(c : char, lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, ClassMember), &'static str> {
    if let Some((_, rest)) = lexemes.split_first() {
        if let Some((ch, rest)) = rest.split_first() {
            match ch {
                Char(d) => Ok((rest.to_vec(), Range(c, *d))),
                _       => Err("invalid token syntax"),
            }

        } else {
            Err("invalid token syntax")
        }
    
    } else {
        Err("invalid token syntax")
    }
}

pub fn parse_class_member(lexemes : &Vec<Lexemes>) -> Result<(Vec<Lexemes>, ClassMember), &'static str> {
    if let Some((first, rest)) = lexemes.split_first() {
        match first {
            Char(c) => {
                match lexemes.get(1) {
                    // character is part of a range
                    Some(Operator('-')) => parse_range(*c, rest.to_vec()),
                    _ => Ok((rest.to_vec(), Ch(*c))),
                }
            }
            
            Meta(c) => Ok((rest.to_vec(), Ch(*c))),
            _       => Err("Invalid class member"),
        }
    
    } else {
        Err("Lexemes are empty")
    }
}


pub fn parse_character_class(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Atom), &'static str> {
    let mut ch_class = Vec::new();
    let mut tokens = lexemes;

    while let Some(first) = tokens.first() {
        match first {
            RSquare => {
                match tokens.split_first() {
                    Some((_, rest)) => {
                        tokens = rest.to_vec();
                        break;
                    }
                    // since we know RSquare is the first token this should never be reached
                    None => break,
                }
            }
            _       => {
                let (rest, member) = parse_class_member(&tokens)?; 
                ch_class.push(member);
                tokens = rest;        
            }
        }
    }
    
    Ok((tokens, CharClass(ch_class)))
}


pub fn parse_atom(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Atom), &'static str> {
    if let Some((lexeme, rest)) = lexemes.split_first() {
        match lexeme {
            LSquare => parse_character_class(rest.to_vec()),
            LRound  => {
                let (rest, expr) = parse_expression(rest.to_vec())?;
                Ok((rest, AtomExpr(expr)))
            }
            Char(c) => Ok((rest.to_vec(), AtomCh(*c))),
            Meta(c) => Ok((rest.to_vec(), AtomCh(*c))),
            _       => Err("Lexeme is not an atom"),
        }
    } else {
        Err("No lexemes present")
    }
}

pub fn parse_operation<'a>(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Box<dyn Fn(Atom) -> Operation + 'a>), &'static str> {
    if let Some((first, rest)) = lexemes.split_first() {
        match first {
            Operator('+') => Ok((rest.to_vec(), Box::new(|x| Plus(x)))),
            Operator('*') => Ok((rest.to_vec(), Box::new(|x| Multiply(x)))),
            Operator('?') => Ok((rest.to_vec(), Box::new(|x| Question(x)))),
            Operator('^') => Ok((rest.to_vec(), Box::new(|x| Invert(x)))),
            _ => Err("Lexeme is not an operation"),
        }
    } else {
        Err("Error parsing operation")
    }
}


pub fn parse_term(lexemes : &Vec<Lexemes>) -> Result<(Vec<Lexemes>, Node), &'static str> {
    match lexemes.first() {
        Some(Operator(_)) => Err("No atom before Operator"),
        
        Some(_) => {
            let (rest, atom) = parse_atom(lexemes.to_vec())?;
            match rest.first() {
                Some(Operator(_)) => {
                    let (rest, op) = parse_operation(rest)?;
                    Ok((rest, TOp(op(atom))))
                }
                _ => Ok((rest, TAtom(atom))),
            }
        }
        
        None => Err("No lexemes present"),
    }
}

pub fn parse_expression(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Expr), &'static str> {
    let mut expr = Vec::new();
    let mut tokens = lexemes;
    let mut invalid = false;

    while let Some(lexeme) = tokens.first() {
        match lexeme {
            RRound => {
                if let Some((_, rest)) = tokens.split_first() {
                    tokens = rest.to_vec();
                    break;
                } else {
                    invalid = true;
                    break;
                }
            }
            _ => {
                let (rest, node) = parse_term(&tokens)?;
                expr.push(node);
                tokens = rest.to_vec();
            }
        }
    }

    if invalid {
        Err("Error processing lexemes")
    } else {
        Ok((tokens, expr))
    }
}


pub fn parse(lexemes : Vec<Lexemes>) -> Result<Expr, &'static str> {
    let (rest, expr) = parse_expression(lexemes)?;
    if rest.is_empty() {
        Ok(expr)
    } else {
        Err("Lexemes left over")
    }
}

#[cfg(test)]
mod parser_tests {    
    use crate::parser::*;
    use crate::lexer::lex;
    
    #[test]
    fn test_parse_class_member() {
        if let Ok((rest, result)) = parse_class_member(&vec![Char('a'), Char('b')]) {
            assert_eq!(false, rest.is_empty());
            assert_eq!(result, Ch('a'));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_class_member(&vec![Char('A'), Operator('-'), Char('Z')]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, Range('A', 'Z'));
        } else {
            assert!(false);
        }
    }
    
    #[test]
    fn test_parse_character_class() {
        if let Ok((rest, result)) = parse_character_class(vec![Char('a'), Char('b')]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, CharClass(vec![Ch('a'), Ch('b')]));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_character_class(vec![Char('A'), Operator('-'), Char('Z')]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, CharClass(vec![Range('A', 'Z')]));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_character_class(vec![Char('A'), Operator('-'), Char('Z'), Char('a'), Operator('-'), Char('z')]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, CharClass(vec![Range('A', 'Z'), Range('a', 'z')]));
        } else {
            assert!(false);
        }
    }
    
    #[test]
    fn test_parse_atom() {
        if let Ok((rest, result)) = parse_atom(vec![Char('a'), Char('b')]) {
            assert_eq!(rest, vec![Char('b')]);
            assert_eq!(result, AtomCh('a'));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_atom(vec![LSquare, Char('A'), Operator('-'), Char('Z'), RSquare]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, CharClass(vec![Range('A', 'Z')]));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_atom(vec![LSquare, Char('A'), Operator('-'), Char('Z'), Char('a'), Operator('-'), Char('z'), RSquare]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, CharClass(vec![Range('A', 'Z'), Range('a', 'z')]));
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_parse_term() {
        if let Ok((rest, result)) = parse_term(&vec![Char('a'), Char('b')]) {
            assert_eq!(rest, vec![Char('b')]);
            assert_eq!(result, TAtom(AtomCh('a')));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_term(&vec![LSquare, Char('A'), Operator('-'), Char('Z'), RSquare, Operator('+')]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, TOp(Plus(CharClass(vec![Range('A', 'Z')]))));
        } else {
            assert!(false);
        }
        
        if let Ok((rest, result)) = parse_term(&vec![LSquare, Char('A'), Operator('-'), Char('Z'), Char('a'), Operator('-'), Char('z'), RSquare, Operator('?')]) {
            assert_eq!(true, rest.is_empty());
            assert_eq!(result, TOp(Question(CharClass(vec![Range('A', 'Z'), Range('a', 'z')]))));
        } else {
            assert!(false);
        }
    }
    
    #[test]
    fn test_parse() {
        if let Ok(lexed) = lex(&String::from("yee+t")) {
            if let Ok(result) = parse(lexed) {
                assert_eq!(result, vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))]);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
        
        if let Ok(lexed) = lex(&String::from("(mailto:)?[\\w\\-.]+'[\\w\\-]+(.[A-Za-z]+)+")) {
            if let Ok(result) = parse(lexed) {
                assert_eq!(result, vec![TOp(Question(AtomExpr(vec![TAtom(AtomCh('m')), TAtom(AtomCh('a')), TAtom(AtomCh('i')), TAtom(AtomCh('l')), TAtom(AtomCh('t')), TAtom(AtomCh('o')), TAtom(AtomCh(':'))]))), TOp(Plus(CharClass(vec![Ch('w'), Ch('-'), Ch('.')]))), TAtom(AtomCh('\'')), TOp(Plus(CharClass(vec![Ch('w'), Ch('-')]))), TOp(Plus(AtomExpr(vec![TAtom(AtomCh('.')), TOp(Plus(CharClass(vec![Range('A', 'Z'), Range('a', 'z')])))])))]);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
    }
}
