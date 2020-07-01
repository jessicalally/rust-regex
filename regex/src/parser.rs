use crate::lexer::Lexemes;
use crate::lexer::Lexemes::*;
use crate::parser::ClassMember::*;
use crate::parser::Atom::*;
use crate::parser::Operation::*;
use crate::parser::Node::*;

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
  Question(Atom)
}

fn parse_range(c : char, lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, ClassMember), String> {
    if let Some((_, rest)) = lexemes.split_first() {
        if let Some((ch, rest)) = rest.split_first() {
        
            match ch {
                Char(d) => Ok((rest.to_vec(), Range(c, *d))),
                _       => Err(String::from("invalid token syntax")),
            }
        } else {
            Err(String::from("invalid token syntax"))
        }
    
    } else {
        Err(String::from("invalid token syntax"))
    }
}

pub fn parse_class_member(lexemes : &Vec<Lexemes>) -> Result<(Vec<Lexemes>, ClassMember), String> {
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
            _       => Err(String::from("Invalid class member")),
        }
    
    } else {
        Err(String::from("Lexemes are empty"))
    }
}


pub fn parse_character_class(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Atom), String> {
    let mut ch_class = Vec::new();
    let mut tokens = lexemes;
    let mut invalid_tokens = false;
    let mut error = String::from("");

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
                match parse_class_member(&tokens) {
                    Ok((rest, member)) => {
                        ch_class.push(member);
                        tokens = rest;
                    
                    }

                    Err(err) => {
                        invalid_tokens = true;
                        error = err;
                    }
                } 
            }
        }
    }
    
    if invalid_tokens {
        Err(error)
    } else {
        Ok((tokens, CharClass(ch_class)))
    }
}


pub fn parse_atom(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Atom), String> {
    if let Some((lexeme, rest)) = lexemes.split_first() {
        match lexeme {
            LSquare => parse_character_class(rest.to_vec()),
            LRound  => {
                if let Ok((rest, expr)) = parse_expression(rest.to_vec()) {
                    Ok((rest, AtomExpr(expr)))
                } else {
                    Err(String::from("Error parsing sub-expression"))
                }
            }
            Char(c) => Ok((rest.to_vec(), AtomCh(*c))),
            Meta(c) => Ok((rest.to_vec(), AtomCh(*c))),
            _       => Err(String::from("Lexeme is not an atom")),
        }
    } else {
        Err(String::from("No lexemes present"))
    }
}


fn plus_atom(atom : Atom) -> Operation {
    Plus(atom)
}

fn multiply_atom(atom : Atom) -> Operation {
    Multiply(atom)
}

fn question_atom(atom : Atom) -> Operation {
    Question(atom)
}

pub fn parse_operation<'a>(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Box<dyn Fn(Atom) -> Operation + 'a>), String> {
    
    if let Some((first, rest)) = lexemes.split_first() {
        match first {
            Operator('+') => Ok((rest.to_vec(), Box::new(move |x:Atom| plus_atom(x)))),
            Operator('*') => Ok((rest.to_vec(), Box::new(move |x:Atom| multiply_atom(x)))),
            Operator('?') => Ok((rest.to_vec(), Box::new(move |x:Atom| question_atom(x)))),
            _ => Err(String::from("Lexeme not operation")),
        }
    } else {
        Err(String::from("Error parsing operation"))
    }
}


pub fn parse_term(lexemes : &Vec<Lexemes>) -> Result<(Vec<Lexemes>, Node), String> {
    match lexemes.first() {
        Some(Operator(_)) => Err(String::from("No atom before Operator")),
        Some(_) => {
            if let Ok((rest, atom)) = parse_atom(lexemes.to_vec()) {
                match rest.first() {
                    Some(Operator(_)) => {
                        if let Ok((rest, op)) = parse_operation(rest) {
                            Ok((rest, TOp(op(atom))))
                        } else {
                            Err(String::from("Error parsing operation"))
                        }
                    }
                    
                    _ => Ok((rest, TAtom(atom))),
                }
            } else {
                Err(String::from("Error parsing atom"))
            }
        }
        None => Err(String::from("No lexemes present")),
    }
}

pub fn parse_expression(lexemes : Vec<Lexemes>) -> Result<(Vec<Lexemes>, Expr), String> {
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
                if let Ok((rest, node)) = parse_term(&tokens) {
                    expr.push(node);
                    tokens = rest.to_vec();
                } else {
                    invalid = true;
                    break;
                }
            }
        }
    }

    if invalid {
        Err(String::from("Error processing lexemes"))
    } else {
        Ok((tokens, expr))
    }
}


pub fn parse(lexemes : Vec<Lexemes>) -> Result<Expr, String> {
    if let Ok((rest, expr)) = parse_expression(lexemes) {
        if rest.is_empty() {
            Ok(expr)
        } else {
            Err(String::from("Lexemes left over"))
        }
    } else {
        Err(String::from("Error parsing expression"))
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
