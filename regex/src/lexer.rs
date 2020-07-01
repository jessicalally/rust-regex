use crate::lexer::Lexemes::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Lexemes {
    Char(char),
    Meta(char),
    Operator(char),
    LSquare,
    LRound,
    RSquare,
    RRound,
}

pub fn lex_class(s : &String) -> (Result<Vec<Lexemes>, String>, String) {
    let mut result = Vec::new();
    let mut iterator = s.chars();
    let mut invalid = false;

    while let Some(c) = iterator.next() {
        match c {
            '+' | '*' | '?' | '-' => result.push(Operator(c)),
            '(' => result.push(LRound),
            ')' => result.push(RRound),
            '[' => invalid = true,
            ']' => {
                let mut rest = String::from("]");
                rest.push_str(&iterator.as_str().to_string());
                return (Ok(result), rest)
            }
            '\\' => {
                match iterator.next() {
                    Some(c) => {
                        if "wbdsWBDS".contains(c) {
                            result.push(Meta(c));
                        } else {
                            result.push(Char(c));
                        }
                    }
            None => invalid = true,
                }
            }
            _ => result.push(Char(c)),
        }
    }
    
    if invalid {
        (Err(String::from("Invalid character class")), iterator.as_str().to_string())
    } else {
        (Ok(result), iterator.as_str().to_string())
    }
}

pub fn lex(s : &String) -> Result<Vec<Lexemes>, String> {
    let mut result = Vec::new();
    let mut temp_str;
    let mut iter = s.chars();
    let mut invalid = false;
    let mut error = String::from("");

    while let Some(c) = iter.next() {
        match c {
            '[' => {
                result.push(LSquare);
                let (res, rest) = lex_class(&iter.as_str().to_string());
                match res {
                    Ok(mut tokens) => {
                        result.append(&mut tokens);
                        temp_str = rest.clone();
                        iter = temp_str.chars();
                    }
                    Err(err) => {
                        invalid = true;
                        error = err;
                    }
                }
            }
            ']' => result.push(RSquare),
            '+' | '*' | '?' | '-' => result.push(Operator(c)),
            '(' => result.push(LRound),
            ')' => result.push(RRound),
             _  => result.push(Char(c)),
        }
    }

    if invalid {
        Err(error)
    } else {    
        Ok(result)
    }
}

#[cfg(test)]
mod lexer_tests {    
    use crate::lexer::*;
    
    #[test]
    fn test_lex_class() {
        let (result, rest) = lex_class(&String::from("hello!"));
        assert_eq!(rest, "");
        assert_eq!(result, Ok(vec![Char('h'), Char('e'), Char('l'), Char('l'), Char('o'), Char('!')]));
        
        let (result, rest) = lex_class(&String::from("A-Za-z"));
        assert_eq!(rest, "");
        assert_eq!(result, Ok(vec![Char('A'), Operator('-'), Char('Z'), Char('a'), Operator('-'), Char('z')]));
        
        let (result, rest) = lex_class(&String::from("A-Z_]world"));
        assert_eq!(rest, "]world");
        assert_eq!(result, Ok(vec![Char('A'), Operator('-'), Char('Z'), Char('_')]));
    }
    
    #[test]
    fn test_lex() {
        let result = lex(&String::from("hello!"));
        assert_eq!(result, Ok(vec![Char('h'), Char('e'), Char('l'), Char('l'), Char('o'), Char('!')]));
        
        let result = lex(&String::from("yee+t"));
        assert_eq!(result, Ok(vec![Char('y'), Char('e'), Char('e'), Operator('+'), Char('t')]));

        let result = lex(&String::from("[\\w\\-.]"));
        assert_eq!(result, Ok(vec![LSquare, Meta('w'), Char('-'), Char('.'), RSquare]));

        let result = lex(&String::from("(mailto:)?[\\w\\-.]+@[\\w\\-]+(.[A-Za-z]+)+"));
        assert_eq!(result, Ok(vec![LRound, Char('m'), Char('a'), Char('i'), Char('l'), Char('t'), Char('o'), Char(':'), RRound, Operator('?'), LSquare, Meta('w'), Char('-'), Char('.'), RSquare, Operator('+'), Char('@'), LSquare, Meta('w'), Char('-'), RSquare, Operator('+'), LRound, Char('.'), LSquare, Char('A'), Operator('-'), Char('Z'), Char('a'), Operator('-'), Char('z'), RSquare, Operator('+'), RRound, Operator('+')]));

    }
}
