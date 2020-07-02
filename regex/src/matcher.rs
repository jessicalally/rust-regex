fn match_character(ch : char, s : &str) -> Option<(String, String)> {
    let (first, rest) = s.split_at(1);

    if first == ch.to_string() {
        Some((first.to_string(), rest.to_string()))
    } else {
        None
    }
}
/*
fn is_alpha_numeric(s : &str) -> bool {
    match s.chars().nth(0) {
        Some('a'..='z') => true,
        Some('A'..='Z') => true,
        Some('0'..='9') => true,
         _        => false,
    }
}

fn match_meta_character(ch : char, s : &str) -> Option<(String, String)> {
    let (mut first, mut rest) = s.split_at(1);
    let mut matched = false;

    match ch {
        '.' => matched = true,
        'b' => {
            if first == "\\" {
                let (first, rest) = s.split_at(1);
                matched = true;
            }
        }
        's' => {
            if first == " " {
                matched = true; 
            }
        }
        'w' => {
            if is_alpha_numeric(first) {
                matched = true;
            }
        }
        'd' => {
            if first >= "0" && first <= "9" {
                matched = true;
            }
        }
        'B' => {
            if first != "\\b" {
                matched = true;
            }
        }
        'S' => {
            if first != " " {
                matched = true;
            }
        }
        'W' => {
            if !is_alpha_numeric(first) {
                matched = true;
            }        
        }
        'D' => {
            if !(first >= "0" && first <= "9") {
                matched = true;
            }
        }

        _ => panic!("Unexpected meta character"),
    }  

    if matched {
        Some((first.to_string(), rest.to_string()))
    } else {
        None
    }

fn match_character_class(members : Vec<ClassMember>, s : &String) -> Option<(String, String)> {
    let (first, rest) = s.split_at(1);

    for member in members {
        match member {
            Ch(c) => {
                if (first == c) {
                    Some(first, rest)
                }
            }

            MetaCh(c) => match_meta_character(c, s),

            Range(c1, c2) {
                if (first >= c1 && first <= c2) {
                    Some(first, rest)
                }
            }
        }
    }

    None
}

fn match_atom(atom : Atom, s : &String) -> Option<(String, String)> {
    match atom {
        AtomExpr(expr) => match_expression(expr, s),
        AtomCh(c) => match_character(c, s),
        MetaAtomCh(c) => match_meta_character(c, s),
        CharClass(members) => match_character_class(members, s),
    }
}

fn match_plus(atom : Atom, s : &String) -> Option<(String, String)> {
    // matches one or more successive occurences of the atom 
    
    let mut str_matched = "";
    
    match match_atom(atom, s) {
        Some((matched, rest)) => {
            str_matched.push_str(matched);

            let mut remaining = rest;

            while let Some((matched, rest)) = match_atom(atom, remaining) {
                str_matched.push_str(matched);
                remaining = rest;
            }

            Some((str_matched, remaining))
        }   

        None => None,
    }
}

fn match_multiply(atom : Atom, s : &String) -> Option<(String, String)> {
    // matches zero or more successive occurences of the atom

    let mut str_matched = "";
    let mut remaining = s;

    while let Some((matched, rest)) = match_atom(atom, remaining) {
        str_matched.push_str(matched);
        remaining = rest;
    }

    Some((str_matched, remaining))
}

fn match_question(atom : Atom, s : &String) -> Option<(String, String)> {
    // matches zero or one successive occurences of the atom
    
    match match_atom(atom, s) {
        Some((matched, rest)) => Some((matched, rest)),
        None => Some(String::from(""), s),
    }
}

fn match_invert(atom : Atom, s : &String) -> Option<(String, String)> {
    // pre: atom is a CharacterClass
    
    match atom {
        CharClass(members) => {
            match match_character_class(members, s) {
                Some(_) => None,
                None => Some(s.split_at(1)),
            }
        }
        _ => panic!("Expected a character class"),
    }
}

fn match_term(term : Node, s : &String) -> Option<(String, String)> {
    match term {
        TAtom(atom) => match_atom(atom, s),
        TOp(Plus(atom)) => match_plus(atom, s),
        TOp(Multiply(atom)) => match_multiply(atom, s),
        TOp(Question(atom)) => match_question(atom, s),
        TOp(Invert(atom)) => match_invert(atom, s),
    }
}

fn match_expression(expr : Expression, s : String) -> Option<String, String> {   
    expr.iter()
        .fold(Some((String::new(), s)), |acc, x| {
            let (matched, rest) = acc;
            let (next_matched, rest) = match_term(x, rest);
            Some((format!("{}{}", matched, next_matched), rest))
        })
}

pub fn match(expr : Expression, s : String) -> Option<String> {
    let (matched, rest) = match_expression(expr, s)?;

    if (rest.is_empty()) {
        Some(matched)
    } else {
        panic!("Expression not formatted properly");
    }
}
*/
#[cfg(test)]
mod matcher_tests {

    use crate::matcher::*;
    
    #[test]
    fn test_match_character() {
         if let Some((matched, rest)) = match_character('a', "ab") {
            assert_eq!(matched, "a");
            assert_eq!(rest, "b");
         } else {
            assert!(false);
         }

         assert_eq!(match_character('b', "ab"), None);
    }
    
}
