use crate::parser::{Node, Node::*, Atom, Atom::*, ClassMember, ClassMember::*, Expr, Operation::*};

fn match_character(ch : char, s : &str) -> Option<(String, String)> {
    if !(s.is_empty()) {
        let (first, rest) = s.split_at(1);

        if first == ch.to_string() {
            Some((first.to_string(), rest.to_string()))
        } else {
            None
        }
    } else {
        None
    }
}

fn match_character_class(members : &Vec<ClassMember>, s : &str) -> Option<(String, String)> {
    let (first, rest) = s.split_at(1);

    for member in members {
        match member {
            Ch(c) => {
                if first == c.to_string() {
                    return Some((first.to_string(), rest.to_string()));
                }
            }

            Range(c1, c2) => {
                if first.to_string() >= c1.to_string() && first.to_string() <= c2.to_string() {
                    return Some((first.to_string(), rest.to_string()));
                }
            }
        }
    }

    return None;
}

fn match_atom(atom : &Atom, s : &str) -> Option<(String, String)> {
    match atom {
        AtomExpr(expr) => match_expression(expr, s),
        AtomCh(c) => match_character(*c, s),
        CharClass(members) => match_character_class(members, s),
    }
}

fn match_plus(atom : &Atom, s : &str) -> Option<(String, String)> {
    // matches one or more successive occurences of the atom 
    
    let (mut str_matched, mut rest) = match_atom(&atom, s)?;

    while let Some((matched, remaining)) = match_atom(&atom, &rest) {
        str_matched = format!("{}{}", str_matched, matched);
        rest = remaining;
    }
    
    return Some((str_matched.to_string(), rest));
}

fn match_multiply(atom : &Atom, s : &str) -> Option<(String, String)> {
    // matches zero or more successive occurences of the atom
    
    let (mut str_matched, mut rest) = (String::new(), s.to_string());

    while let Some((matched, remaining)) = match_atom(&atom, rest.as_str()) {
        str_matched = format!("{}{}", str_matched, matched);
        rest = remaining;
    }
    
    return Some((str_matched.to_string(), rest));
}

fn match_question(atom : &Atom, s : &str) -> Option<(String, String)> {
    // matches zero or one successive occurences of the atom
    
    match match_atom(&atom, s) {
        Some((matched, rest)) => Some((matched, rest)),
        None => Some((String::from(""), s.to_string())),
    }
}

fn match_invert(atom : &Atom, s : &str) -> Option<(String, String)> {
    // pre: atom is a CharacterClass   
    match atom {
        CharClass(members) => {
            match match_character_class(members, s) {
                Some(_) => None,
                None => {
                    let (matched, rest) = s.split_at(1);
                    return Some((matched.to_string(), rest.to_string()));
                }
            }
        }
        _ => panic!("Expected a character class"),
    }
}

fn match_term(term : &Node, s : &str) -> Option<(String, String)> {
    match term {
        TAtom(atom) => match_atom(&atom, s),
        TOp(Plus(atom)) => match_plus(atom, s),
        TOp(Multiply(atom)) => match_multiply(atom, s),
        TOp(Question(atom)) => match_question(atom, s),
        TOp(Invert(atom)) => match_invert(atom, s),
    }
}

fn match_expression(expr : &Expr, s : &str) -> Option<(String, String)> {   
    expr.iter()
        .fold(Some((String::new(), s.to_string())), |acc, x| {
            let (matched, rest) = acc?;
            let (next_matched, rest) = match_term(x, &rest)?;
            Some((format!("{}{}", matched, next_matched), rest))
        })
}

fn matcher(expr : &Expr, s : &str) -> Option<String> {
    Some(match_expression(expr, s)?.0)
}

pub fn find(expr : &Expr, s : &str) -> Option<(String, u32)> {
    for i in 0..s.len() {
        if let Some(matched) = matcher(&expr, &s[i..]) {
            return Some((matched, i as u32))
        }
    }

    None
}

#[cfg(test)]
mod matcher_tests {

    use crate::matcher::*;
    use crate::parser::*;
    use crate::lexer::*;
    
    #[test]
    fn test_match_character() {
        let (matched, rest) = match_character('a', "ab").expect("Failure to match character");
        assert_eq!(matched, "a");
        assert_eq!(rest, "b");

         assert_eq!(match_character('b', "ab"), None);
    }
    
    #[test]
    fn test_match_character_class() {
         let (matched, rest) = match_character_class(&vec![Ch('a')], "ab").expect("Failure on character class member");
         assert_eq!(matched, "a");
         assert_eq!(rest, "b");

         let (matched, rest) = match_character_class(&vec![Ch('0'), Range('a', 'z')], "ba").expect("Failure on range class member");
         assert_eq!(matched, "b");
         assert_eq!(rest, "a");

         assert_eq!(match_character_class(&vec![Ch('c')], "ab"), None);
    }

    #[test]
    fn match_atom_test() {
        let (matched, rest) = match_atom(&AtomCh('a'), "a").expect("Failure with AtomCh");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_atom(&CharClass(vec![Ch('_'), Range('a', 'z')]), "abc").expect("Failure with CharClass");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("bc"));
        
        let (matched, rest) = match_atom(&AtomExpr(vec![TAtom(AtomCh('a')), TAtom(AtomCh('b')), TAtom(CharClass(vec![Ch('a'), Ch('c')]))]), "abc").expect("Failure with subexpression");
        assert_eq!(matched, String::from("abc"));
        assert_eq!(rest, String::from(""));
    }
    
    #[test]
    fn match_operations_test() {
        let (matched, rest) = match_plus(&AtomCh('a'), "a").expect("Failure with Plus operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));
       
        let (matched, rest) = match_plus(&AtomCh('a'), "aaa").expect("Failure with Plus operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        assert_eq!(match_plus(&AtomCh('a'), ""), None);
        
        let (matched, rest) = match_multiply(&AtomCh('a'), "").expect("Failure with Multiply operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_multiply(&AtomCh('a'), "aaa").expect("Failure with Multiply operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_question(&AtomCh('a'), "").expect("Failure with Question operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_question(&AtomCh('a'), "aaa").expect("Failure with Question operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("aa"));
        
        assert_eq!(None, match_invert(&CharClass(vec![Ch('a')]), "a"));

        let (matched, rest) = match_invert(&CharClass(vec![Ch('b')]), "a").expect("Failure with Invert operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_invert(&CharClass(vec![Ch('_'), Range('a', 'z')]), "Abc").expect("Failure with Invert operation");
        assert_eq!(matched, String::from("A"));
        assert_eq!(rest, String::from("bc"));
    }

    #[test]
    fn test_match_term() {
        let (matched, rest) = match_term(&TAtom(AtomCh('a')), "a").expect("Failure with AtomCh");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_term(&TAtom(CharClass(vec![Ch('_'), Range('a', 'z')])), "abc").expect("Failure with CharClass");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("bc"));
        
        let (matched, rest) = match_term(&TAtom(AtomExpr(vec![TAtom(AtomCh('a')), TAtom(AtomCh('b')), TAtom(CharClass(vec![Ch('a'), Ch('c')]))])), "abc").expect("Failure with subexpression");
        assert_eq!(matched, String::from("abc"));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_term(&TOp(Plus(AtomCh('a'))), "a").expect("Failure with Plus operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));
       
        let (matched, rest) = match_term(&TOp(Plus(AtomCh('a'))), "aaa").expect("Failure with Plus operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        assert_eq!(match_term(&TOp(Plus(AtomCh('a'))), ""), None);
        
        let (matched, rest) = match_term(&TOp(Multiply(AtomCh('a'))), "").expect("Failure with Multiply operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_term(&TOp(Multiply(AtomCh('a'))), "aaa").expect("Failure with Multiply operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_term(&TOp(Question(AtomCh('a'))), "").expect("Failure with Question operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_term(&TOp(Question(AtomCh('a'))), "aaa").expect("Failure with Question operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("aa"));
        
        assert_eq!(None, match_term(&TOp(Invert(CharClass(vec![Ch('a')]))), "a"));

        let (matched, rest) = match_term(&TOp(Invert(CharClass(vec![Ch('b')]))), "a").expect("Failure with Invert operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));
        
        let (matched, rest) = match_term(&TOp(Invert(CharClass(vec![Ch('_'), Range('a', 'z')]))), "Abc").expect("Failure with Invert operation");
        assert_eq!(matched, String::from("A"));
        assert_eq!(rest, String::from("bc"));
    }

    #[test]
    fn test_match_expression() {
        let (matched, rest) = match_expression(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yeet the wheat").expect("Failure with expression matcher");
        assert_eq!(matched, "yeet");
        assert_eq!(rest, " the wheat");
        
        let (matched, rest) = match_expression(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yeeeeet the wheat").expect("Failure with expression matcher");
        assert_eq!(matched, "yeeeeet");
        assert_eq!(rest, " the wheat");

        assert_eq!(None, match_expression(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yet the wheat"));
    }

    #[test]
    fn test_matcher() {
        
        assert_eq!(matcher(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yeet the wheat").expect("Failure with expression matcher"), "yeet");
        
        assert_eq!(matcher(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yeeeeet the wheat").expect("Failure with expression matcher"), "yeeeeet");

        assert_eq!(None, matcher(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yet the wheat"));
    }

    #[test]
    fn test_find() {

        assert_eq!(find(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yeet the wheat").expect("Failure with expression at head"), (String::from("yeet"), 0));
        
        assert_eq!(find(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "don't yeet the wheat").expect("Failure with expression in middle"), (String::from("yeet"), 6));

        assert_eq!(None, find(&vec![TAtom(AtomCh('y')), TAtom(AtomCh('e')), TOp(Plus(AtomCh('e'))), TAtom(AtomCh('t'))], "yet the wheat"));
    }
}
