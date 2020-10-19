use crate::parser::{
    Atom, Atom::*, ClassMember, ClassMember::*, Operation::*, Term, Term::*,
};

fn split_first(s: &str) -> Option<(String, String)> {
    Some((s.chars().next()?.to_string(), s[1..].to_string()))
}

fn match_character(regex_c: char, s: &str) -> Option<(String, String)> {
    if regex_c == s.chars().next()? {
        return split_first(s);
    }

    None
}

fn match_character_class(members: &Vec<ClassMember>, s: &str) -> Option<(String, String)> {
    for member in members {
        match member {
            Ch(regex_c) => {
                if *regex_c == s.chars().next()? {
                    return split_first(s);
                }
            }
            Range(lower, upper) => {
                let input_c = s.chars().next()?;

                if input_c >= *lower && input_c <= *upper {
                   return split_first(s);
                }
            }
        }
    }

    None        
}

fn match_atom(atom: &Atom, s: &str) -> Option<(String, String)> {
    match atom {
        AtomExpr(expr) => match_expression(expr, s),
        AtomCh(c) => match_character(*c, s),
        CharClass(members) => match_character_class(members, s),
    }
}

fn match_quantifier(atom: &Atom, acc: &str, s: &str) -> Option<(String, String)> {
    let mut acc = String::from(acc);
    let mut rest = String::from(s);
    
    while let Some((next_match, remaining)) = match_atom(&atom, &rest) {
        acc = format!("{}{}", acc, next_match);
        rest = remaining;
    }

    return Some((acc, rest))
}

/// Matches one or more successive occurences of the atom
fn match_plus(atom: &Atom, s: &str) -> Option<(String, String)> {
    let (str_matched, rest) = match_atom(&atom, s)?;

    return match_quantifier(atom, &str_matched, &rest);
}

/// Matches zero or more successive occurences of the atom
fn match_star(atom: &Atom, s: &str) -> Option<(String, String)> {
    return match_quantifier(atom, "", s);
}

/// Matches zero or one successive occurences of the atom
fn match_question(atom: &Atom, s: &str) -> Option<(String, String)> {
    Some(match_atom(&atom, s).unwrap_or((String::from(""), s.to_string())))
}

/// Matches of if character is not contained in the character class
fn match_invert(atom: &Atom, s: &str) -> Option<(String, String)> {
    // pre: atom is a CharacterClass
    match atom {
        CharClass(members) => match match_character_class(members, s) {
            Some(_) => None,
            None => {
                Some((s.chars().next().unwrap().to_string(), s[1..].to_string()))
            }
        }
        _ => return None,
    }
}

fn match_term(term: &Term, s: &str) -> Option<(String, String)> {
    match term {
        TAtom(atom) => match_atom(&atom, s),
        TOp(Plus(atom)) => match_plus(atom, s),
        TOp(Star(atom)) => match_star(atom, s),
        TOp(Question(atom)) => match_question(atom, s),
        TOp(Invert(atom)) => match_invert(atom, s),
    }
}

fn match_expression(expr: &[Term], s: &str) -> Option<(String, String)> {
    expr.iter()
        .fold(Some((String::new(), s.to_string())), |acc, x| {
            let (matched, rest) = acc?;
            let (next_matched, rest) = match_term(x, &rest)?;
            Some((format!("{}{}", matched, next_matched), rest))
    })
}

fn matcher(expr: &[Term], s: &str) -> Option<String> {
    Some(match_expression(expr, s)?.0)
}

pub fn find(expr: &[Term], s: &str) -> Option<(String, u32)> {
    for i in 0..s.len() {
        if let Some(matched) = matcher(&expr, &s[i..]) {
            return Some((matched, i as u32));
        }
    }

    None
}

#[cfg(test)]
mod matcher_tests {

    use crate::matcher::*;

    #[test]
    fn test_match_character() {
        let (matched, rest) = match_character('a', "ab").expect("Failure to match character");
        assert_eq!(matched, "a");
        assert_eq!(rest, "b");

        assert_eq!(match_character('b', "ab"), None);
    }

    #[test]
    fn test_match_character_class() {
        let (matched, rest) =
            match_character_class(&vec![Ch('a')], "ab").expect("Failure on character class member");
        assert_eq!(matched, "a");
        assert_eq!(rest, "b");

        let (matched, rest) = match_character_class(&vec![Ch('0'), Range('a', 'z')], "ba")
            .expect("Failure on range class member");
        assert_eq!(matched, "b");
        assert_eq!(rest, "a");

        assert_eq!(match_character_class(&vec![Ch('c')], "ab"), None);
    }

    #[test]
    fn match_atom_test() {
        let (matched, rest) = match_atom(&AtomCh('a'), "a").expect("Failure with AtomCh");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_atom(&CharClass(vec![Ch('_'), Range('a', 'z')]), "abc")
            .expect("Failure with CharClass");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("bc"));

        let (matched, rest) = match_atom(
            &AtomExpr(vec![
                TAtom(AtomCh('a')),
                TAtom(AtomCh('b')),
                TAtom(CharClass(vec![Ch('a'), Ch('c')])),
            ]),
            "abc",
        )
        .expect("Failure with subexpression");
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

        let (matched, rest) =
            match_star(&AtomCh('a'), "").expect("Failure with Star operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));

        let (matched, rest) =
            match_star(&AtomCh('a'), "aaa").expect("Failure with Star operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) =
            match_question(&AtomCh('a'), "").expect("Failure with Question operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));

        let (matched, rest) =
            match_question(&AtomCh('a'), "aaa").expect("Failure with Question operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("aa"));

        assert_eq!(None, match_invert(&CharClass(vec![Ch('a')]), "a"));

        let (matched, rest) =
            match_invert(&CharClass(vec![Ch('b')]), "a").expect("Failure with Invert operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_invert(&CharClass(vec![Ch('_'), Range('a', 'z')]), "Abc")
            .expect("Failure with Invert operation");
        assert_eq!(matched, String::from("A"));
        assert_eq!(rest, String::from("bc"));
    }

    #[test]
    fn test_match_term() {
        let (matched, rest) = match_term(&TAtom(AtomCh('a')), "a").expect("Failure with AtomCh");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_term(&TAtom(CharClass(vec![Ch('_'), Range('a', 'z')])), "abc")
            .expect("Failure with CharClass");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("bc"));

        let (matched, rest) = match_term(
            &TAtom(AtomExpr(vec![
                TAtom(AtomCh('a')),
                TAtom(AtomCh('b')),
                TAtom(CharClass(vec![Ch('a'), Ch('c')])),
            ])),
            "abc",
        )
        .expect("Failure with subexpression");
        assert_eq!(matched, String::from("abc"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) =
            match_term(&TOp(Plus(AtomCh('a'))), "a").expect("Failure with Plus operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) =
            match_term(&TOp(Plus(AtomCh('a'))), "aaa").expect("Failure with Plus operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        assert_eq!(match_term(&TOp(Plus(AtomCh('a'))), ""), None);

        let (matched, rest) =
            match_term(&TOp(Star(AtomCh('a'))), "").expect("Failure with Star operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_term(&TOp(Star(AtomCh('a'))), "aaa")
            .expect("Failure with Star operation");
        assert_eq!(matched, String::from("aaa"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) =
            match_term(&TOp(Question(AtomCh('a'))), "").expect("Failure with Question operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_term(&TOp(Question(AtomCh('a'))), "aaa")
            .expect("Failure with Question operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from("aa"));

        assert_eq!(
            None,
            match_term(&TOp(Invert(CharClass(vec![Ch('a')]))), "a")
        );

        let (matched, rest) = match_term(&TOp(Invert(CharClass(vec![Ch('b')]))), "a")
            .expect("Failure with Invert operation");
        assert_eq!(matched, String::from("a"));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_term(
            &TOp(Invert(CharClass(vec![Ch('_'), Range('a', 'z')]))),
            "Abc",
        )
        .expect("Failure with Invert operation");
        assert_eq!(matched, String::from("A"));
        assert_eq!(rest, String::from("bc"));
    }

    #[test]
    fn test_match_expression() {
        let (matched, rest) = match_expression(
            &vec![
                TAtom(AtomCh('h')),
                TAtom(AtomCh('e')),
                TOp(Plus(AtomCh('l'))),
                TAtom(AtomCh('o')),
            ],
            "helo there",
        )
        .expect("Failure with expression matcher");
        assert_eq!(matched, "helo");
        assert_eq!(rest, " there");

        let (matched, rest) = match_expression(
            &vec![
                TAtom(AtomCh('h')),
                TAtom(AtomCh('e')),
                TOp(Plus(AtomCh('l'))),
                TAtom(AtomCh('o')),
            ],
            "helllo there",
        )
        .expect("Failure with expression matcher");
        assert_eq!(matched, "helllo");
        assert_eq!(rest, " there");

        assert_eq!(
            None,
            match_expression(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "hi there"
            )
        );
    }

    #[test]
    fn test_matcher() {
        assert_eq!(
            matcher(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "helo"
            )
            .expect("Failure with expression matcher"),
            "helo"
        );

        assert_eq!(
            matcher(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "helllo"
            )
            .expect("Failure with expression matcher"),
            "helllo"
        );

        assert_eq!(
            None,
            matcher(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "heo"
            )
        );
    }

    #[test]
    fn test_find() {
        assert_eq!(
            find(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "hello"
            )
            .expect("Failure with expression at head"),
            (String::from("hello"), 0)
        );

        assert_eq!(
            find(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "hi there, hello"
            )
            .expect("Failure with expression in middle"),
            (String::from("hello"), 10)
        );

        assert_eq!(
            None,
            find(
                &vec![
                    TAtom(AtomCh('h')),
                    TAtom(AtomCh('e')),
                    TOp(Plus(AtomCh('l'))),
                    TAtom(AtomCh('o'))
                ],
                "heo"
            )
        );
    }
}
