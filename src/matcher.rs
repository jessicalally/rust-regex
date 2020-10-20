use crate::parser::{Atom, Atom::*, ClassMember, ClassMember::*, Quantifier::*, Term, Term::*};

/// Splits a string into its first character and remainder
///
/// # Arguments
///
/// * 's': the string to be split
fn split_first(s: &str) -> Option<(String, String)> {
    Some((s.chars().next()?.to_string(), s[1..].to_string()))
}

/// Attempts to match the first character of the input string against a single regex character
///
/// # Arguments
///
/// * 'regex_c': the character contained in a regex oattern
/// * 's': the input string to be matched against
fn match_character(regex_c: char, s: &str) -> Option<(String, String)> {
    if regex_c == s.chars().next()? {
        return split_first(s);
    }

    None
}

/// Matches any character which is not contained in the character class (i.e. the inverted character class)
///
/// # Arguments
///
/// * 'members': a slice of the members of the character class
/// * 's': the input string to be matched against
fn match_inverted_character_class(members: &[ClassMember], s: &str) -> Option<(String, String)> {
    for member in members {
        match member {
            Ch(regex_c) => {
                if *regex_c == s.chars().next()? {
                    return None;
                }
            }

            Range(lower, upper) => {
                let input_c = s.chars().next()?;

                if input_c >= *lower && input_c <= *upper {
                    return None;
                }
            }

            Caret(_) => panic!("Cannot have a doubly inverted character class"),
        }
    }

    split_first(s)
}

/// Matches any character which is contained in the character class
///
/// # Arguments
///
/// * 'members': a slice of the members of the character class
/// * 's': the input string to be matched against
fn match_character_class(members: &[ClassMember], s: &str) -> Option<(String, String)> {
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

            Caret(inverted_members) => {
                if let Some(result) = match_inverted_character_class(inverted_members, s) {
                    return Some(result);
                }
            }
        }
    }

    None
}

/// Matches the first character of the input string against a regex atom
///
/// # Arguments
///
/// * 'atom': the regex atom to match
/// * 's': the input string to be matched against
fn match_atom(atom: &Atom, s: &str) -> Option<(String, String)> {
    match atom {
        AtomExpr(expr) => match_expression(expr, s),
        AtomCh(c) => match_character(*c, s),
        CharClass(members) => match_character_class(members, s),
    }
}

/// Matches a regex atom which has a quantifier associated with it
///
/// # Arguments
///
/// * 'atom': the regex atom to match
/// * 'acc': a slice of the input string which has currently been matched
/// * 's': the input string to be matched against
fn match_quantifier(atom: &Atom, acc: &str, s: &str) -> Option<(String, String)> {
    let mut acc = String::from(acc);
    let mut rest = String::from(s);
    while let Some((next_match, remaining)) = match_atom(&atom, &rest) {
        // If the atom is equivalent to an empty expression, e.g. ()?, the function
        // will always return next_match = "", which result in an infinite loop, hence
        // we need to break if the matched string is empty
        if next_match.is_empty() {
            return Some((acc, rest));
        }

        acc = format!("{}{}", acc, next_match);
        rest = remaining;
    }

    Some((acc, rest))
}

/// Matches one or more successive occurences of a regex atom
///
/// # Arguments
///
/// * 'atom': the regex atom to match
/// * 's': the input string to be matched against
fn match_plus(atom: &Atom, s: &str) -> Option<(String, String)> {
    let (str_matched, rest) = match_atom(&atom, s)?;
    match_quantifier(atom, &str_matched, &rest)
}

/// Matches zero or more successive occurences of the atom
///
/// # Arguments
///
/// * 'atom': the regex atom to match
/// * 's': the input string to be matched against
fn match_star(atom: &Atom, s: &str) -> Option<(String, String)> {
    match_quantifier(atom, "", s)
}

/// Matches zero or one successive occurences of the atom
///
/// # Arguments
///
/// * 'atom': the regex atom to match
/// * 's': the input string to be matched against
fn match_question(atom: &Atom, s: &str) -> Option<(String, String)> {
    Some(match_atom(&atom, s).unwrap_or((String::from(""), s.to_string())))
}

/// Matches if the character is not contained in the character class
///
/// PRE: atom is a CharacterClass
///
/// # Arguments
///
/// * 'atom': the regex atom to match
/// * 's': the input string to be matched against
fn match_invert(atom: &Atom, s: &str) -> Option<(String, String)> {
    match atom {
        CharClass(members) => match match_character_class(members, s) {
            Some(_) => None,
            None => Some((s.chars().next().unwrap().to_string(), s[1..].to_string())),
        },
        _ => None,
    }
}

/// Matches a single term of the regex string
///
/// # Arguments
///
/// * 'term': the regex term to match
/// * 's': the input string to be matched against
fn match_term(term: &Term, s: &str) -> Option<(String, String)> {
    match term {
        TAtom(atom) => match_atom(&atom, s),
        TOp(Plus(atom)) => match_plus(atom, s),
        TOp(Star(atom)) => match_star(atom, s),
        TOp(Question(atom)) => match_question(atom, s),
        TOp(Invert(atom)) => match_invert(atom, s),
    }
}

/// Matches a regex expression against the input string
///
/// # Arguments
///
/// * 'expr': a slice of regex terms to match
/// * 's': the input string to be matched against
fn match_expression(expr: &[Term], s: &str) -> Option<(String, String)> {
    expr.iter()
        .fold(Some((String::new(), s.to_string())), |acc, x| {
            let (matched, rest) = acc?;
            let (next_matched, rest) = match_term(x, &rest)?;
            Some((format!("{}{}", matched, next_matched), rest))
        })
}

/// Matches a regex expression against the input string, dropping any
/// characters which have not been matched
///
/// # Arguments
///
/// * 'expr': a slice of regex terms to match
/// * 's': the input string to be matched against
fn matcher(expr: &[Term], s: &str) -> Option<String> {
    Some(match_expression(expr, s)?.0)
}

/// Attempts to find a regex expression in the input string, returning the
/// matched string and its starting index or None if the regex pattern cannot be found
///
/// # Arguments
///
/// * 'expr': a slice of regex terms to match
/// * 's': the input string to be matched against
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

        let (matched, rest) = match_star(&AtomCh('a'), "").expect("Failure with Star operation");
        assert_eq!(matched, String::from(""));
        assert_eq!(rest, String::from(""));

        let (matched, rest) = match_star(&AtomCh('a'), "aaa").expect("Failure with Star operation");
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

        let (matched, rest) =
            match_term(&TOp(Star(AtomCh('a'))), "aaa").expect("Failure with Star operation");
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
