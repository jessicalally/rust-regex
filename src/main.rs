use rust_regex::lexer::lex;
use rust_regex::matcher::find;
use rust_regex::parser::parse;
use std::env;

fn main() -> Result<(), &'static str> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        panic!(
            "Error: too few arguments\nUsage: {} regex_pattern input_string",
            args[0]
        );
    }

    let pattern = &args[1];
    let input = &args[2];

    println!("{:?}", find(&parse(&lex(pattern)?)?, input));
    Ok(())
}
