use rust_regex::lexer::lex;
use rust_regex::matcher::find;
use rust_regex::parser::parse;
use std::env;

fn main() -> Result<(), &'static str> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        panic!("Error: too few arguments\nUsage: ./rust-regex regex_pattern input_string");
    }

    let pattern = &args[1];
    let input = &args[2];

    println!("{:?}", find(&parse(&lex(pattern)?)?, input));
    Ok(())
}
