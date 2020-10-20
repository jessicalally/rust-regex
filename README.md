# rust-regex
A learning project for Rust, creating a regular expression matcher. The program attempts to find a given regex expression in the given input string, returning the matched string and its starting index, or None if the regex pattern cannot be found.

## Usage
### Build
Assuming that you have Rust installed, rust-regex can be built by running:
```
git clone https://www.github.com/jessicalally/rust-regex.git
cd rust-regex
cargo build
```
### Run
To run the program, enter the terminal command:
```
cargo run <regex_pattern> <input_string>
```

## Examples
```
cargo run "ab" "abc"                                  // returns Some("ab", 0)
cargo run "\." "abc"                                  // returns Some("a", 0)
cargo run "[\.]+" "abc"                               // returns Some("abc", 0)
cargo run "https?://[\w.]+" "http://www.example.com"  // returns Some(("http://www.example.com", 0))
cargo run "[\w]+@[\w.]+" "jml19@ic.ac.uk"             // returns Some(("jml19@ic.ac.uk", 0))
cargo run "(Hello )?World" "Goodbye World"            // returns Some(("World", 8))
cargo run "[3-7]+" "0123456789"                       // returns Some(("34567", 3))
cargo run "[^3-7]+" "0123456789"                      // returns Some(("012", 0))
cargo run "0x[0-9A-F]+" "0x123456FF"                  // returns Some(("0x123456FF", 0))
cargo run "\W+" "221B Baker Street"                   // returns Some((" ", 4))
cargo run "[\d]+" "$%^&*()"                           // returns None
```

## Current Features
* `+`, `*`, `?` and `^` quantifiers
* Character classes
* Ranges of characters and numbers
* Meta-characters, e.g. `\.`, `\w`, `\d`, `\s` and their inverse
* Capturing groups

## Proposed Features and Limitations
* `|` Operator
* Non-capturing groups
* Numbered quantifiers, e.g. `{2, 4}`
* Anchors, e.g. `^`, `$` and `\b`
* Currently quantifiers are too greedy, need to implement [backtracking](https://www.rexegg.com/regex-quantifiers.html#docile)
