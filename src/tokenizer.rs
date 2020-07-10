use itertools::Itertools;
use lazy_static::lazy_static;

lazy_static! {
    static ref KEYWORDS: Vec<String> = vec!["fn".into()];
    static ref SYMBOLS: Vec<String> = ["+", "-", "*", "/", "%", "=>", "=", "(", ")"]
        .iter()
        .map(|x| x.to_string())
        .collect();
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Key(String), // keywords and symbols are keys
    Id(String),  // identifiers
    Val(f32),    // literals
}

enum ScanState {
    Punc,
    Alpha,
    Digit,
    Init,
}

fn scan_one(state: ScanState, prev: Option<String>, iter: &mut std::str::Chars) -> Option<Token> {
    match state {
        ScanState::Init => {
            if let Some(c) = iter.next() {
                if c.is_numeric() {
                    scan_one(ScanState::Digit, Some(c.to_string()), iter)
                } else if c.is_alphabetic() {
                    scan_one(ScanState::Alpha, Some(c.to_string()), iter)
                } else if c.is_ascii_punctuation() {
                    scan_one(ScanState::Punc, Some(c.to_string()), iter)
                } else if c == ' ' {
                    scan_one(ScanState::Init, None, iter)
                } else {
                    None
                }
            } else {
                None
            }
        }
        ScanState::Digit => {
            let predicate = |&x: &char| x.is_numeric() || x == '.';
            // since `take_while` will consume some chars, we use `peeking_take_while`
            let digits = prev.unwrap_or_else(|| "".into())
                + &iter.peeking_take_while(predicate).collect::<String>();
            match digits.parse::<f32>() {
                Ok(v) => Some(Token::Val(v)),
                Err(_) => None,
            }
        }
        ScanState::Alpha => {
            let predicate = |&x: &char| x.is_ascii_alphanumeric() || x == '_';
            let alphas = prev.unwrap_or_else(|| "".into())
                + &iter.peeking_take_while(predicate).collect::<String>();
            if KEYWORDS.contains(&alphas) {
                Some(Token::Key(alphas))
            } else {
                Some(Token::Id(alphas))
            }
        }
        ScanState::Punc => {
            // dirty, but for simplicity...
            let predicate = |&x: &char| x == '>';
            let symbols = prev.unwrap_or_else(|| "".into())
                + &iter.peeking_take_while(predicate).collect::<String>();
            if SYMBOLS.contains(&symbols) {
                Some(Token::Key(symbols))
            } else {
                None
            }
        }
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut iter = input.chars();
    let mut res = vec![];
    while let Some(t) = scan_one(ScanState::Init, None, &mut iter) {
        res.push(t);
    }
    res
}
