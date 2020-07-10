use crate::evaluator::Env;
use crate::parser::parse;
use crate::tokenizer::tokenize;
use std::collections::HashMap;

#[derive(Default)]
pub struct Interpreter {
  env: Env,
}

impl Interpreter {
  pub fn new() -> Interpreter {
    Interpreter {
      env: HashMap::default(),
    }
  }

  pub fn input(&mut self, input: &str) -> Result<Option<f32>, String> {
    if input.trim() == "" {
      return Ok(None);
    }
    let tokens = tokenize(input);
    match parse(&tokens) {
      Ok(expr) => expr.eval(&mut self.env),
      Err(s) => Err(s),
    }
  }
}
