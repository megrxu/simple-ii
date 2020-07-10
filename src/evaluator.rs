use crate::parser::*;
use crate::tokenizer::Token;
use std::collections::HashMap;

pub type Env = HashMap<String, Value>;

#[derive(Debug)]
pub enum Value {
    Num(f32),
    // dirty, since we can not easily deep copy the
    // recursive AST for further using, we may store
    // the tokens instead
    Func(Vec<Token>),
}

impl Factor {
    fn eval(&self, env: &mut Env) -> Result<Option<f32>, String> {
        match self {
            Factor::Num(v) => Ok(Some(*v)),
            Factor::Assign(assign) => match assign.expr.eval(env) {
                Ok(Some(v)) => {
                    if let Some(Value::Func(_)) = env.get(&assign.id) {
                        return Err("Has been defined as function.".into());
                    }
                    env.insert(assign.id.to_string(), Value::Num(v));
                    Ok(Some(v))
                }
                res => res,
            },
            Factor::Id(id) => match env.get(&id.to_string()) {
                Some(Value::Num(v)) => Ok(Some(*v)),
                Some(Value::Func(f)) => {
                    let (ast, _) = parse_function(&f).unwrap();
                    ast.expr.eval(env)
                }
                None => Err(format!("`{}` is not defined yet.", id)),
            },
            Factor::FuncCall(fc) => match env.get(&fc.id) {
                Some(Value::Func(fts)) => {
                    let (ast, _) = parse_function(&fts).unwrap();
                    if ast.ids.len() != fc.exprs.len() {
                        return Err("Arguments numbers not fit.".into());
                    }
                    let mut tmp: HashMap<String, Value> = HashMap::default();
                    let mut iter = fc.exprs.iter();
                    for id in &ast.ids {
                        if let Some(expr) = iter.next() {
                            match expr.eval(env) {
                                Ok(Some(res)) => {
                                    tmp.insert(id.to_string(), Value::Num(res));
                                }
                                Ok(None) => return Err("Unexpected.".into()),
                                err => return err,
                            }
                        } else {
                            return Err(format!("Function `{}` has not enough arguments.", fc.id));
                        }
                    }
                    ast.expr.eval(&mut tmp)
                }
                _ => Err("Not defined yet.".into()),
            },
            Factor::Expr(expr) => expr.eval(env),
        }
    }
}

impl Expr {
    fn eval(&self, env: &mut Env) -> Result<Option<f32>, String> {
        match self {
            Expr::Factor(f) => f.eval(env),
            Expr::ExprCombination((el, op, er)) => match (el.eval(env), er.eval(env)) {
                (Ok(Some(a)), Ok(Some(b))) => match op {
                    Op::Add => Ok(Some(a + b)),
                    Op::Sub => Ok(Some(a - b)),
                    Op::Mul => Ok(Some(a * b)),
                    Op::Div => Ok(Some(a / b)),
                    Op::Mod => Ok(Some(a % b)),
                },
                _ => Err("Error occured.".into()),
            },
        }
    }
}

impl Evaluable {
    pub fn eval(&self, env: &mut Env) -> Result<Option<f32>, String> {
        match self {
            Evaluable::Expr(exp) => exp.eval(env),
            Evaluable::Func(func) => {
                let (ast, _) = parse_function(func).unwrap();
                if let Some(Value::Num(_)) = env.get(&ast.fname) {
                    return Err("Has been defined as variable.".into());
                }
                env.insert(ast.fname, Value::Func(func.to_vec()));
                Ok(None)
            }
        }
    }
}
