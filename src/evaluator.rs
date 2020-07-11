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

// rebuild the function call according to the runtime information
fn rebuild_function(fc: &FuncCall, env: &Env, root: bool) -> Option<(FuncCall, usize)> {
    match env.get(&fc.id) {
        Some(Value::Func(fts)) => {
            let (ast, _) = parse_function(fts)?;
            let args = ast.ids.len();
            let (exprs, n, flag) = rebuild_exprs(&fc.exprs, args, env)?;
            if !flag && root {
                None
            } else {
                Some((
                    FuncCall {
                        id: fc.id.to_string(),
                        exprs,
                    },
                    n,
                ))
            }
        }
        _ => None,
    }
}

fn rebuild_exprs(exprs: &[Expr], args: usize, env: &Env) -> Option<(Vec<Expr>, usize, bool)> {
    let mut iter = exprs.iter();
    let mut idx = 0;
    let mut exprs: Vec<Expr> = vec![];
    while idx < args {
        let e = iter.next();
        match e {
            Some(Expr::Factor(Factor::FuncCall(fc))) => {
                let res = env.get(&fc.id)?;
                let len = fc.exprs.len();
                match res {
                    Value::Num(v) => {
                        let mut i = 0;
                        exprs.push(Expr::Factor(Factor::Num(*v)));
                        idx += 1;
                        while i < len {
                            let (mut es, n, _) = rebuild_exprs(&fc.exprs[i..], args - idx, env)?;
                            if n == 0 {
                                break
                            }
                            idx += es.len();
                            exprs.append(&mut es);
                            i += n;
                        }
                    }
                    Value::Func(_) => {
                        let (fci, mut i) = rebuild_function(fc, env, false)?;
                        exprs.push(Expr::Factor(Factor::FuncCall(fci)));
                        idx += 1;
                        while i < len {
                            let (mut es, n, _) = rebuild_exprs(&fc.exprs[i..], args - idx, env)?;
                            idx += es.len();
                            exprs.append(&mut es);
                            i += n;
                        }
                    }
                }
            }
            Some(e) => {
                exprs.push(e.clone());
                idx += 1;
            }
            None => return Some((exprs, idx, false)),
        }
    }
    Some((exprs, idx, iter.next().is_none()))
}

impl Factor {
    fn eval(&self, env: &mut Env) -> Result<Option<f32>, String> {
        match self {
            Factor::Num(v) => Ok(Some(v.clone())),
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
                Some(Value::Num(v)) => Ok(Some(v.clone())),
                Some(Value::Func(f)) => {
                    let (ast, _) = parse_function(&f).unwrap();
                    ast.expr.eval(env)
                }
                None => Err(format!("`{}` is not defined yet.", id)),
            },
            Factor::FuncCall(fc) => {
                // rebuild the fuction call according to the contexts
                if let Some((fc, _)) = rebuild_function(fc, env, true) {
                    match env.get(&fc.id) {
                        Some(Value::Func(fts)) => {
                            let (ast, _) = parse_function(&fts).unwrap();
                            let mut tmp: HashMap<String, Value> = HashMap::default();
                            let mut iter = fc.exprs.iter();
                            for id in &ast.ids {
                                if let Some(expr) = iter.next() {
                                    if let Some(v) = expr.eval(env)? {
                                        tmp.insert(id.to_string(), Value::Num(v));
                                    } else {
                                        return Err("".into());
                                    }
                                } else {
                                    return Err(format!(
                                        "Function `{}` does not have enough arguments.",
                                        fc.id
                                    ));
                                }
                            }
                            ast.expr.eval(&mut tmp)
                        }
                        _ => Err(format!("{} is not defined yet or not a function.", &fc.id)),
                    }
                } else {
                    return Err("Not defined yet.".into());
                }
            }
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
