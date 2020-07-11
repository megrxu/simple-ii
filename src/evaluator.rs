use crate::parser::*;
use std::collections::HashMap;

pub type Env = HashMap<String, Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(f32),
    // dirty, since we can not easily deep copy the
    // recursive AST for further using, we may store
    // the tokens instead
    Func(Func),
}

// rebuild the function call according to the runtime information
fn rebuild_function(fc: &FuncCall, env: &Env, root: bool) -> Option<(FuncCall, usize)> {
    match env.get(&fc.id) {
        Some(Value::Func(ast)) => {
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
    let mut flag = true;
    while idx < args {
        let e = iter.next();
        match e {
            Some(Expr::Factor(Factor::FuncCall(fc))) => {
                let res = env.get(&fc.id)?;
                let len = fc.exprs.len();
                match res {
                    Value::Num(v) => {
                        let mut i = 0;
                        exprs.push(Expr::Factor(Factor::Num(v.clone())));
                        idx += 1;
                        while i < len && args - idx != 0 {
                            let (mut es, n, b) = rebuild_exprs(&fc.exprs[i..], args - idx, env)?;
                            idx += es.len();
                            exprs.append(&mut es);
                            i += n;
                            flag = b;
                        }
                        flag &= fc.exprs.get(i).is_none();
                    }
                    Value::Func(_) => {
                        let (fci, mut i) = rebuild_function(fc, env, false)?;
                        exprs.push(Expr::Factor(Factor::FuncCall(fci)));
                        idx += 1;
                        while i < len && args - idx != 0 {
                            let (mut es, n, b) = rebuild_exprs(&fc.exprs[i..], args - idx, env)?;
                            idx += es.len();
                            exprs.append(&mut es);
                            i += n;
                            flag = b;
                        }
                        flag &= fc.exprs.get(i).is_none();
                    }
                }
            }
            Some(e) => {
                exprs.push(e.clone());
                idx += 1;
            }
            None => return Some((exprs, idx, flag)),
        }
    }
    Some((exprs, idx, iter.next().is_none() && flag))
}

impl Factor {
    fn eval(&self, env: &mut Env) -> Result<Option<f32>, String> {
        match self {
            Factor::Num(v) => Ok(Some(v.clone())),
            Factor::Assign(assign) => {
                let v = assign.expr.eval(env)?;
                let var = env.get(&assign.id);
                match (var, v) {
                    (Some(Value::Func(_)), _) => Err(format!("Has been defined as function.")),
                    (_, Some(val)) => {
                        env.insert(assign.id.to_string(), Value::Num(val));
                        Ok(v)
                    }
                    (_, _) => Ok(v),
                }
            }
            Factor::Id(id) => match env.get(&id.to_string()) {
                Some(Value::Num(v)) => Ok(Some(v.clone())),
                Some(Value::Func(ast)) => ast.expr.clone().eval(env),
                None => Err(format!("`{}` is not defined yet.", id)),
            },
            Factor::FuncCall(fc) => {
                // rebuild the fuction call according to the contexts
                let (fc, _) = rebuild_function(fc, env, true).ok_or("Not defined yet.")?;
                match env.clone().get(&fc.id) {
                    Some(Value::Func(ast)) => {
                        let mut tmp: HashMap<String, Value> = HashMap::default();
                        let mut iter = fc.exprs.iter();
                        for id in &ast.ids {
                            let expr = iter.next().ok_or(format!(
                                "Function `{}` does not have enough arguments.",
                                fc.id
                            ))?;
                            let v = expr.clone().eval(env)?;
                            tmp.insert(
                                id.to_string(),
                                Value::Num(v.ok_or("Eval error.".to_string())?),
                            );
                        }
                        ast.expr.eval(&mut tmp)
                    }
                    _ => Err(format!("{} is not defined yet or not a function.", &fc.id)),
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
            Evaluable::Func(ast) => {
                if let Some(Value::Num(_)) = env.get(&ast.fname) {
                    return Err("Has been defined as variable.".into());
                }
                env.insert(ast.fname.clone(), Value::Func(ast.clone()));
                Ok(None)
            }
        }
    }
}
