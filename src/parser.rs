use crate::tokenizer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Evaluable {
    Expr(Expr),
    Func(Func),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Factor(Factor),
    ExprCombination((Box<Expr>, Op, Box<Expr>)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Factor {
    Num(f32),
    Id(String),
    Assign(Assign),
    Expr(Box<Expr>),
    FuncCall(FuncCall),
}
#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub id: String,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncCall {
    pub id: String,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub fname: String,
    pub ids: Vec<String>,
    pub expr: Box<Expr>,
}

pub fn parse(tl: &[Token]) -> Result<Evaluable, String> {
    parse_expr(tl)
        .and_then(|(expr, _)| Some(Evaluable::Expr(expr)))
        .ok_or(format!("Expression parsed error."))
        .or(parse_function(tl)
            .and_then(|(ast, _)| Some(Evaluable::Func(ast.clone())))
            .ok_or(format!("Expression parsed error.")))
}

fn parse_op(tl: &[Token]) -> Option<(Op, usize)> {
    if let Some(Token::Key(op)) = tl.first() {
        match op.as_str() {
            "*" => Some((Op::Mul, 1)),
            "/" => Some((Op::Div, 1)),
            "+" => Some((Op::Add, 1)),
            "-" => Some((Op::Sub, 1)),
            "%" => Some((Op::Mod, 1)),
            _ => None,
        }
    } else {
        None
    }
}

fn parse_atom(tl: &[Token]) -> Option<(Factor, usize)> {
    if let Some(res) = parse_paren_expression(tl) {
        Some(res)
    } else {
        match tl.first() {
            Some(Token::Id(v)) => Some((Factor::Id(v.to_string()), 1)),
            Some(Token::Val(v)) => Some((Factor::Num(v.clone()), 1)),
            _ => None,
        }
    }
}

fn parse_expr_combination(tl: &[Token]) -> Option<(Expr, usize)> {
    if let Some((f, n)) = parse_atom(tl) {
        build_expr_combination(Expr::Factor(f), &tl[n..], n)
    } else {
        None
    }
}

fn build_expr_combination(left: Expr, tl: &[Token], shift: usize) -> Option<(Expr, usize)> {
    if let Some((op, n)) = parse_op(tl) {
        match op {
            Op::Add | Op::Sub => {
                if let Some((e, m)) = parse_expr(&tl[n..]) {
                    build_expr_combination(
                        Expr::ExprCombination((Box::new(left), op, Box::new(e))),
                        &tl[(n + m)..],
                        shift + n + m,
                    )
                } else {
                    Some((left, shift + n))
                }
            }
            Op::Div | Op::Mul | Op::Mod => {
                if let Some((f, m)) = parse_atom(&tl[n..]) {
                    build_expr_combination(
                        Expr::ExprCombination((Box::new(left), op, Box::new(Expr::Factor(f)))),
                        &tl[(n + m)..],
                        shift + n + m,
                    )
                } else {
                    Some((left, shift + n))
                }
            }
        }
    } else if let Some(Token::Key(k)) = tl.first() {
        if k == "=" || k == "(" {
            None
        } else {
            Some((left, shift))
        }
    } else if tl.first().is_none() {
        Some((left, shift))
    } else {
        None
    }
}

fn parse_expr(tl: &[Token]) -> Option<(Expr, usize)> {
    if let Some(res) = parse_expr_combination(tl) {
        Some(res)
    } else if let Some((f, n)) = parse_factor(tl) {
        Some((Expr::Factor(f), n))
    } else {
        None
    }
}

fn parse_val(tl: &[Token]) -> Option<(Factor, usize)> {
    if let Some(Token::Val(v)) = tl.first() {
        Some((Factor::Num(v.clone()), 1))
    } else {
        None
    }
}

fn parse_assignment(tl: &[Token]) -> Option<(Factor, usize)> {
    let tk_id = tl.first()?;
    let tk_op = tl.get(1)?;
    match (tk_id, tk_op) {
        (Token::Id(id), Token::Key(op)) => {
            if op == "=" {
                let (expr, n) = parse_expr(&tl[2..])?;
                Some((
                    Factor::Assign(Assign {
                        id: id.to_string(),
                        expr: Box::new(expr),
                    }),
                    2 + n,
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn parse_paren_expression(tl: &[Token]) -> Option<(Factor, usize)> {
    let tk_lp = tl.first()?;
    let (expr, n) = parse_expr(&tl[1..])?;
    let tk_rp = tl.get(n + 1)?;
    match (tk_lp, tk_rp) {
        (Token::Key(lp), Token::Key(rp)) => {
            if lp == "(" && rp == ")" {
                Some((Factor::Expr(Box::new(expr)), n + 2))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn parse_function_call(tl: &[Token]) -> Option<(Factor, usize)> {
    if let Some(Token::Id(fname)) = tl.first() {
        let mut exprs: Vec<Expr> = vec![];
        let mut shift = 1;
        while let Some((f, n)) = parse_factor(&tl[shift..]) {
            exprs.push(Expr::Factor(f));
            shift += n;
        }
        if exprs.is_empty() {
            None
        } else {
            Some((
                Factor::FuncCall(FuncCall {
                    id: fname.to_string(),
                    exprs,
                }),
                shift,
            ))
        }
    } else {
        None
    }
}

fn get_ids(expr: &Expr) -> Vec<String> {
    let mut res = vec![];
    match expr {
        Expr::Factor(Factor::Id(id)) => res.push(id.to_string()),
        Expr::Factor(Factor::Expr(e)) => {
            let mut other = get_ids(e);
            res.append(&mut other);
        }
        Expr::ExprCombination((le, _, re)) => {
            let mut l = get_ids(le);
            let mut r = get_ids(re);
            res.append(&mut l);
            res.append(&mut r);
        }
        _ => (),
    }
    res
}

pub fn parse_function(tl: &[Token]) -> Option<(Func, usize)> {
    let tk_key = tl.first()?;
    let tk_id = tl.get(1)?;
    let ids: Vec<String> = tl[2..]
        .iter()
        .take_while(|x| match x {
            Token::Id(_) => true,
            _ => false,
        })
        .filter_map(|x| match x {
            Token::Id(vname) => Some(vname.to_string()),
            _ => None,
        })
        .collect();
    let skip = 3 + ids.len();
    let tk_op = tl.get(skip - 1)?;
    let mut args = ids.to_vec();
    args.dedup();
    if ids.len() != args.len() {
        return None;
    }
    match (tk_key, tk_id, tk_op) {
        (Token::Key(key), Token::Id(fname), Token::Key(op)) => {
            if key == "fn" && op == "=>" {
                let (expr, n) = parse_expr(&tl[skip..])?;
                let _ids = get_ids(&expr);
                for id in _ids {
                    if !ids.contains(&id) {
                        return None;
                    }
                }
                Some((
                    Func {
                        fname: fname.to_string(),
                        ids,
                        expr: Box::new(expr),
                    },
                    skip + n,
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn parse_factor(tl: &[Token]) -> Option<(Factor, usize)> {
    parse_assignment(tl)
        .or(parse_paren_expression(tl))
        .or(parse_function_call(tl))
        .or(match tl.first() {
            Some(Token::Id(id)) => Some((Factor::Id(id.to_string()), 1)),
            _ => None,
        }
        .or(parse_val(tl).or(None)))
}
