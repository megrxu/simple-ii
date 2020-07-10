use crate::tokenizer::Token;

#[derive(Debug, PartialEq)]
pub enum Evaluable {
    Expr(Expr),
    Func(Vec<Token>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Factor(Factor),
    ExprCombination((Box<Expr>, Op, Box<Expr>)),
}

#[derive(Debug, PartialEq)]
pub enum Factor {
    Num(f32),
    Id(String),
    Assign(Assign),
    Expr(Box<Expr>),
    FuncCall(FuncCall),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, PartialEq)]
pub struct Assign {
    pub id: String,
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct FuncCall {
    pub id: String,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Func {
    pub fname: String,
    pub ids: Vec<String>,
    pub expr: Box<Expr>,
}

pub fn parse(tl: &[Token]) -> Result<Evaluable, String> {
    if let Some((expr, n)) = parse_expr(tl, false) {
        if n == tl.len() {
            Ok(Evaluable::Expr(expr))
        } else {
            Err("Expression parsed error.".into())
        }
    } else if let Some((_, n)) = parse_function(tl) {
        if n == tl.len() {
            Ok(Evaluable::Func(tl.to_vec()))
        } else {
            Err("Function parsed error.".into())
        }
    } else {
        Err("Syntax error.".into())
    }
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
            Some(Token::Val(v)) => Some((Factor::Num(*v), 1)),
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
            // Op::Add | Op::Sub => None,
            // Op::Div | Op::Mul => None,
            _ => {
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

fn parse_expr(tl: &[Token], no_new_define: bool) -> Option<(Expr, usize)> {
    if let Some(res) = parse_expr_combination(tl) {
        Some(res)
    } else if let Some((f, n)) = parse_factor(tl, no_new_define) {
        Some((Expr::Factor(f), n))
    } else {
        None
    }
}

fn parse_val(tl: &[Token]) -> Option<(Factor, usize)> {
    if let Some(Token::Val(v)) = tl.first() {
        Some((Factor::Num(*v), 1))
    } else {
        None
    }
}

fn parse_assignment(tl: &[Token]) -> Option<(Factor, usize)> {
    if let Some(Token::Id(id)) = tl.first() {
        if let Some(Token::Key(op)) = tl.get(1) {
            if op == "=" {
                if let Some((expr, n)) = parse_expr(&tl[2..], false) {
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
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_paren_expression(tl: &[Token]) -> Option<(Factor, usize)> {
    if let Some(Token::Key(lp)) = tl.first() {
        if lp == "(" {
            if let Some((expr, n)) = parse_expr(&tl[1..], false) {
                if let Some(Token::Key(rp)) = tl.get(n + 1) {
                    if rp == ")" {
                        Some((Factor::Expr(Box::new(expr)), n + 2))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_function_call(tl: &[Token], no_new_define: bool) -> Option<(Factor, usize)> {
    if no_new_define {
        return None;
    };
    if let Some(Token::Id(fname)) = tl.first() {
        let mut exprs: Vec<Expr> = vec![];
        let mut shift = 1;
        while let Some((expr, n)) = parse_expr(&tl[shift..], true) {
            exprs.push(expr);
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
    if let Some(Token::Key(key)) = tl.first() {
        if key == "fn" {
            if let Some(Token::Id(fname)) = tl.get(1) {
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
                // check the valid op
                if tl.get(skip - 1) != Some(&Token::Key("=>".into())) {
                    return None;
                }
                // check the dedup args
                let mut args = ids.to_vec();
                args.dedup();
                if ids.len() != args.len() {
                    return None;
                }
                // check the invalid args
                if let Some((expr, n)) = parse_expr(&tl[skip..], false) {
                    {
                        let _ids = get_ids(&expr);
                        for id in _ids {
                            if !ids.contains(&id) {
                                return None;
                            }
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
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn parse_factor(tl: &[Token], no_new_define: bool) -> Option<(Factor, usize)> {
    if let Some((a, n)) = parse_assignment(tl) {
        Some((a, n))
    } else if let Some((fe, n)) = parse_paren_expression(tl) {
        Some((fe, n))
    } else if let Some((fc, n)) = parse_function_call(tl, no_new_define) {
        Some((fc, n))
    } else if let Some(Token::Id(id)) = tl.first() {
        Some((Factor::Id(id.to_string()), 1))
    } else if let Some((f, n)) = parse_val(tl) {
        Some((f, n))
    } else {
        None
    }
}
