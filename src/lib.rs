mod common {
    use std::{fmt, error, result};

    pub type Id = String;

    #[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
    pub enum Op2 {
        LT,
        GT,
        LTE,
        GTE,
        Eq,
        Add,
        Sub,
        Mul,
        And,
        Or,
        Impl,
        Iff,
    }

    #[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
    pub enum Const {
        Int(i64),
        Bool(bool),
    }

    #[derive(Debug)]
    pub struct LiquidError {
        msg: String,
    }

    impl LiquidError {
        pub fn new(msg: String) -> LiquidError {
            LiquidError { msg: msg }
        }
    }

    impl fmt::Display for LiquidError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.msg)
        }
    }

    impl error::Error for LiquidError {
        fn description(&self) -> &str {
            &self.msg
        }
    }

    pub type Result<T> = result::Result<T, LiquidError>;
}

mod typed {
    pub use common::{Id, Op2, Const};

    #[derive(PartialEq, Eq, Debug, Clone)]
    pub enum Expr<Ty> {
        Var(Id),
        Const(Const),
        Op2(Op2, Box<Expr<Ty>>, Box<Expr<Ty>>),
        Fun(Id, Ty, Box<Expr<Ty>>),
        App(Box<Expr<Ty>>, Box<Expr<Ty>>),
        If(Box<Expr<Ty>>, Box<Expr<Ty>>, Box<Expr<Ty>>),
        Let(Id, Box<Expr<Ty>>, Box<Expr<Ty>>),
    }
}

mod explicit {
    use typed;

    pub type Metavar = (i32, String);

    #[derive(PartialEq, Eq, Hash, Debug, Clone)]
    pub enum Type {
        TInt,
        TBool,
    }

    pub type Expr = typed::Expr<Type>;
}

use common::{Result, Id, Op2, Const};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Imm {
    Bool(bool),
    Int(i64),
    Var(Id),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Op {
    Op2(Op2, Box<Imm>, Box<Imm>),
    Imm(Imm),
}
//    WellFormed(Imm), // Var-only

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expr {
    Let(Id, Box<Expr>, Box<Expr>),
    Op(Op),
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct ConvEnv {
    next_id: i32,
}

impl ConvEnv {
    fn new() -> ConvEnv {
        ConvEnv {
            next_id: 0,
        }
    }

    fn tmp(&self) -> (ConvEnv, Id) {
        let id = self.next_id;
        let c = ConvEnv{
            next_id: self.next_id+1,
        };

        (c, format!("!tmp!{}", id))
    }
}

fn op(cenv: &ConvEnv, e: &common::Const) -> (ConvEnv, Op) {
    match *e {
        Const::Bool(c) => ((*cenv).clone(), Op::Imm(Imm::Bool(c))),
        Const::Int(c) => ((*cenv).clone(), Op::Imm(Imm::Int(c))),
    }
}

fn expr<F>(cenv: &ConvEnv, e: &explicit::Expr, k: F) -> (ConvEnv, Expr)
    where F: FnOnce(&ConvEnv, &Expr) -> (ConvEnv, Expr) {

    use self::Imm as I;
    use typed::Expr as E;

    match *e {
        E::Const(ref c) => {
            let (cenv, eop) = op(cenv, c);
            (cenv, Expr::Op(eop))
        }
        /********************************************** this causes issues **/
        E::Op2(op, ref l, ref r) => {
            let (cenv, l_tmp) = cenv.tmp();
            let (cenv, r_tmp) = cenv.tmp();

            expr(&cenv, l, |cenv2: &ConvEnv, ll: &Expr| {

                let (cenv5, outer_expr) = expr(&cenv2, r, |cenv3: &ConvEnv, rr: &Expr| {

                    // value to pass to the continuation
                    let val = Expr::Op(Op::Op2(op,
                                     Box::new(I::Var(l_tmp.clone())),
                                     Box::new(I::Var(r_tmp.clone()))));

                    // our inner expression is whatever the
                    // continuation says it is.
                    let (cenv4, inner_expr) = k(&cenv3, &val);
                    let r = Expr::Let(r_tmp.clone(), Box::new(rr.clone()), Box::new(inner_expr));
                    (cenv4, r)
                });

                let l = Expr::Let(l_tmp.clone(), Box::new(ll.clone()), Box::new(outer_expr));
                (cenv5, l)
            })
        }
        _ => {
            panic!("TODO: implement expr for {:?}", e);
        }
    }
}

pub fn anf(explicit_expr: &explicit::Expr) -> Result<Expr> {
    let cenv = ConvEnv::new();
    let (_, expr) = expr(&cenv, &explicit_expr, |ce: &ConvEnv, x: &Expr| ((*ce).clone(), x.clone()));

    Ok(expr)
}

#[test]
fn anf_transforms() {
    use self::Imm as I;
    use self::Op::*;
    use self::Expr::*;

    let explicit_expr = typed::Expr::Const(common::Const::Int(-22));
    let anf_expr = match anf(&explicit_expr) {
        Ok(anf_expr) => anf_expr,
        Err(_) => {
            std::process::exit(1);
        }
    };
    if anf_expr != Op(Imm(I::Int(-22))) {
        std::process::exit(1);
    }
}
