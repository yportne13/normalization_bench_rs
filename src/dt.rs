use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Var(Variable),
    Universe(i32),
    Pi(Abstraction),
    Lambda(Abstraction),
    App(Rc<Expr>, Rc<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
struct Abstraction {
    var: Variable,
    typ: Rc<Expr>,
    body: Rc<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Variable {
    String(String),
    Gensym(String, i32),
    Dummy,
}

#[derive(Clone, Debug)]
struct Context {
    vars: HashMap<Variable, (Rc<Expr>, Option<Rc<Expr>>)>,
}

impl Context {
    fn new() -> Self {
        Context {
            vars: HashMap::new(),
        }
    }

    fn lookup_ty(&self, x: &Variable) -> Option<&Rc<Expr>> {
        self.vars.get(x).map(|(ty, _)| ty)
    }

    fn lookup_value(&self, x: &Variable) -> Option<&Option<Rc<Expr>>> {
        self.vars.get(x).map(|(_, value)| value)
    }

    fn extend(&mut self, x: Variable, ty: Rc<Expr>, value: Option<Rc<Expr>>) {
        self.vars.insert(x, (ty, value));
    }
}

fn refresh(x: &Variable) -> Variable {
    static mut K: i32 = 0;
    unsafe {
        K += 1;
        match x {
            Variable::String(s) | Variable::Gensym(s, _) => Variable::Gensym(s.clone(), K),
            Variable::Dummy => Variable::Gensym("_".to_string(), K),
        }
    }
}

fn subst(s: &HashMap<Variable, Rc<Expr>>, e: &Rc<Expr>) -> Rc<Expr> {
    match &**e {
        Expr::Var(x) => s.get(x).cloned().unwrap_or_else(|| Rc::new(Expr::Var(x.clone()))),
        Expr::Universe(k) => Rc::new(Expr::Universe(*k)),
        Expr::Pi(abstraction) => Rc::new(Expr::Pi(subst_abstraction(s, abstraction))),
        Expr::Lambda(abstraction) => Rc::new(Expr::Lambda(subst_abstraction(s, abstraction))),
        Expr::App(e1, e2) => Rc::new(Expr::App(subst(s, e1), subst(s, e2))),
    }
}

fn subst_abstraction(s: &HashMap<Variable, Rc<Expr>>, a: &Abstraction) -> Abstraction {
    let x_prime = refresh(&a.var);
    let new_s = {
        let mut new_s = s.clone();
        new_s.insert(a.var.clone(), Rc::new(Expr::Var(x_prime.clone())));
        new_s
    };
    Abstraction {
        var: x_prime,
        typ: subst(s, &a.typ),
        body: subst(&new_s, &a.body),
    }
}

fn infer_type(ctx: &mut Context, e: &Rc<Expr>) -> Result<Rc<Expr>, String> {
    match &**e {
        Expr::Var(x) => ctx
            .lookup_ty(x)
            .cloned()
            .ok_or_else(|| format!("unknown identifier: {:?}", x)),
        Expr::Universe(k) => Ok(Rc::new(Expr::Universe(k + 1))),
        Expr::Pi(abstraction) => {
            let k1 = infer_universe(ctx, &abstraction.typ)?;
            let mut new_ctx = ctx.clone();
            new_ctx.extend(abstraction.var.clone(), abstraction.typ.clone(), None);
            let k2 = infer_universe(&mut new_ctx, &abstraction.body)?;
            Ok(Rc::new(Expr::Universe(std::cmp::max(k1, k2))))
        }
        Expr::Lambda(abstraction) => {
            let _ = infer_universe(ctx, &abstraction.typ)?;
            let mut new_ctx = ctx.clone();
            new_ctx.extend(abstraction.var.clone(), abstraction.typ.clone(), None);
            let te = infer_type(&mut new_ctx, &abstraction.body)?;
            Ok(Rc::new(Expr::Pi(Abstraction {
                var: abstraction.var.clone(),
                typ: abstraction.typ.clone(),
                body: te,
            })))
        }
        Expr::App(e1, e2) => {
            let (x, s, t) = infer_pi(ctx, e1)?;
            let te = infer_type(ctx, e2)?;
            check_equal(ctx, &s, &te)?;
            let mut subst_map = HashMap::new();
            subst_map.insert(x, e2.clone());
            Ok(subst(&subst_map, &t))
        }
    }
}

fn infer_universe(ctx: &mut Context, t: &Rc<Expr>) -> Result<i32, String> {
    let u = infer_type(ctx, t)?;
    /*match normalize(ctx, &u) {
        Expr::Universe(k) => Ok(k),
        _ => Err(""type expected"".to_string()),
    }*/
    todo!()
}

fn infer_pi(ctx: &mut Context, e: &Rc<Expr>) -> Result<(Variable, Rc<Expr>, Rc<Expr>), String> {
    match &**e {
        Expr::Pi(abstraction) => Ok((
            abstraction.var.clone(),
            abstraction.typ.clone(),
            abstraction.body.clone(),
        )),
        _ => Err(format!("Expected a Pi type but get {:?}", e)),
    }
}

fn check_equal(ctx: &mut Context, e1: &Rc<Expr>, e2: &Rc<Expr>) -> Result<(), String> {
    if e1 == e2 {
        Ok(())
    } else {
        Err("Types are not equal".to_string())
    }
}

#[test]
fn main() {
    // Example usage
    let mut ctx = Context::new();
    let x = Variable::String("x".to_string());
    let ty = Rc::new(Expr::Universe(0));
    ctx.extend(x.clone(), ty.clone(), None);

    let expr = Rc::new(Expr::Var(x.clone()));
    match infer_type(&mut ctx, &expr) {
        Ok(ty) => println!("Type: {:?}", ty),
        Err(e) => println!("Error: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complex_expression() {
        // 创建一个新的上下文
        let mut ctx = Context::new();

        // 定义 Type0
        let type0 = Rc::new(Expr::Universe(0));

        // 定义变量 f, x, y
        let f = Variable::String("f".to_string());
        let x = Variable::String("x".to_string());
        let y = Variable::String("y".to_string());

        // 构造恒等函数 (λy: Type0. y)
        let identity_function = Rc::new(Expr::Lambda(Abstraction {
            var: y.clone(),
            typ: type0.clone(),
            body: Rc::new(Expr::Var(y.clone())),
        }));

        // 构造高阶函数 (λf: (Type0 -> Type0). λx: Type0. f (f x))
        let higher_order_function = Rc::new(Expr::Lambda(Abstraction {
            var: f.clone(),
            typ: Rc::new(Expr::Pi(Abstraction {
                var: x.clone(),
                typ: type0.clone(),
                body: type0.clone(),
            })),
            body: Rc::new(Expr::Lambda(Abstraction {
                var: x.clone(),
                typ: type0.clone(),
                body: Rc::new(Expr::App(
                    Rc::new(Expr::Var(f.clone())),
                    Rc::new(Expr::App(Rc::new(Expr::Var(f.clone())), Rc::new(Expr::Var(x.clone())))),
                )),
            })),
        }));

        // 构造应用表达式 (λf: (Type0 -> Type0). λx: Type0. f (f x)) (λy: Type0. y) (Type0)
        let application = Rc::new(Expr::App(
            Rc::new(Expr::App(higher_order_function, identity_function)),
            type0.clone(),
        ));

        // 推断应用表达式的类型
        match infer_type(&mut ctx, &application) {
            Ok(ty) => assert_eq!(ty, type0, "Expected Type0, but got {:?}", ty),
            Err(e) => panic!("Error during type inference: {}", e),
        }
    }
}
