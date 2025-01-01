use crate::list::List;

#[derive(Debug, Clone, PartialEq)]
enum Raw {
    Var(String),
    Lam(String, Box<Raw>),
    App(Box<Raw>, Box<Raw>),
    U,
    Pi(String, Box<Raw>, Box<Raw>),
    Let(String, Box<Raw>, Box<Raw>, Box<Raw>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Idx(usize),
    Lam(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    U,
    Pi(String, Box<Term>, Box<Term>),
    Let(String, Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone)]
enum Value {
    Lvl(usize),
    App(Box<Value>, Box<Value>),
    Lam(String, Closure),
    Pi(String, Box<Value>, Closure),
    U,
}

impl Default for Value {
    fn default() -> Self {
        Value::U
    }
}

#[derive(Debug, Clone)]
pub struct Closure (List<Value>, Term);

impl Closure {
    fn apply(self, arg: Value) -> Value {
        let new_env = self.0;
        new_env.prepend(arg);
        eval(new_env, self.1)
    }
}

/// eval env tm =
///      match tm with
///      | Idx idx   -> List.nth env idx
///      | Lam tm'   -> VLam(env, tm')
///      | App(f, a) -> apply_val (eval env f) (eval env a)
fn eval(env: List<Value>, tm: Term) -> Value {
    /*println!(
        "eval: [{}] {:?}",
        env.iter().map(|x| format!("{:?}", x)).reduce(|a, b| a + ", " + &b).unwrap_or(String::new()),
        tm,
    );*/
    match tm {
        Term::Idx(idx) => env.iter().nth(idx).unwrap().clone(),
        Term::App(f, a) => apply_val(eval(env.clone(), *f), eval(env, *a)),
        Term::Lam(name, t) => Value::Lam(name, Closure(env, *t)),
        Term::Pi(name, term, term1) => Value::Pi(name, Box::new(eval(env.clone(), *term)), Closure(env, *term1)),
        Term::Let(_, _, term1, term2) => eval(
            env.prepend(eval(env.clone(), *term1)),
            *term2
        ),
        Term::U => Value::U,
    }
}

/// apply_val vf va =
///      match vf with
///      | VLam(env, body) -> eval (va :: env) body
///      | _               -> VApp(vf, va)
fn apply_val(vf: Value, va: Value) -> Value {
    match vf {
        Value::Lam(_, body) => eval(body.0.prepend(va), body.1),
        _ => Value::App(Box::new(vf), Box::new(va)),
    }
}

/// quote level value =
///      match value with
///      | VLvl lvl        -> Idx(level - lvl - 1)
///      | VLam(env, body) -> Lam(quote (level + 1) @@ eval (VLvl level :: env) body)
///      | VApp(vf, va)    -> App(quote level vf, quote level va)
fn quote(level: usize, value: Value) -> Term {
    match value {
        Value::Lvl(lvl) => Term::Idx(level - lvl - 1),
        Value::Lam(name, body) => Term::Lam(
            name,
            Box::new(
                quote(
                    level + 1,
                    eval(body.0.prepend(Value::Lvl(level)), body.1)
                )
            )
        ),
        Value::App(vf, va) => Term::App(
            Box::new(quote(level, *vf)),
            Box::new(quote(level, *va))
        ),
        Value::Pi(name, value, closure) => Term::Pi(
            name,
            Box::new(quote(level, *value)),
            Box::new(
                quote(
                    level + 1,
                    eval(closure.0.prepend(Value::Lvl(level)), closure.1)
                )
            )
        ),
        Value::U => Term::U,
    }
}

pub fn normalize(t: Term) -> Term {
    quote(0, eval(List::new(), t))
}


fn conv(l: usize, t: &Value, u: &Value) -> bool {
    match (t, u) {
        (Value::U, Value::U) => true,
        (Value::Pi(_, a, b), Value::Pi(_, a_, b_)) => {
            conv(l, a, a_) && conv(l + 1, &b.clone().apply(Value::Lvl(l)), &b_.clone().apply(Value::Lvl(l)))
        }
        (Value::Lam(_, t), Value::Lam(_, t_)) => {
            conv(l + 1, &t.clone().apply(Value::Lvl(l)), &t_.clone().apply(Value::Lvl(l)))
        }
        (Value::Lam(_, t), u) => conv(l + 1, &t.clone().apply(Value::Lvl(l)), &Value::App(Box::new(u.clone()), Box::new(Value::Lvl(l)))),
        (u, Value::Lam(_, t)) => conv(l + 1, &Value::App(Box::new(u.clone()), Box::new(Value::Lvl(l))), &t.clone().apply(Value::Lvl(l))),
        (Value::Lvl(x), Value::Lvl(x_)) => x == x_,
        (Value::App(t, u), Value::App(t_, u_)) => conv(l, t, t_) && conv(l, u, u_),
        _ => false,
    }
}

// Elaboration context
#[derive(Debug, Clone)]
struct Cxt {
    env: List<Value>,
    types: Vec<(String, Value)>,
    lvl: usize,
    pos: usize, // Simplified SourcePos for error reporting
}

impl Cxt {
    fn empty(pos: usize) -> Self {
        Cxt {
            env: Default::default(),
            types: vec![],
            lvl: 0,
            pos,
        }
    }

    // Extend Cxt with a bound variable
    fn bind(&self, x: String, a: Value) -> Self {
        let env = List::new();
        env.prepend(Value::Lvl(self.lvl));
        Cxt {
            env,
            types: vec![(x, a)],
            lvl: self.lvl + 1,
            pos: self.pos,
        }
    }

    // Extend Cxt with a definition
    fn define(&self, x: String, t: Value, a: Value) -> Self {
        let env = List::new();
        env.prepend(t);
        Cxt {
            env,
            types: vec![(x, a)],
            lvl: self.lvl + 1,
            pos: self.pos,
        }
    }
}

fn report(cxt: &Cxt, msg: &str) -> Result<(Term, Value), (String, usize)> {
    Err((msg.to_string(), cxt.pos))
}

/*fn show_val(cxt: &Cxt, val: &Val) -> String {
    let names: Vec<Name> = cxt.types.iter().map(|(name, _)| name.clone()).collect();
    let tm = quote(cxt.lvl, val);
    pretty_tm(0, &names, &tm)
}*/

// Bidirectional algorithm:
//   use check when the type is already known
//   use infer if the type is unknown
fn check(cxt: &Cxt, t: &Raw, a: &Value) -> Result<Term, (String, usize)> {
    match (t, a) {
        // Setting the source pos
        //(Raw::RSrcPos(pos, t), a) => check(&Cxt { pos: *pos, ..cxt.clone() }, t, a),

        // Checking Lam with Pi type (canonical checking case)
        // (\x. t) : ((x : A) -> B)
        (Raw::Lam(x, t), Value::Pi(x_, a_, b)) => {
            let new_cxt = cxt.bind(x.clone(), *a_.clone());
            let body = check(&new_cxt, t, &b.clone().apply(Value::Lvl(cxt.lvl)))?;
            Ok(Term::Lam(x.clone(), Box::new(body)))
        }

        // Fall-through checking
        (Raw::Let(x, a, t, u), a_) => {
            let a_tm = check(cxt, a, &Value::U)?;
            let va = eval(cxt.env.clone(), a_tm.clone());
            let t_tm = check(cxt, t, &va)?;
            let vt = eval(cxt.env.clone(), t_tm.clone());
            let new_cxt = cxt.define(x.clone(), vt, va);
            let u_tm = check(&new_cxt, u, a_)?;
            Ok(Term::Let(x.clone(), Box::new(a_tm), Box::new(t_tm), Box::new(u_tm)))
        }

        // If the term is not checkable, switch to infer (change of direction)
        _ => {
            let (t_tm, tty) = infer(cxt, t)?;
            if !conv(cxt.lvl, &tty, a) {
                report(
                    cxt,
                    &format!(
                        "type mismatch\n\nexpected type:\n\n  {:?}\n\ninferred type:\n\n  {:?}\n",
                        //show_val(cxt, a),
                        //show_val(cxt, &tty)
                        a, tty
                    ),
                )?;
            }
            Ok(t_tm)
        }
    }
}

fn infer(cxt: &Cxt, t: &Raw) -> Result<(Term, Value), (String, usize)> {
    match t {
        //Raw::RSrcPos(pos, t) => infer(&Cxt { pos: *pos, ..cxt.clone() }, t),

        Raw::Var(x) => {
            let mut i = 0;
            for (x_, a) in &cxt.types {
                if x == x_ {
                    return Ok((Term::Idx(i), a.clone()));
                }
                i += 1;
            }
            report(cxt, &format!("variable out of scope: {}", x))
        }

        Raw::U => Ok((Term::U, Value::U)), // U : U rule

        Raw::App(t, u) => {
            let (t_tm, tty) = infer(cxt, t)?;
            match tty {
                Value::Pi(_, a, b) => {
                    let u_tm = check(cxt, u, &a)?;
                    let u_val = eval(cxt.env.clone(), u_tm.clone());
                    Ok((Term::App(Box::new(t_tm), Box::new(u_tm)), b.apply(u_val)))
                }
                _ => report(cxt, "Expected a function type"),
            }
        }

        Raw::Lam { .. } => report(cxt, "Can't infer type for lambda expression"),

        Raw::Pi(x, a, b) => {
            let a_tm = check(cxt, a, &Value::U)?;
            let va = eval(cxt.env.clone(), a_tm.clone());
            let new_cxt = cxt.bind(x.clone(), va);
            let b_tm = check(&new_cxt, b, &Value::U)?;
            Ok((Term::Pi(x.clone(), Box::new(a_tm), Box::new(b_tm)), Value::U))
        }

        Raw::Let(x, a, t, u) => {
            let a_tm = check(cxt, a, &Value::U)?;
            let va = eval(cxt.env.clone(), a_tm.clone());
            let t_tm = check(cxt, t, &va)?;
            let vt = eval(cxt.env.clone(), t_tm.clone());
            let new_cxt = cxt.define(x.clone(), vt, va);
            let (u_tm, uty) = infer(&new_cxt, u)?;
            Ok((Term::Let(x.clone(), Box::new(a_tm), Box::new(t_tm), Box::new(u_tm)), uty))
        }
    }
}
