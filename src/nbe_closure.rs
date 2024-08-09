use crate::Term;


#[derive(Debug, Clone)]
enum Value {
    Lvl(usize),
    Lam(Vec<Value>, Term),
    App(Box<Value>, Box<Value>),
}

/// eval env tm =
///      match tm with
///      | Idx idx   -> List.nth env idx
///      | Lam tm'   -> VLam(env, tm')
///      | App(f, a) -> apply_val (eval env f) (eval env a)
fn eval(env: &[Value], tm: Term) -> Value {
    match tm {
        Term::Idx(idx) => env[env.len() - idx - 1].clone(),
        Term::Lam(tm) => Value::Lam(env.to_vec(), *tm),
        Term::App(f, a) => apply_val(eval(env, *f), eval(env, *a)),
    }
}

/// apply_val vf va =
///      match vf with
///      | VLam(env, body) -> eval (va :: env) body
///      | _               -> VApp(vf, va)
fn apply_val(vf: Value, va: Value) -> Value {
    match vf {
        Value::Lam(mut env, body) => eval(&{env.push(va);env}, body),
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
        Value::Lam(mut env, body) => Term::Lam(
            Box::new(
                quote(
                    level + 1,
                    eval(&{env.push(Value::Lvl(level));env}, body)
                )
            )
        ),
        Value::App(vf, va) => Term::App(
            Box::new(quote(level, *vf)),
            Box::new(quote(level, *va))
        ),
    }
}

pub fn normalize(t: Term) -> Term {
    quote(0, eval(&[], t))
}
