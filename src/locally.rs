//base on https://github.com/Boarders/LocallyNameless
//and translate to rust with the help of llm

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Term<A> {
    Var(A),
    Lam(A, Box<Term<A>>),
    App(Box<Term<A>>, Box<Term<A>>),
}

impl<A: std::fmt::Display> std::fmt::Display for Term<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Var(a) => write!(f, "{}", a),
            Term::Lam(a, t) => write!(f, "λ{}.{}", a, t),
            Term::App(l, r) => write!(f, "({} {})", l, r),
        }
    }
}

impl<A> Term<A> {
    fn app(self, other: Term<A>) -> Term<A> {
        Term::App(Box::new(self), Box::new(other))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Var<A> {
    B(i32),
    F(A),
}

impl<A: std::hash::Hash + Eq> std::hash::Hash for Var<A> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Var::B(bv) => bv.hash(state),
            Var::F(a) => a.hash(state),
        }
    }
}

fn to_locally_nameless<A: Ord + std::hash::Hash + Eq + Clone>(term: &Term<A>, env: &mut HashMap<A, i32>) -> Term<Var<A>> {
    match term {
        Term::Var(a) => {
            if let Some(&bv) = env.get(a) {
                Term::Var(Var::B(bv))
            } else {
                Term::Var(Var::F(a.clone()))
            }
        },
        Term::App(l, r) => Term::app(to_locally_nameless(l, env), to_locally_nameless(r, env)),
        Term::Lam(n, e) => {
            let new_env = env.iter().map(|(k, v)| (k.clone(), *v + 1)).collect::<HashMap<_, _>>();
            let mut new_env = new_env;
            new_env.insert(n.clone(), 0);
            Term::Lam(Var::F(n.clone()), Box::new(to_locally_nameless(e, &mut new_env)))
        },
    }
}

fn from_locally_nameless<A: std::hash::Hash + Eq + Clone>(term: &Term<Var<A>>, env: &mut HashMap<i32, A>) -> Result<Term<A>, String> {
    match term {
        Term::Var(v) => match v {
            Var::F(a) => Ok(Term::Var(a.clone())),
            Var::B(bv) => {
                if let Some(name) = env.get(&bv) {
                    Ok(Term::Var(name.clone()))
                } else {
                    Err(format!("Found bound variable without binder: {}", bv))
                }
            },
        },
        Term::App(l, r) => {
            Ok(Term::App(
                Box::new(from_locally_nameless(l, env)?),
                Box::new(from_locally_nameless(r, env)?),
            ))
        },
        Term::Lam(n, e) => match n {
            Var::B(bv) => Err(format!("Found unnamed variable at binding site: {}", bv)),
            Var::F(v) => {
                let mut new_env: HashMap<_, _> = env.iter().map(|(k, v)| (k + 1, v.clone())).collect();
                new_env.insert(0, v.clone());
                Ok(Term::Lam(v.clone(), Box::new(from_locally_nameless(e, &mut new_env)?)))
            },
        },
    }
}

fn open<A: std::hash::Hash + Eq + Clone>(image: &Term<Var<A>>, scope: &Term<Var<A>>) -> Term<Var<A>> {
    fn go<A: std::hash::Hash + Eq + Clone>(outer: i32, image: &Term<Var<A>>, term: &Term<Var<A>>) -> Term<Var<A>> {
        match term {
            Term::Var(fbv) => match fbv {
                Var::B(bv) if *bv == outer => image.clone(),
                Var::B(bv) => Term::Var(Var::B(*bv)),
                Var::F(fv) => Term::Var(Var::F(fv.clone())),
            },
            Term::App(l, r) => Term::App(Box::new(go(outer, image, l)), Box::new(go(outer, image, r))),
            Term::Lam(n, b) => Term::Lam(n.clone(), Box::new(go(outer + 1, image, b))),
        }
    }

    go(0, image, scope)
}

fn whnf<A: std::hash::Hash + Eq + Clone>(term: &Term<Var<A>>, mut vec: Vec<Term<Var<A>>>) -> Term<Var<A>> {
    match term {
        Term::App(l, r) => {
            vec.push(*r.clone());
            whnf(l, vec)
        },
        Term::Lam(_, body) if !vec.is_empty() => {
            let a = vec.pop().unwrap();
            whnf(&open(&a, body), vec)
        },
        a => vec.iter().fold(a.clone(), |x, y| Term::App(Box::new(x), Box::new(y.clone())))
    }
}

impl From<String> for Term<String> {
    fn from(val: String) -> Self {
        Term::Var(val)
    }
}

impl From<&str> for Term<String> {
    fn from(value: &str) -> Self {
        Term::Var(value.to_owned())
    }
}

fn full(x: Term<String>) -> Term<String> {
    let mut env = HashMap::new();
    let l1 = to_locally_nameless(&x, &mut env);
    let l2 = whnf(&l1, vec![]);
    let mut env = HashMap::new();
    let l3 = from_locally_nameless(&l2, &mut env);
    l3.unwrap()
}

#[test]
fn test0() {
    fn lam<A: Into<Term<String>>>(l: &str, r: A) -> Term<String> {
        Term::Lam(l.to_owned(), Box::new(r.into()))
    }
    fn app<A: Into<Term<String>>, B: Into<Term<String>>>(l: A, r: B) -> Term<String> {
        Term::App(Box::new(l.into()), Box::new(r.into()))
    }
    println!("Hello, world!");
    let item = app(lam("A", "A"), "I");
    let uncurry = app(app(lam("A", lam("B", "B")), item), "Y");
    let uncurry = app(app(lam("A", lam("A", uncurry)), "M"), "N");
    println!("{}", uncurry);
    let mut env = HashMap::new();
    let uc = to_locally_nameless(&uncurry, &mut env);
    println!("{:?}", uc);
    let ucb = whnf(&uc, vec![]);
    println!("whnf: {:?}", ucb);
    let mut env = HashMap::new();
    let cu = from_locally_nameless(&uc, &mut env);
    println!("{:?}", cu);
    let mut env = HashMap::new();
    let cu = from_locally_nameless(&ucb, &mut env);
    println!("{:?}", cu);

}

#[test]
fn test() {
    fn lam<A: Into<Term<String>>>(l: &str, r: A) -> Term<String> {
        Term::Lam(l.to_owned(), Box::new(r.into()))
    }
    fn app<A: Into<Term<String>>, B: Into<Term<String>>>(l: A, r: B) -> Term<String> {
        Term::App(Box::new(l.into()), Box::new(r.into()))
    }
    //f: a -> b -> c
    let f = lam("A", lam("B", "C"));
    let c1 = app(app(f.clone(), "B"), "A");
    println!("{:?}", full(c1));
    let c2 = app(app(f, "A"), "B");
    println!("{:?}", full(c2));
}