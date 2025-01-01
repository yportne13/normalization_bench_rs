#![feature(pattern)]

pub mod nbe_closure;
pub mod nbe_closure_dt;
pub mod locally;
pub mod list;
pub mod dt;
mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Idx(usize),
    Lam(Box<Term>),
    App(Box<Term>, Box<Term>),
}

fn lam(body: Term) -> Term {
    Term::Lam(Box::new(body))
}

fn apply(f: Term, args: Vec<Term>) -> Term {
    args.into_iter().fold(f, |acc, a| Term::App(Box::new(acc), Box::new(a)))
}

/// let rec church_aux = function
///    | 0 -> Idx 0
///    | n -> App(Idx 1, church_aux (n - 1))
fn church_aux(n: usize) -> Term {
    match n {
        0 => Term::Idx(0),
        _ => Term::App(Box::new(Term::Idx(1)), Box::new(church_aux(n - 1))),
    }
}

fn church(n: usize) -> Term {
    Term::Lam(Box::new(Term::Lam(Box::new(church_aux(n)))))
}

fn church_add() -> Term {
    lam(
        lam(
            lam(
                lam(
                    apply(
                        Term::Idx(3),
                        vec![Term::Idx(1), apply(Term::Idx(2), vec![Term::Idx(1), Term::Idx(0)])])
                )
            )
        )
    )
}

fn open(image: &Term, scope: &Term) -> Term {
    fn go(outer: usize, image: &Term, term: &Term) -> Term {
        match term {
            Term::Idx(fbv) => if *fbv == outer {image.clone()} else {Term::Idx(*fbv)},
            Term::App(l, r) => Term::App(
                Box::new(go(outer, image, l)),
                Box::new(go(outer, image, r))
            ),
            Term::Lam(b) => Term::Lam(Box::new(go(outer + 1, image, b))),
        }
    }

    go(0, image, scope)
}

fn whnf(term: &Term, mut vec: Vec<Term>) -> Term {
    match term {
        Term::App(l, r) => {
            vec.push(*r.clone());
            whnf(l, vec)
        },
        Term::Lam(body) if !vec.is_empty() => {
            let a = vec.pop().unwrap();
            whnf(&open(&a, body), vec)
        },
        a => vec.iter().fold(a.clone(), |x, y| Term::App(Box::new(x), Box::new(y.clone())))
    }
}

fn main() {
    //println!("Hello, world!");
    let i = 20;
    let a = church(i);
    let b = church(i);
    let add = apply(church_add(), vec![a, b]);
    let start = std::time::Instant::now();
    //let result = nbe_closure::normalize(add);
    let result = whnf(&add, vec![]);
    let end = start.elapsed();
    println!("{:?}s", end.as_secs_f64());
    println!("{:?}", result);
    let check = church(i + i);
    println!("{}", result == check);
}
