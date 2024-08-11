pub mod nbe_closure;
pub mod list;

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

fn main() {
    //println!("Hello, world!");
    let i = 1000;
    let a = church(i);
    let b = church(i);
    let add = apply(church_add(), vec![a, b]);
    let start = std::time::Instant::now();
    let result = nbe_closure::normalize(add);
    let end = start.elapsed();
    println!("{:?}s", end.as_secs_f64());
    let check = church(i + i);
    println!("{}", result == check);
}
