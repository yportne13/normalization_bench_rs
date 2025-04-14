use std::collections::HashMap;
use std::rc::Rc;
use std::vec;

// 使用整数作为变量以便于生成
type Var = i32;

type TypeName = String;
type Constructor = String;

#[derive(Debug, Clone)]
pub enum Type {
    Cons(TypeName, Vec<(Constructor, Vec<Type>)>),
    Rec(TypeName),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    PAny,
    PCon(Constructor, Vec<Pattern>),
}

// 为了简单起见，仅用于区分不同的匹配体
type MatchBody = i32;

#[derive(Debug, Clone)]
pub enum DecisionTree {
    Fail,
    Leaf(MatchBody),
    Branch(TypeName, Var, Vec<(Constructor, Vec<Var>, Rc<DecisionTree>)>),
}

#[derive(Debug, Clone)]
pub enum Warning {
    Unreachable(MatchBody),
    Unmatched(Pattern),
}

pub struct Compiler {
    warnings: Vec<Warning>,
    reachable: HashMap<MatchBody, ()>,
    seed: i32,
    rec_table: HashMap<TypeName, Vec<(Constructor, Vec<Type>)>>,
}

impl Compiler {
    pub fn new() -> Self {
        let mut rec_table = HashMap::new();
        let nat_type = vec![("zero".to_string(), vec![]), ("succ".to_string(), vec![Type::Rec("Nat".to_string())])];
        rec_table.insert("Nat".to_string(), nat_type);
        Compiler {
            warnings: Vec::new(),
            reachable: HashMap::new(),
            seed: 0,
            rec_table,
        }
    }

    fn fresh(&mut self) -> i32 {
        self.seed += 1;
        self.seed
    }

    fn fill_context(ctx: &MatchContext, pat: &Pattern) -> Pattern {
        match ctx {
            MatchContext::Outermost => pat.clone(),
            MatchContext::InCons {
                parent,
                constr,
                before,
                after,
            } => {
                let mut new_before = before.clone();
                new_before.reverse();
                new_before.push(pat.clone());
                new_before.extend(after.clone());
                Self::fill_context(parent, &Pattern::PCon(constr.clone(), new_before))
            }
        }
    }

    fn next_hole(ctx: &MatchContext, pat: &Pattern) -> MatchContext {
        println!(" >>>  {:?}", ctx);
        println!(" >>>> {:?}", pat);
        match ctx {
            MatchContext::Outermost => MatchContext::Outermost,
            MatchContext::InCons {
                parent,
                constr,
                before,
                after,
            } => match after[..] {
                [] => Self::next_hole(parent, &Pattern::PCon(constr.clone(), before.clone())),
                _ => MatchContext::InCons {
                    parent: parent.clone(),
                    constr: constr.clone(),
                    before: vec![pat.clone()],
                    after: after[1..].to_vec(),
                },
            },
        }
    }

    fn compile_aux(
        &mut self,
        heads: &[(Var, Type)],
        arms: &[MatchArm],
        context: &MatchContext,
    ) -> Rc<DecisionTree> {
        match heads {
            [] => match arms {
                [arm, ..] if arm.pats.is_empty() => {
                    self.reachable.insert(arm.body, ());
                    Rc::new(DecisionTree::Leaf(arm.body))
                }
                _ => panic!("impossible"),
            },
            [(var, typ), heads_rest @ ..] => {
                println!("------------------");
                println!("{:?}", arms);
                println!("   {:?}", context);
                let is_necessary = arms.iter().any(|arm| matches!(arm.pats[..], [Pattern::PCon(..), ..]));

                if !is_necessary {
                    let new_context = Self::next_hole(context, &Pattern::PAny);
                    let new_arms = arms
                        .iter()
                        .map(|arm| MatchArm {
                            pats: arm.pats[1..].to_vec(),
                            body: arm.body,
                        })
                        .collect::<Vec<_>>();
                    self.compile_aux(heads_rest, &new_arms, &new_context)
                } else {
                    let (typename, constrs) = match typ {
                        Type::Cons(typename, constrs) => {
                            (typename, constrs.clone())
                        },
                        Type::Rec(typename) => {
                            (typename, self.rec_table.get(typename).unwrap().clone())
                        }
                    };

                    let decision_tree_branches = constrs
                        .iter()
                        .map(|(constr, item_typs)| {
                            let new_heads = item_typs
                                .iter()
                                .map(|typ| (self.fresh(), typ.clone()))
                                .collect::<Vec<_>>();
                            let remaining_arms = arms
                                .iter()
                                .filter_map(|arm| match &arm.pats[..] {
                                    [Pattern::PAny, ..] => Some(MatchArm {
                                        pats: vec![Pattern::PAny; item_typs.len()]
                                            .into_iter()
                                            .chain(arm.pats[1..].iter().cloned())
                                            .collect(),
                                        body: arm.body,
                                    }),
                                    [Pattern::PCon(constr_, item_pats), ..] if constr_ == constr => {
                                        Some(MatchArm {
                                            pats: item_pats
                                                .iter()
                                                .chain(&arm.pats[1..])
                                                .cloned()
                                                .collect(),
                                            body: arm.body,
                                        })
                                    }
                                    _ => None,
                                })
                                .collect::<Vec<_>>();

                            let subtree = if remaining_arms.is_empty() {
                                let unmatched = Self::fill_context(
                                    context,
                                    &Pattern::PCon(constr.clone(), vec![Pattern::PAny; item_typs.len()]),
                                );
                                self.warnings.push(Warning::Unmatched(unmatched));
                                Rc::new(DecisionTree::Fail)
                            } else {
                                let context_ = if new_heads.is_empty() {
                                    if heads_rest.is_empty() {
                                        context.clone()
                                    } else {
                                        Self::next_hole(context, &Pattern::PCon(constr.clone(), vec![]))
                                    }
                                } else {
                                    MatchContext::InCons {
                                        parent: context.clone().into(),
                                        constr: constr.clone(),
                                        before: vec![],
                                        after: vec![Pattern::PAny; new_heads.len() - 1],
                                    }
                                };
                                self.compile_aux(
                                    &new_heads.iter().chain(heads_rest).cloned().collect::<Vec<_>>(),
                                    &remaining_arms,
                                    &context_,
                                )
                            };

                            (constr.clone(), new_heads.iter().map(|(var, _)| *var).collect(), subtree)
                        })
                        .collect::<Vec<_>>();

                    Rc::new(DecisionTree::Branch(
                        typename.clone(),
                        *var,
                        decision_tree_branches,
                    ))
                }
            }
        }
    }

    pub fn compile(&mut self, typ: &Type, arms: &[(Pattern, MatchBody)]) -> (Rc<DecisionTree>, Vec<Warning>) {
        let tree = self.compile_aux(
            &[(0, typ.clone())],
            &arms.iter()
                .map(|(pat, body)| MatchArm {
                    pats: vec![pat.clone()],
                    body: *body,
                })
                .collect::<Vec<_>>(),
            &MatchContext::Outermost,
        );

        let unreachable = arms
            .iter()
            .filter_map(|(_, body)| {
                if !self.reachable.contains_key(body) {
                    Some(Warning::Unreachable(*body))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        (tree, unreachable.into_iter().chain(self.warnings.clone()).collect())
    }
}

#[derive(Debug, Clone)]
enum MatchContext {
    Outermost,
    InCons {
        parent: Rc<MatchContext>,
        constr: Constructor,
        before: Vec<Pattern>,
        after: Vec<Pattern>,
    },
}

#[derive(Debug, Clone)]
struct MatchArm {
    pats: Vec<Pattern>,
    body: MatchBody,
}

#[test]
fn test() {
    let bool_type = Type::Cons("bool".to_string(), vec![("True".to_string(), vec![]), ("False".to_string(), vec![])]);
    let pair_type = |x: Type, y: Type| Type::Cons("pair".to_string(), vec![("Pair".to_string(), vec![x, y])]);
    let option_type = |x: Type| Type::Cons("option".to_string(), vec![("None".to_string(), vec![]), ("Some".to_string(), vec![x])]);
    let either_type = |x: Type, y: Type| Type::Cons("either".to_string(), vec![("Left".to_string(), vec![x]), ("Right".to_string(), vec![y])]);
    let nat_type = Type::Cons("Nat".to_string(), vec![("zero".to_string(), vec![]), ("succ".to_string(), vec![Type::Rec("Nat".to_string())])]);

    let p_any = Pattern::PAny;
    let p_true = Pattern::PCon("True".to_string(), vec![]);
    let p_false = Pattern::PCon("False".to_string(), vec![]);
    let p_pair = |p1: Pattern, p2: Pattern| Pattern::PCon("Pair".to_string(), vec![p1, p2]);
    let p_none = Pattern::PCon("None".to_string(), vec![]);
    let p_some = |p: Pattern| Pattern::PCon("Some".to_string(), vec![p]);
    let p_left = |p: Pattern| Pattern::PCon("Left".to_string(), vec![p]);
    let p_right = |p: Pattern| Pattern::PCon("Right".to_string(), vec![p]);

    let p_zero = Pattern::PCon("zero".to_string(), vec![]);
    let p_succ = |p: Pattern| Pattern::PCon("succ".to_string(), vec![p]);

    let ex1 = (
        bool_type.clone(),
        vec![(p_true.clone(), 1), (p_false.clone(), 2)],
    );

    let ex2 = (
        bool_type.clone(),
        vec![(p_true.clone(), 1), (p_true.clone(), 2)],
    );

    let ex3 = (
        pair_type(bool_type.clone(), bool_type.clone()),
        vec![
            (p_pair(p_true.clone(), p_true.clone()), 1),
            (p_pair(p_true.clone(), p_false.clone()), 2),
            (p_pair(p_false.clone(), p_true.clone()), 3),
            (p_pair(p_false.clone(), p_false.clone()), 4),
        ],
    );

    let ex4 = (
        pair_type(bool_type.clone(), bool_type.clone()),
        vec![
            (p_pair(p_any.clone(), p_true.clone()), 1),
            (p_pair(p_true.clone(), p_any.clone()), 2),
            (p_pair(p_false.clone(), p_false.clone()), 3),
        ],
    );

    let ex5 = (
        pair_type(bool_type.clone(), bool_type.clone()),
        vec![
            (p_pair(p_any.clone(), p_true.clone()), 1),
            (p_pair(p_true.clone(), p_any.clone()), 2),
        ],
    );

    let ex6 = (
        pair_type(bool_type.clone(), pair_type(bool_type.clone(), bool_type.clone())),
        vec![
            (p_pair(p_true.clone(), p_pair(p_true.clone(), p_false.clone())), 1),
            (p_pair(p_any.clone(), p_pair(p_any.clone(), p_false.clone())), 2),
            (p_pair(p_true.clone(), p_pair(p_any.clone(), p_true.clone())), 3),
            (p_pair(p_false.clone(), p_pair(p_true.clone(), p_any.clone())), 4),
        ],
    );

    let ex7 = (
        nat_type,
        vec![
            (p_zero.clone(), 1),
            (p_succ(p_zero.clone()), 2),
            //(p_succ(p_any.clone()), 2),
            (p_succ(p_succ(p_any.clone())), 3),
            //(p_succ(p_succ(p_any.clone())), 3),
        ]
    );

    let ex8 = (
        option_type(bool_type.clone()),
        vec![
            (p_none.clone(), 1),
            //(p_some(p_any.clone()), 2),
            (p_some(p_true.clone()), 2),
            (p_some(p_false.clone()), 3),
        ],
    );

    let mut compiler = Compiler::new();
    //let (tree, warnings) = compiler.compile(&ex6.0, &ex6.1);
    let (tree, warnings) = compiler.compile(&ex7.0, &ex7.1);
    //let (tree, warnings) = compiler.compile(&ex8.0, &ex8.1);
    println!("{:?}", tree);
    println!("{:?}", warnings);
}