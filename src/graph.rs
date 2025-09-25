use anyhow::Context as _;
use anyhow::Result;
use derive_more::Deref;
use derive_more::DerefMut;
use derive_more::IntoIterator;
use petgraph::acyclic::Acyclic;
use petgraph::data::Build as _;
use petgraph::data::DataMapMut as _;
use petgraph::stable_graph::NodeIndex;
use petgraph::stable_graph::StableDiGraph;
use petgraph::visit::EdgeRef as _;
use petgraph::visit::IntoEdgeReferences as _;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Kind {
    Type,
    Arrow(Box<Self>, Box<Self>)
}

impl Kind {
    pub fn arrow(a: Self, b: Self) -> Self {
        Self::Arrow(Box::new(a), Box::new(b))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeCon {
    // arity = 0
    Boolean,
    Character,
    Integer,
    Real,

    // arity = 1
    IO,
    List,

    // arity = 2
    Arrow,

    // arity = N
    Tuple(usize)
}

impl TypeCon {
    pub fn kind(&self) -> Kind {
        match self {
            Self::Boolean   => Kind::Type,
            Self::Character => Kind::Type,
            Self::Integer   => Kind::Type,
            Self::Real      => Kind::Type,

            Self::IO        => Kind::arrow(Kind::Type, Kind::Type),
            Self::List      => Kind::arrow(Kind::Type, Kind::Type),

            Self::Arrow     => Kind::arrow(Kind::Type, Kind::arrow(Kind::Type, Kind::Type)),

            Self::Tuple(n)  => (0..*n).fold(Kind::Type, |acc, _| Kind::arrow(Kind::Type, acc))
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TypeVar(pub u32);

#[derive(Clone, Debug)]
pub enum Type {
    Var(TypeVar),
    App(TypeCon, Vec<Type>)
}

#[derive(Clone, Default, Deref, DerefMut, IntoIterator)]
pub struct SubstitutionMap(HashMap<TypeVar, Type>);

impl SubstitutionMap {
    pub fn with_capacity(capacity: usize) -> Self {
        Self(HashMap::with_capacity(capacity))
    }

    pub fn compose_with(&mut self, s: &Self) {
        for ty in self.values_mut() {
            *ty = ty.substitute(s);
        }
        self.extend(s.clone());
    }
}

impl core::iter::FromIterator<(TypeVar, Type)> for SubstitutionMap {
    fn from_iter<I: IntoIterator<Item = (TypeVar, Type)>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl Type {
    pub const fn boolean() -> Self {
        Self::App(TypeCon::Boolean, vec![])
    }

    pub const fn character() -> Self {
        Self::App(TypeCon::Character, vec![])
    }

    pub const fn integer() -> Self {
        Self::App(TypeCon::Integer, vec![])
    }

    pub const fn real() -> Self {
        Self::App(TypeCon::Real, vec![])
    }

    pub fn list(elem_ty: Self) -> Self {
        Self::App(TypeCon::List, vec![elem_ty])
    }

    pub fn arrow(a: Self, b: Self) -> Self {
        Self::App(TypeCon::Arrow, vec![a, b])
    }

    pub const fn tuple(elem_tys: Vec<Self>) -> Self {
        Self::App(TypeCon::Tuple(elem_tys.len()), elem_tys)
    }

    pub const fn unit() -> Self {
        Self::tuple(vec![])
    }

    pub fn singleton(elem_ty: Self) -> Self {
        Self::tuple(vec![elem_ty])
    }

    pub fn break_arrow(&self) -> Option<(&Self, &Self)> {
        if let Self::App(TypeCon::Arrow, args) = self {
            Some((args.first()?, args.get(1)?))
        } else {
            None
        }
    }

    pub fn break_tuple(&self) -> Option<&[Self]> {
        if let Self::App(TypeCon::Tuple(_), args) = self {
            Some(args)
        } else {
            None
        }
    }

    pub fn substitute(&self, subs: &SubstitutionMap) -> Self {
        match self {
            Self::Var(v) => subs.get(v).unwrap_or(self).clone(),
            Self::App(con, args) => Self::App(
                con.clone(),
                args.iter().map(|a| a.substitute(subs)).collect()
            )
        }
    }

    pub fn unify(t0: &Self, t1: &Self) -> Result<SubstitutionMap> {
        fn bind(a: TypeVar, t: &Type) -> Result<SubstitutionMap> {
            Ok(core::iter::once((a, t.clone())).collect())
        }

        log::info!("{t0:?}, {t1:?}");

        match (t0, t1) {
            (Self::Var(a), Self::Var(b)) if a == b => Ok(SubstitutionMap::default()),
            (Self::Var(a), t) => bind(*a, t),
            (t, Self::Var(a)) => bind(*a, t),
            (Self::App(con0, a0), Self::App(con1, a1)) if con0 == con1 && a0.len() == a1.len() => {
                let mut subs = SubstitutionMap::default();
                for (x, y) in a0.iter().zip(a1) {
                    let x = x.substitute(&subs);
                    let y = y.substitute(&subs);
                    subs.compose_with(&Self::unify(&x, &y)?);
                }
                Ok(subs)
            },
            _ => anyhow::bail!("type clash")
        }
    }
}

#[derive(Debug)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type
}

impl Scheme {
    pub fn instantiate(&self, ctx: &mut Context) -> Type {
        let mut subs = SubstitutionMap::with_capacity(self.vars.len());
        for v in &self.vars {
            subs.insert(*v, Type::Var(ctx.fresh_var()));
        }
        self.ty.substitute(&subs)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Boolean(bool),
    Character(char),
    Integer(i64),
    Real(f64),
    Tuple(Vec<Value>),
    List {
        elem_ty: Type,
        elems: Vec<Value>
    }
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Self::Boolean(_)           => Type::boolean(),
            Self::Character(_)         => Type::character(),
            Self::Integer(_)           => Type::integer(),
            Self::Real(_)              => Type::real(),
            Self::Tuple(vals)          => Type::tuple(vals.iter().map(Self::ty).collect()),
            Self::List { elem_ty, .. } => Type::list(elem_ty.clone())
        }
    }
}

#[derive(Clone, Debug)]
pub enum Op {
    Constant(Value),
    Identity
}

impl Op {
    pub fn scheme(&self) -> Scheme {
        match self {
            Self::Constant(v) => {
                Scheme {
                    vars: vec![],
                    ty: Type::arrow(
                        Type::unit(),
                        Type::singleton(v.ty())
                    )
                }
            },
            Self::Identity => {
                let a = TypeVar(0);
                Scheme {
                    vars: vec![a],
                    ty: Type::arrow(
                        Type::singleton(Type::Var(a)),
                        Type::singleton(Type::Var(a))
                    )
                }
            }
        }
    }

    pub fn instantiate(&self, ctx: &mut Context) -> Instance {
        Instance {
            op: self.clone(),
            ty: self.scheme().instantiate(ctx)
        }
    }
}

#[derive(Debug)]
pub struct Instance {
    pub op: Op,
    pub ty: Type
}

#[derive(Debug, Default)]
pub struct Context {
    next: u32
}

impl Context {
    pub const fn fresh_var(&mut self) -> TypeVar {
        let v = self.next;
        self.next += 1;
        TypeVar(v)
    }
}

#[derive(Debug)]
pub struct Port(pub usize);

#[derive(Debug)]
pub struct Edge {
    from: Port,
    to: Port
}

#[derive(Debug, Default)]
pub struct Graph(pub Acyclic<StableDiGraph<Instance, Edge>>);

impl Graph {
    pub fn add(&mut self, inst: Instance) -> NodeIndex {
        self.0.add_node(inst)
    }

    pub fn connect(&mut self, u: NodeIndex, from: usize, v: NodeIndex, to: usize) {
        self.0.add_edge(u, v, Edge {
            from: Port(from),
            to: Port(to)
        });
    }

    pub fn type_check(&mut self) -> Result<()> {
        let mut subs = SubstitutionMap::default();

        for e in self.0.edge_references() {
            let u = e.source();
            let v = e.target();
            let edge = e.weight();

            let (_, u_out) = self.0[u].ty.break_arrow().context("failed to break `u` instance as Arrow")?;
            let u_tuple = u_out.break_tuple().context("failed to break `u` output as Tuple")?;

            let (v_in, _) = self.0[v].ty.break_arrow().context("failed to break `v` instance as Arrow")?;
            let v_tuple = v_in.break_tuple().context("failed to break `v` output as Tuple")?;

            let t0 = &u_tuple.get(edge.from.0).context("`from` port out of bounds")?.substitute(&subs);
            let t1 = &v_tuple.get(edge.to.0).context("`to` port out of bounds")?.substitute(&subs);

            subs.compose_with(&Type::unify(t0, t1)?);
        }

        let indices: Vec<_> = self.0.nodes_iter().collect();
        for i in indices {
            if let Some(n) = self.0.node_weight_mut(i) {
                n.ty = n.ty.substitute(&subs);
            }
        }

        Ok(())
    }
}
