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

#[derive(Clone, Debug)]
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

    pub fn substitute(&self, subs: &HashMap<TypeVar, Type>) -> Self {
        match self {
            Self::Var(v) => subs.get(v).unwrap_or(self).clone(),
            Self::App(con, args) => Self::App(
                con.clone(),
                args.iter().map(|a| a.substitute(subs)).collect()
            )
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
        let mut subs = HashMap::with_capacity(self.vars.len());
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
                    ty: Type::singleton(v.ty())
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
