use smallstr::SmallString;
use smallvec::SmallVec;
use smallvec::smallvec;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    IO(Box<Type>),
    Boolean,
    Integer,
    Real,
    Character,
    Tuple(Vec<Type>),
    List(Box<Type>),
    Function(Vec<Type>, Box<Type>)
}

impl Type {
    pub fn label(&self) -> SmallString<[u8; 64]> {
        match self {
            Self::IO(ty) => {
                let mut s = SmallString::from("IO(");
                s.push_str(&ty.label());
                s.push(')');
                s
            },
            Self::Boolean => "Boolean".into(),
            Self::Integer => "Integer".into(),
            Self::Real => "Real".into(),
            Self::Character => "Character".into(),
            Self::Tuple(tys) => {
                let mut s = SmallString::from('(');
                let labels: SmallVec<[_; 4]> = tys.iter().map(Self::label).collect();
                s.push_str(&labels.join(", "));
                s
            },
            Self::List(ty) => {
                let mut s = SmallString::from('[');
                s.push_str(&ty.label());
                s.push(']');
                s
            },
            Self::Function(inputs, ret) => {
                let mut s = SmallString::from("Function((");
                let labels: Vec<_> = inputs.iter().map(Self::label).collect();
                s.push_str(&labels.join(", "));
                s.push_str(") -> ");
                s.push_str(&ret.label());
                s
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Integer(i64),
    Real(f64),
    Character(char),
    Tuple(Vec<Value>),
    List(Type, Vec<Value>)
}

impl Value {
    fn ty(&self) -> Type {
        match self {
            Self::Boolean(_)   => Type::Boolean,
            Self::Integer(_)   => Type::Integer,
            Self::Real(_)      => Type::Real,
            Self::Character(_) => Type::Character,
            Self::Tuple(vals)  => Type::Tuple(vals.iter().map(Self::ty).collect()),
            Self::List(ety, _) => Type::List(Box::new(ety.clone()))
        }
    }

    const fn get_boolean(&self) -> Option<bool> {
        if let Self::Boolean(b) = self { Some(*b) } else { None }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        todo!()
    }
}

#[derive(Debug)]
pub enum ComparisonType {
    EQ,
    NE,
    GT,
    GE,
    LT,
    LE
}

impl ComparisonType {
    pub fn label(&self) -> SmallString<[u8; 32]> {
        match self {
            Self::EQ => "Equal".into(),
            Self::NE => "Not Equal".into(),
            Self::GT => "Greater Than".into(),
            Self::GE => "Greater Than or Equal".into(),
            Self::LT => "Less Than".into(),
            Self::LE => "Less Than or Equal".into()
        }
    }
}

#[derive(Clone, Debug)]
pub struct Input {
    ty: Type,
    label: SmallString<[u8; 16]>
}

#[derive(Debug)]
pub struct Output {
    ty: Type,
    label: SmallString<[u8; 16]>
}

#[derive(Debug)]
pub enum Node {
    Parameter(SmallString<[u8; 16]>, Type),
    Return(SmallString<[u8; 16]>, Type),

    Constant(Value),
    Bind(Type, Type),

    TwitchSendMessage
}

impl Node {
    pub fn label(&self) -> SmallString<[u8; 64]> {
        match self {
            Self::Parameter(label, ty) => {
                let mut s = SmallString::from("Parameter(");
                s.push_str(label);
                s.push_str(": ");
                s.push_str(&ty.label());
                s.push(')');
                s
            },
            Self::Return(label, ty) => {
                let mut s = SmallString::from("Return(");
                s.push_str(label);
                s.push_str(": ");
                s.push_str(&ty.label());
                s.push(')');
                s
            },

            Self::Constant(val) => {
                let mut s = SmallString::from(val.ty().label().as_ref());
                s.push_str(" Constant");
                s
            },
            Self::Bind(..) => "Bind".into(),

            Self::TwitchSendMessage => "TwitchSendMessage".into()
        }
    }

    pub fn inputs(&self) -> <SmallVec<[Input; 4]> as IntoIterator>::IntoIter {
        match self {
            Self::Parameter(..) => smallvec![],
            Self::Return(_, ty) => smallvec![
                Input {
                    ty: ty.clone(),
                    label: "Value".into()
                }
            ],

            Self::Constant(_) => smallvec![],

            Self::TwitchSendMessage => smallvec![
                Input {
                    ty: Type::List(Box::new(Type::Character)),
                    label: "Message".into()
                }
            ]
        }.into_iter()
    }

    pub fn output(&self) -> Output {
        match self {
            Self::Parameter(_, ty) => Output {
                ty: ty.clone(),
                label: "Value".into()
            },
            Self::Return(..) => Output {
                ty: Type::Tuple(vec![]),
                label: "".into()
            },

            Self::Constant(val) => Output {
                ty: val.ty(),
                label: "Value".into()
            },

            Self::TwitchSendMessage => Output {
                ty: Type::IO(Box::new(Type::Tuple(vec![]))),
                label: "Action".into()
            }
        }
    }
}
