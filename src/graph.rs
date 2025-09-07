use smallstr::SmallString;
use smallvec::SmallVec;
use smallvec::smallvec;

#[derive(Clone, Debug)]
pub enum Type {
    Execution,
    Boolean,
    Integer,
    Real,
    Character,
    List(Box<Type>),
    Generic1
}

impl Type {
    pub fn label(&self) -> SmallString<[u8; 32]> {
        match self {
            Self::Execution => "Execution".into(),
            Self::Boolean => "Boolean".into(),
            Self::Integer => "Integer".into(),
            Self::Real => "Real".into(),
            Self::Character => "Character".into(),
            Self::List(ty) => {
                let mut s = SmallString::from("List of ");
                s.push_str(&ty.label());
                s
            },
            Self::Generic1 => "<T>".into()
        }
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

#[derive(Debug)]
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
    Constant(Type),
    Comparison(ComparisonType),
    Select,
    Branch,

    CommandEvent,

    SendMessageAction
}

impl Node {
    pub fn label(&self) -> SmallString<[u8; 64]> {
        match self {
            Self::Constant(ty) => {
                let mut s = SmallString::from(ty.label().as_ref());
                s.push_str(" Constant");
                s
            },
            Self::Comparison(cty) => {
                let mut s = SmallString::from(cty.label().as_ref());
                s.push_str(" Comparison");
                s
            },
            Self::Select => "Select".into(),
            Self::Branch => "Branch".into(),

            Self::CommandEvent => "Command".into(),

            Self::SendMessageAction => "Send Message".into()
        }
    }

    pub fn inputs(&self) -> <SmallVec<[Input; 4]> as IntoIterator>::IntoIter {
        match self {
            Self::Constant(_) => smallvec![],
            Self::Comparison(_) => smallvec![
                Input {
                    ty: Type::Generic1,
                    label: "First".into()
                },
                Input {
                    ty: Type::Generic1,
                    label: "Second".into()
                }
            ],
            Self::Select => smallvec![
                Input {
                    ty: Type::Boolean,
                    label: "Condition".into()
                },
                Input {
                    ty: Type::Generic1,
                    label: "True".into()
                },
                Input {
                    ty: Type::Generic1,
                    label: "False".into()
                },
            ],
            Self::Branch => smallvec![
                Input {
                    ty: Type::Execution,
                    label: "Execution".into()
                },
                Input {
                    ty: Type::Boolean,
                    label: "Condition".into()
                }
            ],

            Self::CommandEvent => smallvec![],

            Self::SendMessageAction => smallvec![
                Input {
                    ty: Type::Execution,
                    label: "Execution".into()
                },
                Input {
                    ty: Type::List(Box::new(Type::Character)),
                    label: "Message".into()
                }
            ]
        }.into_iter()
    }

    pub fn outputs(&self) -> <SmallVec<[Output; 4]> as IntoIterator>::IntoIter {
        match self {
            Self::Constant(ty) => smallvec![
                Output {
                    ty: ty.clone(),
                    label: "Value".into()
                }
            ],
            Self::Comparison(_) => smallvec![
                Output {
                    ty: Type::Boolean,
                    label: "Result".into()
                }
            ],
            Self::Select => smallvec![
                Output {
                    ty: Type::Generic1,
                    label: "Result".into()
                }
            ],
            Self::Branch => smallvec![
                Output {
                    ty: Type::Execution,
                    label: "True".into()
                },
                Output {
                    ty: Type::Execution,
                    label: "False".into()
                }
            ],

            Self::CommandEvent => smallvec![
                Output {
                    ty: Type::Execution,
                    label: "Execution".into()
                },
                Output {
                    ty: Type::List(Box::new(Type::Character)),
                    label: "Name".into()
                }
            ],

            Self::SendMessageAction => smallvec![]
        }.into_iter()
    }
}
