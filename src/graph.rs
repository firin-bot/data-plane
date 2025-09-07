use smallstr::SmallString;
use smallvec::SmallVec;
use smallvec::smallvec;

#[derive(Clone, Debug)]
pub enum Type {
    Execution,
    Boolean,
    Integer,
    Real,
    Char,
    List(Box<Type>),
    Generic1
}

#[derive(Debug)]
pub enum ComparisonType {
    EQ
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
    Branch,
    Select,

    CommandEvent,

    SendMessageAction
}

impl Node {
    pub fn inputs(&self) -> <SmallVec<[Input; 8]> as IntoIterator>::IntoIter {
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

            Self::CommandEvent => smallvec![],

            Self::SendMessageAction => smallvec![
                Input {
                    ty: Type::Execution,
                    label: "Execution".into()
                },
                Input {
                    ty: Type::List(Box::new(Type::Char)),
                    label: "Message".into()
                }
            ]
        }.into_iter()
    }

    pub fn outputs(&self) -> <SmallVec<[Output; 8]> as IntoIterator>::IntoIter {
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
            Self::Select => smallvec![
                Output {
                    ty: Type::Generic1,
                    label: "Result".into()
                }
            ],

            Self::CommandEvent => smallvec![
                Output {
                    ty: Type::Execution,
                    label: "Execution".into()
                },
                Output {
                    ty: Type::List(Box::new(Type::Char)),
                    label: "Name".into()
                }
            ],

            Self::SendMessageAction => smallvec![]
        }.into_iter()
    }
}
