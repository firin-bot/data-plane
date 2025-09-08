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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Execution(bool),
    Boolean(bool),
    Integer(i64),
    Real(f64),
    Character(char),
    List(Vec<Value>)
}

impl Value {
    const fn get_execution(&self) -> Option<bool> {
        if let Self::Execution(b) = self { Some(*b) } else { None }
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
    Comparison(ComparisonType),
    Select,
    Branch,

    Constant(Type),

    CommandEvent,

    SendMessageAction
}

pub enum EvalError {
    Unexpected
}

impl Node {
    pub fn label(&self) -> SmallString<[u8; 64]> {
        match self {
            Self::Comparison(cty) => {
                let mut s = SmallString::from(cty.label().as_ref());
                s.push_str(" Comparison");
                s
            },
            Self::Select => "Select".into(),
            Self::Branch => "Branch".into(),

            Self::Constant(ty) => {
                let mut s = SmallString::from(ty.label().as_ref());
                s.push_str(" Constant");
                s
            },

            Self::CommandEvent => "Command".into(),

            Self::SendMessageAction => "Send Message".into()
        }
    }

    pub fn inputs(&self) -> <SmallVec<[Input; 4]> as IntoIterator>::IntoIter {
        match self {
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

            Self::Constant(_) => smallvec![],

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

    pub fn evaluate(&self, inputs: &[Value]) -> Result<<SmallVec<[Value; 4]> as IntoIterator>::IntoIter, EvalError> {
        match self {
            Self::Comparison(cty) => {
                inputs[0].partial_cmp(&inputs[1]).map(|ord| smallvec![
                    Value::Boolean(match cty {
                        ComparisonType::EQ => ord.is_eq(),
                        ComparisonType::NE => ord.is_ne(),
                        ComparisonType::GT => ord.is_gt(),
                        ComparisonType::GE => ord.is_ge(),
                        ComparisonType::LT => ord.is_lt(),
                        ComparisonType::LE => ord.is_le(),
                    })
                ]).ok_or(EvalError::Unexpected)
            },
            Self::Select => {
                inputs[0].get_boolean().map(|b| smallvec![
                    if b {
                        inputs[1].clone()
                    } else {
                        inputs[2].clone()
                    }
                ]).ok_or(EvalError::Unexpected)
            },
            Self::Branch => {
                todo!()
            },

            Self::Constant(ty) => {
                todo!()
            }

            _ => {
                todo!()
            }
        }.map(|v| v.into_iter())
    }
}
