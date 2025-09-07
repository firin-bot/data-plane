use smallstr::SmallString;
use smallvec::SmallVec;
use smallvec::smallvec;

pub enum Type {
    Execution,
    Integer,
    Real,
    Char,
    List(Box<Type>)
}

pub struct Input {
    ty: Type,
    label: SmallString<[u8; 16]>
}

pub struct Output {
    ty: Type,
    label: SmallString<[u8; 16]>
}

pub enum Node {
    CommandEvent,
    NamedCommandEvent(String),
}

impl Node {
    pub fn inputs(&self) -> <SmallVec<[Input; 8]> as IntoIterator>::IntoIter {
        match self {
            Self::CommandEvent         => smallvec![],
            Self::NamedCommandEvent(_) => smallvec![]
        }.into_iter()
    }

    pub fn outputs(&self) -> <SmallVec<[Output; 8]> as IntoIterator>::IntoIter {
        match self {
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
            Self::NamedCommandEvent(_) => smallvec![
                Output {
                    ty: Type::Execution,
                    label: "Execution".into()
                }
            ]
        }.into_iter()
    }
}
