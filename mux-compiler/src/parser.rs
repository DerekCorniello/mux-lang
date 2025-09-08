use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum AstNode {
    Declaration(DeclarationNode),
    Statement(StatementNode),
    Expression(ExpressionNode),
    Type(TypeNode),
}

#[derive(Debug, Clone)]
pub enum DeclarationNode {
    Function {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        params: Vec<Param>,
        return_type: TypeNode,
        body: Vec<StatementNode>,
    },
    Class {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        traits: Vec<TraitRef>,
        fields: Vec<Field>,
        methods: Vec<FunctionNode>,
    },
    Interface {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        methods: Vec<FunctionNode>,
    },
    Enum {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        variants: Vec<EnumVariant>,
    },
    TypeAlias {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        target: TypeNode,
    },
    Const {
        name: String,
        type_: Option<TypeNode>,
        value: ExpressionNode,
    },
    Let {
        name: String,
        type_: Option<TypeNode>,
        value: ExpressionNode,
    },
}

#[derive(Debug, Clone)]
pub enum StatementNode {
    Let {
        name: String,
        type_: Option<TypeNode>,
        value: ExpressionNode,
    },
    Return(Option<ExpressionNode>),
    Expression(ExpressionNode),
    If {
        cond: ExpressionNode,
        then_block: Vec<StatementNode>,
        else_block: Option<Vec<StatementNode>>,
    },
    While {
        cond: ExpressionNode,
        body: Vec<StatementNode>,
    },
    For {
        var: String,
        iter: ExpressionNode,
        body: Vec<StatementNode>,
    },
    Break,
    Continue,
    Block(Vec<StatementNode>),
}

#[derive(Debug, Clone)]
pub enum ExpressionNode {
    Literal(LiteralNode),
    Identifier(String),
    Binary {
        left: Box<ExpressionNode>,
        op: BinaryOp,
        right: Box<ExpressionNode>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<ExpressionNode>,
    },
    Call {
        func: Box<ExpressionNode>,
        args: Vec<ExpressionNode>,
    },
    FieldAccess {
        expr: Box<ExpressionNode>,
        field: String,
    },
    ArrayAccess {
        expr: Box<ExpressionNode>,
        index: Box<ExpressionNode>,
    },
    Lambda {
        params: Vec<Param>,
        body: Vec<StatementNode>,
    },
    IfExpr {
        cond: Box<ExpressionNode>,
        then_block: Vec<StatementNode>,
        else_block: Option<Vec<StatementNode>>,
    },
    Match {
        expr: Box<ExpressionNode>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
    Str,
}

#[derive(Debug, Clone)]
pub enum TypeNode {
    Primitive(PrimitiveType),
    Named(String),
    Generic {
        base: String,
        args: Vec<TypeNode>,
        bounds: Vec<TraitBound>,
    },
    List(Box<TypeNode>),
    Map(Box<TypeNode>, Box<TypeNode>),
    Array(Box<TypeNode>, usize),
    Tuple(Vec<TypeNode>),
    Optional(Box<TypeNode>),
    Result(Box<TypeNode>, Box<TypeNode>),
    Function {
        params: Vec<TypeNode>,
        returns: Box<TypeNode>,
    },
    Alias(String),
}

#[derive(Debug, Clone)]
pub struct TraitBound {
    pub name: String,
    pub type_params: Vec<TypeNode>,
}

#[derive(Debug, Clone)]
pub enum LiteralNode {
    Number(f64),
    String(String),
    Boolean(bool),
    Char(char),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub type_: TypeNode,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub type_: TypeNode,
}

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub name: String,
    pub type_params: Vec<(String, Vec<TraitBound>)>,
    pub params: Vec<Param>,
    pub return_type: TypeNode,
    pub body: Vec<StatementNode>,
}

#[derive(Debug, Clone)]
pub struct TraitRef {
    pub name: String,
    pub type_args: Vec<TypeNode>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub data: Option<Vec<TypeNode>>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: PatternNode,
    pub body: Vec<StatementNode>,
}

#[derive(Debug, Clone)]
pub enum PatternNode {
    Literal(LiteralNode),
    Identifier(String),
    Wildcard, // `_`
    Tuple(Vec<PatternNode>),
    EnumVariant {
        name: String,
        args: Vec<PatternNode>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

fn parse_from_tokens(tokens: Vec<Token>) -> Vec<AstNode> {
    let parsed_result: Vec<AstNode> = Vec::new();

    parsed_result
}
