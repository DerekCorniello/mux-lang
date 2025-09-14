use crate::lexer::{Span, Token, TokenType};
use ordered_float::OrderedFloat;
impl std::error::Error for ParserError {}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone)]
pub struct ParserError {
    pub message: String,
    pub span: Span,
}

impl ParserError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn from_token(message: impl Into<String>, token: &Token) -> Self {
        Self {
            message: message.into(),
            span: token.span,
        }
    }

    pub fn with_context(&self, source: &str) -> String {
        let start_line = self.span.row_start.saturating_sub(1);
        let end_line = self
            .span
            .row_end
            .unwrap_or(self.span.row_start)
            .saturating_sub(1);

        let lines: Vec<&str> = source
            .lines()
            .skip(start_line)
            .take(end_line - start_line + 1)
            .collect();

        let line_numbers: Vec<String> = (start_line..=end_line)
            .map(|n| format!("{} | ", n + 1))
            .collect();

        let max_line_num_len = line_numbers.last().map_or(0, |s| s.len());

        let mut message = String::new();
        message.push_str(&format!(
            "Error at {}:{} - {}\n",
            self.span.row_start, self.span.col_start, self.message
        ));

        for (i, (line_num, line)) in line_numbers.into_iter().zip(lines).enumerate() {
            message.push_str(&format!(
                "{:width$}{}\n",
                line_num,
                line,
                width = max_line_num_len
            ));

            // Add error indicator
            if i == 0 {
                let indent = max_line_num_len + self.span.col_start.saturating_sub(1);
                message.push_str(&format!("{}^~\n", " ".repeat(indent)));
            }
        }

        message
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parser error at {}:{} - {}",
            self.span.row_start, self.span.col_start, self.message
        )
    }
}

pub trait SpanExt {
    fn combine(&self, other: &Span) -> Span;

    fn from_token(token: &Token) -> Span;

    fn from_tokens(start: &Token, end: &Token) -> Span;

    fn is_valid(&self) -> bool;
}

impl SpanExt for Span {
    fn combine(&self, other: &Span) -> Span {
        let mut span = *self;
        if let (Some(end_row), Some(end_col)) = (other.row_end, other.col_end) {
            span.complete(end_row, end_col);
        }
        span
    }

    fn from_token(token: &Token) -> Span {
        token.span
    }

    fn from_tokens(start: &Token, end: &Token) -> Span {
        let mut span = start.span;
        if let (Some(row_end), Some(col_end)) = (end.span.row_end, end.span.col_end) {
            span.complete(row_end, col_end);
        }
        span
    }

    fn is_valid(&self) -> bool {
        self.row_end.is_some() && self.col_end.is_some()
    }
}

pub trait Spanned {
    fn span(&self) -> &Span;

    fn start_pos(&self) -> (usize, usize) {
        let span = self.span();
        (span.row_start, span.col_start)
    }

    fn end_pos(&self) -> Option<(usize, usize)> {
        let span = self.span();
        span.row_end.zip(span.col_end)
    }
}

macro_rules! impl_spanned {
    ($t:ty) => {
        impl Spanned for $t {
            fn span(&self) -> &Span {
                &self.span
            }
        }
    };
}
#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Function {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        params: Vec<Param>,
        return_type: TypeKind,
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
        target: TypeKind,
    },
    Const {
        name: String,
        type_: Option<TypeKind>,
        value: ExpressionNode,
    },
    Let {
        name: String,
        type_: Option<TypeKind>,
        value: ExpressionNode,
    },
}

#[derive(Debug, Clone)]
pub struct DeclarationNode {
    pub kind: DeclarationKind,
    pub span: Span,
}

impl_spanned!(DeclarationNode);

#[derive(Debug, Clone)]
pub enum AstNode {
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
        type_: TypeNode,  // No longer optional, must be a type or 'auto'
        value: ExpressionNode,
    },
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Let {
        name: String,
        type_: TypeKind,  // No longer optional, must be a type or 'auto'
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
pub struct StatementNode {
    pub kind: StatementKind,
    pub span: Span,
}

impl_spanned!(StatementNode);

#[derive(Debug, Clone)]
pub enum ExpressionKind {
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
pub struct ExpressionNode {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl_spanned!(ExpressionNode);

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
    Str,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Auto,  // For type inference with 'auto' keyword
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
pub struct TypeNode {
    pub kind: TypeKind,
    pub span: Span,
}

impl_spanned!(TypeNode);

#[derive(Debug, Clone)]
pub struct TraitBound {
    pub name: String,
    pub type_params: Vec<TypeNode>,
}

#[derive(Debug, Clone)]
pub enum LiteralNode {
    Float(OrderedFloat<f64>),
    Integer(i64),
    String(String),
    Boolean(bool),
    Char(char),
}

impl LiteralNode {
    pub fn parse(token: Token) -> Result<LiteralNode, ParserError> {
        match token.token_type {
            TokenType::Char(char) => {
                Ok (LiteralNode::Char(char))
            }
            TokenType::Str(str) => {
                Ok (LiteralNode::String(str))
            }
            TokenType::Int(integer) => {
                Ok (LiteralNode::Integer(integer))
            }
            TokenType::Float(float) => {
                Ok (LiteralNode::Float(float))
            }
            TokenType::Bool(bool) => {
                Ok (LiteralNode::Boolean(bool))
            }
            _ => {Err (ParserError{message: format!("Invalid literal token: {:?}", token.token_type).to_string(), span: token.span})}
        }
    }
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

fn parse_from_tokens(_tokens: Vec<Token>) -> Vec<AstNode> {
    let parsed_result: Vec<AstNode> = Vec::new();
    parsed_result
}
