use crate::lexer::{Span, Token, TokenType};
use ordered_float::OrderedFloat;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], source: &'a str) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParserResult<Vec<AstNode>> {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            todo!("parsing will happen here");
        }
        Ok(nodes)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn consume(&mut self) -> &Token {
        let ret = &self.tokens[self.current];
        self.current += 1;
        ret
    }

    fn consume_until(&mut self, token_type: TokenType) -> &[Token] {
        let start = self.current;
        while !self.is_at_end() && self.peek().token_type != token_type {
            self.consume();
        }
        let end = self.current;
        &self.tokens[start..end]
    }
}

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
        type_: TypeNode,
        value: ExpressionNode,
    },
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Let {
        name: String,
        type_: TypeKind,
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

impl From<(Box<ExpressionNode>, BinaryOp, Box<ExpressionNode>)> for ExpressionKind {
    fn from(value: (Box<ExpressionNode>, BinaryOp, Box<ExpressionNode>)) -> Self {
        ExpressionKind::Binary {
            left: value.0,
            op: value.1,
            right: value.2,
        }
    }
}

impl From<(UnaryOp, Box<ExpressionNode>)> for ExpressionKind {
    fn from((op, expr): (UnaryOp, Box<ExpressionNode>)) -> Self {
        ExpressionKind::Unary { op, expr }
    }
}

impl From<(Box<ExpressionNode>, String)> for ExpressionKind {
    fn from((expr, field): (Box<ExpressionNode>, String)) -> Self {
        ExpressionKind::FieldAccess { expr, field }
    }
}

impl From<(Box<ExpressionNode>, Box<ExpressionNode>)> for ExpressionKind {
    fn from((expr, index): (Box<ExpressionNode>, Box<ExpressionNode>)) -> Self {
        ExpressionKind::ArrayAccess { expr, index }
    }
}

impl From<LiteralNode> for ExpressionKind {
    fn from(lit: LiteralNode) -> Self {
        ExpressionKind::Literal(lit)
    }
}

impl From<OrderedFloat<f64>> for LiteralNode {
    fn from(f: OrderedFloat<f64>) -> Self {
        LiteralNode::Float(f)
    }
}

impl From<i64> for LiteralNode {
    fn from(i: i64) -> Self {
        LiteralNode::Integer(i)
    }
}

impl From<String> for LiteralNode {
    fn from(s: String) -> Self {
        LiteralNode::String(s)
    }
}

impl From<&str> for LiteralNode {
    fn from(s: &str) -> Self {
        LiteralNode::String(s.to_string())
    }
}

impl From<bool> for LiteralNode {
    fn from(b: bool) -> Self {
        LiteralNode::Boolean(b)
    }
}

impl From<char> for LiteralNode {
    fn from(c: char) -> Self {
        LiteralNode::Char(c)
    }
}

impl From<String> for ExpressionKind {
    fn from(ident: String) -> Self {
        ExpressionKind::Identifier(ident)
    }
}

impl From<&str> for ExpressionNode {
    fn from(ident: &str) -> Self {
        ExpressionNode {
            kind: ExpressionKind::Identifier(ident.to_string()),
            span: Span::new(0, 0),
        }
    }
}

impl From<String> for ExpressionNode {
    fn from(ident: String) -> Self {
        ExpressionNode {
            kind: ExpressionKind::Identifier(ident),
            span: Span::new(0, 0),
        }
    }
}

impl From<LiteralNode> for ExpressionNode {
    fn from(lit: LiteralNode) -> Self {
        ExpressionNode {
            kind: ExpressionKind::Literal(lit),
            span: Span::new(0, 0),
        }
    }
}

impl From<&str> for ExpressionKind {
    fn from(ident: &str) -> Self {
        ExpressionKind::Identifier(ident.to_string())
    }
}

impl From<ExpressionNode> for StatementKind {
    fn from(expr: ExpressionNode) -> Self {
        StatementKind::Expression(expr)
    }
}

impl From<ExpressionNode> for StatementNode {
    fn from(expr: ExpressionNode) -> Self {
        StatementNode {
            kind: expr.to_owned().into(),
            span: expr.span,
        }
    }
}

impl From<StatementKind> for StatementNode {
    fn from(kind: StatementKind) -> Self {
        StatementNode {
            kind,
            span: Span::new(0, 0),
        }
    }
}

impl From<ExpressionNode> for Vec<StatementNode> {
    fn from(expr: ExpressionNode) -> Self {
        vec![StatementNode {
            kind: StatementKind::Expression(expr.to_owned()),
            span: expr.span,
        }]
    }
}

impl From<Vec<StatementNode>> for StatementKind {
    fn from(stmts: Vec<StatementNode>) -> Self {
        StatementKind::Block(stmts)
    }
}

impl From<PrimitiveType> for TypeKind {
    fn from(prim: PrimitiveType) -> Self {
        TypeKind::Primitive(prim)
    }
}

impl From<&str> for TypeNode {
    fn from(s: &str) -> Self {
        TypeNode {
            kind: TypeKind::Named(s.to_string()),
            span: Span::new(0, 0),
        }
    }
}

impl From<String> for TypeNode {
    fn from(s: String) -> Self {
        TypeNode {
            kind: TypeKind::Named(s),
            span: Span::new(0, 0),
        }
    }
}

impl From<PrimitiveType> for TypeNode {
    fn from(prim: PrimitiveType) -> Self {
        TypeNode {
            kind: prim.into(),
            span: Span::new(0, 0),
        }
    }
}

impl From<&str> for TypeKind {
    fn from(s: &str) -> Self {
        TypeKind::Named(s.to_string())
    }
}

impl From<String> for TypeKind {
    fn from(s: String) -> Self {
        TypeKind::Named(s)
    }
}

impl From<LiteralNode> for PatternNode {
    fn from(lit: LiteralNode) -> Self {
        PatternNode::Literal(lit)
    }
}

impl From<&str> for PatternNode {
    fn from(s: &str) -> Self {
        PatternNode::Identifier(s.to_string())
    }
}

impl From<String> for PatternNode {
    fn from(s: String) -> Self {
        PatternNode::Identifier(s)
    }
}

impl From<(Box<ExpressionNode>, Vec<ExpressionNode>)> for ExpressionKind {
    fn from((func, args): (Box<ExpressionNode>, Vec<ExpressionNode>)) -> Self {
        ExpressionKind::Call { func, args }
    }
}

impl From<(Vec<Param>, Vec<StatementNode>)> for ExpressionKind {
    fn from((params, body): (Vec<Param>, Vec<StatementNode>)) -> Self {
        ExpressionKind::Lambda { params, body }
    }
}

impl
    From<(
        Box<ExpressionNode>,
        Vec<StatementNode>,
        Option<Vec<StatementNode>>,
    )> for ExpressionKind
{
    fn from(
        (cond, then_block, else_block): (
            Box<ExpressionNode>,
            Vec<StatementNode>,
            Option<Vec<StatementNode>>,
        ),
    ) -> Self {
        ExpressionKind::IfExpr {
            cond,
            then_block,
            else_block,
        }
    }
}

impl From<(Box<ExpressionNode>, Vec<MatchArm>)> for ExpressionKind {
    fn from((expr, arms): (Box<ExpressionNode>, Vec<MatchArm>)) -> Self {
        ExpressionKind::Match { expr, arms }
    }
}

impl ExpressionKind {
    pub fn parse(token: Token) -> ParserResult<Self> {
        match token.token_type {
            TokenType::Int(_)
            | TokenType::Float(_)
            | TokenType::Str(_)
            | TokenType::Char(_)
            | TokenType::Bool(_) => LiteralNode::parse(token).map(ExpressionKind::Literal),
            TokenType::Id(ident) => Ok(ExpressionKind::Identifier(ident)),
            _ => Err(ParserError::new(
                format!("Unexpected token in expression: {:?}", token.token_type),
                token.span,
            )),
        }
    }
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

impl PrimitiveType {
    pub fn parse(token: Token) -> ParserResult<PrimitiveType> {
        match token.token_type {
            TokenType::Id(value) => match value.as_str() {
                "int" => Ok(PrimitiveType::Int),
                "float" => Ok(PrimitiveType::Float),
                "bool" => Ok(PrimitiveType::Bool),
                "char" => Ok(PrimitiveType::Char),
                "str" => Ok(PrimitiveType::Str),
                _ => Err(ParserError {
                    message: format!("Unknown primitive type: {}", value),
                    span: token.span,
                }),
            },
            _ => Err(ParserError {
                message: format!("Expected an identifier, found {:?}", token.token_type),
                span: token.span,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Auto,
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
    pub fn parse(token: Token) -> ParserResult<LiteralNode> {
        match token.token_type {
            TokenType::Char(char) => Ok(LiteralNode::Char(char)),
            TokenType::Str(str) => Ok(LiteralNode::String(str)),
            TokenType::Int(integer) => Ok(LiteralNode::Integer(integer)),
            TokenType::Float(float) => Ok(LiteralNode::Float(float)),
            TokenType::Bool(bool) => Ok(LiteralNode::Boolean(bool)),
            _ => Err(ParserError {
                message: format!("Invalid literal token: {:?}", token.token_type).to_string(),
                span: token.span,
            }),
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
    And,
    Or,
}

impl BinaryOp {
    pub fn parse(token: Token) -> ParserResult<BinaryOp> {
        match token.token_type {
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::Minus => Ok(BinaryOp::Sub),
            TokenType::Star => Ok(BinaryOp::Mul),
            TokenType::Slash => Ok(BinaryOp::Div),
            TokenType::Percent => Ok(BinaryOp::Mod),
            TokenType::And => Ok(BinaryOp::Sub),
            TokenType::Or => Ok(BinaryOp::Mul),
            _ => Err(ParserError {
                message: format!(
                    "Unexpected token type for binary operation: {:?}",
                    token.token_type
                )
                .to_string(),
                span: token.span,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryComp {
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

impl BinaryComp {
    pub fn parse(token: Token) -> ParserResult<BinaryComp> {
        match token.token_type {
            TokenType::Eq => Ok(BinaryComp::Eq),
            TokenType::NotEq => Ok(BinaryComp::Neq),
            TokenType::Lt => Ok(BinaryComp::Lt),
            TokenType::Gt => Ok(BinaryComp::Gt),
            TokenType::Le => Ok(BinaryComp::Leq),
            TokenType::Ge => Ok(BinaryComp::Geq),
            _ => Err(ParserError {
                message: format!(
                    "Unexpected token type for binary comparison: {:?}",
                    token.token_type
                )
                .to_string(),
                span: token.span,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub fn parse(token: Token) -> ParserResult<UnaryOp> {
        match token.token_type {
            TokenType::Minus => Ok(UnaryOp::Neg),
            TokenType::Bang => Ok(UnaryOp::Not),
            _ => Err(ParserError {
                message: format!(
                    "Unexpected token type for unary operation: {:?}",
                    token.token_type
                )
                .to_string(),
                span: token.span,
            }),
        }
    }
}

fn parse_from_tokens(_tokens: Vec<Token>) -> Vec<AstNode> {
    let parsed_result: Vec<AstNode> = Vec::new();
    parsed_result
}
