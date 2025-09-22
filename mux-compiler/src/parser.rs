use crate::lexer::{Span, Token, TokenType};
use ordered_float::OrderedFloat;
use std::fmt;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    source: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    pub fn next_higher(self) -> Self {
        match self {
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }

    pub fn is_assignment(&self) -> bool {
        matches!(self, Precedence::Assignment)
    }

    /// Check if this operator is right-associative
    pub fn is_right_associative(&self) -> bool {
        // Assignment operators are right-associative: a = b = c is a = (b = c)
        self.is_assignment()
    }
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token], source: &'a str) -> Self {
        Self {
            tokens,
            current: 0,
            source,
        }
    }

    pub fn parse(&mut self) -> ParserResult<Vec<AstNode>> {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => nodes.push(decl),
                Err(e) => {
                    // Skip to next statement for error recovery
                    self.synchronize();
                    return Err(e);
                }
            }
        }
        Ok(nodes)
    }

    fn declaration(&mut self) -> ParserResult<AstNode> {
        if self.matches(&[TokenType::Auto]) {
            self.auto_declaration()
        } else if self.matches(&[TokenType::Func]) {
            self.function_declaration()
        } else if matches!(self.peek().token_type, TokenType::Id(_)) {
            // Check for type name
            let type_name = match &self.peek().token_type {
                TokenType::Id(name) => name.clone(),
                _ => return self.statement(),
            };
            match type_name.as_str() {
                "int" | "float" | "bool" | "char" | "str" | "void" => self.typed_declaration(),
                _ => self.statement(),
            }
        } else {
            self.statement()
        }
    }

    fn typed_declaration(&mut self) -> ParserResult<AstNode> {
        let type_node = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name after type")?;

        self.consume_token(TokenType::Eq, "Expected '=' after variable name")?;
        let value = self.parse_expression()?;
        let span = type_node.span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::AutoDecl(name, type_node, value),
            span,
        }))
    }

    fn auto_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.peek().span;
        self.consume_token(TokenType::Auto, "Expected 'auto' keyword")?;

        let name = self.consume_identifier("Expected variable name after 'auto'")?;
        let name_span = self.tokens[self.current - 1].span;
        let type_node = if self.matches(&[TokenType::Colon]) {
            self.parse_type()?
        } else {
            TypeNode {
                kind: TypeKind::Auto,
                span: name_span,
            }
        };

        self.consume_token(TokenType::Eq, "Expected '=' after variable name")?;
        let value = self.parse_expression()?;
        let span = start_span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::AutoDecl(name, type_node, value),
            span,
        }))
    }

    fn function_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.peek().span;
        self.consume_token(TokenType::Func, "Expected 'func' keyword")?;

        let name = self.consume_identifier("Expected function name")?;

        let type_params = if self.matches(&[TokenType::Lt]) {
            let mut params = Vec::new();
            if !self.check(TokenType::Gt) {
                loop {
                    let param = self.consume_identifier("Expected type parameter name")?;
                    let mut bounds = Vec::new();

                    if self.matches(&[TokenType::Colon]) {
                        loop {
                            let bound_name =
                                self.consume_identifier("Expected trait name in bound")?;
                            let type_args = if self.matches(&[TokenType::Lt]) {
                                let args = self.parse_type_arguments()?;
                                self.consume_token(
                                    TokenType::Gt,
                                    "Expected '>' after type arguments",
                                )?;
                                args
                            } else {
                                Vec::new()
                            };

                            bounds.push(TraitBound {
                                name: bound_name,
                                type_params: type_args,
                            });

                            if !self.matches(&[TokenType::Plus]) {
                                break;
                            }
                        }
                    }

                    params.push((param, bounds));

                    if !self.matches(&[TokenType::Comma]) {
                        break;
                    }
                }
            }
            self.consume_token(TokenType::Gt, "Expected '>' after type parameters")?;
            params
        } else {
            Vec::new()
        };

        self.consume_token(TokenType::OpenParen, "Expected '(' after function name")?;
        let mut params = Vec::new();

        if !self.check(TokenType::CloseParen) {
            loop {
                let param_name = self.consume_identifier("Expected parameter name")?;
                self.consume_token(TokenType::Colon, "Expected ':' after parameter name")?;

                let param_type = self.parse_type()?;

                // TODO: Handle default values
                if self.matches(&[TokenType::Eq]) {
                    self.parse_expression()?;
                }

                params.push(Param {
                    name: param_name,
                    type_: param_type,
                });

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume_token(TokenType::CloseParen, "Expected ')' after parameters")?;

        let return_type = if self.matches(&[TokenType::Returns]) {
            self.parse_type()?
        } else {
            TypeNode {
                kind: TypeKind::Primitive(PrimitiveType::Void),
                span: self.peek().span,
            }
        };

        let body = self.block_statement()?;
        let body_statements = match body {
            AstNode::Statement(stmt) => match stmt.kind {
                StatementKind::Block(block) => block,
                _ => vec![stmt],
            },
            _ => {
                return Err(ParserError::new(
                    "Expected block statement for function body",
                    start_span,
                ));
            }
        };

        let end_span = body_statements.last().map(|s| s.span).unwrap_or(start_span);
        let span = start_span.combine(&end_span);

        Ok(AstNode::Function(FunctionNode {
            name,
            type_params,
            params,
            return_type,
            body: body_statements,
            span,
        }))
    }

    fn statement(&mut self) -> ParserResult<AstNode> {
        if self.matches(&[TokenType::If]) {
            self.if_statement()
        } else if self.matches(&[TokenType::While]) {
            self.while_statement()
        } else if self.matches(&[TokenType::For]) {
            self.for_statement()
        } else if self.matches(&[TokenType::Return]) {
            self.return_statement()
        } else if self.matches(&[TokenType::OpenBrace]) {
            self.block_statement()
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current - 1].span;

        self.consume_token(TokenType::OpenParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expression()?;
        self.consume_token(TokenType::CloseParen, "Expected ')' after if condition")?;

        let then_branch = self.block_statement()?;
        let then_block = match then_branch {
            AstNode::Statement(stmt) => match stmt.kind {
                StatementKind::Block(block) => block,
                _ => vec![stmt],
            },
            _ => {
                return Err(ParserError::new(
                    "Expected block statement after if condition",
                    start_span,
                ));
            }
        };

        let (else_block, end_span) = if self.matches(&[TokenType::Else]) {
            if self.matches(&[TokenType::If]) {
                match self.if_statement()? {
                    AstNode::Statement(stmt) => (Some(vec![stmt.clone()]), stmt.span),
                    _ => {
                        return Err(ParserError::new(
                            "Expected statement after else if",
                            self.tokens[self.current - 1].span,
                        ));
                    }
                }
            } else {
                match self.block_statement()? {
                    AstNode::Statement(stmt) => match stmt.kind {
                        StatementKind::Block(block) => (Some(block), stmt.span),
                        _ => (Some(vec![stmt.clone()]), stmt.span),
                    },
                    _ => {
                        return Err(ParserError::new(
                            "Expected block statement after else",
                            self.tokens[self.current - 1].span,
                        ));
                    }
                }
            }
        } else {
            (
                None,
                then_block.last().map(|s| s.span).unwrap_or(start_span),
            )
        };

        let span = start_span.combine(&end_span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::If {
                cond: condition,
                then_block,
                else_block,
            },
            span,
        }))
    }

    fn while_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current - 1].span;

        self.consume_token(TokenType::OpenParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume_token(TokenType::CloseParen, "Expected ')' after while condition")?;

        let body = self.block_statement()?;
        let body_statements = match body {
            AstNode::Statement(stmt) => match stmt.kind {
                StatementKind::Block(block) => block,
                _ => vec![stmt],
            },
            _ => {
                return Err(ParserError::new(
                    "Expected block statement after while condition",
                    start_span,
                ));
            }
        };

        let end_span = body_statements.last().map(|s| s.span).unwrap_or(start_span);
        let span = start_span.combine(&end_span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::While {
                cond: condition,
                body: body_statements,
            },
            span,
        }))
    }

    fn for_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current - 1].span;

        self.consume_token(TokenType::OpenParen, "Expected '(' after 'for'")?;

        let var = self.consume_identifier("Expected loop variable name")?;

        self.consume_token(TokenType::In, "Expected 'in' after loop variable")?;

        let iter = self.parse_expression()?;

        self.consume_token(TokenType::CloseParen, "Expected ')' after for loop header")?;

        let body = self.block_statement()?;
        let body_statements = match body {
            AstNode::Statement(stmt) => match stmt.kind {
                StatementKind::Block(block) => block,
                _ => vec![stmt],
            },
            _ => {
                return Err(ParserError::new(
                    "Expected block statement after for loop",
                    start_span,
                ));
            }
        };

        let end_span = body_statements.last().map(|s| s.span).unwrap_or(start_span);
        let span = start_span.combine(&end_span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::For {
                var,
                iter,
                body: body_statements,
            },
            span,
        }))
    }

    fn return_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current - 1].span;

        let value = if !self.check(TokenType::Semicolon) && !self.check_next_line() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if self.matches(&[TokenType::Semicolon]) {
            // Already consumed
        }

        let end_span = value.as_ref().map_or(start_span, |v| v.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Return(value),
            span: start_span.combine(&end_span),
        }))
    }

    fn block_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current - 1].span;

        let mut statements = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume_token(TokenType::CloseBrace, "Expected '}' after block")?;

        let end_span = self.tokens[self.current - 1].span;

        let stmts: Vec<StatementNode> = statements
            .into_iter()
            .filter_map(|node| node.into_statement())
            .collect();

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Block(stmts),
            span: start_span.combine(&end_span),
        }))
    }

    fn expression_statement(&mut self) -> ParserResult<AstNode> {
        let expr = self.parse_expression()?;

        if self.matches(&[TokenType::Semicolon]) {
            // Already consumed
        }

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Expression(expr),
            span: self.tokens[self.current - 1].span,
        }))
    }

    fn parse_type(&mut self) -> ParserResult<TypeNode> {
        let token = self.consume();
        let start_span = token.span;

        if let TokenType::Id(name) = &token.token_type {
            if let Ok(prim_type) = PrimitiveType::parse(token.clone()) {
                return Ok(TypeNode {
                    kind: TypeKind::Primitive(prim_type),
                    span: start_span,
                });
            }

            let name_clone = name.clone();

            let type_args = if self.matches(&[TokenType::Lt]) {
                let args = self.parse_type_arguments()?;
                self.consume_token(TokenType::Gt, "Expected '>' after type arguments")?;
                args
            } else {
                Vec::new()
            };

            return Ok(TypeNode {
                kind: TypeKind::Named(name_clone, type_args),
                span: start_span,
            });
        }

        match token.token_type {
            TokenType::OpenParen => {
                let mut param_types = Vec::new();

                if !self.check(TokenType::CloseParen) {
                    loop {
                        param_types.push(self.parse_type()?);

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                self.consume_token(TokenType::CloseParen, "Expected ')' after parameter types")?;
                self.consume_token(TokenType::Minus, "Expected '->' in function type")?;
                self.consume_token(TokenType::Gt, "Expected '->' in function type")?;

                let return_type = Box::new(self.parse_type()?);

                Ok(TypeNode {
                    kind: TypeKind::Function {
                        params: param_types,
                        returns: return_type,
                    },
                    span: start_span,
                })
            }
            _ => Err(ParserError::from_token("Expected type", token)),
        }
    }

    fn parse_type_arguments(&mut self) -> ParserResult<Vec<TypeNode>> {
        let mut args = Vec::new();

        if !self.check(TokenType::Gt) {
            loop {
                args.push(self.parse_type()?);

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        Ok(args)
    }

    fn check_next_line(&self) -> bool {
        if self.is_at_end() || self.current == 0 {
            return false;
        }

        let current_token = &self.tokens[self.current - 1];
        let next_token = &self.tokens[self.current];

        next_token.span.row_start > current_token.span.row_start
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            match self.peek().token_type {
                TokenType::Semicolon => {
                    self.consume();
                    break;
                }
                TokenType::Func
                | TokenType::Auto
                | TokenType::If
                | TokenType::While
                | TokenType::For
                | TokenType::Return
                | TokenType::OpenBrace => {
                    // New statement found
                    break;
                }
                _ => {
                    self.consume();
                }
            }
        }
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

    pub fn parse_expression(&mut self) -> ParserResult<ExpressionNode> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn parse_precedence(&mut self, min_precedence: Precedence) -> ParserResult<ExpressionNode> {
        let left = self.parse_unary()?;
        let mut value = Box::new(left);

        while let Some(op_token) = self.consume_operator() {
            // Check operator precedence
            let op_precedence = self.get_operator_precedence(&op_token)?;
            if op_precedence < min_precedence {
                break;
            }

            // Handle operator associativity
            let next_precedence = if op_token.is_right_associative() {
                op_precedence
            } else {
                op_precedence.next_higher()
            };

            let right = self.parse_precedence(next_precedence)?;

            let left_span = *value.span();
            let right_span = *right.span();
            let left_expr = value.clone();
            let new_value = ExpressionNode {
                kind: ExpressionKind::Binary {
                    left: left_expr,
                    op: op_token,
                    right: Box::new(right),
                },
                span: left_span.combine(&right_span),
            };
            value = Box::new(new_value);
        }

        Ok(*value)
    }

    fn parse_unary(&mut self) -> ParserResult<ExpressionNode> {
        if let Some(op_token) = self.consume_if_unary_operator() {
            let expr = self.parse_precedence(Precedence::Unary)?;
            let expr_span = *expr.span();
            Ok(ExpressionNode {
                kind: ExpressionKind::Unary {
                    op: UnaryOp::parse(op_token.clone())?,
                    expr: Box::new(expr),
                },
                span: op_token.span.combine(&expr_span),
            })
        } else {
            let expr = self.parse_primary()?;
            self.parse_postfix_operators(expr)
        }
    }

    fn parse_primary(&mut self) -> ParserResult<ExpressionNode> {
        if self.is_at_end() {
            return Err(ParserError::new(
                "Expected expression, found end of input",
                Span::new(0, 0),
            ));
        }

        let token = self.consume();
        let token_type = token.token_type.clone();
        let token_span = token.span;

        match token_type {
            TokenType::OpenParen => {
                let expr = self.parse_expression()?;
                self.consume_token(TokenType::CloseParen, "Expected ')' after expression")?;
                self.parse_postfix_operators(expr)
            }

            TokenType::Int(n) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Literal(LiteralNode::Integer(n)),
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            TokenType::Str(s) => {
                // Determine if this is an identifier or string literal
                let is_identifier = if let Some(next_token) = self.tokens.get(self.current) {
                    matches!(
                        next_token.token_type,
                        TokenType::Colon
                            | TokenType::OpenParen
                            | TokenType::Dot
                            | TokenType::OpenBracket
                    )
                } else {
                    false
                };

                let expr = if is_identifier {
                    // Identifier
                    ExpressionNode {
                        kind: ExpressionKind::Identifier(s),
                        span: token_span,
                    }
                } else {
                    // String literal
                    ExpressionNode {
                        kind: ExpressionKind::Literal(LiteralNode::String(s)),
                        span: token_span,
                    }
                };

                self.parse_postfix_operators(expr)
            }

            TokenType::Bool(b) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Literal(LiteralNode::Boolean(b)),
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            // Array literals
            TokenType::OpenBracket => {
                let start_span = token_span;
                let mut elements = Vec::new();

                // Parse array elements
                if !self.check(TokenType::CloseBracket) {
                    loop {
                        elements.push(self.parse_expression()?);

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                let end_span = self
                    .consume_token(TokenType::CloseBracket, "Expected ']' after array elements")?;
                let expr = ExpressionNode {
                    kind: ExpressionKind::ArrayLiteral(elements),
                    span: start_span.combine(&end_span),
                };

                self.parse_postfix_operators(expr)
            }

            // Map literals
            TokenType::OpenBrace => {
                let start_span = token_span;
                let mut entries = Vec::new();

                if !self.check(TokenType::CloseBrace) {
                    loop {
                        // Parse key
                        let key = self.parse_expression()?;

                        // Consume colon
                        self.consume_token(TokenType::Colon, "Expected ':' after map key")?;

                        // Parse value
                        let value = self.parse_expression()?;

                        entries.push((key, value));

                        // Check for comma or closing brace
                        if self.matches(&[TokenType::Comma]) {
                            if self.check(TokenType::CloseBrace) {
                                // Trailing comma before closing brace
                                break;
                            }
                            continue;
                        } else if self.check(TokenType::CloseBrace) {
                            break;
                        } else {
                            return Err(ParserError::new(
                                "Expected ',' or '}' after map entry",
                                self.peek().span,
                            ));
                        }
                    }
                }

                let end_span =
                    self.consume_token(TokenType::CloseBrace, "Expected '}' after map entries")?;
                let expr = ExpressionNode {
                    kind: ExpressionKind::MapLiteral(entries),
                    span: start_span.combine(&end_span),
                };

                self.parse_postfix_operators(expr)
            }

            _ => Err(ParserError::from_token(
                "Expected expression",
                &Token {
                    token_type,
                    span: token_span,
                },
            )),
        }
    }

    fn parse_postfix_operators(
        &mut self,
        mut expr: ExpressionNode,
    ) -> ParserResult<ExpressionNode> {
        loop {
            if self.matches(&[TokenType::OpenParen]) {
                // Function call
                let mut args = Vec::new();
                if !self.check(TokenType::CloseParen) {
                    loop {
                        args.push(self.parse_expression()?);

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
                let end_span =
                    self.consume_token(TokenType::CloseParen, "Expected ')' after arguments")?;
                let expr_span = *expr.span();
                expr = ExpressionNode {
                    kind: ExpressionKind::Call {
                        func: Box::new(expr),
                        args,
                    },
                    span: expr_span.combine(&end_span),
                };
            } else if self.matches(&[TokenType::Dot]) {
                // Member access
                let field = self.consume_identifier("Expected field name after '.'")?;
                let expr_span = *expr.span();
                let field_span = self.tokens[self.current - 1].span;
                expr = ExpressionNode {
                    kind: ExpressionKind::FieldAccess {
                        expr: Box::new(expr),
                        field,
                    },
                    span: expr_span.combine(&field_span),
                };
            } else if self.matches(&[TokenType::OpenBracket]) {
                // Array access
                let index = self.parse_expression()?;
                let end_span =
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after index")?;
                let expr_span = *expr.span();
                expr = ExpressionNode {
                    kind: ExpressionKind::ArrayAccess {
                        expr: Box::new(expr),
                        index: Box::new(index),
                    },
                    span: expr_span.combine(&end_span),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        for ty in types {
            if self.check(ty.clone()) {
                self.current += 1;
                return true;
            }
        }
        false
    }

    fn check(&self, ty: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == ty
    }

    fn consume_token(&mut self, expected: TokenType, error_msg: &str) -> ParserResult<Span> {
        if self.is_at_end() {
            return Err(ParserError::new(
                error_msg,
                self.tokens.last().map(|t| t.span).unwrap_or_else(|| Span {
                    row_start: 0,
                    row_end: None,
                    col_start: 0,
                    col_end: None,
                }),
            ));
        }

        let token = &self.tokens[self.current];
        if token.token_type == expected {
            self.current += 1;
            Ok(token.span)
        } else {
            Err(ParserError::new(error_msg, token.span))
        }
    }

    fn consume_identifier(&mut self, error_msg: &str) -> ParserResult<String> {
        if self.is_at_end() {
            return Err(ParserError::new(error_msg, self.peek().span));
        }

        match &self.peek().token_type {
            TokenType::Str(name) => {
                let name_clone = name.clone();
                self.current += 1;
                Ok(name_clone)
            }
            _ => Err(ParserError::new(error_msg, self.peek().span)),
        }
    }

    fn check_next(&self, ty: TokenType) -> bool {
        if self.current + 1 >= self.tokens.len() {
            return false;
        }
        self.tokens[self.current + 1].token_type == ty
    }

    fn is_valid_assignment_target(expr: &ExpressionNode) -> bool {
        match &expr.kind {
            ExpressionKind::Identifier(_) => true,
            ExpressionKind::FieldAccess { .. } => true,
            ExpressionKind::ArrayAccess { .. } => true,
            ExpressionKind::MapLiteral(_) => {
                // Map literals are not valid lvalues on their own, but map access is
                // This is handled by the ArrayAccess case above
                false
            }
            _ => false,
        }
    }

    fn consume_operator(&mut self) -> Option<BinaryOp> {
        if self.is_at_end() {
            return None;
        }

        let token = self.peek();
        let result = match token.token_type {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Subtract),
            TokenType::Star => Some(BinaryOp::Multiply),
            TokenType::Slash => Some(BinaryOp::Divide),
            TokenType::Percent => Some(BinaryOp::Modulo),
            TokenType::Eq => Some(BinaryOp::Assign),
            TokenType::EqEq => Some(BinaryOp::Equal),
            TokenType::NotEq => Some(BinaryOp::NotEqual),
            TokenType::Lt => Some(BinaryOp::Less),
            TokenType::Gt => Some(BinaryOp::Greater),
            TokenType::Le => Some(BinaryOp::LessEqual),
            TokenType::Ge => Some(BinaryOp::GreaterEqual),
            TokenType::And => Some(BinaryOp::LogicalAnd),
            TokenType::Or => Some(BinaryOp::LogicalOr),
            TokenType::PlusEq => Some(BinaryOp::AddAssign),
            TokenType::MinusEq => Some(BinaryOp::SubtractAssign),
            TokenType::StarEq => Some(BinaryOp::MultiplyAssign),
            TokenType::SlashEq => Some(BinaryOp::DivideAssign),
            _ => None,
        };

        if result.is_some() {
            self.current += 1;
        }
        result
    }

    fn consume_if_unary_operator(&mut self) -> Option<Token> {
        if self.is_at_end() {
            return None;
        }

        let token = self.peek();
        match &token.token_type {
            TokenType::Minus | TokenType::Bang => {
                let token_clone = token.clone();
                self.current += 1;
                Some(token_clone)
            }
            _ => None,
        }
    }

    fn get_operator_precedence(&self, op: &BinaryOp) -> ParserResult<Precedence> {
        let precedence = match op {
            // Assignment operators
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubtractAssign
            | BinaryOp::MultiplyAssign
            | BinaryOp::DivideAssign
            | BinaryOp::ModuloAssign => Precedence::Assignment,

            // Logical OR
            BinaryOp::LogicalOr => Precedence::Or,

            // Logical AND
            BinaryOp::LogicalAnd => Precedence::And,

            // Equality/inequality
            BinaryOp::Equal | BinaryOp::NotEqual => Precedence::Equality,

            // Comparisons
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                Precedence::Comparison
            }

            // Addition, subtraction
            BinaryOp::Add | BinaryOp::Subtract => Precedence::Term,

            // Multiplication, division, modulo
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => Precedence::Factor,
        };
        Ok(precedence)
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
// ===== Top-level AST Node =====

/// Top-level AST node that can represent any construct in the Mux language
#[derive(Debug, Clone)]
pub enum AstNode {
    // Declarations
    Function(FunctionNode),
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
    ConstDecl(ConstDeclNode),
    AutoDecl(AutoDeclNode),

    // Statements
    Statement(StatementNode),

    // Expressions
    Expression(ExpressionNode),

    // Types
    Type(TypeNode),
}

// ===== Declaration Nodes =====

/// Auto variable declaration (type-inferred or explicitly typed)
#[derive(Debug, Clone)]
pub struct AutoDeclNode {
    pub name: String,
    pub type_: TypeNode, // Can be TypeKind::Auto for inferred types
    pub value: ExpressionNode,
    pub span: Span,
}

/// Constant declaration (must have explicit type)
#[derive(Debug, Clone)]
pub struct ConstDeclNode {
    pub name: String,
    pub type_: TypeNode,
    pub value: ExpressionNode,
    pub span: Span,
}

impl_spanned!(AutoDeclNode);
impl_spanned!(ConstDeclNode);

impl AstNode {
    pub fn into_statement(self) -> Option<StatementNode> {
        match self {
            AstNode::Statement(stmt) => Some(stmt),
            AstNode::Expression(expr) => Some(StatementNode {
                kind: StatementKind::Expression(expr.clone()),
                span: *expr.span(),
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    // Declaration statements
    AutoDecl(String, TypeNode, ExpressionNode),
    ConstDecl(String, TypeNode, ExpressionNode),

    // Control flow
    Return(Option<ExpressionNode>),
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

    // Expression statement
    Expression(ExpressionNode),

    // Block statement
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
    // Literals
    Literal(LiteralNode),
    Identifier(String),

    // Operations
    Binary {
        left: Box<ExpressionNode>,
        op: BinaryOp,
        right: Box<ExpressionNode>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<ExpressionNode>,
    },

    // Function calls and member access
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

    // Collection literals
    ArrayLiteral(Vec<ExpressionNode>),
    MapLiteral(Vec<(ExpressionNode, ExpressionNode)>),

    // Control flow expressions
    If {
        cond: Box<ExpressionNode>,
        then_expr: Box<ExpressionNode>,
        else_expr: Box<ExpressionNode>,
    },
    Match {
        expr: Box<ExpressionNode>,
        arms: Vec<MatchArm>,
    },
    Block(Vec<StatementNode>),

    // Functions and closures
    Lambda {
        params: Vec<Param>,
        body: Vec<StatementNode>,
    },
}

#[derive(Debug, Clone)]
pub struct ExpressionNode {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl_spanned!(ExpressionNode);

#[derive(Debug, Clone)]
pub enum TypeKind {
    // Primitive types
    Primitive(PrimitiveType),

    // Named types (including user-defined)
    Named(String, Vec<TypeNode>),

    // Function types
    Function {
        params: Vec<TypeNode>,
        returns: Box<TypeNode>,
    },

    // Trait objects
    TraitObject(Vec<TraitBound>),

    // Type variables for generics
    TypeVar(String),

    // Auto type for type inference
    Auto,
}

#[derive(Debug, Clone)]
pub struct TypeNode {
    pub kind: TypeKind,
    pub span: Span,
}

impl_spanned!(TypeNode);

#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
    Str,
    Void,
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
                "void" => Ok(PrimitiveType::Void),
                _ => Err(ParserError::new(
                    format!("Unknown primitive type: {}", value),
                    token.span,
                )),
            },
            _ => Err(ParserError::new(
                format!("Expected an identifier, found {:?}", token.token_type),
                token.span,
            )),
        }
    }
}

// Literal conversions
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

impl From<LiteralNode> for ExpressionNode {
    fn from(lit: LiteralNode) -> Self {
        ExpressionNode {
            kind: ExpressionKind::Literal(lit),
            span: Span::new(0, 0),
        }
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

impl From<PrimitiveType> for TypeNode {
    fn from(prim: PrimitiveType) -> Self {
        TypeNode {
            kind: TypeKind::Primitive(prim),
            span: Span::new(0, 0),
        }
    }
}

impl From<&str> for TypeNode {
    fn from(s: &str) -> Self {
        TypeNode {
            kind: TypeKind::Named(s.to_string(), Vec::new()),
            span: Span::new(0, 0),
        }
    }
}

impl From<ExpressionNode> for StatementNode {
    fn from(expr: ExpressionNode) -> Self {
        StatementNode {
            kind: StatementKind::Expression(expr),
            span: Span::new(0, 0),
        }
    }
}

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
    pub span: Span,
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

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Logical
    LogicalAnd,
    LogicalOr,

    // Assignment
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

impl BinaryOp {
    pub fn parse(token: Token) -> ParserResult<BinaryOp> {
        match token.token_type {
            // Arithmetic operators
            TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::Minus => Ok(BinaryOp::Subtract),
            TokenType::Star => Ok(BinaryOp::Multiply),
            TokenType::Slash => Ok(BinaryOp::Divide),
            TokenType::Percent => Ok(BinaryOp::Modulo),

            // Comparison operators
            TokenType::Eq => Ok(BinaryOp::Assign), // Single = is assignment
            TokenType::EqEq => Ok(BinaryOp::Equal), // == is equality comparison
            TokenType::NotEq => Ok(BinaryOp::NotEqual),
            TokenType::Lt => Ok(BinaryOp::Less),
            TokenType::Le => Ok(BinaryOp::LessEqual),
            TokenType::Gt => Ok(BinaryOp::Greater),
            TokenType::Ge => Ok(BinaryOp::GreaterEqual),

            // Logical operators
            TokenType::And => Ok(BinaryOp::LogicalAnd),
            TokenType::Or => Ok(BinaryOp::LogicalOr),

            // Assignment operators
            TokenType::PlusEq => Ok(BinaryOp::AddAssign),
            TokenType::MinusEq => Ok(BinaryOp::SubtractAssign),
            TokenType::StarEq => Ok(BinaryOp::MultiplyAssign),
            TokenType::SlashEq => Ok(BinaryOp::DivideAssign),

            _ => Err(ParserError {
                message: format!(
                    "Unexpected token type for binary operation: {:?}",
                    token.token_type
                ),
                span: token.span,
            }),
        }
    }

    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            BinaryOp::Assign
                | BinaryOp::AddAssign
                | BinaryOp::SubtractAssign
                | BinaryOp::MultiplyAssign
                | BinaryOp::DivideAssign
                | BinaryOp::ModuloAssign
        )
    }

    /// Check if this operator is right-associative
    pub fn is_right_associative(&self) -> bool {
        // Assignment operators are right-associative: a = b = c is a = (b = c)
        self.is_assignment()
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
