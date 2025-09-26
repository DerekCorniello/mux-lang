use crate::lexer::{Span, Token, TokenType};
use ordered_float::OrderedFloat;
use std::fmt;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<&'a Token>,
    current: usize,
    pub errors: Vec<ParserError>,
    in_function_body: bool,
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
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter()
                .filter(|&token| !matches!(token.token_type, TokenType::LineComment(_) | TokenType::MultilineComment(_)))
                .collect(),
            current: 0,
            errors: Vec::new(),
            in_function_body: false,
        }
    }
    
    fn error(&mut self, message: impl Into<String>, span: Span) -> ParserError {
        let error = ParserError::new(message, span);
        self.errors.push(error.clone());
        error
    }
    
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    
    fn previous(&self) -> &Token {
        self.tokens[(self.current - 1).max(0)]
    }

    fn check_statement_termination(&self) -> ParserResult<()> {
        if self.is_at_end() {
            return Ok(());
        }

        let token = &self.tokens[self.current];
        
        if matches!(token.token_type, TokenType::CloseBrace | TokenType::Eof) {
            return Ok(());
        }
        
        // Check if the next token is a newline or we're at the end of the line
        if (self.current + 1 < self.tokens.len() && 
            matches!(self.tokens[self.current + 1].token_type, TokenType::NewLine)) ||
           (self.current > 0 && 
            self.tokens[self.current].span.row_start > self.tokens[self.current - 1].span.row_start) {
            return Ok(());
        }
        
        // Allow statements to be followed by a closing brace without a newline
        let next_token = if self.current + 1 < self.tokens.len() {
            &self.tokens[self.current + 1]
        } else {
            return Ok(());
        };
        
        if matches!(next_token.token_type, TokenType::CloseBrace) {
            return Ok(());
        }
        
        // Allow statements to be followed by certain tokens without newlines
        if matches!(token.token_type, 
            TokenType::CloseBrace | TokenType::CloseParen | TokenType::CloseBracket |
            TokenType::Comma | TokenType::Colon) {
            return Ok(());
        }
        
        if matches!(token.token_type, 
            TokenType::Auto | TokenType::Const | TokenType::Func | 
            TokenType::Class | TokenType::Interface | TokenType::Enum) {
            
            Err(ParserError::new(
                "missing newline before statement".to_string(),
                token.span,
            ))
        } else {
            // Only require newline if we're not at the end of a block
            if self.current + 1 < self.tokens.len() && 
               !matches!(self.tokens[self.current + 1].token_type, TokenType::CloseBrace | TokenType::Eof) {
                Err(ParserError::new(
                    "expected newline after statement".to_string(),
                    self.tokens[self.current - 1].span,
                ))
            } else {
                Ok(())
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>, (Vec<AstNode>, Vec<ParserError>)> {
        let mut nodes = Vec::new();
        let mut errors = Vec::new();
        
        let mut last_position = self.current;
        
        while !self.is_at_end() {
            while self.matches(&[TokenType::NewLine]) {}
            
            if self.is_at_end() {
                break;
            }
            
            let start_position = self.current;
            
            match self.declaration() {
                Ok(Some(decl)) => {
                    nodes.push(decl);
                    
                    if let Err(e) = self.check_statement_termination() {
                        errors.push(e);
                        self.synchronize();
                    } else {
                        while self.matches(&[TokenType::NewLine]) {}
                    }
                    
                    last_position = self.current;
                }
                Ok(None) => { 
                    if self.current == start_position {
                        self.advance();
                    }
                }
                Err(e) => {
                    errors.push(e);
                    
                    self.synchronize();
                    
                    if self.current == start_position {
                        self.advance();
                    }
                }
            }
            
            if self.current == last_position && !self.is_at_end() {
                self.advance();
            }
            
            last_position = self.current;
        }
        
        let mut all_errors = std::mem::take(&mut self.errors);
        all_errors.extend(errors);
        
        if all_errors.is_empty() {
            Ok(nodes)
        } else {
            Err((nodes, all_errors))
        }
    }

    fn declaration(&mut self) -> ParserResult<Option<AstNode>> {
        while self.matches(&[TokenType::NewLine]) {}
        
        if self.is_at_end() {
            return Ok(None);
        }
        
        let result = if self.check(TokenType::Auto) {
            match self.auto_declaration() {
                Ok(node) => Ok(Some(node)),
                Err(e) if matches!(e.message.as_str(), "must be terminated with a newline" | "expected newline after statement") => {
                    self.error(e.message, e.span);
                    self.synchronize();
                    Ok(None)
                }
                Err(e) => Err(e),
            }
        } else if self.check(TokenType::Const) {
            self.const_declaration().map(Some)
        } else if self.check(TokenType::Func) {
            self.function_declaration().map(Some)
        } else if let TokenType::Id(_) = &self.peek().token_type {
            let start = self.current;
            let _ = self.consume();
            
            if let TokenType::Id(_) = &self.peek().token_type {
                let next = self.current + 1;
                if next < self.tokens.len() && self.tokens[next].token_type == TokenType::Eq {
                    self.current = start;
                    match self.typed_declaration() {
                        Ok(node) => Ok(Some(node)),
                        Err(e) if matches!(e.message.as_str(), "must be terminated with a newline" | "expected newline after statement") => {
                            self.error(e.message, e.span);
                            self.synchronize();
                            Ok(None)
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    self.current = start;
                    self.statement().map(Some)
                }
            } else {
                self.current = start;
                self.statement().map(Some)
            }
        } else if self.check(TokenType::Class) {
            self.class_declaration().map(Some)
        } else if self.check(TokenType::Interface) {
            self.interface_declaration().map(Some)
        } else if self.check(TokenType::Enum) {
            self.enum_declaration().map(Some)
        } else if self.check(TokenType::Import) {
            self.import_declaration().map(Some)
        } else {
            self.statement().map(Some)
        };
        
        while self.matches(&[TokenType::NewLine]) {}
        
        if let Err(ref e) = result {
            if matches!(e.message.as_str(), "must be terminated with a newline" | "expected newline after statement") {
                self.error(e.message.clone(), e.span);
                self.synchronize();
                if !self.is_at_end() {
                    return self.declaration();
                }
                return Ok(None);
            }
        }
        
        result
    }

    fn typed_declaration(&mut self) -> ParserResult<AstNode> {
        let type_node = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name after type")?;

        self.consume_token(TokenType::Eq, "Expected '=' after variable name")?;
        let value = self.parse_expression()?;
        
        if !self.is_at_end() && 
           !matches!(self.peek().token_type, TokenType::NewLine | TokenType::CloseBrace | TokenType::Eof) {
            return Err(ParserError::new(
                "expected newline after statement".to_string(),
                self.peek().span
            ));
        }
        
        let span = type_node.span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::TypedDecl(name, type_node, value),
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
        
        self.matches(&[TokenType::NewLine]);
        
        let span = start_span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::AutoDecl(name, type_node, value),
            span,
        }))
    }

    fn const_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.peek().span;
        self.current += 1;
 
        let type_node = self.parse_type()?;
        let name = self.consume_identifier("Expected constant name after type")?;

        self.consume_token(TokenType::Eq, "Expected '=' after constant name")?;
        let value = self.parse_expression()?;
        let span = start_span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::ConstDecl(name, type_node, value),
            span,
        }))
    }

    fn class_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
        self.consume_token(TokenType::Class, "Expected 'class' keyword")?;

        let name = self.consume_identifier("Expected class name")?;

        let type_params = if self.matches(&[TokenType::OpenBracket]) {
            let mut params = Vec::new();
            
            // Require at least one type parameter
            loop {
                let param = self.consume_identifier("Expected type parameter name")?;
                let mut bounds = Vec::new();

                if self.matches(&[TokenType::Is]) {
                    loop {
                        let bound_name = self.consume_identifier("Expected trait name in bound")?;
                        let type_args = if self.matches(&[TokenType::OpenBracket]) {
                            self.parse_type_arguments()?
                        } else {
                            Vec::new()
                        };

                        bounds.push(TraitBound {
                            name: bound_name,
                            type_params: type_args,
                        });

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                params.push((param, bounds));

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
            
            self.consume_token(TokenType::CloseBracket, "Expected ']' after type parameters")?;
            params
        } else {
            Vec::new()
        };

        let traits = if self.matches(&[TokenType::Is]) {
            let mut traits_list = Vec::new();
            loop {
                let trait_name = self.consume_identifier("Expected trait name")?;
                let type_args = if self.matches(&[TokenType::OpenBracket]) {
                    let args = self.parse_type_arguments()?;
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after type arguments")?;
                    args
                } else {
                    Vec::new()
                };

                traits_list.push(TraitRef {
                    name: trait_name,
                    type_args,
                });

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
            traits_list
        } else {
            Vec::new()
        };

        self.consume_token(TokenType::OpenBrace, "Expected '{' after class header")?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            match self.peek().token_type {
                TokenType::Func => {
                    let func_node = self.function_declaration()?;
                    match func_node {
                        AstNode::Function(func) => methods.push(func),
                        _ => return Err(ParserError::new("Expected function in class", start_span)),
                    }
                }
                _ => {
                    let field_type = self.parse_type()?;
                    let field_name = self.consume_identifier("Expected field name")?;
                    fields.push(Field {
                        name: field_name,
                        type_: field_type,
                    });
                }
            }
        }

        let _end_span = self.consume_token(TokenType::CloseBrace, "Expected '}' after class body")?;

        Ok(AstNode::Class {
            name,
            type_params,
            traits,
            fields,
            methods,
        })
    }

    fn interface_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
        self.consume_token(TokenType::Interface, "Expected 'interface' keyword")?;

        let name = self.consume_identifier("Expected interface name")?;

        let type_params = if self.matches(&[TokenType::Lt]) {
            let mut params = Vec::new();
            if !self.check(TokenType::Gt) {
                loop {
                    let param = self.consume_identifier("Expected type parameter name")?;
                    let mut bounds = Vec::new();

                    if self.matches(&[TokenType::Colon]) {
                        loop {
                            let bound_name = self.consume_identifier("Expected trait name in bound")?;
                            let type_args = if self.matches(&[TokenType::Lt]) {
                                let args = self.parse_type_arguments()?;
                                self.consume_token(TokenType::Gt, "Expected '>' after type arguments")?;
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

        self.consume_token(TokenType::OpenBrace, "Expected '{' after interface header")?;

        let mut methods = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            if self.matches(&[TokenType::Func]) {
                let func_node = self.function_declaration()?;
                match func_node {
                    AstNode::Function(func) => methods.push(func),
                    _ => return Err(ParserError::new("Expected function in interface", start_span)),
                }
            } else {
                return Err(ParserError::new("Expected function declaration in interface", self.peek().span));
            }
        }

        // need to consume this and throw error if not brace, but still need to throw if its a
        // problem
        let _ = self.consume_token(TokenType::CloseBrace, "Expected '}' after interface body")?;

        Ok(AstNode::Interface {
            name,
            type_params,
            methods,
        })
    }

    fn enum_declaration(&mut self) -> ParserResult<AstNode> {
        self.consume_token(TokenType::Enum, "Expected 'enum' keyword")?;
        let name = self.consume_identifier("Expected enum name")?;
        let type_params = if self.matches(&[TokenType::Lt]) {
            let mut params = Vec::new();
            if !self.check(TokenType::Gt) {
                loop {
                    let param = self.consume_identifier("Expected type parameter name")?;
                    let mut bounds = Vec::new();

                    if self.matches(&[TokenType::Colon]) {
                        loop {
                            let bound_name = self.consume_identifier("Expected trait name in bound")?;
                            let type_args = if self.matches(&[TokenType::Lt]) {
                                let args = self.parse_type_arguments()?;
                                self.consume_token(TokenType::Gt, "Expected '>' after type arguments")?;
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

        self.consume_token(TokenType::OpenBrace, "Expected '{' after enum header")?;

        let mut variants = Vec::new();

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let variant_name = self.consume_identifier("Expected variant name")?;

            let data = if self.matches(&[TokenType::OpenParen]) {
                let mut types = Vec::new();
                if !self.check(TokenType::CloseParen) {
                    loop {
                        types.push(self.parse_type()?);
                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
                self.consume_token(TokenType::CloseParen, "Expected ')' after variant data")?;
                Some(types)
            } else {
                None
            };

            variants.push(EnumVariant {
                name: variant_name,
                data,
            });

            if self.matches(&[TokenType::Comma]) {
                if self.check(TokenType::CloseBrace) {
                    break;
                }
            } else {
                break;
            }
        }

        let _end_span = self.consume_token(TokenType::CloseBrace, "Expected '}' after enum variants")?;

        Ok(AstNode::Enum {
            name,
            type_params,
            variants,
        })
    }

    fn import_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;

        let module_path = self.consume_identifier("Expected module path after 'import'")?;

        let alias = if self.matches(&[TokenType::As]) {
            Some(self.consume_identifier("Expected alias after 'as'")?)
        } else {
            None
        };

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Import { module_path, alias },
            span: start_span,
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
                let param_type = if !self.is_at_end() && matches!(&self.tokens[self.current].token_type, TokenType::Id(name) if name == "auto") {
                    let token = self.consume();
                    TypeNode {
                        kind: TypeKind::Primitive(PrimitiveType::Auto),
                        span: token.span,
                    }
                } else {
                    let type_token = self.consume();
                    let type_name = match &type_token.token_type {
                        TokenType::Id(name) => name.clone(),
                        _ => return Err(ParserError::from_token("Expected type name", type_token)),
                    };
                    
                    if let Ok(prim_type) = PrimitiveType::parse(type_token.clone()) {
                        TypeNode {
                            kind: TypeKind::Primitive(prim_type),
                            span: type_token.span,
                        }
                    } else {
                        TypeNode {
                            kind: TypeKind::Named(type_name, Vec::new()),
                            span: type_token.span,
                        }
                    }
                };
                
                let param_name = self.consume_identifier("Expected parameter name")?;

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
        let result = if self.matches(&[TokenType::If]) {
            self.if_statement()
        } else if self.matches(&[TokenType::While]) {
            self.while_statement()
        } else if self.matches(&[TokenType::For]) {
            self.for_statement()
        } else if self.matches(&[TokenType::Match]) {
            self.match_statement()
        } else if self.matches(&[TokenType::Break]) {
            self.break_statement()
        } else if self.matches(&[TokenType::Continue]) {
            self.continue_statement()
        } else if self.matches(&[TokenType::Return]) {
            self.return_statement()
        } else if self.matches(&[TokenType::OpenBrace]) {
            self.block_statement()
        } else {
            self.expression_statement()
        }?;
        
        if let Err(e) = self.check_statement_termination() {
            self.error(e.message, e.span);
        }
        
        Ok(result)
    }

    fn if_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
        let condition = self.parse_expression()?;
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
        let start_span = self.tokens[self.current].span;

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
        let start_span = self.tokens[self.current].span;

        self.consume_token(TokenType::OpenParen, "Expected '(' after 'for'")?;

        // Check if this is a traditional for loop (type var = init; condition; increment)
        if let Some(next_token) = self.tokens.get(self.current) {
            if matches!(next_token.token_type, TokenType::Id(_)) {
                // Check if next token is a type name
                let type_name = match &next_token.token_type {
                    TokenType::Id(name) => name.clone(),
                    _ => return Err(ParserError::new("Expected type name in for loop", next_token.span)),
                };

                if type_name == "int" || type_name == "float" || type_name == "bool" || type_name == "char" || type_name == "str" {
                    // Traditional for loop: for int i = 0; i < 10; i += 1
                    self.current += 1; // consume type
                    let var = self.consume_identifier("Expected variable name after type")?;
                    self.consume_token(TokenType::Eq, "Expected '=' after variable name")?;
                    let init = self.parse_expression()?;
                    let condition = self.parse_expression()?;
                    let increment = self.parse_expression()?;
                    self.consume_token(TokenType::CloseParen, "Expected ')' after increment")?;

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
                            var: var.clone(),
                            iter: ExpressionNode {
                                kind: ExpressionKind::Binary {
                                    left: Box::new(init),
                                    op: BinaryOp::Assign,
                                    right: Box::new(ExpressionNode {
                                        kind: ExpressionKind::Binary {
                                            left: Box::new(ExpressionNode {
                                                kind: ExpressionKind::Binary {
                                                    left: Box::new(ExpressionNode {
                                                        kind: ExpressionKind::Identifier(var.clone()),
                                                        span: start_span,
                                                    }),
                                                    op: BinaryOp::Less,
                                                    right: Box::new(condition),
                                                },
                                                span: start_span,
                                            }),
                                            op: BinaryOp::Assign,
                                            right: Box::new(ExpressionNode {
                                                kind: ExpressionKind::Binary {
                                                    left: Box::new(ExpressionNode {
                                                        kind: ExpressionKind::Identifier(var.clone()),
                                                        span: start_span,
                                                    }),
                                                    op: BinaryOp::AddAssign,
                                                    right: Box::new(increment),
                                                },
                                                span: start_span,
                                            }),
                                        },
                                        span: start_span,
                                    }),
                                },
                                span: start_span,
                            },
                            body: body_statements,
                        },
                        span,
                    }))
                } else {
                    // For-in loop: for item in collection
                    let var = self.consume_identifier("Expected variable name")?;
                    self.consume_token(TokenType::In, "Expected 'in' after variable")?;
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
            } else {
                // For-in loop: for item in collection
                let var = self.consume_identifier("Expected variable name")?;
                self.consume_token(TokenType::In, "Expected 'in' after variable")?;
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
        } else {
            Err(ParserError::new("Unexpected end of input in for loop", start_span))
        }
    }

    fn match_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;

        self.consume_token(TokenType::OpenParen, "Expected '(' after 'match'")?;
        let expr = self.parse_expression()?;
        self.consume_token(TokenType::CloseParen, "Expected ')' after match expression")?;

        self.consume_token(TokenType::OpenBrace, "Expected '{' after match expression")?;

        let mut arms = Vec::new();
        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            // Parse pattern
            let pattern = self.parse_pattern()?;

            // Parse optional guard (if condition)
            let guard = if self.matches(&[TokenType::If]) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let body = self.block_statement()?;
            let body_statements = match body {
                AstNode::Statement(stmt) => match stmt.kind {
                    StatementKind::Block(block) => block,
                    _ => vec![stmt],
                },
                _ => {
                    return Err(ParserError::new(
                        "Expected block statement for match arm body",
                        start_span,
                    ));
                }
            };

            arms.push(MatchArm { pattern, guard, body: body_statements });

            if self.matches(&[TokenType::Comma]) && self.check(TokenType::CloseBrace){
                    break; // Trailing comma
            }
        }

        let end_span = self.consume_token(TokenType::CloseBrace, "Expected '}' after match arms")?;

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Match { expr, arms },
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_pattern(&mut self) -> ParserResult<PatternNode> {
        match &self.peek().token_type {
            TokenType::Str(name) => {
                let name_clone = name.clone();
                self.current += 1; // consume the identifier

                // Check if this is an enum variant pattern (e.g., Some, None, Ok, Err)
                match name_clone.as_str() {
                    "Some" | "None" | "Ok" | "Err" => {
                        // Parse enum variant pattern
                        if self.matches(&[TokenType::OpenParen]) {
                            // Has arguments
                            let mut args = Vec::new();
                            if !self.check(TokenType::CloseParen) {
                                loop {
                                    args.push(self.parse_pattern()?);
                                    if !self.matches(&[TokenType::Comma]) {
                                        break;
                                    }
                                }
                            }
                            self.consume_token(TokenType::CloseParen, "Expected ')' after enum variant arguments")?;
                            Ok(PatternNode::EnumVariant { name: name_clone, args })
                        } else {
                            // No arguments
                            Ok(PatternNode::EnumVariant { name: name_clone, args: Vec::new() })
                        }
                    }
                    _ => {
                        // Regular identifier pattern
                        Ok(PatternNode::Identifier(name_clone))
                    }
                }
            }
            TokenType::Underscore => {
                self.current += 1; // consume the underscore
                Ok(PatternNode::Wildcard)
            }
            TokenType::OpenParen => {
                // Tuple pattern
                self.current += 1; // consume the opening paren
                let mut elements = Vec::new();

                if !self.check(TokenType::CloseParen) {
                    loop {
                        elements.push(self.parse_pattern()?);
                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                self.consume_token(TokenType::CloseParen, "Expected ')' after tuple pattern")?;
                Ok(PatternNode::Tuple(elements))
            }
            _ => {
                // Try to parse as literal
                let token = self.consume();
                match &token.token_type {
                    TokenType::Int(n) => Ok(PatternNode::Literal(LiteralNode::Integer(*n))),
                    TokenType::Float(f) => Ok(PatternNode::Literal(LiteralNode::Float(*f))),
                    TokenType::Bool(b) => Ok(PatternNode::Literal(LiteralNode::Boolean(*b))),
                    TokenType::Char(c) => Ok(PatternNode::Literal(LiteralNode::Char(*c))),
                    TokenType::Str(s) => Ok(PatternNode::Literal(LiteralNode::String(s.clone()))),
                    _ => Err(ParserError::from_token("Expected pattern", token)),
                }
            }
        }
    }

    fn return_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;

        let value = if !self.check_next_line() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end_span = value.as_ref().map_or(start_span, |v| v.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Return(value),
            span: start_span.combine(&end_span),
        }))
    }

    fn break_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Break,
            span: start_span,
        }))
    }

    fn continue_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Continue,
            span: start_span,
        }))
    }

    fn skip_newlines(&mut self) {
        while self.matches(&[TokenType::NewLine]) {}
    }

    fn block_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
        let mut statements = Vec::new();
        self.skip_newlines();
        
        let in_function_body = self.in_function_body;
        self.in_function_body = true;
        
        let initial_error_count = self.errors.len();
        
        while !self.is_at_end() {
            self.skip_newlines();
            
            // Check for closing brace
            if self.check(TokenType::CloseBrace) {
                break;
            }
            
            let start_pos = self.current;
            
            match self.declaration() {
                Ok(Some(decl)) => {
                    statements.push(decl);
                    
                    // Only check statement termination if we're not at the end of the block
                    if !in_function_body && !self.check(TokenType::CloseBrace) {
                        if let Err(e) = self.check_statement_termination() {
                            self.error(e.message, e.span);
                        } else {
                            while self.matches(&[TokenType::NewLine]) {}
                        }
                    } else {
                        while self.matches(&[TokenType::NewLine]) {}
                    }
                },
                Ok(None) => {
                    if self.current == start_pos {
                        self.advance();
                    }
                },
                Err(e) => {
                    if !e.message.contains("newline") || self.errors.len() == initial_error_count {
                        self.error(e.message, e.span);
                    }
                    
                    self.synchronize();
                    
                    if self.current == start_pos && !self.check(TokenType::CloseBrace) {
                        self.advance();
                    }
                }
            }
            
            // Break if we've consumed all tokens
            if self.is_at_end() {
                break;
            }
        }
        
        self.in_function_body = in_function_body;
        
        // Consume the closing brace if it exists
        if self.check(TokenType::CloseBrace) {
            self.consume_token(TokenType::CloseBrace, "Expected '}' after block")?;
        } else if !self.is_at_end() {
            return Err(ParserError::new(
                "Expected '}' after block".to_string(),
                self.peek().span,
            ));
        }

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
        
        if !self.in_function_body {
            self.check_statement_termination()?;
        }

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Expression(expr),
            span: self.tokens[self.current - 1].span,
        }))
    }

    fn parse_type(&mut self) -> ParserResult<TypeNode> {
        // Handle reference type: &T
        if self.matches(&[TokenType::Ampersand]) {
            let start_span = self.previous().span;
            let referenced_type = self.parse_type()?;
            return Ok(TypeNode {
                kind: TypeKind::Reference(Box::new(referenced_type)),
                span: Span {
                    row_start: start_span.row_start,
                    col_start: start_span.col_start,
                    row_end: self.previous().span.row_end,
                    col_end: self.previous().span.col_end,
                },
            });
        }
        
        // we are essentially doing a consume here, but without borrowing th
        // parser again so we dont have to clone it
        let token = &self.tokens[self.current];
        let start_span = token.span;
        self.current += 1;

        match token.token_type {
            TokenType::Id(ref name) => {
                if let Ok(prim_type) = PrimitiveType::parse(token.to_owned().clone()) {
                    return Ok(TypeNode {
                        kind: TypeKind::Primitive(prim_type),
                        span: start_span,
                    });
                }
                
                let type_args = if self.matches(&[TokenType::OpenBracket]) {
                    let args = self.parse_type_arguments()?;
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after type arguments")?;
                    args
                } else {
                    Vec::new()
                };

                if name == "dyn" && !type_args.is_empty() {
                    return Ok(TypeNode {
                        kind: TypeKind::TraitObject(Box::new(type_args[0].clone())),
                        span: start_span,
                    });
                }

                let next_is_bracket = !self.is_at_end() && 
                    self.tokens.get(self.current).is_some_and(|t| 
                        t.token_type == TokenType::OpenBracket
                    );
                
                if name == "list" && next_is_bracket {
                    self.consume_token(TokenType::OpenBracket, "Expected '[' for list element type")?;
                    let element_type = self.parse_type()?;
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after list element type")?;
                    return Ok(TypeNode {
                        kind: TypeKind::List(Box::new(element_type)),
                        span: start_span,
                    });
                }
                
                if name == "map" && next_is_bracket {
                    self.current += 1; // Consume the '['
                    let key_type = self.parse_type()?;
                    self.consume_token(TokenType::Comma, "Expected ',' between key and value types in map")?;
                    let value_type = self.parse_type()?;
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after map value type")?;
                    
                    return Ok(TypeNode {
                        kind: TypeKind::Map(Box::new(key_type), Box::new(value_type)),
                        span: start_span,
                    });
                }
                
                if name == "tuple" && next_is_bracket {
                    self.current += 1; // Consume the '['
                    let mut element_types = Vec::new();
                    element_types.push(self.parse_type()?);
                    while self.matches(&[TokenType::Comma]) {
                        element_types.push(self.parse_type()?);
                    }
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after tuple element types")?;
                    
                    return Ok(TypeNode {
                        kind: TypeKind::Tuple(element_types),
                        span: start_span,
                    });
                }

                // handle named types with generic parameters
                let name_clone = name.clone();
                let type_args = if self.matches(&[TokenType::OpenBracket]) {
                    let args = self.parse_type_arguments()?;
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after type arguments")?;
                    args
                } else {
                    Vec::new()
                };

                Ok(TypeNode {
                    kind: TypeKind::Named(name_clone, type_args),
                    span: start_span,
                })
            },
            
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
            },
            
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
        if !self.is_at_end() {
            self.consume();
        }
        
        while !self.is_at_end() {
            if self.matches(&[TokenType::NewLine]) {
                return;
            }
            
            match self.peek().token_type {
                TokenType::Func
                | TokenType::Auto
                | TokenType::Const
                | TokenType::Class
                | TokenType::Interface
                | TokenType::Enum
                | TokenType::Import => {
                    return;
                }
                TokenType::If
                | TokenType::Else
                | TokenType::While
                | TokenType::For
                | TokenType::Match
                | TokenType::Return => {
                    return;
                }
                TokenType::OpenBrace
                | TokenType::CloseBrace => {
                    return;
                }
                _ => {
                    self.consume();
                }
            }
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        self.tokens[self.current]
    }

    fn consume(&mut self) -> &Token {
        let ret = &self.tokens[self.current];
        self.current += 1;
        ret
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
            
            TokenType::Float(f) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Literal(LiteralNode::Float(f)),
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            // Identifier expressions
            TokenType::Id(name) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Identifier(name),
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            // List literals
            TokenType::OpenBracket => {
                let start_span = token_span;
                let mut elements = Vec::new();

                if !self.check(TokenType::CloseBracket) {
                    loop {
                        elements.push(self.parse_expression()?);

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                let end_span =
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after list elements")?;
                let expr = ExpressionNode {
                    kind: ExpressionKind::ListLiteral(elements),
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
                        let key = self.parse_expression()?;
                        self.consume_token(TokenType::Colon, "Expected ':' after map key")?;
                        let value = self.parse_expression()?;
                        entries.push((key, value));

                        if !self.matches(&[TokenType::Comma]) {
                            break;
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

            // Lambda expressions
            TokenType::Func => {
                let start_span = token_span;
                self.current += 1;

                self.consume_token(TokenType::OpenParen, "Expected '(' after 'func' in lambda")?;
                let mut params = Vec::new();

                if !self.check(TokenType::CloseParen) {
                    loop {
                        let param_name = self.consume_identifier("Expected parameter name")?;
                        self.consume_token(TokenType::Colon, "Expected ':' after parameter name")?;
                        let param_type = self.parse_type()?;
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

                let _return_type = if self.matches(&[TokenType::Returns]) {
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
                            "Expected block statement for lambda body",
                            start_span,
                        ));
                    }
                };

                let end_span = body_statements.last().map(|s| s.span).unwrap_or(start_span);
                let expr = ExpressionNode {
                    kind: ExpressionKind::Lambda {
                        params,
                        body: body_statements,
                    },
                    span: start_span.combine(&end_span),
                };

                self.parse_postfix_operators(expr)
            }

            // If expressions
            TokenType::If => {
                let start_span = token_span;

                // Parse condition
                let cond = self.parse_expression()?;

                // Parse then branch
                self.consume_token(TokenType::OpenBrace, "Expected '{' after if condition")?;
                let then_expr = self.parse_expression()?;
                self.consume_token(TokenType::CloseBrace, "Expected '}' after then expression")?;

                // Parse else branch
                self.consume_token(TokenType::Else, "Expected 'else' after then branch")?;
                self.consume_token(TokenType::OpenBrace, "Expected '{' after else")?;
                let else_expr = self.parse_expression()?;
                self.consume_token(TokenType::CloseBrace, "Expected '}' after else expression")?;

                let expr = ExpressionNode {
                    kind: ExpressionKind::If {
                        cond: Box::new(cond),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    },
                    span: start_span.combine(&self.previous().span),
                };

                self.parse_postfix_operators(expr)
            }
            
            _ => Err(ParserError::from_token(
                format!("Expected expression, got {:?}", token_type),
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
                let index = self.parse_expression()?;
                let end_span =
                    self.consume_token(TokenType::CloseBracket, "Expected ']' after index")?;
                let expr_span = *expr.span();
                expr = ExpressionNode {
                    kind: ExpressionKind::ListAccess {
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
            TokenType::Id(name)=> {
                let name_clone = name.clone();
                self.current += 1;
                Ok(name_clone)
            }
            _ => Err(ParserError::new(error_msg, self.peek().span)),
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
            TokenType::PercentEq => Some(BinaryOp::ModuloAssign),
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
            BinaryOp::Assign
            | BinaryOp::AddAssign
            | BinaryOp::SubtractAssign
            | BinaryOp::MultiplyAssign
            | BinaryOp::DivideAssign
            | BinaryOp::ModuloAssign => Precedence::Assignment,

            BinaryOp::LogicalOr => Precedence::Or,

            BinaryOp::LogicalAnd => Precedence::And,

            BinaryOp::Equal | BinaryOp::NotEqual => Precedence::Equality,

            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                Precedence::Comparison
            }

            BinaryOp::Add | BinaryOp::Subtract => Precedence::Term,

            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => Precedence::Factor,
        };
        Ok(precedence)
    }
}

impl std::error::Error for ParserError {}

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Clone, PartialEq)]
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
}

impl SpanExt for Span {
    fn combine(&self, other: &Span) -> Span {
        let mut span = *self;
        if let (Some(end_row), Some(end_col)) = (other.row_end, other.col_end) {
            span.complete(end_row, end_col);
        }
        span
    }
}

pub trait Spanned {
    fn span(&self) -> &Span;
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
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
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
    Statement(StatementNode),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstDeclNode {
    pub name: String,
    pub type_: TypeNode,
    pub value: ExpressionNode,
    pub span: Span,
}

impl_spanned!(ConstDeclNode);

impl AstNode {
    pub fn into_statement(self) -> Option<StatementNode> {
        match self {
            AstNode::Statement(stmt) => Some(stmt),
            AstNode::Function(func) => {
                let span = func.span;
                Some(StatementNode {
                    kind: StatementKind::Function(func),
                    span,
                })
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    AutoDecl(String, TypeNode, ExpressionNode),
    TypedDecl(String, TypeNode, ExpressionNode),
    ConstDecl(String, TypeNode, ExpressionNode),
    Function(FunctionNode),
    Import {
        module_path: String,
        alias: Option<String>,
    },
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
    Match {
        expr: ExpressionNode,
        arms: Vec<MatchArm>,
    },
    Break,
    Continue,
    Expression(ExpressionNode),
    Block(Vec<StatementNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatementNode {
    pub kind: StatementKind,
    pub span: Span,
}

impl_spanned!(StatementNode);

#[derive(Debug, Clone, PartialEq)]
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
    ListAccess {
        expr: Box<ExpressionNode>,
        index: Box<ExpressionNode>,
    },
    ListLiteral(Vec<ExpressionNode>),
    MapLiteral(Vec<(ExpressionNode, ExpressionNode)>),
    If {
        cond: Box<ExpressionNode>,
        then_expr: Box<ExpressionNode>,
        else_expr: Box<ExpressionNode>,
    },
    Lambda {
        params: Vec<Param>,
        body: Vec<StatementNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNode {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl_spanned!(ExpressionNode);

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    Primitive(PrimitiveType),
    Named(String, Vec<TypeNode>),
    TraitObject(Box<TypeNode>),
    Function {
        params: Vec<TypeNode>,
        returns: Box<TypeNode>,
    },
    Reference(Box<TypeNode>),
    List(Box<TypeNode>),
    Map(Box<TypeNode>, Box<TypeNode>),
    Tuple(Vec<TypeNode>),
    Auto,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeNode {
    pub kind: TypeKind,
    pub span: Span,
}

impl_spanned!(TypeNode);

#[derive(Debug, Clone, PartialEq)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
    Str,
    Void,
    Auto,
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
                "auto" => Ok(PrimitiveType::Auto),
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

#[derive(Debug, Clone, PartialEq)]
pub struct TraitBound {
    pub name: String,
    pub type_params: Vec<TypeNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralNode {
    Float(OrderedFloat<f64>),
    Integer(i64),
    String(String),
    Boolean(bool),
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub type_: TypeNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_: TypeNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionNode {
    pub name: String,
    pub type_params: Vec<(String, Vec<TraitBound>)>,
    pub params: Vec<Param>,
    pub return_type: TypeNode,
    pub body: Vec<StatementNode>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitRef {
    pub name: String,
    pub type_args: Vec<TypeNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub data: Option<Vec<TypeNode>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: PatternNode,
    pub guard: Option<ExpressionNode>,
    pub body: Vec<StatementNode>,
}

#[derive(Debug, Clone, PartialEq)]
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

    pub fn is_right_associative(&self) -> bool {
        self.is_assignment()
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Lexer, Token};
    use crate::source::Source;
    use std::rc::Rc;

    #[derive(Debug)]
    struct TestParser {
        parser: Parser<'static>,
    }

    impl TestParser {
        fn new(source: &str) -> Self {
            let mut src = Source::from_test_str(source);
            let tokens = collect_tokens(&mut src);
            
            let tokens_rc = Rc::new(tokens);
            
            let tokens_ptr = Rc::into_raw(tokens_rc.clone());
            
            let tokens_ref = unsafe { &*tokens_ptr };
            
            let parser = Parser::new(tokens_ref);
            
            Self {
                parser,
            }
        }
        
        fn parse(&mut self) -> Result<Vec<AstNode>, (Vec<AstNode>, Vec<ParserError>)> {
            self.parser.parse()
        }
    }
    
    fn collect_tokens(source: &mut Source) -> Vec<Token> {
        let input = source.input.clone();
        println!("Collecting tokens from source:\n{}", input);
        
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        
        loop {
            match lexer.next_token() {
                Ok(token) => {
                    println!("Token: {:?}", token);
                    if token.token_type == TokenType::Eof {
                        break;
                    }
                    tokens.push(token);
                }
                Err(e) => {
                    panic!("Lexer error: {}", e);
                }
            }
        }
        
        println!("Collected {} tokens", tokens.len());
        tokens
    }
    
    fn create_parser(source: &str) -> TestParser {
        TestParser::new(source)
    }

    fn parse_expr(source: &str) -> ExpressionNode {
        let mut test_parser = create_parser(source);
        test_parser.parser.parse_expression().unwrap()
    }

    fn parse_stmts(source: &str) -> Vec<StatementNode> {
        let mut test_parser = create_parser(source);
        test_parser.parse()
            .unwrap()
            .into_iter()
            .map(|node| node.into_statement().unwrap())
            .collect()
    }

    #[test]
    fn test_newline_termination() {
        let stmts = parse_stmts("auto x = 1\n");
        assert_eq!(stmts.len(), 1);
        
        let source = "auto x = 1\nauto y = 2\n";
        let mut test_parser = create_parser(source);
        let result = test_parser.parse();
        
        match result {
            Ok(nodes) => {
                println!("Successfully parsed {} nodes", nodes.len());
                for (i, node) in nodes.iter().enumerate() {
                    println!("Node {}: {:?}", i, node);
                }
                assert_eq!(nodes.len(), 2);
            },
            Err((nodes, errors)) => {
                println!("Parse failed with nodes: {:?} and errors: {:?}", nodes, errors);
                panic!("Failed to parse multiple statements with newlines");
            }
        }
        
        let stmts = parse_stmts("auto x = 1\n\nauto y = 2\n");
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn test_missing_newline_error() {
        let mut test_parser = create_parser("auto x = 1 auto y = 2");
        let result = test_parser.parse();
        
        match result {
            Ok(nodes) => {
                panic!("Expected an error about missing newline, but got successful parse with nodes: {:?}", nodes);
            }
            Err((_nodes, errors)) => {
                let has_newline_error = errors.iter().any(|e| 
                    e.message.contains("expected newline after statement") ||
                    e.message.contains("missing newline") ||
                    e.message.contains("expected newline")
                );
                
                if !has_newline_error {
                    panic!("Expected an error about missing newline, but got: {:?}", errors);
                }
            }
        }
    }
    #[test]
    fn test_variable_declaration() {
        let stmts = parse_stmts("auto x = 42\n");
        assert_eq!(stmts.len(), 1);
        match &stmts[0].kind {
            StatementKind::AutoDecl(name, _, expr) => {
                assert_eq!(name, "x");
                assert!(matches!(expr.kind, ExpressionKind::Literal(_)));
            }
            _ => panic!("Expected auto variable declaration"),
        }
        
        let stmts = parse_stmts("int x = 42\n");
        assert_eq!(stmts.len(), 1);
        
        let stmts = parse_stmts("const int PI = 3.14159\n");
        assert_eq!(stmts.len(), 1);
    }
    
    #[test]
    fn test_expressions() {
        let stmts = parse_stmts("1 + 2 * 3\n");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0].kind, StatementKind::Expression { .. }));
        
        let expr = parse_expr("(1 + 2) * 3");
        assert!(matches!(expr.kind, ExpressionKind::Binary { .. }));
    }

    #[test]
    fn test_block_statements() {
        let stmts = parse_stmts("{\n  auto x = 1\n  auto y = 2\n}\n");
        assert!(!stmts.is_empty());
    }

    #[test]
    fn test_control_flow() {
        let stmts = parse_stmts("if x {\n  auto y = 1\n}\n");
        assert!(!stmts.is_empty());
    }

    #[test]
    fn test_function_declaration() {
        let mut test_parser = create_parser("fn add(int a, int b) returns int {\n  a + b\n}\n");
        let result = test_parser.parse();
        
        match result {
            Ok(nodes) => {
                assert!(!nodes.is_empty());
                
                if let AstNode::Function(func) = &nodes[0] {
                    assert_eq!(func.name, "add");
                    assert_eq!(func.params.len(), 2);
                    assert_eq!(func.return_type.kind, TypeKind::Primitive(PrimitiveType::Int));
                } else {
                    panic!("Expected a function node, got {:?}", nodes[0]);
                }
            },
            Err((nodes, errors)) => {
                if !nodes.is_empty() {
                    if let AstNode::Function(func) = &nodes[0] {
                        assert_eq!(func.name, "add");
                        assert_eq!(func.params.len(), 2);
                        assert_eq!(func.return_type.kind, TypeKind::Primitive(PrimitiveType::Int));
                        return;
                    }
                }
                panic!("Failed to parse function: {:?}", errors);
            }
        }
        
        let mut test_parser = create_parser("fn double(int x) {\n  return x * 2\n}\n");
        let result = test_parser.parse();
        
        match result {
            Ok(nodes) => {
                assert!(!nodes.is_empty(), "Expected at least one statement");
            },
            Err((nodes, errors)) => {
                if nodes.is_empty() {
                    panic!("Failed to parse function: {:?}", errors);
                }
            }
        }
        
        let mut test_parser = create_parser("fn greet(string name, int times = 1) {\n  return 0\n}\n");
        let result = test_parser.parse();
        
        match result {
            Ok(nodes) => {
                assert!(!nodes.is_empty(), "Expected at least one statement");
            },
            Err((nodes, errors)) => {
                if nodes.is_empty() {
                    panic!("Failed to parse function: {:?}", errors);
                }
            }
        }
    }
    
    #[test]
    fn test_error_recovery() {
        // Test recovery after various types of syntax errors
        let source = r#"
            let x = 5
            auto y = 10

            func foo() {
                return 42

            123abc = 99

            let z = 20 30

            auto valid1 = 100

            let result = (5 + 10

            func bar() {
                return
                auto x = 42
            }

            auto valid2 = 200

            struct BadStruct {
                field1: InvalidType
                field2: int
            }

            auto valid3 = 300
        "#;
        
        let mut parser = create_parser(source);
        let result = parser.parse();
        
        assert!(&result.is_err());
        
        match result {
            Ok(nodes) => {
                assert!(!nodes.is_empty(), "Expected some valid nodes to be parsed");
                println!("Successfully parsed {} nodes despite errors", nodes.len());
                
                assert!(nodes.len() >= 4, "Expected at least 4 valid nodes to be parsed");
                
                let node_texts: Vec<String> = nodes.iter()
                    .map(|n| format!("{:?}", n))
                    .collect();
                
                println!("Successfully parsed nodes: {:?}", node_texts);
            }
            Err((_nodes, errors)) => {
                assert!(!errors.is_empty(), "Expected at least one error");
                
                // Print all errors for debugging
                for error in errors {
                    println!("Found error: {} at {:?}", error.message, error.span);
                    
                    // Verify the error message indicates a parsing error
                    assert!(!error.message.is_empty(), "Expected a non-empty error message");
                    
                    // Check for specific error messages we expect
                    let error_msg = error.message.to_lowercase();
                    assert!(
                        error_msg.contains("expected") || 
                        error_msg.contains("missing") ||
                        error_msg.contains("unexpected"),
                        "Error message should indicate what went wrong"
                    );
                }
            }
        }

        // Test recovery in the middle of expressions
        let source = "auto x = 10 + * 5 - / 3\nauto y = 20\n";
        let mut parser = create_parser(source);
        let _ = parser.parse();
    }
}
