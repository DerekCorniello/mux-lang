use crate::lexer::{Span, Token, TokenType};
use ordered_float::OrderedFloat;
use std::fmt;

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: Vec<&'a Token>,
    current: usize,
    pub errors: Vec<ParserError>,
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
    Exponent,
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
            Precedence::Factor => Precedence::Exponent,
            Precedence::Exponent => Precedence::Unary,
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
            tokens: tokens
                .iter()
                .filter(|&token| {
                    !matches!(
                        token.token_type,
                        TokenType::LineComment(_) | TokenType::MultilineComment(_)
                    )
                })
                .collect(),
            current: 0,
            errors: Vec::new(),
        }
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

    fn is_statement_starter(&self, token_type: &TokenType) -> bool {
        matches!(
            token_type,
            TokenType::Auto
                | TokenType::Const
                | TokenType::Func
                | TokenType::Class
                | TokenType::Interface
                | TokenType::Enum
                | TokenType::If
                | TokenType::While
                | TokenType::For
                | TokenType::Match
                | TokenType::Break
                | TokenType::Continue
                | TokenType::Return
                | TokenType::OpenBrace
                | TokenType::CloseBrace
        )
    }

    fn check_statement_termination(&self) -> ParserResult<()> {
        if self.current >= self.tokens.len() {
            return Ok(());
        }

        let token = &self.tokens[self.current];

        // statements can end at eof, closing braces, or newlines
        if matches!(
            token.token_type,
            TokenType::CloseBrace | TokenType::Eof | TokenType::NewLine
        ) {
            return Ok(());
        }

        // check if next token starts a new statement
        if self.current + 1 < self.tokens.len() {
            let next_token = &self.tokens[self.current + 1];
            if self.is_statement_starter(&next_token.token_type)
                && !matches!(token.token_type, TokenType::NewLine)
                && next_token.token_type != TokenType::Eof
            {
                return Err(ParserError::new(
                    "Expected newline before statement".to_string(),
                    next_token.span,
                ));
            }
        }
        Ok(())
    }

    pub fn parse(&mut self) -> Result<Vec<AstNode>, (Vec<AstNode>, Vec<ParserError>)> {
        let mut nodes = Vec::new();
        while !self.is_at_end() {
            if self.is_at_end() {
                break;
            }

            let start_position = self.current;

            match self.declaration() {
                Ok(Some(decl)) => {
                    nodes.push(decl);

                    // check statement termination for top-level, non-control flow statements.
                    let should_check_termination = matches!(
                        nodes.last().unwrap(),
                        AstNode::Statement(stmt) if !matches!(
                            stmt.kind,
                            StatementKind::If { .. } | StatementKind::While { .. } |
                            StatementKind::For { .. } | StatementKind::Match { .. } |
                            StatementKind::Block(_)
                        )
                    );

                    if should_check_termination {
                        if let Err(e) = self.check_statement_termination() {
                            self.errors.push(e);
                            self.synchronize();
                        }
                    }

                    let _ = self.skip_newlines();
                }
                Ok(None) => {
                    if self.current == start_position {
                        self.advance();
                    }
                }
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                }
            }

            let _ = self.skip_newlines();
        }

        let all_errors = std::mem::take(&mut self.errors);
        if all_errors.is_empty() {
            Ok(nodes)
        } else {
            Err((nodes, all_errors))
        }
    }

    fn declaration(&mut self) -> ParserResult<Option<AstNode>> {
        let _ = self.skip_newlines();
        if self.is_at_end() {
            return Ok(None);
        }
        let result = if self.check(TokenType::Auto) {
            self.auto_declaration().map(Some)
        } else if self.check(TokenType::Const) {
            self.const_declaration().map(Some)
        } else if self.check(TokenType::Common) {
            self.consume();
            self.function_declaration(true).map(Some)
        } else if self.check(TokenType::Func) {
            self.function_declaration(false).map(Some)
        } else if let TokenType::Id(_) = &self.peek().token_type {
            let start = self.current;
            if self.parse_type().is_ok() {
                if let TokenType::Id(_) = &self.peek().token_type {
                    let next = self.current + 1;
                    if next < self.tokens.len() && self.tokens[next].token_type == TokenType::Eq {
                        self.current = start;
                        match self.typed_declaration() {
                            Ok(node) => Ok(Some(node)),
                            Err(e)
                                if matches!(
                                    e.message.as_str(),
                                    "must be terminated with a newline"
                                        | "expected newline after statement"
                                ) =>
                            {
                                self.errors.push(e);
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
            self.errors.push(e.clone());
            if !self.is_at_end() {
                return self.declaration();
            }
            return Ok(None);
        }

        result
    }

    fn auto_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.peek().span;
        self.current += 1;

        let name = self.consume_identifier("Expected variable name after 'auto'")?;
        let name_span = self.tokens[self.current - 1].span;

        self.consume_token(TokenType::Eq, "Expected '=' after variable name")?;
        let value = self.parse_expression()?;

        // Validate that postfix ++ and -- don't appear in declarations
        self.check_no_postfix_increment_decrement(&value)?;

        if !self.is_in_block() {
            if self.current < self.tokens.len() {
                let next_token = &self.tokens[self.current];
                if self.is_statement_starter(&next_token.token_type)
                    && !matches!(self.tokens[self.current - 1].token_type, TokenType::NewLine)
                {
                    return Err(ParserError::new(
                        "Expected newline after statement".to_string(),
                        next_token.span,
                    ));
                }
            }
        } else {
            let _ = self.skip_newlines();
        }

        let span = start_span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::AutoDecl(
                name,
                TypeNode {
                    kind: TypeKind::Auto,
                    span: name_span,
                },
                value,
            ),
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

        // Validate that postfix ++ and -- don't appear in declarations
        self.check_no_postfix_increment_decrement(&value)?;

        let span = start_span.combine(&value.span);

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::ConstDecl(name, type_node, value),
            span,
        }))
    }

    fn typed_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.peek().span;
        let type_node = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name after type")?;
        self.consume_token(TokenType::Eq, "Expected '=' after variable name")?;
        let value = self.parse_expression()?;

        // Validate that postfix ++ and -- don't appear in declarations
        self.check_no_postfix_increment_decrement(&value)?;

        let span = start_span.combine(&value.span);
        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::TypedDecl(name, type_node, value),
            span,
        }))
    }

    fn class_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
        self.consume_token(TokenType::Class, "Expected 'class' keyword")?;

        let name = self.consume_identifier("Expected class name")?;

        let type_params = if self.matches(&[TokenType::Lt]) {
            let mut params = Vec::new();

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

                        if !self.matches(&[TokenType::Ref]) {
                            break;
                        }
                    }
                }

                params.push((param, bounds));

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
            self.consume_token(TokenType::Gt, "Expected '>' after type parameters")?;
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
                    self.consume_token(
                        TokenType::CloseBracket,
                        "Expected ']' after type arguments",
                    )?;
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
                    let func_node = self.function_declaration(false)?;
                    match func_node {
                        AstNode::Function(func) => methods.push(func),
                        _ => {
                            return Err(ParserError::new("Expected function in class", start_span));
                        }
                    }
                }
                TokenType::Common => {
                    self.consume();
                    let func_node = self.function_declaration(true)?;
                    match func_node {
                        AstNode::Function(func) => methods.push(func),
                        _ => {
                            return Err(ParserError::new("Expected function in class", start_span));
                        }
                    }
                }
                TokenType::Id(_) | TokenType::Const => {
                    let field = self.parse_field_declaration(&type_params)?;
                    fields.push(field);
                }
                TokenType::NewLine => {
                    self.consume_token(TokenType::NewLine, "Expected newline")?;
                }
                _ => {
                    let token_desc = match &self.peek().token_type {
                        TokenType::Func => "'func' keyword".to_string(),
                        TokenType::Class => "'class' keyword".to_string(),
                        TokenType::Interface => "'interface' keyword".to_string(),
                        TokenType::If => "'if' keyword".to_string(),
                        TokenType::For => "'for' keyword".to_string(),
                        TokenType::While => "'while' keyword".to_string(),
                        TokenType::Return => "'return' keyword".to_string(),
                        TokenType::OpenBrace => "'{'".to_string(),
                        TokenType::CloseBrace => "'}'".to_string(),
                        TokenType::OpenParen => "'('".to_string(),
                        TokenType::CloseParen => "')'".to_string(),
                        TokenType::Id(name) => format!("identifier '{}'", name),
                        TokenType::Int(n) => format!("integer literal '{}'", n),
                        TokenType::Float(n) => format!("float literal '{}'", n),
                        TokenType::Str(s) => format!("string literal '\"{}\"'", s),
                        TokenType::Eof => "end of file".to_string(),
                        t => format!("'{:?}'", t),
                    };
                    return Err(ParserError::new(
                        format!("Expected field declaration in class, found {}", token_desc),
                        start_span,
                    ));
                }
            }
        }

        let end_span =
            self.consume_token(TokenType::CloseBrace, "Expected '}' after class body")?;
        let full_span = start_span.combine(&end_span);

        Ok(AstNode::Class {
            name,
            type_params,
            traits,
            fields,
            methods,
            span: full_span,
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

        self.consume_token(TokenType::OpenBrace, "Expected '{' after interface header")?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        while !self.is_at_end() {
            self.skip_newlines();
            if self.check(TokenType::CloseBrace) {
                break;
            }

            match self.peek().token_type {
                TokenType::Func => {
                    self.consume();
                    let name = self.consume_identifier("Expected method name")?;

                    let type_params = if self.matches(&[TokenType::Lt]) {
                        let mut params = Vec::new();
                        if !self.check(TokenType::Gt) {
                            loop {
                                let param =
                                    self.consume_identifier("Expected type parameter name")?;
                                params.push((param, Vec::new()));
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

                    self.consume_token(TokenType::OpenParen, "Expected '(' after method name")?;
                    let mut params = Vec::new();
                    if !self.check(TokenType::CloseParen) {
                        loop {
                            let param_type = self.parse_type()?;
                            let param_name = self.consume_identifier("Expected parameter name")?;
                            params.push(Param {
                                name: param_name,
                                type_: param_type,
                                default_value: None,
                            });
                            if !self.matches(&[TokenType::Comma]) {
                                break;
                            }
                        }
                    }
                    self.consume_token(TokenType::CloseParen, "Expected ')' after parameters")?;

                    let return_type = if self.matches(&[TokenType::Minus, TokenType::Gt])
                        || self.matches(&[TokenType::Returns])
                    {
                        self.parse_type()?
                    } else {
                        TypeNode {
                            kind: TypeKind::Primitive(PrimitiveType::Void),
                            span: self.peek().span,
                        }
                    };

                    methods.push(FunctionNode {
                        name,
                        type_params,
                        params,
                        return_type,
                        body: vec![],
                        span: start_span,
                        is_common: false,
                    });
                }
                TokenType::Id(_) | TokenType::Const => {
                    let field = self.parse_field_declaration(&type_params)?;
                    fields.push(field);
                }
                TokenType::NewLine => {
                    self.consume_token(TokenType::NewLine, "Expected newline")?;
                }
                _ => {
                    return Err(ParserError::new(
                        "Expected field or function declaration in interface",
                        self.peek().span,
                    ));
                }
            }
        }

        let end_span =
            self.consume_token(TokenType::CloseBrace, "Expected '}' after interface body")?;
        let full_span = start_span.combine(&end_span);

        Ok(AstNode::Interface {
            name,
            type_params,
            fields,
            methods,
            span: full_span,
        })
    }

    fn enum_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
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

        self.consume_token(TokenType::OpenBrace, "Expected '{' after enum header")?;

        let mut variants = Vec::new();
        self.skip_newlines(); // Allow newlines after opening brace

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let variant_name = self.consume_identifier("Expected variant name")?;

            let data = if self.matches(&[TokenType::OpenParen]) {
                let mut fields = Vec::new();
                if !self.check(TokenType::CloseParen) {
                    loop {
                        let field_type = self.parse_type()?;

                        // skip over any field names, we only care about the types for enum variants
                        if let TokenType::Id(_) = self.peek().token_type {
                            self.advance(); // Skip the field name
                        }

                        fields.push(field_type);

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }
                self.consume_token(TokenType::CloseParen, "Expected ')' after variant data")?;
                Some(fields)
            } else {
                None
            };

            variants.push(EnumVariant {
                name: variant_name,
                data,
            });

            if self.matches(&[TokenType::Comma]) {
                self.skip_newlines(); // Allow newlines after comma
                if self.check(TokenType::CloseBrace) {
                    break;
                }
            } else {
                self.skip_newlines(); // Allow newlines before closing brace
                break;
            }
        }

        let end_span =
            self.consume_token(TokenType::CloseBrace, "Expected '}' after enum variants")?;
        let full_span = start_span.combine(&end_span);

        Ok(AstNode::Enum {
            name,
            type_params,
            variants,
            span: full_span,
        })
    }

    fn import_declaration(&mut self) -> ParserResult<AstNode> {
        let start_span = self.consume_token(TokenType::Import, "Expected 'import' keyword")?;

        // Parse module path (supports dots, relative ./, absolute /)
        let module_path = self.parse_module_path()?;

        // Parse import specification
        let spec = if self.matches(&[TokenType::Dot]) {
            // After dot: could be .item, .(items), or .*
            if self.matches(&[TokenType::Star]) {
                // import module.*
                ImportSpec::Wildcard
            } else if self.matches(&[TokenType::OpenParen]) {
                // import module.(item1, item2 as alias, item3)
                self.parse_import_items()?
            } else {
                // import module.item (as alias)
                let item = self.consume_identifier("Expected item name after '.'")?;
                let alias = if self.matches(&[TokenType::As]) {
                    Some(self.consume_identifier("Expected alias after 'as'")?)
                } else {
                    None
                };
                ImportSpec::Item { item, alias }
            }
        } else {
            // import module (as alias or as _)
            let alias = if self.matches(&[TokenType::As]) {
                let alias_name = self.consume_identifier("Expected alias after 'as'")?;
                // Check for side-effect import (as _)
                if alias_name == "_" {
                    None // Side-effect only, don't add symbols
                } else {
                    Some(alias_name)
                }
            } else {
                // No alias, use module basename as namespace
                Some(
                    module_path
                        .split('.')
                        .last()
                        .unwrap_or(&module_path)
                        .to_string(),
                )
            };
            ImportSpec::Module { alias }
        };

        let end_span = self.previous().span;

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Import { module_path, spec },
            span: start_span.combine(&end_span),
        }))
    }

    // Parse module path with support for dots, relative (./, ../), and absolute (/)
    fn parse_module_path(&mut self) -> ParserResult<String> {
        let mut parts = Vec::new();

        // Handle relative/absolute paths
        if self.matches(&[TokenType::Dot]) {
            parts.push(".".to_string());
            // Could be ./ or ../
            if self.matches(&[TokenType::Slash]) {
                // ./module
                parts.push("".to_string()); // Will join as ./
            } else if self.matches(&[TokenType::Dot]) {
                // ../module
                parts.push(".".to_string());
                self.consume_token(TokenType::Slash, "Expected '/' after '..'")?;
                parts.push("".to_string());
            }
        } else if self.matches(&[TokenType::Slash]) {
            // Absolute path /module
            parts.push("".to_string()); // Leading slash
        }

        // Parse first identifier
        parts.push(self.consume_identifier("Expected module path")?);

        // Parse remaining dotted parts (utils.logger.helpers)
        // Stop if we see:
        // - .* (wildcard import)
        // - .( (multiple items import)
        // - .identifier followed by end/as/newline (single item import)
        while self.check(TokenType::Dot) {
            let next = self.peek_ahead(1);

            // Stop if next token after dot is * or (
            if next.is_some_and(|t| matches!(t.token_type, TokenType::Star | TokenType::OpenParen))
            {
                break;
            }

            // Stop if next is identifier but there's no dot after it (single item import)
            if next.is_some_and(|t| matches!(t.token_type, TokenType::Id(_))) {
                let after_identifier = self.peek_ahead(2);
                if !after_identifier.is_some_and(|t| matches!(t.token_type, TokenType::Dot)) {
                    // This is .identifier at end - it's an item import, not part of module path
                    break;
                }
            }

            self.advance(); // consume dot
            parts.push(self.consume_identifier("Expected module name after '.'")?);
        }

        Ok(parts.join("."))
    }

    // Parse (item1, item2 as alias, item3)
    fn parse_import_items(&mut self) -> ParserResult<ImportSpec> {
        let mut items = Vec::new();

        if !self.check(TokenType::CloseParen) {
            loop {
                let item = self.consume_identifier("Expected item name")?;
                let alias = if self.matches(&[TokenType::As]) {
                    Some(self.consume_identifier("Expected alias after 'as'")?)
                } else {
                    None
                };
                items.push((item, alias));

                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume_token(TokenType::CloseParen, "Expected ')' after import items")?;
        Ok(ImportSpec::Items { items })
    }

    // Look ahead n tokens without consuming
    fn peek_ahead(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.current + n).copied()
    }

    fn function_declaration(&mut self, is_common: bool) -> ParserResult<AstNode> {
        let start_span = self.peek().span;
        self.consume_token(TokenType::Func, "Expected 'func' keyword")?;

        let name = self.consume_identifier("Expected function name")?;
        let type_params = if self.matches(&[TokenType::Lt]) {
            let mut params = Vec::new();
            if !self.check(TokenType::Gt) {
                loop {
                    let param = self.consume_identifier("Expected type parameter name")?;
                    let mut bounds = Vec::new();

                    if self.matches(&[TokenType::Is]) {
                        loop {
                            let bound_name =
                                self.consume_identifier("Expected trait name in bound")?;
                            let type_args = if self.matches(&[TokenType::OpenBracket]) {
                                self.parse_type_arguments()?
                            } else {
                                Vec::new()
                            };

                            bounds.push(TraitBound {
                                name: bound_name,
                                type_params: type_args,
                            });

                            if !self.matches(&[TokenType::Ref]) {
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
            let mut has_default = false;
            loop {
                let param_type = self.parse_type()?;
                let param_name = self.consume_identifier("Expected parameter name")?;
                let default_value = if self.matches(&[TokenType::Eq]) {
                    let default_expr = self.parse_expression()?;
                    // Validate that default value is a literal
                    if !Self::is_literal_expression(&default_expr) {
                        return Err(ParserError::new(
                            "Default parameter values must be literals (int, float, string, bool, or char)",
                            default_expr.span,
                        ));
                    }
                    has_default = true;
                    Some(default_expr)
                } else {
                    // If we've already seen a default param, this one must also have a default
                    if has_default {
                        return Err(ParserError::new(
                            "Parameters without default values cannot come after parameters with default values",
                            self.previous().span,
                        ));
                    }
                    None
                };
                params.push(Param {
                    name: param_name,
                    type_: param_type,
                    default_value,
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
            return Err(ParserError::new(
                "Expected 'returns' before return type",
                self.peek().span,
            ));
        };

        self.skip_newlines();
        let body = self.block()?;

        let body_statements = match body {
            AstNode::Statement(StatementNode {
                kind: StatementKind::Block(block),
                ..
            }) => block,
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
            is_common,
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
        } else if self.matches(&[TokenType::Func]) {
            self.function_declaration(false)
        } else if self.looks_like_typed_decl() {
            self.typed_declaration()
        } else if self.check(TokenType::OpenBrace) {
            self.block()
        } else {
            self.expression_statement()
        }?;

        // only do newline-based termination at top level, skip for control-flow statements.
        if !self.is_in_block()
            && !matches!(
                &result,
                AstNode::Statement(stmt) if matches!(
                    stmt.kind,
                    StatementKind::If { .. } | StatementKind::While { .. } | StatementKind::For { .. } | StatementKind::Match { .. } | StatementKind::Function { .. }
                )
            )
        {
            if let Err(e) = self.check_statement_termination() {
                self.errors.push(e);
            }
        }

        Ok(result)
    }

    fn looks_like_typed_decl(&self) -> bool {
        let n = self.tokens.len();
        let i = self.current;
        // helper to skip over a type without consuming.
        fn skip_type(tokens: &[&Token], mut i: usize) -> Option<usize> {
            let n = tokens.len();
            if i >= n {
                return None;
            }
            match tokens[i].token_type {
                TokenType::Ref => {
                    i += 1;
                    skip_type(tokens, i)
                }
                TokenType::OpenParen => {
                    // function type: (types) returns type.
                    i += 1;
                    let mut depth = 1usize;
                    while i < n && depth > 0 {
                        match tokens[i].token_type {
                            TokenType::OpenParen => depth += 1,
                            TokenType::CloseParen => depth -= 1,
                            _ => {}
                        }
                        i += 1;
                    }
                    if depth != 0 {
                        return None;
                    }
                    if i >= n || tokens[i].token_type != TokenType::Returns {
                        return None;
                    }
                    i += 1;
                    skip_type(tokens, i)
                }
                TokenType::Id(_) => {
                    i += 1;
                    // handle generic angles: < ... > (nested)
                    if i < n && tokens[i].token_type == TokenType::Lt {
                        i += 1;
                        let mut depth = 1usize;
                        while i < n && depth > 0 {
                            match tokens[i].token_type {
                                TokenType::Lt => depth += 1,
                                TokenType::Gt => depth -= 1,
                                _ => {}
                            }
                            i += 1;
                        }
                        if depth != 0 {
                            return None;
                        }
                    }
                    Some(i)
                }
                _ => None,
            }
        }

        if let Some(j) = skip_type(&self.tokens, i) {
            if j < n {
                if let TokenType::Id(_) = self.tokens[j].token_type {
                    if j + 1 < n && self.tokens[j + 1].token_type == TokenType::Eq {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn if_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current - 1].span;
        let condition = self.parse_expression()?;

        // Validate that postfix ++ and -- don't appear in condition
        self.check_no_postfix_increment_decrement(&condition)?;

        self.skip_newlines();
        if !self.check(TokenType::OpenBrace) {
            return Err(ParserError::new(
                "Expected '{' after if condition",
                self.peek().span,
            ));
        }

        // parse then branch, require a braced block.
        let then_block = match self.block()? {
            AstNode::Statement(StatementNode {
                kind: StatementKind::Block(block),
                ..
            }) => block,
            _ => {
                return Err(ParserError::new(
                    "Expected block after if condition",
                    self.peek().span,
                ));
            }
        };

        // handle else/else if.
        // allow newline between then-block '}' and 'else'.
        self.skip_newlines();
        let (else_block, end_span) = if self.matches(&[TokenType::Else]) {
            self.skip_newlines();
            // support 'else if' sugar by nesting the parsed if inside the else_block.
            if self.matches(&[TokenType::If]) {
                let nested = self.if_statement()?;
                match nested {
                    AstNode::Statement(stmt) => {
                        let end_span = stmt.span;
                        (Some(vec![stmt]), end_span)
                    }
                    _ => {
                        return Err(ParserError::new(
                            "Expected statement after else if",
                            self.previous().span,
                        ));
                    }
                }
            } else {
                if !self.check(TokenType::OpenBrace) {
                    return Err(ParserError::new(
                        "Expected '{' after else",
                        self.peek().span,
                    ));
                }
                let else_block = match self.block()? {
                    AstNode::Statement(StatementNode {
                        kind: StatementKind::Block(block),
                        ..
                    }) => block,
                    _ => {
                        return Err(ParserError::new(
                            "Expected block after else",
                            self.peek().span,
                        ));
                    }
                };

                let end_span = else_block
                    .last()
                    .map(|s| s.span)
                    .unwrap_or_else(|| self.tokens[self.current - 1].span);

                (Some(else_block), end_span)
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
        let condition = self.parse_expression()?;

        // Validate that postfix ++ and -- don't appear in condition
        self.check_no_postfix_increment_decrement(&condition)?;

        // allow newline(s) before body.
        self.skip_newlines();
        let body = self.block()?;

        let body_statements = match body {
            AstNode::Statement(stmt) => match stmt.kind {
                StatementKind::Block(block) => block,
                _ => vec![stmt],
            },
            _ => {
                return Err(ParserError::new(
                    "Expected statement after while condition",
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

        // parse the variable type.
        let var_type = self.parse_type()?;

        let var = self.consume_identifier("Expected variable name")?;
        self.consume_token(TokenType::In, "Expected 'in' after variable")?;
        let iter = self.parse_expression()?;

        // Validate that postfix ++ and -- don't appear in iterator expression
        self.check_no_postfix_increment_decrement(&iter)?;

        // allow newline(s) before body.
        self.skip_newlines();
        let body = if self.check(TokenType::OpenBrace) {
            self.block()?
        } else {
            self.statement()?
        };

        let body_statements = match body {
            AstNode::Statement(stmt) => match stmt.kind {
                StatementKind::Block(block) => block,
                _ => vec![stmt],
            },
            _ => {
                return Err(ParserError::new(
                    "Expected statement after for loop",
                    start_span,
                ));
            }
        };

        let end_span = body_statements.last().map(|s| s.span).unwrap_or(start_span);
        let span = start_span.combine(&end_span);
        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::For {
                var,
                var_type,
                iter,
                body: body_statements,
            },
            span,
        }))
    }

    fn match_statement(&mut self) -> ParserResult<AstNode> {
        let start_span = self.tokens[self.current].span;
        let expr = self.parse_expression()?;

        // Validate that postfix ++ and -- don't appear in match expression
        self.check_no_postfix_increment_decrement(&expr)?;

        self.consume_token(TokenType::OpenBrace, "Expected '{' after match expression")?;
        self.skip_newlines();

        let mut arms = Vec::new();
        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            let pattern = self.parse_pattern()?;
            let guard = if self.matches(&[TokenType::If]) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            self.skip_newlines();

            let body = if self.check(TokenType::OpenBrace) {
                self.block()?
            } else if self.matches(&[TokenType::Colon]) {
                self.skip_newlines();
                if self.check(TokenType::OpenBrace) {
                    self.block()?
                } else {
                    self.statement()?
                }
            } else {
                self.statement()?
            };

            let body_statements = match body {
                AstNode::Statement(stmt) => match stmt.kind {
                    StatementKind::Block(block) => block,
                    _ => vec![stmt],
                },
                _ => {
                    return Err(ParserError::new(
                        "Expected statement for match arm body",
                        start_span,
                    ));
                }
            };

            arms.push(MatchArm {
                pattern,
                guard,
                body: body_statements,
            });

            self.skip_newlines();
            if self.matches(&[TokenType::Comma]) {
                self.skip_newlines();
                // if the next token is a closing brace, break to handle trailing comma
                if self.check(TokenType::CloseBrace) {
                    break;
                }
            }
        }

        let end_span =
            self.consume_token(TokenType::CloseBrace, "Expected '}' after match arms")?;

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Match { expr, arms },
            span: start_span.combine(&end_span),
        }))
    }

    fn parse_pattern(&mut self) -> ParserResult<PatternNode> {
        match &self.peek().token_type {
            TokenType::None => {
                self.current += 1; // consume None
                Ok(PatternNode::EnumVariant {
                    name: "None".to_string(),
                    args: vec![],
                })
            }
            TokenType::Id(name) => {
                let name_clone = name.clone();
                self.current += 1; // consume the identifier
                if self.matches(&[TokenType::OpenParen]) {
                    let mut args = Vec::new();
                    if !self.check(TokenType::CloseParen) {
                        loop {
                            args.push(self.parse_pattern()?);
                            if !self.matches(&[TokenType::Comma]) {
                                break;
                            }
                        }
                    }
                    self.consume_token(
                        TokenType::CloseParen,
                        "Expected ')' after enum variant arguments",
                    )?;
                    Ok(PatternNode::EnumVariant {
                        name: name_clone,
                        args,
                    })
                } else {
                    Ok(PatternNode::Identifier(name_clone))
                }
            }
            TokenType::Underscore => {
                self.current += 1; // consume the underscore
                Ok(PatternNode::Wildcard)
            }

            _ => {
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

        // Check if there's an expression after return
        let value = if self.is_at_end()
            || self.check(TokenType::NewLine)
            || self.check(TokenType::CloseBrace)
        {
            // return at end of input, or followed by newline/closing brace - void return
            None
        } else {
            let expr = self.parse_expression()?;

            // Validate that postfix ++ and -- don't appear in return value
            self.check_no_postfix_increment_decrement(&expr)?;

            Some(expr)
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

    fn skip_newlines(&mut self) -> usize {
        let mut count = 0;
        while self.matches(&[TokenType::NewLine]) {
            count += 1;
        }
        count
    }

    fn block(&mut self) -> ParserResult<AstNode> {
        let start_span = self.consume_token(TokenType::OpenBrace, "Expected '{' before block")?;
        let mut statements = Vec::new();
        self.skip_newlines();

        if self.matches(&[TokenType::CloseBrace]) {
            return Ok(AstNode::Statement(StatementNode {
                kind: StatementKind::Block(Vec::new()),
                span: start_span.combine(&self.previous().span),
            }));
        }

        while !self.check(TokenType::CloseBrace) && !self.is_at_end() {
            self.skip_newlines();
            if self.check(TokenType::CloseBrace) {
                break;
            }
            match self.declaration() {
                Ok(Some(decl)) => {
                    statements.push(decl);
                    self.skip_newlines();
                }
                Ok(None) => {
                    // if we didn't make progress, advance to avoid infinite loops.
                    let current_pos = self.current;
                    if current_pos == self.current {
                        self.advance();
                    }
                }
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize();
                    let current_pos = self.current;
                    if self.current == current_pos && !self.is_at_end() {
                        self.advance();
                    }
                }
            }
        }

        let end_span = if self.check(TokenType::CloseBrace) {
            // use the span returned by consume_token so row/col end are correct.
            self.consume_token(TokenType::CloseBrace, "Expected '}' after block")?
        } else {
            return Err(ParserError::new(
                "Expected '}' after block".to_string(),
                self.peek().span,
            ));
        };

        let stmts: Vec<StatementNode> = statements
            .into_iter()
            .filter_map(|node| node.into_statement())
            .collect();

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Block(stmts),
            span: start_span.combine(&end_span),
        }))
    }

    fn is_in_block(&self) -> bool {
        // look backwards for an opening brace that doesn't have a matching closing brace.
        let mut brace_count: usize = 0;
        for i in (0..self.current).rev() {
            match self.tokens[i].token_type {
                TokenType::CloseBrace => brace_count += 1,
                TokenType::OpenBrace => {
                    if brace_count == 0 {
                        return true;
                    }
                    brace_count = brace_count.saturating_sub(1);
                }
                _ => {}
            }
        }
        false
    }

    fn expression_statement(&mut self) -> ParserResult<AstNode> {
        let expr = self.parse_expression()?;
        let has_newline = self.check(TokenType::NewLine);
        if !self.is_in_block() && !has_newline && self.current < self.tokens.len() {
            let next_token = &self.tokens[self.current];
            if self.is_statement_starter(&next_token.token_type) {
                return Err(ParserError::new(
                    "Expected newline before statement".to_string(),
                    next_token.span,
                ));
            }
        }
        let _ = self.skip_newlines();
        let span = *expr.span();

        // Validate that postfix ++ and -- only appear at statement level
        self.validate_postfix_in_statement(&expr)?;

        Ok(AstNode::Statement(StatementNode {
            kind: StatementKind::Expression(expr),
            span,
        }))
    }

    fn validate_postfix_in_statement(&self, expr: &ExpressionNode) -> ParserResult<()> {
        // If this is a postfix ++ or -- at the top level, it's valid
        if let ExpressionKind::Unary { op, postfix, .. } = &expr.kind {
            if *postfix && matches!(op, UnaryOp::Incr | UnaryOp::Decr) {
                return Ok(());
            }
        }
        // Otherwise, check that no nested postfix ++ or -- exist
        self.check_no_postfix_increment_decrement(expr)
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_no_postfix_increment_decrement(&self, expr: &ExpressionNode) -> ParserResult<()> {
        match &expr.kind {
            ExpressionKind::Unary {
                op,
                op_span: _,
                expr: inner,
                postfix,
            } => {
                // If this is a postfix ++ or --, it's nested and invalid
                if *postfix && matches!(op, UnaryOp::Incr | UnaryOp::Decr) {
                    return Err(ParserError::new(
                        "Increment/Decrement operator can only be used individually, not as a part of an expression".to_string(),
                        expr.span,
                    ));
                }
                // Otherwise, recurse into the inner expression
                self.check_no_postfix_increment_decrement(inner)
            }
            ExpressionKind::Binary { left, right, .. } => {
                self.check_no_postfix_increment_decrement(left)?;
                self.check_no_postfix_increment_decrement(right)
            }
            ExpressionKind::Call { func, args } => {
                self.check_no_postfix_increment_decrement(func)?;
                for arg in args {
                    self.check_no_postfix_increment_decrement(arg)?;
                }
                Ok(())
            }
            ExpressionKind::FieldAccess { expr: inner, .. } => {
                self.check_no_postfix_increment_decrement(inner)
            }
            ExpressionKind::ListAccess { expr: inner, index } => {
                self.check_no_postfix_increment_decrement(inner)?;
                self.check_no_postfix_increment_decrement(index)
            }
            ExpressionKind::ListLiteral(elems) => {
                for elem in elems {
                    self.check_no_postfix_increment_decrement(elem)?;
                }
                Ok(())
            }
            ExpressionKind::SetLiteral(elems) => {
                for elem in elems {
                    self.check_no_postfix_increment_decrement(elem)?;
                }
                Ok(())
            }
            ExpressionKind::MapLiteral { entries, .. } => {
                for (key, value) in entries {
                    self.check_no_postfix_increment_decrement(key)?;
                    self.check_no_postfix_increment_decrement(value)?;
                }
                Ok(())
            }
            ExpressionKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                self.check_no_postfix_increment_decrement(cond)?;
                self.check_no_postfix_increment_decrement(then_expr)?;
                self.check_no_postfix_increment_decrement(else_expr)
            }
            ExpressionKind::Lambda { body, .. } => {
                // Check all statements in the lambda body
                for stmt in body {
                    if let StatementKind::Expression(e) = &stmt.kind {
                        self.check_no_postfix_increment_decrement(e)?;
                    }
                }
                Ok(())
            }
            // For literals, identifiers, and other leaf nodes, they can't contain postfix ops
            _ => Ok(()),
        }
    }

    fn parse_type(&mut self) -> ParserResult<TypeNode> {
        if self.matches(&[TokenType::Ref]) {
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

        // we are essentially doing a consume here, but without borrowing the parser again so we dont have to clone it.
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
                    self.parse_type_arguments()?
                } else {
                    Vec::new()
                };

                if name == "dyn" && !type_args.is_empty() {
                    return Ok(TypeNode {
                        kind: TypeKind::TraitObject(Box::new(type_args[0].clone())),
                        span: start_span,
                    });
                }

                let next_is_gt = !self.is_at_end()
                    && self
                        .tokens
                        .get(self.current)
                        .is_some_and(|t| t.token_type == TokenType::Lt);

                if name == "list" && next_is_gt {
                    self.consume_token(TokenType::Lt, "Expected '<' for list element type")?;
                    let element_type = self.parse_type()?;
                    self.consume_token(TokenType::Gt, "Expected '>' after list element type")?;
                    return Ok(TypeNode {
                        kind: TypeKind::List(Box::new(element_type)),
                        span: start_span,
                    });
                }

                if name == "map" && next_is_gt {
                    self.consume_token(TokenType::Lt, "Expected '<' for map key type")?;
                    let key_type = self.parse_type()?;
                    self.consume_token(
                        TokenType::Comma,
                        "Expected ',' between key and value types in map",
                    )?;
                    let value_type = self.parse_type()?;
                    self.consume_token(TokenType::Gt, "Expected '>' after map value type")?;

                    return Ok(TypeNode {
                        kind: TypeKind::Map(Box::new(key_type), Box::new(value_type)),
                        span: start_span,
                    });
                }

                if name == "set" && next_is_gt {
                    self.consume_token(TokenType::Lt, "Expected '<' for set element type")?;
                    let element_type = self.parse_type()?;
                    self.consume_token(TokenType::Gt, "Expected '>' after set element type")?;

                    return Ok(TypeNode {
                        kind: TypeKind::Set(Box::new(element_type)),
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

                Ok(TypeNode {
                    kind: TypeKind::Named(name_clone, type_args),
                    span: start_span,
                })
            }

            TokenType::Func => {
                self.consume_token(
                    TokenType::OpenParen,
                    "Expected '(' after 'func' in function type",
                )?;
                let mut param_types = Vec::new();

                if !self.check(TokenType::CloseParen) {
                    loop {
                        // Parse parameter types only (no parameter names for function types)
                        param_types.push(self.parse_type()?);

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }
                    }
                }

                self.consume_token(TokenType::CloseParen, "Expected ')' after parameter types")?;
                self.consume_token(TokenType::Returns, "Expected 'returns' in function type")?;

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
        while !self.check(TokenType::Gt) && !self.is_at_end() {
            let arg = self.parse_type()?;
            args.push(arg);

            if !self.matches(&[TokenType::Comma]) {
                break;
            }
        }
        Ok(args)
    }

    fn synchronize(&mut self) {
        while !self.is_at_end() {
            match self.peek().token_type {
                TokenType::NewLine => {
                    self.current += 1;
                    return;
                }
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
                TokenType::OpenBrace | TokenType::CloseBrace => {
                    return;
                }
                TokenType::OpenParen | TokenType::CloseParen => {
                    return;
                }
                TokenType::OpenBracket | TokenType::CloseBracket => {
                    return;
                }
                _ => {
                    self.current += 1;
                }
            }
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        if self.is_at_end() {
            static EOF_TOKEN: Token = Token {
                token_type: TokenType::Eof,
                span: Span {
                    row_start: 0,
                    row_end: None,
                    col_start: 0,
                    col_end: None,
                },
            };
            &EOF_TOKEN
        } else {
            self.tokens[self.current]
        }
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

        // important, do not consume the operator until after checking precedence.
        // otherwise we may consume a lower-precedence operator in a recursive call and lose it.
        while let Some(op_token) = self.peek_operator() {
            let op_precedence = self.get_operator_precedence(&op_token)?;
            if op_precedence < min_precedence {
                break;
            }

            // now it is safe to consume the operator
            let _ = self.consume_operator();

            let next_precedence = if op_token.is_right_associative() {
                op_precedence
            } else {
                op_precedence.next_higher()
            };

            let right = self.parse_precedence(next_precedence)?;

            let left_span = *value.span();
            let right_span = *right.span();
            let left_expr = value.clone();
            let op_span = self.previous().span;
            let new_value = ExpressionNode {
                kind: ExpressionKind::Binary {
                    left: left_expr,
                    op: op_token,
                    op_span,
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
            // Reject prefix ++ and --
            if matches!(op_token.token_type, TokenType::Incr | TokenType::Decr) {
                return Err(ParserError::new(
                    "Increment/Decrement operator can only be in the postfix position".to_string(),
                    op_token.span,
                ));
            }
            let expr = self.parse_precedence(Precedence::Unary)?;
            let expr_span = *expr.span();
            Ok(ExpressionNode {
                kind: ExpressionKind::Unary {
                    op: UnaryOp::parse(op_token.clone())?,
                    op_span: op_token.span,
                    expr: Box::new(expr),
                    postfix: false,
                },
                span: op_token.span.combine(&expr_span),
            })
        } else {
            let expr = self.parse_primary()?;
            self.parse_postfix_operators(expr)
        }
    }

    fn parse_collection_literal(&mut self, start_span: Span) -> ParserResult<ExpressionNode> {
        let mut set_elements = Vec::new();
        let mut map_entries = Vec::new();
        let mut is_map = false;

        self.skip_newlines();

        if !self.check(TokenType::CloseBrace) {
            // parse first expression to determine if map or set
            let first_expr = self.parse_expression()?;

            if self.matches(&[TokenType::Colon]) {
                // it is a map, key, value
                is_map = true;
                let value = self.parse_expression()?;
                map_entries.push((first_expr, value));
            } else {
                // it is a set, just elements
                set_elements.push(first_expr);
            }

            // parse remaining entries
            while self.matches(&[TokenType::Comma]) {
                self.skip_newlines();
                if is_map {
                    let key = self.parse_expression()?;
                    self.consume_token(TokenType::Colon, "Expected ':' after map key")?;
                    let value = self.parse_expression()?;
                    map_entries.push((key, value));
                } else {
                    let elem = self.parse_expression()?;
                    set_elements.push(elem);
                }
            }
        }

        let end_span =
            self.consume_token(TokenType::CloseBrace, "Expected '}' after collection")?;

        let expr = if is_map {
            ExpressionNode {
                kind: ExpressionKind::MapLiteral {
                    key_type: Box::new(TypeNode {
                        kind: TypeKind::Auto,
                        span: start_span,
                    }),
                    value_type: Box::new(TypeNode {
                        kind: TypeKind::Auto,
                        span: start_span,
                    }),
                    entries: map_entries,
                },
                span: start_span.combine(&end_span),
            }
        } else {
            ExpressionNode {
                kind: ExpressionKind::SetLiteral(set_elements),
                span: start_span.combine(&end_span),
            }
        };

        self.parse_postfix_operators(expr)
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

            TokenType::Bool(b) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Literal(LiteralNode::Boolean(b)),
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            TokenType::None => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::None,
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            TokenType::Char(c) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Literal(LiteralNode::Char(c)),
                    span: token_span,
                };
                self.parse_postfix_operators(expr)
            }

            TokenType::Str(s) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Literal(LiteralNode::String(s)),
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

            TokenType::OpenBracket => {
                // regular list literal.
                let start_span = token_span;
                let mut elements = Vec::new();

                self.skip_newlines();

                if !self.check(TokenType::CloseBracket) {
                    loop {
                        elements.push(self.parse_expression()?);

                        self.skip_newlines();

                        if !self.matches(&[TokenType::Comma]) {
                            break;
                        }

                        self.skip_newlines();
                    }
                }

                let end_span = self
                    .consume_token(TokenType::CloseBracket, "Expected ']' after list elements")?;
                let expr = ExpressionNode {
                    kind: ExpressionKind::ListLiteral(elements),
                    span: start_span.combine(&end_span),
                };

                self.parse_postfix_operators(expr)
            }

            TokenType::OpenBrace => self.parse_collection_literal(token_span),

            TokenType::Func => {
                let start_span = token_span;
                self.consume_token(TokenType::OpenParen, "Expected '(' after 'func' in lambda")?;
                let mut params = Vec::new();

                if !self.check(TokenType::CloseParen) {
                    loop {
                        let param_type = self.parse_type()?;
                        let param_name = self.consume_identifier("Expected parameter name")?;
                        if self.matches(&[TokenType::Eq]) {
                            return Err(ParserError::new(
                                "Default arguments are not supported in lambda expressions. Use named functions instead.",
                                self.previous().span,
                            ));
                        }
                        params.push(Param {
                            name: param_name,
                            type_: param_type,
                            default_value: None,
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
                    return Err(ParserError::new(
                        "Expected 'returns' after parameters",
                        start_span,
                    ));
                };

                let body = self.block()?;
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
                        return_type,
                        body: body_statements,
                    },
                    span: start_span.combine(&end_span),
                };

                self.parse_postfix_operators(expr)
            }

            TokenType::If => {
                let cond = self.parse_expression()?;
                self.consume_token(TokenType::OpenBrace, "Expected '{' after if condition")?;
                let then_expr = self.parse_expression()?;
                self.consume_token(TokenType::CloseBrace, "Expected '}' after then expression")?;
                self.consume_token(TokenType::Else, "Expected 'else' after then branch")?;
                self.consume_token(TokenType::OpenBrace, "Expected '{' after else")?;
                let else_expr = self.parse_expression()?;
                self.consume_token(TokenType::CloseBrace, "Expected '}' after else expression")?;
                let span = token_span.combine(&self.previous().span);
                Ok(ExpressionNode {
                    kind: ExpressionKind::If {
                        cond: Box::new(cond),
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    },
                    span,
                })
            }

            TokenType::Id(id) => {
                let expr = ExpressionNode {
                    kind: ExpressionKind::Identifier(id.clone()),
                    span: token_span,
                };
                // defer handling of '<' to binary operator parsing or parse_postfix_operators, which can disambiguate generics more safely.
                self.parse_postfix_operators(expr)
            }

            _ => {
                let token_desc = match &token_type {
                    TokenType::Func => "'func' keyword".to_string(),
                    TokenType::Class => "'class' keyword".to_string(),
                    TokenType::Interface => "'interface' keyword".to_string(),
                    TokenType::If => "'if' keyword".to_string(),
                    TokenType::For => "'for' keyword".to_string(),
                    TokenType::While => "'while' keyword".to_string(),
                    TokenType::Return => "'return' keyword".to_string(),
                    TokenType::OpenBrace => "'{'".to_string(),
                    TokenType::CloseBrace => "'}'".to_string(),
                    TokenType::OpenParen => "'('".to_string(),
                    TokenType::CloseParen => "')'".to_string(),
                    TokenType::Id(name) => format!("identifier '{}'", name),
                    TokenType::Int(n) => format!("integer literal '{}'", n),
                    TokenType::Float(n) => format!("float literal '{}'", n),
                    TokenType::Str(s) => format!("string literal '\"{}\"'", s),
                    TokenType::Eof => "end of file".to_string(),
                    t => format!("'{:?}'", t),
                };
                Err(ParserError::from_token(
                    format!("Expected expression, found {}", token_desc),
                    &Token {
                        token_type,
                        span: token_span,
                    },
                ))
            }
        }
    }

    fn parse_postfix_operators(
        &mut self,
        mut expr: ExpressionNode,
    ) -> ParserResult<ExpressionNode> {
        loop {
            if self.matches(&[TokenType::OpenParen]) {
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
            } else if self.check(TokenType::Lt) {
                // only treat '<...>' as generic type arguments in expression position when:
                // - current expr is an identifier.
                // - there is a matching '>'.
                // - and the token immediately following '>' is either '.' or '('.
                let should_consume_generics = if let ExpressionKind::Identifier(_) = &expr.kind {
                    let mut i = self.current + 1; // after '<'
                    let mut depth = 1usize;
                    let mut gt_idx: Option<usize> = None;
                    while i < self.tokens.len() {
                        match self.tokens[i].token_type {
                            TokenType::Lt => depth += 1,
                            TokenType::Gt => {
                                depth -= 1;
                                if depth == 0 {
                                    gt_idx = Some(i);
                                    break;
                                }
                            }
                            TokenType::Eof
                            | TokenType::NewLine
                            | TokenType::OpenBrace
                            | TokenType::CloseBrace => {
                                break;
                            }
                            _ => {}
                        }
                        i += 1;
                    }
                    if let Some(end) = gt_idx {
                        if let Some(next) = self.tokens.get(end + 1) {
                            matches!(next.token_type, TokenType::Dot | TokenType::OpenParen)
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if should_consume_generics {
                    // consume type arguments and wrap identifier in generictype.
                    let _ = self.matches(&[TokenType::Lt]);
                    let type_args = self.parse_type_arguments()?;
                    let end_span =
                        self.consume_token(TokenType::Gt, "Expected '>' after type arguments")?;
                    if let ExpressionKind::Identifier(name) = expr.kind {
                        expr = ExpressionNode {
                            kind: ExpressionKind::GenericType(name, type_args),
                            span: expr.span.combine(&end_span),
                        };
                    }
                    continue;
                } else {
                    // not a generic argument list, leave '<' to be parsed as binary less-than.
                    break;
                }
            } else if self.matches(&[TokenType::Incr]) {
                let expr_span = *expr.span();
                let op_span = self.previous().span;
                expr = ExpressionNode {
                    kind: ExpressionKind::Unary {
                        op: UnaryOp::Incr,
                        op_span,
                        expr: Box::new(expr),
                        postfix: true,
                    },
                    span: expr_span.combine(&op_span),
                };
            } else if self.matches(&[TokenType::Decr]) {
                let expr_span = *expr.span();
                let op_span = self.previous().span;
                expr = ExpressionNode {
                    kind: ExpressionKind::Unary {
                        op: UnaryOp::Decr,
                        op_span,
                        expr: Box::new(expr),
                        postfix: true,
                    },
                    span: expr_span.combine(&op_span),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        if self.is_at_end() {
            return false;
        }

        let token = self.peek();
        for ty in types {
            if token.token_type == *ty {
                if token.token_type == TokenType::Eof {
                    return false;
                }
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
            TokenType::Id(name) => {
                let name_clone = name.clone();
                self.current += 1;
                Ok(name_clone)
            }
            TokenType::Underscore => {
                let name_clone = "_".to_string();
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
            TokenType::StarStar => Some(BinaryOp::Exponent),
            TokenType::Eq => Some(BinaryOp::Assign),
            TokenType::EqEq => Some(BinaryOp::Equal),
            TokenType::NotEq => Some(BinaryOp::NotEqual),
            TokenType::Lt => Some(BinaryOp::Less),
            TokenType::Gt => Some(BinaryOp::Greater),
            TokenType::Le => Some(BinaryOp::LessEqual),
            TokenType::Ge => Some(BinaryOp::GreaterEqual),
            TokenType::And => Some(BinaryOp::LogicalAnd),
            TokenType::Or => Some(BinaryOp::LogicalOr),
            TokenType::In => Some(BinaryOp::In),
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

    fn peek_operator(&self) -> Option<BinaryOp> {
        if self.is_at_end() {
            return None;
        }

        match self.peek().token_type {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Subtract),
            TokenType::Star => Some(BinaryOp::Multiply),
            TokenType::Slash => Some(BinaryOp::Divide),
            TokenType::Percent => Some(BinaryOp::Modulo),
            TokenType::StarStar => Some(BinaryOp::Exponent),
            TokenType::Eq => Some(BinaryOp::Assign),
            TokenType::EqEq => Some(BinaryOp::Equal),
            TokenType::NotEq => Some(BinaryOp::NotEqual),
            TokenType::Lt => Some(BinaryOp::Less),
            TokenType::Gt => Some(BinaryOp::Greater),
            TokenType::Le => Some(BinaryOp::LessEqual),
            TokenType::Ge => Some(BinaryOp::GreaterEqual),
            TokenType::And => Some(BinaryOp::LogicalAnd),
            TokenType::Or => Some(BinaryOp::LogicalOr),
            TokenType::In => Some(BinaryOp::In),
            TokenType::PlusEq => Some(BinaryOp::AddAssign),
            TokenType::MinusEq => Some(BinaryOp::SubtractAssign),
            TokenType::StarEq => Some(BinaryOp::MultiplyAssign),
            TokenType::SlashEq => Some(BinaryOp::DivideAssign),
            TokenType::PercentEq => Some(BinaryOp::ModuloAssign),
            _ => None,
        }
    }

    fn consume_if_unary_operator(&mut self) -> Option<Token> {
        if self.is_at_end() {
            return None;
        }

        let token = self.peek();
        match &token.token_type {
            TokenType::Minus
            | TokenType::Bang
            | TokenType::Ref
            | TokenType::Incr
            | TokenType::Decr
            | TokenType::Star => {
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

            BinaryOp::In => Precedence::Comparison,

            BinaryOp::Equal | BinaryOp::NotEqual => Precedence::Equality,

            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                Precedence::Comparison
            }

            BinaryOp::Add | BinaryOp::Subtract => Precedence::Term,

            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => Precedence::Factor,

            BinaryOp::Exponent => Precedence::Exponent,
        };
        Ok(precedence)
    }

    /// Check if a field type is a direct generic parameter (e.g., T, U, not List<T>)
    fn parse_field_declaration(
        &mut self,
        type_param_names: &[(String, Vec<TraitBound>)],
    ) -> ParserResult<Field> {
        // Check if this is a const field
        let is_const = if self.check(TokenType::Const) {
            self.consume();
            true
        } else {
            false
        };

        let field_type = self.parse_type()?;
        let field_name = self.consume_identifier("Expected field name")?;

        // Check for optional default value
        let default_value = if self.matches(&[TokenType::Eq]) {
            let expr = self.parse_primary()?;

            // Validate it's a literal expression
            if !Self::is_literal_expression(&expr) {
                return Err(ParserError::new(
                    "Field default values must be literals (int, float, string, bool, char)",
                    expr.span,
                ));
            }
            Some(expr)
        } else {
            None
        };

        // For const fields, require a default value
        if is_const && default_value.is_none() {
            return Err(ParserError::new(
                "Const fields must have a default value",
                self.previous().span,
            ));
        }

        let is_generic_param = Self::is_field_generic_param(&field_type, type_param_names);
        Ok(Field {
            name: field_name,
            type_: field_type,
            is_generic_param,
            is_const,
            default_value,
        })
    }

    fn is_field_generic_param(
        field_type: &TypeNode,
        type_param_names: &[(String, Vec<TraitBound>)],
    ) -> bool {
        match &field_type.kind {
            TypeKind::Named(name, type_args) => {
                // A field is a generic parameter if:
                // 1. It has no type arguments (e.g., T not T<int>)
                // 2. Its name matches a type parameter (e.g., T or U)
                type_args.is_empty()
                    && type_param_names
                        .iter()
                        .any(|(param_name, _)| param_name == name)
            }
            _ => false,
        }
    }

    fn is_literal_expression(expr: &ExpressionNode) -> bool {
        matches!(expr.kind, ExpressionKind::Literal(_))
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

    #[allow(unused)]
    pub fn with_help(message: impl Into<String>, span: Span, help: impl Into<String>) -> Self {
        Self {
            message: format!("{}\n  = help: {}", message.into(), help.into()),
            span,
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
        span: Span,
    },
    Interface {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        fields: Vec<Field>,
        methods: Vec<FunctionNode>,
        span: Span,
    },
    Enum {
        name: String,
        type_params: Vec<(String, Vec<TraitBound>)>,
        variants: Vec<EnumVariant>,
        span: Span,
    },
    Statement(StatementNode),
}

impl Spanned for AstNode {
    fn span(&self) -> &Span {
        match self {
            AstNode::Function(func) => &func.span,
            AstNode::Class { span, .. } => span,
            AstNode::Interface { span, .. } => span,
            AstNode::Enum { span, .. } => span,
            AstNode::Statement(stmt) => stmt.span(),
        }
    }
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
    // note, we only convert statement and function variants to statements.
    // class, interface, and enum are top-level declarations in the language
    // and cannot appear as statements, they must be handled separately.
    pub fn into_statement(self) -> Option<StatementNode> {
        match self {
            AstNode::Statement(stmt) => Some(stmt),
            AstNode::Function(func) => {
                let span = func.span;
                Some(StatementNode {
                    kind: StatementKind::Function(func),
                    span,
                })
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportSpec {
    // import logger (as alias)
    Module {
        alias: Option<String>,
    },
    // import logger.log (as alias)
    Item {
        item: String,
        alias: Option<String>,
    },
    // import logger.(log, error, warn as w)
    Items {
        items: Vec<(String, Option<String>)>,
    },
    // import logger.*
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    AutoDecl(String, TypeNode, ExpressionNode),
    TypedDecl(String, TypeNode, ExpressionNode),
    ConstDecl(String, TypeNode, ExpressionNode),
    Function(FunctionNode),
    Import {
        module_path: String,
        spec: ImportSpec,
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
        var_type: TypeNode,
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
    None,
    Identifier(String),
    Binary {
        left: Box<ExpressionNode>,
        op: BinaryOp,
        op_span: Span,
        right: Box<ExpressionNode>,
    },
    Unary {
        op: UnaryOp,
        op_span: Span,
        expr: Box<ExpressionNode>,
        postfix: bool,
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
    MapLiteral {
        key_type: Box<TypeNode>,
        value_type: Box<TypeNode>,
        entries: Vec<(ExpressionNode, ExpressionNode)>,
    },
    SetLiteral(Vec<ExpressionNode>),
    If {
        cond: Box<ExpressionNode>,
        then_expr: Box<ExpressionNode>,
        else_expr: Box<ExpressionNode>,
    },
    Lambda {
        params: Vec<Param>,
        return_type: TypeNode,
        body: Vec<StatementNode>,
    },
    GenericType(String, Vec<TypeNode>),
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
    Set(Box<TypeNode>),

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
                "string" => Ok(PrimitiveType::Str),
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
    pub default_value: Option<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_: TypeNode,
    pub is_generic_param: bool,
    pub is_const: bool,
    pub default_value: Option<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionNode {
    pub name: String,
    pub type_params: Vec<(String, Vec<TraitBound>)>,
    pub params: Vec<Param>,
    pub return_type: TypeNode,
    pub body: Vec<StatementNode>,
    pub span: Span,
    pub is_common: bool,
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

    EnumVariant {
        name: String,
        args: Vec<PatternNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Exponent,

    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    LogicalAnd,
    LogicalOr,

    In,

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
        matches!(self, BinaryOp::Exponent) || self.is_assignment()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
    Incr,
    Decr,
    Deref,
}

impl UnaryOp {
    pub fn parse(token: Token) -> ParserResult<UnaryOp> {
        match token.token_type {
            TokenType::Minus => Ok(UnaryOp::Neg),
            TokenType::Bang => Ok(UnaryOp::Not),
            TokenType::Ref => Ok(UnaryOp::Ref),
            TokenType::Incr => Ok(UnaryOp::Incr),
            TokenType::Decr => Ok(UnaryOp::Decr),
            TokenType::Star => Ok(UnaryOp::Deref),
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

            Self { parser }
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
        let parse_result = test_parser.parse();

        match parse_result {
            Ok(nodes) => {
                let mut statements = Vec::new();
                for node in nodes {
                    if let Some(stmt) = node.into_statement() {
                        statements.push(stmt);
                    }
                }
                statements
            }
            Err((nodes, errors)) => {
                eprintln!("Parse errors: {:?}", errors);
                nodes
                    .into_iter()
                    .filter_map(|node| node.into_statement())
                    .collect()
            }
        }
    }

    #[test]
    fn test_span_propagation() {
        // using a simpler input since the function parsing is more complex.
        let input = r#"
        auto x = 42
        auto y = "hello"
        "#;

        let stmts = parse_stmts(input);
        assert!(!stmts.is_empty(), "No statements were parsed");
        assert!(stmts.len() >= 2, "Expected at least 2 statements");

        // Test first variable declaration span
        if let StatementKind::AutoDecl(_, _, expr) = &stmts[0].kind {
            assert_eq!(stmts[0].span.row_start, 2);
            assert!(stmts[0].span.col_start > 0);

            // The expression should have its own span
            assert_eq!(expr.span.row_start, 2);
            assert!(expr.span.col_start > 0);

            if let ExpressionKind::Literal(lit) = &expr.kind {
                // The literal value should be 42
                if let LiteralNode::Integer(val) = lit {
                    assert_eq!(*val, 42);
                } else {
                    panic!("Expected Integer literal, got {:?}", lit);
                }
            } else {
                panic!("Expected literal expression, got {:?}", expr.kind);
            }
        } else {
            panic!("Expected AutoDecl statement, got {:?}", stmts[0].kind);
        }

        // Test second variable declaration with string literal
        if let StatementKind::AutoDecl(_, _, expr) = &stmts[1].kind {
            assert_eq!(stmts[1].span.row_start, 3);
            assert!(stmts[1].span.col_start > 0);

            if let ExpressionKind::Literal(lit) = &expr.kind {
                assert_eq!(expr.span.row_start, 3);
                assert!(expr.span.col_start > 0);

                if let LiteralNode::String(s) = lit {
                    assert_eq!(s, "hello");
                } else {
                    panic!("Expected String literal, got {:?}", lit);
                }
            } else {
                panic!("Expected literal expression, got {:?}", expr.kind);
            }
        } else {
            panic!("Expected AutoDecl statement, got {:?}", stmts[1].kind);
        }
    }

    #[test]
    fn test_binary_expression_span() {
        let input = "1 + 2 * 3";
        let expr = parse_expr(input);

        // The binary expression should span the entire input
        if let ExpressionKind::Binary {
            left, op: _, right, ..
        } = &expr.kind
        {
            // The entire expression should span from the start of the first token to the end of the last token
            assert_eq!(expr.span.row_start, 1);
            assert_eq!(expr.span.col_start, 1);
            assert_eq!(expr.span.col_end, Some(10)); // 1-based, inclusive of last character

            // The left and right operands should have their own spans
            assert_eq!(left.span.row_start, 1);
            assert_eq!(left.span.col_start, 1);
            assert_eq!(right.span.row_start, 1);
            assert!(right.span.col_start > left.span.col_start);
        } else {
            panic!("Expected binary expression, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_function_call_span() {
        let input = "add(1, 2 + 3)";
        let expr = parse_expr(input);

        // The function call should span the entire input
        if let ExpressionKind::Call { func, args } = &expr.kind {
            assert_eq!(expr.span.row_start, 1);
            assert_eq!(expr.span.col_start, 1);

            // The function name should have its own span
            assert_eq!(func.span.row_start, 1);
            assert_eq!(func.span.col_start, 1);

            // Arguments should have their own spans
            assert_eq!(args.len(), 2);
            assert_eq!(args[0].span.row_start, 1);
            assert_eq!(args[0].span.col_start, 5);

            // The second argument is a binary expression
            if let ExpressionKind::Binary {
                left, op: _, right, ..
            } = &args[1].kind
            {
                assert_eq!(left.span.row_start, 1);
                assert_eq!(left.span.col_start, 8);
                assert_eq!(right.span.row_start, 1);
                assert_eq!(right.span.col_start, 12);
            } else {
                panic!(
                    "Expected binary expression as second argument, got {:?}",
                    args[1].kind
                );
            }
        } else {
            panic!("Expected function call expression, got {:?}", expr.kind);
        }
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
            }
            Err((nodes, errors)) => {
                println!(
                    "Parse failed with nodes: {:?} and errors: {:?}",
                    nodes, errors
                );
                panic!("Failed to parse multiple statements with newlines");
            }
        }

        let stmts = parse_stmts("auto x = 1\n\nauto y = 2\n");
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn test_missing_newline_error() {
        let source = "auto x = 1 auto y = 2";
        let mut test_parser = create_parser(source);
        let result = test_parser.parse();

        match result {
            Ok(nodes) => {
                panic!(
                    "Expected an error about missing newline, but got successful parse with nodes: {:?}",
                    nodes
                );
            }
            Err((_, errors)) => {
                assert!(!errors.is_empty(), "Expected errors but got none");
                let has_newline_error = errors.iter().any(|e| {
                    let msg_lower = e.message.to_lowercase();
                    msg_lower.contains("expected newline after statement")
                        || msg_lower.contains("missing newline")
                        || msg_lower.contains("expected newline")
                });

                if !has_newline_error {
                    panic!(
                        "Expected an error about missing newline, but got: {:?}",
                        errors
                    );
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
        let block_expr = parse_stmts("{\n  auto x = 1\n  auto y = 2\n}\n");
        assert!(
            !block_expr.is_empty(),
            "Failed to parse block expression with newlines"
        );

        let single_stmt = parse_stmts("auto x = 1\n");
        assert!(
            !single_stmt.is_empty(),
            "Failed to parse single statement with newline"
        );

        let multi_stmt = parse_stmts("auto x = 1\n\nauto y = 2\n");
        assert_eq!(
            multi_stmt.len(),
            2,
            "Expected 2 statements with newline separation"
        );

        let block_with_empty_lines = parse_stmts("{\n  auto x = 1\n  \n  auto y = 2\n}\n");
        assert!(
            !block_with_empty_lines.is_empty(),
            "Failed to parse block with empty lines"
        );
    }

    #[test]
    fn test_control_flow() {
        let stmts = parse_stmts("if x {\n  auto y = 1\n}\n");
        assert!(!stmts.is_empty());
    }

    #[test]
    fn test_function_declaration() {
        let mut test_parser = create_parser("func add(int a, int b) returns int {\n  a + b\n}\n");
        let result = test_parser.parse();

        match result {
            Ok(nodes) => {
                assert!(!nodes.is_empty());

                if let AstNode::Function(func) = &nodes[0] {
                    assert_eq!(func.name, "add");
                    assert_eq!(func.params.len(), 2);
                    assert_eq!(
                        func.return_type.kind,
                        TypeKind::Primitive(PrimitiveType::Int)
                    );
                } else {
                    panic!("Expected a function node, got {:?}", nodes[0]);
                }
            }
            Err((nodes, errors)) => {
                if !nodes.is_empty() {
                    if let AstNode::Function(func) = &nodes[0] {
                        assert_eq!(func.name, "add");
                        assert_eq!(func.params.len(), 2);
                        assert_eq!(
                            func.return_type.kind,
                            TypeKind::Primitive(PrimitiveType::Int)
                        );
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
            }
            Err((nodes, errors)) => {
                if nodes.is_empty() {
                    panic!("Failed to parse function: {:?}", errors);
                }
            }
        }

        let mut test_parser =
            create_parser("fn greet(string name, int times = 1) {\n  return 0\n}\n");
        let result = test_parser.parse();

        match result {
            Ok(nodes) => {
                assert!(!nodes.is_empty(), "Expected at least one statement");
            }
            Err((nodes, errors)) => {
                if nodes.is_empty() {
                    panic!("Failed to parse function: {:?}", errors);
                }
            }
        }
    }

    #[test]
    fn test_error_recovery() {
        let source = r#"
            let x = 5
            auto y = 10

            func foo() {
                return 42
            }

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

        // We expect an error due to the invalid number format
        if let Err((nodes, errors)) = result {
            assert!(!errors.is_empty(), "Expected at least one error");

            // Check that we have valid nodes despite the errors
            assert!(!nodes.is_empty(), "Expected some valid nodes to be parsed");
            println!("Successfully parsed {} nodes despite errors", nodes.len());

            // Check that we have at least some valid nodes
            assert!(
                nodes.len() >= 2,
                "Expected at least 2 valid nodes to be parsed"
            );

            let mut found_expected_error = false;
            for error in &errors {
                println!("Found error: {} at {:?}", error.message, error.span);

                if error.message.contains("Invalid number format")
                    || error.message.contains("unknown escape sequence")
                    || error.message.contains("Expected expression")
                {
                    found_expected_error = true;
                }

                assert!(
                    !error.message.is_empty(),
                    "Expected a non-empty error message"
                );
            }

            assert!(
                found_expected_error,
                "Expected to find an error about invalid syntax"
            );
        } else {
            panic!("Expected parsing to fail with errors");
        }

        // Test recovery in the middle of expressions
        let source = "auto x = 10 + * 5 - / 3\nauto y = 20\n";
        let mut parser = create_parser(source);
        let result = parser.parse();

        // This should also fail but not panic
        assert!(result.is_err());
    }
}
