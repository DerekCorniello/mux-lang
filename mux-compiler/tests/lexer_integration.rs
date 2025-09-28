use TokenType::*;
use insta::assert_snapshot;
use mux_compiler::lexer::{Lexer, TokenType};
use mux_compiler::source::Source;
use ordered_float::OrderedFloat;
use std::fs;
use std::option::Option::Some;

#[test]
#[rustfmt::skip]
fn test_file_lexer() {
    let passing_cases = [
        (
            "collections.mux",
            vec![
                LineComment("Collections".to_string()), NewLine,
                Auto, Id("nums".to_string()), Eq, OpenBracket, Int(1), Comma, Int(2), Comma, Int(3), Comma, Int(4), CloseBracket, NewLine,
                Auto, Id("scores".to_string()), Eq, Id("map".to_string()), Lt, Id("string".to_string()), Comma, Id("int".to_string()), Gt, OpenBrace, Str("Alice".to_string()), Colon, Int(90), Comma, Str("Bob".to_string()), Colon, Int(85), CloseBrace, NewLine,
                Auto, Id("matrix".to_string()), Eq, OpenBracket, OpenBracket, Int(1), Comma, Int(2), CloseBracket, Comma, OpenBracket, Int(3), Comma, Int(4), CloseBracket, CloseBracket, NewLine,
                Auto, Id("emptyList".to_string()), Eq, Id("list".to_string()), Lt, Id("int".to_string()), Gt, OpenParen, CloseParen, NewLine,
                Auto, Id("emptyMap".to_string()), Eq, Id("map".to_string()), Lt, Id("string".to_string()), Comma, Id("int".to_string()), Gt, OpenParen, CloseParen, NewLine,
            ]

        ),
        (
            "control_flow.mux",
            vec![
                LineComment("Control flow".to_string()), NewLine,
                Auto, Id("x".to_string()), Eq, Int(5), NewLine,
                If, Id("x".to_string()), Gt, Int(0), OpenBrace, Id("print".to_string()), OpenParen, Str("Positive".to_string()), CloseParen, CloseBrace,
                Else, OpenBrace, Id("print".to_string()), OpenParen, Str("Non-positive".to_string()), CloseParen, CloseBrace, NewLine, NewLine,
                Match, Id("Some".to_string()), OpenParen, Int(15), CloseParen, OpenBrace, NewLine,
                Id("Some".to_string()), OpenParen, Id("v".to_string()), CloseParen, If, Id("v".to_string()), Gt, Int(10), OpenBrace, 
                    Id("print".to_string()), OpenParen, Str("Large: ".to_string()), Plus, Id("v".to_string()), CloseParen, CloseBrace, NewLine,
                Id("Some".to_string()), OpenParen, Id("v".to_string()), CloseParen, OpenBrace, 
                    Id("print".to_string()), OpenParen, Str("Small: ".to_string()), Plus, Id("v".to_string()), CloseParen, CloseBrace, NewLine,
                Id("None".to_string()), OpenBrace, Id("print".to_string()), OpenParen, Str("None".to_string()), CloseParen, CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,
                For, Id("int".to_string()), Id("i".to_string()), In, Id("range".to_string()), OpenParen, Int(0), Comma, Int(3), CloseParen, OpenBrace, 
                    Id("print".to_string()), OpenParen, Id("i".to_string()), CloseParen, CloseBrace, NewLine,
                While, Id("x".to_string()), Gt, Int(0), OpenBrace, 
                    Id("x".to_string()), MinusEq, Int(1), CloseBrace, NewLine,
            ]

        ),
        (
            "enums_classes.mux",
            vec![
                LineComment("Enums and Classes".to_string()), NewLine,
                Enum, Id("Shape".to_string()), OpenBrace, 
                    Id("Circle".to_string()), OpenParen, Id("float".to_string()), Id("r".to_string()), CloseParen, 
                    Comma, 
                    Id("Rectangle".to_string()), OpenParen, Id("float".to_string()), Id("w".to_string()), Comma, Id("float".to_string()), Id("h".to_string()), CloseParen, 
                CloseBrace, NewLine, NewLine,
                
                Interface, Id("Drawable".to_string()), OpenBrace, 
                    Func, Id("draw".to_string()), OpenParen, CloseParen, Returns, Id("void".to_string()), 
                CloseBrace, NewLine, NewLine,
                
                Class, Id("Circle".to_string()), Is, Id("Drawable".to_string()), OpenBrace, NewLine,
                    Id("float".to_string()), Id("radius".to_string()), NewLine,
                    Func, Id("draw".to_string()), OpenParen, CloseParen, Returns, Id("void".to_string()), 
                        OpenBrace, 
                            Id("print".to_string()), OpenParen, Str("Circle radius=".to_string()), Plus, Id("radius".to_string()), CloseParen, 
                        CloseBrace, NewLine,
                CloseBrace, NewLine,
            ]

        ),
        (
            "functions.mux",
            vec![
                LineComment("Functions and lambdas".to_string()), NewLine,
                Func, Id("add".to_string()), OpenParen, 
                    Id("int".to_string()), Id("a".to_string()), Comma, 
                    Id("int".to_string()), Id("b".to_string()),
                CloseParen, Returns, Id("int".to_string()), OpenBrace, NewLine,
                    Return, Id("a".to_string()), Plus, Id("b".to_string()), NewLine,
                CloseBrace, NewLine, NewLine,
                
                Func, Id("greet".to_string()), OpenParen, 
                    Id("string".to_string()), Id("name".to_string()), Comma, 
                    Id("int".to_string()), Id("times".to_string()), Eq, Int(1),
                CloseParen, Returns, Id("void".to_string()), OpenBrace, NewLine,
                    For, Id("i".to_string()), In, Id("range".to_string()), OpenParen, 
                        Int(0), Comma, Id("times".to_string()),
                    CloseParen, OpenBrace, 
                        Id("print".to_string()), OpenParen, Str("Hello ".to_string()), Plus, Id("name".to_string()), CloseParen, 
                    CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,
                
                LineComment("Lambda examples".to_string()), NewLine,
                Auto, Id("square".to_string()), Eq, 
                    Func, OpenParen, Id("int".to_string()), Id("n".to_string()), CloseParen, 
                    OpenBrace, Return, Id("n".to_string()), Star, Id("n".to_string()), CloseBrace, NewLine,
                    
                Auto, Id("doubled".to_string()), Eq, 
                    Func, OpenParen, Auto, Id("x".to_string()), CloseParen, 
                    OpenBrace, Return, Id("x".to_string()), Star, Int(2), CloseBrace, NewLine,
            ]
        ),
        (
            "generics.mux",
            vec![
                LineComment("Generics".to_string()), NewLine,
                Func, Id("max".to_string()), OpenBracket, Id("T".to_string()), Id("comparable".to_string()), CloseBracket,
                OpenParen, Id("T".to_string()), Id("a".to_string()), Comma, Id("T".to_string()), Id("b".to_string()), CloseParen,
                Returns, Id("T".to_string()), OpenBrace, 
                    If, Id("a".to_string()), Gt, Id("b".to_string()), OpenBrace, 
                        Return, Id("a".to_string()), 
                    CloseBrace, 
                    Return, Id("b".to_string()), 
                CloseBrace, NewLine, NewLine,

                Class, Id("Stack".to_string()), OpenBracket, Id("T".to_string()), CloseBracket, OpenBrace, NewLine,
                    Id("list".to_string()), Lt, Id("T".to_string()), Gt, Id("items".to_string()), NewLine,
                    
                    Func, Id("push".to_string()), OpenParen, Id("T".to_string()), Id("item".to_string()), CloseParen, 
                        Returns, Id("void".to_string()), 
                        OpenBrace, 
                            Id("items".to_string()), Dot, Id("append".to_string()), OpenParen, Id("item".to_string()), CloseParen, 
                        CloseBrace, NewLine,
                        
                    Func, Id("pop".to_string()), OpenParen, CloseParen, 
                        Returns, Id("Optional".to_string()), Lt, Id("T".to_string()), Gt, 
                        OpenBrace, NewLine,
                        If, Id("items".to_string()), Dot, Id("isEmpty".to_string()), OpenParen, CloseParen, OpenBrace, 
                            Return, Id("None".to_string()), 
                        CloseBrace, NewLine,
                        Return, Id("Some".to_string()), OpenParen, Id("items".to_string()), Dot, Id("removeLast".to_string()), OpenParen, CloseParen, CloseParen, 
                        NewLine, CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,

                Auto, Id("s".to_string()), Eq, Id("Stack".to_string()), Lt, Id("int".to_string()), Gt, OpenParen, CloseParen, NewLine,
                Id("s".to_string()), Dot, Id("push".to_string()), OpenParen, Int(42), CloseParen, NewLine,
            ],
        ),

        (
            "imports.mux",
            vec![
                LineComment("Imports".to_string()), NewLine,
                Import, Id("math".to_string()), NewLine,
                Import, Id("utils".to_string()), Dot, Id("logger".to_string()), As, Underscore, NewLine, NewLine,

                Auto, Id("pi".to_string()), Eq, Id("math".to_string()), Dot, Id("PI".to_string()), NewLine,
                Id("print".to_string()), OpenParen, Str("PI = ".to_string()), Plus, Id("pi".to_string()), CloseParen, NewLine,
            ],
        ),

        (
            "optionals_results.mux",
            vec![
                LineComment("Optionals & Results".to_string()), NewLine,
                Func, Id("divide".to_string()), OpenParen, Id("int".to_string()), Id("a".to_string()), Comma, Id("int".to_string()), Id("b".to_string()), CloseParen, 
                Returns, Id("Result".to_string()), Lt, Id("int".to_string()), Comma, Id("string".to_string()), Gt, 
                OpenBrace, NewLine,
                    If, Id("b".to_string()), EqEq, Int(0), OpenBrace, Return, Id("Err".to_string()), OpenParen, Str("Division by zero".to_string()), CloseParen, CloseBrace, NewLine,
                    Return, Id("Ok".to_string()), OpenParen, Id("a".to_string()), Slash, Id("b".to_string()), CloseParen, NewLine,
                CloseBrace, NewLine, NewLine,

                Auto, Id("res".to_string()), Eq, Id("divide".to_string()), OpenParen, Int(10), Comma, Int(2), CloseParen, NewLine,
                Match, Id("res".to_string()), OpenBrace, NewLine,
                    Id("Ok".to_string()), OpenParen, Id("v".to_string()), CloseParen, OpenBrace, Id("print".to_string()), OpenParen, Str("Result: ".to_string()), Plus, Id("v".to_string()), CloseParen, CloseBrace, NewLine,
                    Id("Err".to_string()), OpenParen, Id("e".to_string()), CloseParen, OpenBrace, Id("print".to_string()), OpenParen, Str("Error: ".to_string()), Plus, Id("e".to_string()), CloseParen, CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,

                Func, Id("findEven".to_string()), OpenParen, Id("list".to_string()), Lt, Id("int".to_string()), Gt, Id("nums".to_string()), CloseParen, 
                Returns, Id("Optional".to_string()), Lt, Id("int".to_string()), Gt, 
                OpenBrace, NewLine,
                    For, Id("n".to_string()), In, Id("nums".to_string()), OpenBrace, 
                        If, Id("n".to_string()), Percent, Int(2), EqEq, Int(0), OpenBrace, 
                            Return, Id("Some".to_string()), OpenParen, Id("n".to_string()), CloseParen, 
                        CloseBrace, 
                    CloseBrace, NewLine,
                CloseBrace, NewLine,
            ],
        ),

        (
            "references.mux",
            vec![
                LineComment("References".to_string()), NewLine,
                Id("int".to_string()), Id("val".to_string()), Eq, Int(10), NewLine,
                Auto, Id("r".to_string()), Eq, Ref, Id("val".to_string()), NewLine,
                Id("print".to_string()), OpenParen, Id("r".to_string()), CloseParen, NewLine,
                Id("r".to_string()), Eq, Int(20), NewLine,
                Id("print".to_string()), OpenParen, Id("val".to_string()), CloseParen, NewLine, NewLine,

                // References to list elements
                LineComment("References to list elements".to_string()), NewLine,
                Auto, Id("numbers".to_string()), Eq, OpenBracket, Int(1), Comma, Int(2), Comma, Int(3), Comma, Int(4), Comma, Int(5), CloseBracket, NewLine,
                Auto, Id("first".to_string()), Eq, Ref, Id("numbers".to_string()), OpenBracket, Int(0), CloseBracket, NewLine,
                Id("print".to_string()), OpenParen, Id("first".to_string()), CloseParen, NewLine, NewLine,

                // Function taking a reference
                LineComment("Function taking a reference".to_string()), NewLine,
                Func, Id("update".to_string()), OpenParen, Id("ref".to_string()), Colon, Ref, Id("int".to_string()), CloseParen, OpenBrace, NewLine,
                Id("ref".to_string()), Eq, Id("ref".to_string()), Plus, Int(1), NewLine,
                CloseBrace, NewLine, NewLine,

                Id("update".to_string()), OpenParen, Ref, Id("val".to_string()), CloseParen, NewLine,
                Id("print".to_string()), OpenParen, Id("val".to_string()), CloseParen, NewLine,
            ],
        ),

        (
            "variables.mux",
            vec![
                LineComment("Variable declarations and type inference".to_string()), NewLine,
                Const, Id("int".to_string()), Id("MAX".to_string()), Eq, Int(100), NewLine,
                Id("int".to_string()), Id("explicit".to_string()), Eq, Int(42), NewLine,
                Auto, Id("inferred".to_string()), Eq, Float(OrderedFloat(4.89)), NewLine,
                Auto, Id("name".to_string()), Eq, Str("Mux".to_string()), NewLine, NewLine,
                Auto, OpenParen, Id("x".to_string()), Comma, Underscore, CloseParen, Eq, OpenParen, Int(1), Comma, Int(2), CloseParen, LineComment("underscore usage".to_string()), NewLine,
                Auto, Id("flag".to_string()), Eq, Bool(true), NewLine,
                Auto, Id("otherflag".to_string()), Eq, Bool(false), NewLine,
            ],
        ),
        (
            "full_test.mux",
            vec![
                Import, Id("math".to_string()), NewLine,
                Import, Id("utils".to_string()), Dot, Id("logger".to_string()), As, Underscore, NewLine, NewLine,

                Const, Id("int".to_string()), Id("MAX".to_string()), Eq, Int(100), NewLine,
                Auto, Id("flt".to_string()), Eq, Float(OrderedFloat(7.82)), NewLine,
                Auto, Id("message".to_string()), Eq, Str("Mux Parser Test".to_string()), NewLine, NewLine,

                Enum, Id("Shape".to_string()), OpenBrace, Id("Circle".to_string()), OpenParen, Id("float".to_string()), Id("radius".to_string()), CloseParen, Comma, Id("Rectangle".to_string()), OpenParen, Id("float".to_string()), Id("width".to_string()), Comma, Id("float".to_string()), Id("height".to_string()), CloseParen, Comma, Id("Square".to_string()), OpenParen, Id("float".to_string()), Id("size".to_string()), CloseParen, CloseBrace, NewLine, NewLine,

                Interface, Id("Drawable".to_string()), OpenBrace, Func, Id("draw".to_string()), OpenParen, CloseParen, Returns, Id("void".to_string()), CloseBrace, NewLine, NewLine,

                Class, Id("Circle".to_string()), Is, Id("Drawable".to_string()), Comma, Id("ShapeLike".to_string()), OpenBrace, NewLine,
                Id("float".to_string()), Id("radius".to_string()), NewLine,
                Func, Id("draw".to_string()), OpenParen, CloseParen, Returns, Id("void".to_string()), OpenBrace, Id("print".to_string()), OpenParen, Str("Circle radius=".to_string()), Plus, Id("radius".to_string()), CloseParen, CloseBrace, NewLine,
                Func, Id("area".to_string()), OpenParen, CloseParen, Returns, Id("float".to_string()), OpenBrace, Return, Id("pi".to_string()), Star, Id("radius".to_string()), Star, Id("radius".to_string()), CloseBrace, NewLine,
                Func, Id("resize".to_string()), OpenParen, Id("float".to_string()), Id("newRadius".to_string()), Comma, Id("string".to_string()), Underscore, CloseParen, Returns, Id("void".to_string()), OpenBrace, Id("radius".to_string()), Eq, Id("newRadius".to_string()), CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,

                Func, Id("max".to_string()), OpenBracket, Id("T".to_string()), Id("comparable".to_string()), CloseBracket, OpenParen, Id("T".to_string()), Id("a".to_string()), Comma, Id("T".to_string()), Id("b".to_string()), CloseParen, Returns, Id("T".to_string()), OpenBrace, If, Id("a".to_string()), Gt, Id("b".to_string()), OpenBrace, Return, Id("a".to_string()), CloseBrace, Return, Id("b".to_string()), CloseBrace, NewLine,
                Class, Id("Stack".to_string()), OpenBracket, Id("T".to_string()), CloseBracket, OpenBrace, NewLine,
                Id("list".to_string()), Lt, Id("T".to_string()), Gt, Id("items".to_string()), NewLine,
                Func, Id("push".to_string()), OpenParen, Id("T".to_string()), Id("item".to_string()), CloseParen, Returns, Id("void".to_string()), OpenBrace, Id("items".to_string()), Dot, Id("append".to_string()), OpenParen, Id("item".to_string()), CloseParen, CloseBrace, NewLine,
                Func, Id("pop".to_string()), OpenParen, CloseParen, Returns, Id("Optional".to_string()), Lt, Id("T".to_string()), Gt, OpenBrace, NewLine,
                If, Id("items".to_string()), Dot, Id("isEmpty".to_string()), OpenParen, CloseParen, OpenBrace, Return, Id("None".to_string()), CloseBrace, NewLine,
                Return, Id("Some".to_string()), OpenParen, Id("items".to_string()), Dot, Id("removeLast".to_string()), OpenParen, CloseParen, CloseParen, NewLine,
                CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,

                Func, Id("divide".to_string()), OpenParen, Id("int".to_string()), Id("a".to_string()), Comma, Id("int".to_string()), Id("b".to_string()), CloseParen, Returns, Id("Result".to_string()), Lt, Id("int".to_string()), Comma, Id("string".to_string()), Gt, OpenBrace, NewLine,
                If, Id("b".to_string()), EqEq, Int(0), OpenBrace, Return, Id("Err".to_string()), OpenParen, Str("Division by zero".to_string()), CloseParen, CloseBrace, NewLine,
                Return, Id("Ok".to_string()), OpenParen, Id("a".to_string()), Slash, Id("b".to_string()), CloseParen, NewLine,
                CloseBrace, NewLine,
                Func, Id("findEven".to_string()), OpenParen, Id("list".to_string()), Lt, Id("int".to_string()), Gt, Id("nums".to_string()), CloseParen, Returns, Id("Optional".to_string()), Lt, Id("int".to_string()), Gt, OpenBrace, NewLine,
                For, Id("n".to_string()), In, Id("nums".to_string()), OpenBrace, If, Id("n".to_string()), Percent, Int(2), EqEq, Int(0), OpenBrace, Return, Id("Some".to_string()), OpenParen, Id("n".to_string()), CloseParen, CloseBrace, CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,

                Func, Id("map".to_string()), OpenBracket, Id("T".to_string()), Comma, Id("U".to_string()), CloseBracket, OpenParen, Id("list".to_string()), Lt, Id("T".to_string()), Gt, Id("items".to_string()), Comma, Func, OpenParen, Id("T".to_string()), CloseParen, Returns, Id("U".to_string()), Id("transform".to_string()), CloseParen, Returns, Id("list".to_string()), Lt, Id("U".to_string()), Gt, OpenBrace, NewLine,
                Auto, Id("out".to_string()), Eq, Id("list".to_string()), Lt, Id("U".to_string()), Gt, OpenParen, CloseParen, NewLine,
                For, Id("item".to_string()), In, Id("items".to_string()), OpenBrace, Id("out".to_string()), Dot, Id("append".to_string()), OpenParen, Id("transform".to_string()), OpenParen, Id("item".to_string()), CloseParen, CloseParen, CloseBrace, NewLine,
                Return, Id("out".to_string()), NewLine,
                CloseBrace, NewLine, NewLine,

                Func, Id("main".to_string()), OpenParen, CloseParen, Returns, Id("void".to_string()), OpenBrace, NewLine,
                Auto, Id("explicit".to_string()), Eq, Int(10), NewLine,
                Auto, Id("inferred".to_string()), Eq, Int(42), NewLine,
                Auto, Id("floatVal".to_string()), Eq, Float(OrderedFloat(9.4)), NewLine,
                Auto, OpenParen, Id("a".to_string()), Comma, Underscore, CloseParen, Eq, OpenParen, Int(1), Comma, Int(2), CloseParen, NewLine, NewLine,

                If, Id("inferred".to_string()), Gt, Int(40), OpenBrace, Id("print".to_string()), OpenParen, Str("Big number".to_string()), CloseParen, CloseBrace, Else, OpenBrace, Id("print".to_string()), OpenParen, Str("Small number".to_string()), CloseParen, CloseBrace, NewLine,
 NewLine,
                Auto, Id("maybeNum".to_string()), Eq, Id("Some".to_string()), OpenParen, Int(15), CloseParen, NewLine,
                Match, Id("maybeNum".to_string()), OpenBrace, NewLine,
                Id("Some".to_string()), OpenParen, Id("v".to_string()), CloseParen, If, Id("v".to_string()), Gt, Int(10), OpenBrace, Id("print".to_string()), OpenParen, Str("Large value: ".to_string()), Plus, Id("v".to_string()), CloseParen, CloseBrace, NewLine,
                Id("Some".to_string()), OpenParen, Id("v".to_string()), CloseParen, OpenBrace, Id("print".to_string()), OpenParen, Str("Small value: ".to_string()), Plus, Id("v".to_string()), CloseParen, CloseBrace, NewLine,
                Id("None".to_string()), OpenBrace, Id("print".to_string()), OpenParen, Str("No value".to_string()), CloseParen, CloseBrace, NewLine,
                Underscore, OpenBrace, Id("print".to_string()), OpenParen, Str("Unexpected".to_string()), CloseParen, CloseBrace, NewLine,
                CloseBrace, NewLine, NewLine,

                Auto, Id("nums".to_string()), Eq, OpenBracket, Int(1), Comma, Int(2), Comma, Int(3), CloseBracket, NewLine,
                Auto, Id("shapes".to_string()), Eq, OpenBracket, Id("Circle".to_string()), OpenParen, Float(OrderedFloat(2.0)), CloseParen, Comma, Id("Circle".to_string()), OpenParen, Float(OrderedFloat(3.5)), CloseParen, CloseBracket, NewLine,
                For, Id("shape".to_string()), In, Id("shapes".to_string()), OpenBrace, Id("shape".to_string()), Dot, Id("draw".to_string()), OpenParen, CloseParen, CloseBrace, NewLine, NewLine,

                Auto, Id("maximum".to_string()), Eq, Id("max".to_string()), OpenParen, Int(7), Comma, Int(12), CloseParen, NewLine,
                Auto, Id("stack".to_string()), Eq, Id("Stack".to_string()), Lt, Id("int".to_string()), Gt, OpenParen, CloseParen, NewLine,
                Id("stack".to_string()), Dot, Id("push".to_string()), OpenParen, Int(99), CloseParen, NewLine,
                Auto, Id("popped".to_string()), Eq, Id("stack".to_string()), Dot, Id("pop".to_string()), OpenParen, CloseParen, NewLine, NewLine,

                Auto, Id("res".to_string()), Eq, Id("divide".to_string()), OpenParen, Int(10), Comma, Int(2), CloseParen, NewLine,
                Match, Id("res".to_string()), OpenBrace, Id("Ok".to_string()), OpenParen, Id("v".to_string()), CloseParen, OpenBrace, Id("print".to_string()), OpenParen, Id("v".to_string()), CloseParen, CloseBrace, Id("Err".to_string()), OpenParen, Id("e".to_string()), CloseParen, OpenBrace, Id("print".to_string()), OpenParen, Id("e".to_string()), CloseParen, CloseBrace, CloseBrace, NewLine, NewLine,

                Auto, Id("maybeEven".to_string()), Eq, Id("findEven".to_string()), OpenParen, OpenBracket, Int(1), Comma, Int(3), Comma, Int(4), Comma, Int(7), CloseBracket, CloseParen, NewLine,
                Match, Id("maybeEven".to_string()), OpenBrace, Id("Some".to_string()), OpenParen, Id("v".to_string()), CloseParen, OpenBrace, Id("print".to_string()), OpenParen, Id("v".to_string()), CloseParen, CloseBrace, Id("None".to_string()), OpenBrace, Id("print".to_string()), OpenParen, Str("None".to_string()), CloseParen, CloseBrace, CloseBrace, NewLine, NewLine,

                Auto, Id("square".to_string()), Eq, Func, OpenParen, Id("int".to_string()), Id("n".to_string()), CloseParen, OpenBrace, Return, Id("n".to_string()), Star, Id("n".to_string()), CloseBrace, NewLine,
                Auto, Id("doubled".to_string()), Eq, Id("map".to_string()), OpenParen, OpenBracket, Int(1), Comma, Int(2), Comma, Int(3), CloseBracket, Comma, Func, OpenParen, Auto, Id("x".to_string()), CloseParen, OpenBrace, Return, Id("x".to_string()), Star, Int(2), CloseBrace, CloseParen, NewLine,
                Auto, Id("squared".to_string()), Eq, Id("map".to_string()), OpenParen, OpenBracket, Int(1), Comma, Int(2), Comma, Int(3), CloseBracket, Comma, Id("square".to_string()), CloseParen, NewLine, NewLine,

                Id("int".to_string()), Id("val".to_string()), Eq, Int(10), NewLine,
                Auto, Id("p".to_string()), Eq, Ref, Id("val".to_string()), NewLine,
                Star, Id("p".to_string()), Eq, Int(20), NewLine,
                CloseBrace, NewLine,
            ],
        )

    ];

    let test_dir = "../test_scripts";
    for entry in fs::read_dir(test_dir).expect("Failed to read test_scripts directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path(); // PathBuf
        println!("{}", path.display()); // prints the full path in a human-readable way
    }

    for entry in fs::read_dir(test_dir).expect("Failed to read test_scripts directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("mux") {
            let file_name = path.file_name().unwrap().to_str().unwrap();
            let file_path = path.to_str().unwrap();

            if let Some((_, expected_tokens)) =
                passing_cases.iter().find(|(name, _)| *name == file_name)
            {
                let mut src = Source::new(file_path)
                    .unwrap_or_else(|_| panic!("Failed to open source file: {}", file_name));

                let mut lexer = Lexer::new(&mut src);
                let actual_tokens = lexer
                    .lex_all()
                    .unwrap_or_else(|e| panic!("Lexing failed for file {}: {}", file_name, e));

                let actual_token_types: Vec<_> = actual_tokens.into_iter().map(|t| t.token_type).collect();
                assert_eq!(
                    actual_token_types, *expected_tokens,
                    "Token types did not match for file: {}",
                    file_name
                );
                assert_snapshot!(format!("{:#?}", actual_token_types.to_owned()));
                // TODO: need to test the full tokens as well with the positions
            }
        }
    }
}
