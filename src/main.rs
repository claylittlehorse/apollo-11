use std::{
    any,
    cmp::{max, min},
    process::id,
};

use axum::{Json, Router, http::StatusCode, response::Json as JsonResponse, routing::post};
use full_moon::{
    ShortString,
    ast::{
        self, Expression, FunctionCall, Index, Prefix, Suffix, Var, VarExpression,
        span::ContainedSpan,
    },
    node::Node,
    parse,
    tokenizer::{Symbol, Token, TokenKind, TokenReference, TokenType},
    visitors::VisitorMut,
};
use serde;

#[tokio::main]
async fn main() {
    let app = Router::new().route("/source", post(move_script));
    // .route("/ast", post(receive_ast));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8011")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}

fn match_token_literally(token_ref: &TokenReference, match_literal: &str) -> bool {
    match token_ref.token().token_type() {
        TokenType::Identifier { identifier } => **identifier == *match_literal,
        TokenType::StringLiteral {
            literal,
            multi_line_depth: _,
            quote_type: _,
        } => **literal == *match_literal,
        _ => false,
    }
}

fn is_script_identifier_in_expression_recursive(expression: &Expression) -> bool {
    match expression {
        Expression::Var(var) => match var {
            Var::Name(name) => match_token_literally(&name, "script"),
            Var::Expression(var_expr) => is_script_identifier(&*var_expr),
            _ => false,
        },
        Expression::UnaryOperator {
            expression,
            unop: _,
        } => is_script_identifier_in_expression_recursive(&*expression),
        Expression::TypeAssertion {
            expression,
            type_assertion: _,
        } => is_script_identifier_in_expression_recursive(&*expression),
        Expression::Parentheses {
            contained: _,
            expression,
        } => is_script_identifier_in_expression_recursive(&*expression),
        _ => false,
    }
}

fn is_script_identifier(var_expr: &VarExpression) -> bool {
    match var_expr.prefix() {
        Prefix::Name(name) => match_token_literally(name, "script"),
        Prefix::Expression(expression) => {
            is_script_identifier_in_expression_recursive(&**expression)
        }
        _ => false,
    }
}

fn is_script_identifier_fn(var_expr: &FunctionCall) -> bool {
    match var_expr.prefix() {
        Prefix::Name(name) => match_token_literally(name, "script"),
        Prefix::Expression(expression) => {
            is_script_identifier_in_expression_recursive(&**expression)
        }
        _ => false,
    }
}

fn get_index_identifier(index: &Index) -> Option<&str> {
    match index {
        Index::Dot { name, dot: _ } => match name.token().token_type() {
            TokenType::Identifier { identifier } => Some(identifier.as_str()),
            _ => None,
        },
        Index::Brackets {
            expression,
            brackets: _,
        } => {
            if let Expression::String(string) = expression {
                if let TokenType::StringLiteral {
                    literal,
                    multi_line_depth: _,
                    quote_type: _,
                } = string.token().token_type()
                {
                    return Some(literal.as_str());
                }
            };
            None
        }
        _ => None,
    }
}

enum AncestorResult {
    Some(i32),
    None,
    Identical,
}
fn find_closest_common_ancestor(path_a: &Vec<String>, path_b: &Vec<String>) -> AncestorResult {
    let (larger_path, smaller_path) = if path_a.len() > path_b.len() {
        (path_a, path_b)
    } else {
        (path_b, path_a)
    };

    let mut ancestor_index = -1;
    let mut are_identical = false;
    for (index, larger_name) in larger_path.iter().enumerate() {
        if smaller_path.len() < index + 1 {
            break;
        }

        if larger_name.as_str() == smaller_path[index].as_str() {
            ancestor_index = index as i32;
            if index == larger_path.len() - 1 {
                are_identical = true;
            }
        } else {
            break;
        }
    }

    match ancestor_index {
        _ if ancestor_index < 0 => AncestorResult::None,
        _ => match are_identical {
            true => AncestorResult::Identical,
            false => AncestorResult::Some(ancestor_index),
        },
    }
}

fn tk_ref_from_type(token_type: TokenType, trailing_trivia: Option<Vec<Token>>) -> TokenReference {
    TokenReference::new(
        Vec::new(),
        Token::new(token_type),
        trailing_trivia.unwrap_or(Vec::new()),
    )
}

fn tk_ref_from_symbol(symbol: Symbol, trailing_trivia: Option<Vec<Token>>) -> TokenReference {
    tk_ref_from_type(TokenType::Symbol { symbol: (symbol) }, trailing_trivia)
}

fn suffixes_from_path(path: &Vec<String>, trailing_trivia: Option<Vec<Token>>) -> Vec<Suffix> {
    path.iter()
        .map(|segment| {
            let index = if segment.chars().next().map_or(false, |c| c.is_numeric())
                || segment.chars().any(|c| !(c.is_alphanumeric() || c == '_'))
            {
                Index::Brackets {
                    expression: Expression::String(tk_ref_from_type(
                        TokenType::StringLiteral {
                            literal: ShortString::from(segment),
                            multi_line_depth: 0,
                            quote_type: full_moon::tokenizer::StringLiteralQuoteType::Double,
                        },
                        None,
                    )),
                    brackets: ContainedSpan::new(
                        tk_ref_from_symbol(Symbol::LeftBrace, None),
                        tk_ref_from_symbol(Symbol::RightBrace, trailing_trivia.clone()),
                    ),
                }
            } else {
                Index::Dot {
                    name: tk_ref_from_type(
                        TokenType::Identifier {
                            identifier: ShortString::from(segment),
                        },
                        trailing_trivia.clone(),
                    ),
                    dot: tk_ref_from_symbol(Symbol::Dot, None),
                }
            };

            Suffix::Index(index)
        })
        .collect()
}

fn suffix_from_identifier(segment: &String, trailing_trivia: Option<Vec<Token>>) -> Suffix {
    let index = if segment.chars().next().map_or(false, |c| c.is_numeric())
        || segment.chars().any(|c| !(c.is_alphanumeric() || c == '_'))
    {
        Index::Brackets {
            expression: Expression::String(tk_ref_from_type(
                TokenType::StringLiteral {
                    literal: ShortString::from(segment),
                    multi_line_depth: 0,
                    quote_type: full_moon::tokenizer::StringLiteralQuoteType::Double,
                },
                None,
            )),
            brackets: ContainedSpan::new(
                tk_ref_from_symbol(Symbol::LeftBrace, None),
                tk_ref_from_symbol(Symbol::RightBrace, trailing_trivia),
            ),
        }
    } else {
        Index::Dot {
            name: tk_ref_from_type(
                TokenType::Identifier {
                    identifier: ShortString::from(segment),
                },
                trailing_trivia,
            ),
            dot: tk_ref_from_symbol(Symbol::Dot, None),
        }
    };

    Suffix::Index(index)
}

struct VarExpressionCollector {
    old_path: Vec<String>,
    new_path: Vec<String>,
}
impl VisitorMut for VarExpressionCollector {
    fn visit_var_expression(&mut self, var_expr: VarExpression) -> VarExpression {
        if !is_script_identifier(&var_expr) {
            return var_expr;
        }

        let mut non_suffixed_path_to_instance = self.old_path.clone();
        let mut suffixes: Vec<Suffix> = Vec::new();

        let mut any_parents_accessed: bool = false;
        let mut any_non_parent_suffix_accessed = false;

        for suffix in var_expr.suffixes().into_iter().peekable() {
            match suffix {
                Suffix::Call(_) => {
                    suffixes.push(suffix.clone());
                    any_non_parent_suffix_accessed = true;
                }
                Suffix::Index(index) => {
                    if let Some(index_identifier) = get_index_identifier(&index) {
                        if index_identifier == "Parent" && !any_non_parent_suffix_accessed {
                            any_parents_accessed = true;
                            non_suffixed_path_to_instance.pop();
                        } else {
                            any_non_parent_suffix_accessed = true;
                            suffixes.push(suffix.clone());
                        }
                    }
                }
                _ => {}
            }
        }

        // Dont do anything to the expression if accesing direct descendants of
        // the script, assume those are moving with the script
        if !any_parents_accessed {
            return var_expr;
        }

        let closest_common_ancestor =
            find_closest_common_ancestor(&non_suffixed_path_to_instance, &self.new_path);
        match closest_common_ancestor {
            AncestorResult::Identical => {} // How?
            AncestorResult::None => {}      // TODO: Handle aboslute path from service
            AncestorResult::Some(ancestor_index) => {
                let up_levels = self.new_path.len() as i32 - (ancestor_index + 1);

                let trailing_trivia: Option<Vec<Token>> = if suffixes.is_empty() {
                    let (_, trailing) = var_expr.surrounding_trivia();
                    Some(trailing.iter().map(|t| t.clone().to_owned()).collect())
                } else {
                    None
                };

                let mut pre_suffixes = Vec::new();

                for _ in 0..up_levels as usize {
                    pre_suffixes.push(suffix_from_identifier(&"Parent".to_string(), None));
                }

                if (ancestor_index as usize) < non_suffixed_path_to_instance.len() {
                    let mut id_iter = non_suffixed_path_to_instance[ancestor_index as usize..]
                        .iter()
                        .peekable();

                    while let Some(identifier) = id_iter.next() {
                        let trail_triv = if id_iter.peek().is_none() && trailing_trivia.is_some() {
                            &trailing_trivia
                        } else {
                            &None
                        };

                        pre_suffixes.push(suffix_from_identifier(identifier, trail_triv.clone()))
                    }
                };

                pre_suffixes.extend(suffixes);

                return var_expr.with_suffixes(pre_suffixes);
            }
        };

        var_expr
    }
    fn visit_function_call(&mut self, function_call: FunctionCall) -> FunctionCall {
        if !is_script_identifier_fn(&function_call) {
            return function_call;
        }

        let mut non_suffixed_path_to_instance = self.old_path.clone();
        let mut suffixes: Vec<Suffix> = Vec::new();

        let mut any_parents_accessed: bool = false;
        let mut any_non_parent_suffix_accessed = false;

        for suffix in function_call.suffixes().into_iter().peekable() {
            match suffix {
                Suffix::Call(_) => {
                    suffixes.push(suffix.clone());
                    any_non_parent_suffix_accessed = true;
                }
                Suffix::Index(index) => {
                    if let Some(index_identifier) = get_index_identifier(&index) {
                        if index_identifier == "Parent" && !any_non_parent_suffix_accessed {
                            any_parents_accessed = true;
                            non_suffixed_path_to_instance.pop();
                        } else {
                            any_non_parent_suffix_accessed = true;
                            suffixes.push(suffix.clone());
                        }
                    }
                }
                _ => {}
            }
        }

        // Dont do anything to the expression if accesing direct descendants of
        // the script, assume those are moving with the script
        if !any_parents_accessed {
            return function_call;
        }

        let closest_common_ancestor =
            find_closest_common_ancestor(&non_suffixed_path_to_instance, &self.new_path);
        match closest_common_ancestor {
            AncestorResult::Identical => {} // How?
            AncestorResult::None => {}      // TODO: Handle aboslute path from service
            AncestorResult::Some(ancestor_index) => {
                let up_levels = self.new_path.len() as i32 - (ancestor_index + 1);

                let trailing_trivia: Option<Vec<Token>> = if suffixes.is_empty() {
                    let (_, trailing) = function_call.surrounding_trivia();
                    Some(trailing.iter().map(|t| t.clone().to_owned()).collect())
                } else {
                    None
                };

                let mut pre_suffixes = Vec::new();

                for _ in 0..up_levels as usize {
                    pre_suffixes.push(suffix_from_identifier(&"Parent".to_string(), None));
                }

                if (ancestor_index as usize) < non_suffixed_path_to_instance.len() {
                    let mut id_iter = non_suffixed_path_to_instance[ancestor_index as usize..]
                        .iter()
                        .peekable();

                    while let Some(identifier) = id_iter.next() {
                        let trail_triv = if id_iter.peek().is_none() && trailing_trivia.is_some() {
                            &trailing_trivia
                        } else {
                            &None
                        };

                        pre_suffixes.push(suffix_from_identifier(identifier, trail_triv.clone()))
                    }
                };

                pre_suffixes.extend(suffixes);

                return function_call.with_suffixes(pre_suffixes);
            }
        };

        function_call
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
struct MoveScriptPayload {
    script_source: String,
    old_path: Vec<String>,
    new_path: Vec<String>,
}

async fn move_script(
    Json(payload): Json<MoveScriptPayload>,
) -> Result<String, (StatusCode, String)> {
    match parse(&payload.script_source) {
        Ok(ast) => {
            let mut collector = VarExpressionCollector {
                old_path: payload.old_path,
                new_path: payload.new_path,
            };
            let visited_ast = collector.visit_ast(ast);

            Ok(visited_ast.to_string())
        }
        Err(errors) => {
            let error_messages: Vec<String> = errors.into_iter().map(|e| e.to_string()).collect();

            Err((
                StatusCode::BAD_REQUEST,
                serde_json::to_string(&error_messages).unwrap(),
            ))
        }
    }
}
