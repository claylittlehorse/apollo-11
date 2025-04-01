use axum::{Json, Router, http::StatusCode, response::Json as JsonResponse, routing::post};
use full_moon::{
    ast::{self, Expression, Index, Prefix, Suffix, Var, VarExpression},
    parse,
    tokenizer::{TokenReference, TokenType},
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

fn match_token_literally(token_ref: &TokenReference, literal: &str) -> bool {
    match token_ref.token().token_type() {
        TokenType::Identifier { identifier } => **identifier == *literal,
        TokenType::StringLiteral {
            literal: str_literal,
            multi_line_depth: _,
            quote_type: _,
        } => **str_literal == *literal,
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

fn is_suffix_parent(suffix: &Suffix) -> bool {
    if let Suffix::Index(index) = suffix {
        match index {
            Index::Dot { name, dot: _ } => return match_token_literally(&name, "Parent"),
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
                        return **literal == *"Parent";
                    }
                }
            }
            _ => return false,
        }
    }
    false
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

struct VarExpressionCollector {
    up_levels: u8,
    parent_names: Vec<String>,
}

impl VisitorMut for VarExpressionCollector {
    fn visit_var_expression(&mut self, var_expr: VarExpression) -> VarExpression {
        // Collect the VarExpression node
        if is_script_identifier(&var_expr) {
            println!("Got script identifier");
            for suffix in var_expr.suffixes() {
                if is_suffix_parent(suffix) {
                    println!("Got parent suffix")
                } else {
                    println!("Got non parent suffix")
                }
            }
        }

        var_expr
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
struct MoveScriptPayload {
    script_source: String,
    up_levels: u8,
    parent_names: Vec<String>,
}
async fn move_script(
    Json(payload): Json<MoveScriptPayload>,
) -> Result<String, (StatusCode, String)> {
    match parse(&payload.script_source) {
        Ok(ast) => {
            let mut collector = VarExpressionCollector {
                up_levels: payload.up_levels,
                parent_names: payload.parent_names,
            };
            let visited_ast = collector.visit_ast(ast);

            Ok(visited_ast.to_string())
        }
        Err(errors) => {
            // Parsing failed, return the errors as a JSON response
            let error_messages: Vec<String> = errors.into_iter().map(|e| e.to_string()).collect();

            Err((
                StatusCode::BAD_REQUEST,
                serde_json::to_string(&error_messages).unwrap(),
            ))
        }
    }
}

// #[axum::debug_handler]
// async fn receive_ast(
//     Json(ast): Json<full_moon::ast::Ast>,
// ) -> Result<JsonResponse<MoveScriptPayload>, (StatusCode, String)> {
//     let source_code = ast.to_string();
//     let payload = MoveScriptPayload {
//         script_source: source_code,
//     };
//     Ok(JsonResponse(payload))
// }
