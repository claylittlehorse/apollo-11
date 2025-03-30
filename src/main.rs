use axum::{Json, Router, http::StatusCode, response::Json as JsonResponse, routing::post};
use full_moon::parse;
use serde;

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/source", post(receive_source))
        .route("/ast", post(receive_ast));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8011")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}

#[derive(serde::Serialize, serde::Deserialize)]
struct ScriptSourcePayload {
    script_source: String,
}

async fn receive_source(
    Json(payload): Json<ScriptSourcePayload>,
) -> Result<JsonResponse<full_moon::ast::Ast>, (StatusCode, String)> {
    match parse(&payload.script_source) {
        Ok(ast) => {
            // Parsing succeeded, return the AST as JSON
            Ok(JsonResponse(ast))
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

#[axum::debug_handler]
async fn receive_ast(
    Json(ast): Json<full_moon::ast::Ast>,
) -> Result<JsonResponse<ScriptSourcePayload>, (StatusCode, String)> {
    let source_code = ast.to_string();
    let payload = ScriptSourcePayload {
        script_source: source_code,
    };
    Ok(JsonResponse(payload))
}
