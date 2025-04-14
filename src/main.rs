use axum::{Json, Router, http::StatusCode, routing::post};
use full_moon::{
    ShortString,
    ast::{
        self, Assignment, Call, Expression, FunctionArgs, FunctionCall, Index, Prefix, Stmt,
        Suffix, TableConstructor, Var, VarExpression,
        punctuated::{Pair, Punctuated},
        span::ContainedSpan,
    },
    node::Node,
    parse,
    tokenizer::{Symbol, Token, TokenReference, TokenType},
    visitors::VisitorMut,
};
use serde;

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/move", post(move_script))
        .route("/refactor-service", post(refactor_service_acess));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8011")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}

// fn main() {
//     match parse("local ReplicatedStorage = Services.ReplicatedStorage") {
//         Ok(ast) => {
//             let mut service_refactor = ServiceRefactor {};
//             let visited_ast = service_refactor.visit_ast(ast);
//             println!("{}", visited_ast.to_string())
//         }
//         Err(e) => {
//             eprintln!("Couldn't parse: {:?}", e)
//         }
//     }

//     // println!(
//     //     "{:?}",
//     //     pull_expression("local _ = game:GetService(\"ReplicatedStorage\")")
//     // );
// }

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

fn is_identifier_in_expr(expression: &Expression, identifier: &str) -> bool {
    match expression {
        Expression::Var(var) => match var {
            Var::Name(name) => match_token_literally(&name, identifier),
            Var::Expression(var_expr) => is_identifier(var_expr.prefix(), identifier),
            _ => false,
        },
        Expression::UnaryOperator {
            expression,
            unop: _,
        } => is_identifier_in_expr(&*expression, identifier),
        Expression::TypeAssertion {
            expression,
            type_assertion: _,
        } => is_identifier_in_expr(&*expression, identifier),
        Expression::Parentheses {
            contained: _,
            expression,
        } => is_identifier_in_expr(&*expression, identifier),
        _ => false,
    }
}

fn is_identifier(prefix: &Prefix, identifier: &str) -> bool {
    match prefix {
        Prefix::Name(name) => match_token_literally(name, identifier),
        Prefix::Expression(expression) => is_identifier_in_expr(&**expression, identifier),
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

fn is_identifier_literally(index: &Index, match_identifier: &str) -> bool {
    if let Some(identifier) = get_index_identifier(index) {
        return match_identifier == identifier;
    };

    false
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

fn add_trivia_to_token_reference(
    leading: Vec<&Token>,
    token_ref: &TokenReference,
    trailing: Vec<&Token>,
) -> TokenReference {
    TokenReference::new(
        leading.iter().map(|t| t.to_owned().clone()).collect(),
        token_ref.token().clone(),
        trailing.iter().map(|t| t.to_owned().clone()).collect(),
    )
}

fn add_trivia_to_expression(
    leading: Vec<&Token>,
    expression: &Expression,
    trailing: Vec<&Token>,
) -> Box<Expression> {
    match expression {
        Expression::Var(var) => match var {
            Var::Name(name) => Box::new(Expression::Var(Var::Name(add_trivia_to_token_reference(
                leading, name, trailing,
            )))),
            Var::Expression(var_expr) => {
                let new_prefix = add_trivia_to_prefix(leading, var_expr.prefix());

                let mut new_suffixes: Vec<Suffix> =
                    var_expr.suffixes().map(|t| t.to_owned().clone()).collect();
                if let Some(last_suffix) = new_suffixes.pop() {
                    new_suffixes.push(add_trivia_to_suffix(&last_suffix, trailing))
                }

                let var_expr = VarExpression::new(new_prefix).with_suffixes(new_suffixes);
                Box::new(Expression::Var(Var::Expression(Box::new(var_expr))))
            }
            _ => Box::new(expression.clone()),
        },
        Expression::UnaryOperator {
            expression,
            unop: _,
        } => add_trivia_to_expression(leading, expression, trailing),
        Expression::Parentheses {
            contained,
            expression,
        } => {
            let (open, close) = contained.tokens();
            let new_close = add_trivia_to_token_reference(leading, close, trailing);
            Box::new(Expression::Parentheses {
                contained: ContainedSpan::new(open.clone(), new_close),
                expression: expression.clone(),
            })
        }
        _ => Box::new(expression.clone()),
    }
}

fn add_trivia_to_prefix(leading_trivia: Vec<&Token>, prefix: &Prefix) -> Prefix {
    match prefix {
        Prefix::Name(name) => Prefix::Name(TokenReference::new(
            leading_trivia
                .iter()
                .map(|t| t.to_owned().clone())
                .collect(),
            name.token().clone(),
            Vec::new(),
        )),
        Prefix::Expression(expression) => Prefix::Expression(add_trivia_to_expression(
            leading_trivia,
            expression,
            Vec::<&Token>::new(),
        )),
        _ => prefix.clone(),
    }
}

fn add_trivia_to_suffix(suffix: &Suffix, trailing: Vec<&Token>) -> Suffix {
    let leading = Vec::<&Token>::new();
    match suffix {
        Suffix::Call(call) => match call {
            Call::AnonymousCall(function_args) => match function_args {
                FunctionArgs::Parentheses {
                    parentheses,
                    arguments,
                } => {
                    let (open, close) = parentheses.tokens();
                    let new_close = add_trivia_to_token_reference(leading, close, trailing);
                    Suffix::Call(Call::AnonymousCall(FunctionArgs::Parentheses {
                        parentheses: ContainedSpan::new(open.clone(), new_close),
                        arguments: arguments.clone(),
                    }))
                }
                FunctionArgs::String(string) => Suffix::Call(Call::AnonymousCall(
                    FunctionArgs::String(add_trivia_to_token_reference(leading, string, trailing)),
                )),
                FunctionArgs::TableConstructor(table_constructor) => {
                    let (open, close) = table_constructor.braces().tokens();
                    let new_close = add_trivia_to_token_reference(leading, close, trailing);
                    Suffix::Call(Call::AnonymousCall(FunctionArgs::TableConstructor(
                        table_constructor
                            .clone()
                            .with_braces(ContainedSpan::new(open.clone(), new_close)),
                    )))
                }
                _ => suffix.clone(),
            },
            Call::MethodCall(method_call) => {
                let new_args = match method_call.args() {
                    FunctionArgs::Parentheses {
                        parentheses,
                        arguments,
                    } => {
                        let (open, close) = parentheses.tokens();
                        let new_close = add_trivia_to_token_reference(leading, close, trailing);
                        FunctionArgs::Parentheses {
                            parentheses: ContainedSpan::new(open.clone(), new_close),
                            arguments: arguments.clone(),
                        }
                    }
                    FunctionArgs::String(string) => FunctionArgs::String(
                        add_trivia_to_token_reference(leading, string, trailing),
                    ),
                    FunctionArgs::TableConstructor(table_constructor) => {
                        let (open, close) = table_constructor.braces().tokens();
                        let new_close = add_trivia_to_token_reference(leading, close, trailing);
                        FunctionArgs::TableConstructor(
                            table_constructor
                                .clone()
                                .with_braces(ContainedSpan::new(open.clone(), new_close)),
                        )
                    }
                    _ => return suffix.clone(),
                };

                Suffix::Call(Call::MethodCall(method_call.clone().with_args(new_args)))
            }
            _ => suffix.clone(),
        },
        Suffix::Index(index) => match index {
            Index::Brackets {
                brackets,
                expression,
            } => {
                let (open, close) = brackets.tokens();
                let new_close = add_trivia_to_token_reference(leading, close, trailing);
                Suffix::Index(Index::Brackets {
                    brackets: ContainedSpan::new(open.clone(), new_close),
                    expression: expression.clone(),
                })
            }
            Index::Dot { dot, name } => Suffix::Index(Index::Dot {
                dot: dot.clone(),
                name: add_trivia_to_token_reference(leading, name, trailing),
            }),
            _ => suffix.clone(),
        },
        _ => suffix.clone(),
    }
}

struct ExpressionPuller {
    pulled_prefix: Option<Prefix>,
    pulled_suffixes: Option<Vec<Suffix>>,
    leading: Vec<Token>,
    trailing: Vec<Token>,
}
impl VisitorMut for ExpressionPuller {
    fn visit_var_expression(&mut self, node: VarExpression) -> VarExpression {
        let mut new_suffixes: Vec<Suffix> = node.suffixes().map(|t| t.to_owned().clone()).collect();
        if let Some(last_suffix) = new_suffixes.pop() {
            new_suffixes.push(add_trivia_to_suffix(
                &last_suffix,
                self.trailing.iter().collect(),
            ))
        }

        self.pulled_prefix = Some(add_trivia_to_prefix(
            self.leading.iter().collect(),
            node.prefix(),
        ));
        self.pulled_suffixes = Some(new_suffixes);
        node
    }
    fn visit_function_call(&mut self, node: FunctionCall) -> FunctionCall {
        let mut new_suffixes: Vec<Suffix> = node.suffixes().map(|t| t.to_owned().clone()).collect();
        if let Some(last_suffix) = new_suffixes.pop() {
            new_suffixes.push(add_trivia_to_suffix(
                &last_suffix,
                self.trailing.iter().collect(),
            ))
        }

        self.pulled_prefix = Some(add_trivia_to_prefix(
            self.leading.iter().collect(),
            node.prefix(),
        ));
        self.pulled_suffixes = Some(new_suffixes);
        node
    }
}

fn pull_expression(
    source: &str,
    leading: Vec<&Token>,
    trailing: Vec<&Token>,
) -> (Option<Prefix>, Option<Vec<Suffix>>) {
    match parse(&source) {
        Ok(ast) => {
            let mut puller = ExpressionPuller {
                pulled_prefix: None,
                pulled_suffixes: None,
                leading: leading.iter().map(|t| t.to_owned().clone()).collect(),
                trailing: trailing.iter().map(|t| t.to_owned().clone()).collect(),
            };

            puller.visit_ast(ast);
            (puller.pulled_prefix, puller.pulled_suffixes)
        }
        Err(_) => (None, None),
    }
}

fn convert_service_access(node: &VarExpression) -> Option<Expression> {
    let (leading, trailing) = node.surrounding_trivia();

    let suffixes: Vec<&Suffix> = node.suffixes().into_iter().collect();
    if suffixes.len() < 1 {
        return None;
    }

    let service_index = if is_identifier(node.prefix(), "_G") {
        if let Suffix::Index(index) = suffixes[0] {
            if is_identifier_literally(index, "Services") {
                1
            } else {
                return None;
            }
        } else {
            return None;
        }
    } else if is_identifier(node.prefix(), "Services") {
        0
    } else {
        return None;
    };

    if suffixes.len() < service_index + 1 {
        return None;
    }

    let trailing = if suffixes.len() > service_index + 1 {
        Vec::new()
    } else {
        trailing
    };

    let (new_prefix, mut new_suffixes) = if let Suffix::Index(index) = suffixes[service_index] {
        if let Some(identifier) = get_index_identifier(index) {
            let (opt_new_prefix, opt_new_suffixes) = pull_expression(
                &format!("local _ = game:GetService(\"{}\")", identifier),
                leading,
                trailing,
            );

            if opt_new_prefix.is_some() && opt_new_suffixes.is_some() {
                (opt_new_prefix.unwrap(), opt_new_suffixes.unwrap())
            } else {
                return None;
            }
        } else {
            return None;
        }
    } else {
        return None;
    };

    if suffixes.len() > service_index + 1 {
        for suffix in &suffixes[service_index + 1..] {
            new_suffixes.push(suffix.to_owned().clone())
        }
    }

    Some(Expression::FunctionCall(
        FunctionCall::new(new_prefix).with_suffixes(new_suffixes),
    ))
}

fn convert_service_access_varexp(node: &VarExpression) -> Option<VarExpression> {
    let (leading, trailing) = node.surrounding_trivia();

    let suffixes: Vec<&Suffix> = node.suffixes().into_iter().collect();
    if suffixes.len() < 1 {
        return None;
    }

    let service_index = if is_identifier(node.prefix(), "_G") {
        if let Suffix::Index(index) = suffixes[0] {
            if is_identifier_literally(index, "Services") {
                1
            } else {
                return None;
            }
        } else {
            return None;
        }
    } else if is_identifier(node.prefix(), "Services") {
        0
    } else {
        return None;
    };

    if suffixes.len() < service_index + 1 {
        return None;
    }

    let trailing = if suffixes.len() > service_index + 1 {
        Vec::new()
    } else {
        trailing
    };

    let (new_prefix, mut new_suffixes) = if let Suffix::Index(index) = suffixes[service_index] {
        if let Some(identifier) = get_index_identifier(index) {
            let (opt_new_prefix, opt_new_suffixes) = pull_expression(
                &format!("local _ = game:GetService(\"{}\")", identifier),
                leading,
                trailing,
            );

            if opt_new_prefix.is_some() && opt_new_suffixes.is_some() {
                (opt_new_prefix.unwrap(), opt_new_suffixes.unwrap())
            } else {
                return None;
            }
        } else {
            return None;
        }
    } else {
        return None;
    };

    if suffixes.len() > service_index + 1 {
        for suffix in &suffixes[service_index + 1..] {
            new_suffixes.push(suffix.to_owned().clone())
        }
    }

    Some(VarExpression::new(new_prefix).with_suffixes(new_suffixes))
}

fn convert_service_access_fn(node: &FunctionCall) -> Option<FunctionCall> {
    let (leading, trailing) = node.surrounding_trivia();

    let suffixes: Vec<&Suffix> = node.suffixes().into_iter().collect();
    if suffixes.len() < 1 {
        return None;
    }

    let service_index = if is_identifier(node.prefix(), "_G") {
        if let Suffix::Index(index) = suffixes[0] {
            if is_identifier_literally(index, "Services") {
                1
            } else {
                return None;
            }
        } else {
            return None;
        }
    } else if is_identifier(node.prefix(), "Services") {
        0
    } else {
        return None;
    };

    if suffixes.len() < service_index + 1 {
        return None;
    }

    let trailing = if suffixes.len() > service_index + 1 {
        Vec::new()
    } else {
        trailing
    };

    let (new_prefix, mut new_suffixes) = if let Suffix::Index(index) = suffixes[service_index] {
        if let Some(identifier) = get_index_identifier(index) {
            let (opt_new_prefix, opt_new_suffixes) = pull_expression(
                &format!("local _ = game:GetService(\"{}\")", identifier),
                leading,
                trailing,
            );

            if opt_new_prefix.is_some() && opt_new_suffixes.is_some() {
                (opt_new_prefix.unwrap(), opt_new_suffixes.unwrap())
            } else {
                return None;
            }
        } else {
            return None;
        }
    } else {
        return None;
    };

    if suffixes.len() > service_index + 1 {
        for suffix in &suffixes[service_index + 1..] {
            new_suffixes.push(suffix.to_owned().clone())
        }
    }

    Some(FunctionCall::new(new_prefix).with_suffixes(new_suffixes))
}

fn refactor_expressions(
    pairs_iter: &mut dyn Iterator<Item = &Pair<Expression>>,
) -> Option<Punctuated<Expression>> {
    let mut expressions = Punctuated::new();

    let mut any_expressions_refactored = false;
    for pair in pairs_iter {
        let (expression, punctuation) = pair.clone().into_tuple();
        match expression {
            Expression::Var(ref var) => match var {
                Var::Expression(var_expr) => {
                    if let Some(new_expr) = convert_service_access(var_expr) {
                        expressions.push(Pair::new(new_expr, punctuation));
                        any_expressions_refactored = true;
                        continue;
                    }
                }
                _ => {}
            },
            Expression::FunctionCall(ref function_call) => {
                if let Some(function_call) = convert_service_access_fn(function_call) {
                    expressions.push(Pair::new(
                        Expression::FunctionCall(function_call),
                        punctuation,
                    ));
                    any_expressions_refactored = true;
                    continue;
                }
            }
            _ => {}
        };

        expressions.push(Pair::new(expression.clone(), punctuation));
    }

    if any_expressions_refactored {
        return Some(expressions);
    }

    None
}

fn refactor_vars(pairs_iter: &mut dyn Iterator<Item = &Pair<Var>>) -> Option<Punctuated<Var>> {
    let mut vars = Punctuated::new();

    let mut any_vars_refactored = false;
    for pair in pairs_iter {
        let (ref var, punctuation) = pair.clone().into_tuple();
        match var {
            Var::Name(_) => {}
            Var::Expression(var_expr) => {
                if let Some(new_expr) = convert_service_access_varexp(&var_expr) {
                    vars.push(Pair::new(Var::Expression(Box::new(new_expr)), punctuation));
                    any_vars_refactored = true;
                    continue;
                }
            }
            _ => {}
        };

        vars.push(Pair::new(var.clone(), punctuation));
    }

    if any_vars_refactored {
        return Some(vars);
    }

    None
}

struct ServiceRefactor {}
impl VisitorMut for ServiceRefactor {
    fn visit_stmt(&mut self, stmt: Stmt) -> Stmt {
        match stmt {
            Stmt::LocalAssignment(ref assignment) => {
                let expressions = refactor_expressions(&mut assignment.expressions().pairs());

                if let Some(expressions) = expressions {
                    Stmt::LocalAssignment(assignment.clone().with_expressions(expressions))
                } else {
                    stmt.clone()
                }
            }
            Stmt::Assignment(ref assignment) => {
                let expressions = refactor_expressions(&mut assignment.expressions().pairs());

                if let Some(expressions) = expressions {
                    Stmt::Assignment(Assignment::new(assignment.variables().clone(), expressions))
                } else {
                    stmt.clone()
                }
            }
            Stmt::FunctionCall(ref function_call) => {
                if let Some(function_call) = convert_service_access_fn(function_call) {
                    Stmt::FunctionCall(function_call)
                } else {
                    stmt.clone()
                }
            }
            _ => stmt,
        }
    }
    fn visit_function_call(&mut self, function_call: FunctionCall) -> FunctionCall {
        if let Some(function_call) = convert_service_access_fn(&function_call) {
            function_call
        } else {
            function_call
        }
    }
    fn visit_assignment(&mut self, assignment: Assignment) -> Assignment {
        let vars = refactor_vars(&mut assignment.variables().pairs());

        if let Some(vars) = vars {
            assignment.clone().with_variables(vars)
        } else {
            assignment
        }
    }
    fn visit_var(&mut self, var: Var) -> Var {
        match var {
            Var::Name(_) => var,
            Var::Expression(ref var_expr) => {
                if let Some(new_expr) = convert_service_access_varexp(&var_expr) {
                    return Var::Expression(Box::new(new_expr));
                };
                var
            }
            _ => var,
        }
    }
}

struct ScriptMover {
    old_path: Vec<String>,
    new_path: Vec<String>,
}
impl VisitorMut for ScriptMover {
    fn visit_var_expression(&mut self, var_expr: VarExpression) -> VarExpression {
        if !is_identifier(&var_expr.prefix(), "script") {
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
                    Some(trailing.iter().map(|t| t.to_owned().clone()).collect())
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
        if !is_identifier(&function_call.prefix(), "script") {
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
                    Some(trailing.iter().map(|t| t.to_owned().clone()).collect())
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
            let mut collector = ScriptMover {
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

#[derive(serde::Serialize, serde::Deserialize)]
struct RefactorServiceAccessPayload {
    script_source: String,
}
async fn refactor_service_acess(
    Json(payload): Json<RefactorServiceAccessPayload>,
) -> Result<String, (StatusCode, String)> {
    match parse(&payload.script_source) {
        Ok(ast) => {
            let mut refactor = ServiceRefactor {};
            let visited_ast = refactor.visit_ast(ast);
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
