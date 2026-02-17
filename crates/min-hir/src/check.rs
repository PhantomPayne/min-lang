//! Type checking and inference for the Min language.
//!
//! The type checker walks HIR expressions, infers types for local variables,
//! checks type consistency, and reports errors.
//!
//! Uses constraint-based inference with unification:
//! 1. Assign type variables to unknown types
//! 2. Generate constraints from expression usage
//! 3. Unify constraints to resolve type variables
//! 4. Report errors for unsatisfiable constraints

use std::collections::HashMap;

use min_diagnostics::{Diagnostic, Span};
use min_syntax::BinOp;
use min_types::{StructField, Type, TypeVar};

use crate::hir::*;
use crate::lower::LoweringContext;

/// Result of type-checking a module.
#[derive(Debug)]
pub struct TypeCheckResult {
    /// Type of each expression, indexed by ExprId per function.
    pub function_expr_types: HashMap<DefId, Vec<Type>>,
    /// Type of each local variable (from let bindings).
    pub local_types: HashMap<DefId, Type>,
    /// Accumulated type errors.
    pub diagnostics: Vec<Diagnostic>,
}

/// The type-checking context for a single function.
#[allow(dead_code)]
struct InferenceContext<'a> {
    /// The lowering context, for looking up function signatures and def types.
    lower_ctx: &'a LoweringContext,
    /// Next type variable ID.
    next_var: u32,
    /// Substitution map: type variable → resolved type.
    substitutions: HashMap<TypeVar, Type>,
    /// Type of each expression in the current function body.
    expr_types: Vec<Type>,
    /// Types of local variables defined in this function.
    local_types: HashMap<DefId, Type>,
    /// The return type of the current function.
    return_type: Type,
    /// Accumulated diagnostics.
    diagnostics: Vec<Diagnostic>,
}

impl<'a> InferenceContext<'a> {
    fn new(lower_ctx: &'a LoweringContext, return_type: Type, expr_count: usize) -> Self {
        Self {
            lower_ctx,
            next_var: 0,
            substitutions: HashMap::new(),
            expr_types: vec![Type::Error; expr_count],
            local_types: HashMap::new(),
            return_type,
            diagnostics: Vec::new(),
        }
    }

    /// Create a fresh type variable.
    #[allow(dead_code)]
    fn fresh_var(&mut self) -> Type {
        let var = TypeVar(self.next_var);
        self.next_var += 1;
        Type::Var(var)
    }

    /// Resolve a type by following substitutions.
    fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(var) => {
                if let Some(resolved) = self.substitutions.get(var) {
                    self.resolve(resolved)
                } else {
                    ty.clone()
                }
            }
            Type::Struct(fields) => {
                let resolved_fields: Vec<StructField> = fields
                    .iter()
                    .map(|f| StructField {
                        name: f.name.clone(),
                        ty: self.resolve(&f.ty),
                    })
                    .collect();
                Type::Struct(resolved_fields)
            }
            Type::Generic { name, params } => Type::Generic {
                name: name.clone(),
                params: params.iter().map(|p| self.resolve(p)).collect(),
            },
            other => other.clone(),
        }
    }

    /// Unify two types, returning the unified type.
    /// Records substitutions for type variables and reports errors on mismatch.
    fn unify(&mut self, expected: &Type, actual: &Type, span: Span) -> Type {
        let expected = self.resolve(expected);
        let actual = self.resolve(actual);

        // Error types propagate silently
        if expected.is_error() || actual.is_error() {
            return Type::Error;
        }

        match (&expected, &actual) {
            // Same primitive types
            (Type::Int, Type::Int) => Type::Int,
            (Type::Float, Type::Float) => Type::Float,
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::String, Type::String) => Type::String,

            // Type variable on the left: bind it
            (Type::Var(var), _) => {
                if actual.contains_var(*var) {
                    self.diagnostics.push(
                        Diagnostic::error("recursive type detected", span)
                            .with_note("a type cannot contain itself"),
                    );
                    return Type::Error;
                }
                self.substitutions.insert(*var, actual.clone());
                actual
            }

            // Type variable on the right: bind it
            (_, Type::Var(var)) => {
                if expected.contains_var(*var) {
                    self.diagnostics.push(
                        Diagnostic::error("recursive type detected", span)
                            .with_note("a type cannot contain itself"),
                    );
                    return Type::Error;
                }
                self.substitutions.insert(*var, expected.clone());
                expected
            }

            // Structural equality for structs
            (Type::Struct(fields1), Type::Struct(fields2)) => {
                if fields1.len() != fields2.len() {
                    self.diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "type mismatch: expected struct with {} fields, found {} fields",
                                fields1.len(),
                                fields2.len()
                            ),
                            span,
                        )
                        .with_note(format!("expected {}, found {}", expected, actual)),
                    );
                    return Type::Error;
                }

                let mut unified_fields = Vec::new();
                for (f1, f2) in fields1.iter().zip(fields2.iter()) {
                    if f1.name != f2.name {
                        self.diagnostics.push(
                            Diagnostic::error(
                                format!(
                                    "struct field name mismatch: expected '{}', found '{}'",
                                    f1.name, f2.name
                                ),
                                span,
                            ),
                        );
                        return Type::Error;
                    }
                    let unified_ty = self.unify(&f1.ty, &f2.ty, span);
                    unified_fields.push(StructField {
                        name: f1.name.clone(),
                        ty: unified_ty,
                    });
                }
                Type::Struct(unified_fields)
            }

            // Generic types
            (
                Type::Generic {
                    name: n1,
                    params: p1,
                },
                Type::Generic {
                    name: n2,
                    params: p2,
                },
            ) if n1 == n2 && p1.len() == p2.len() => {
                let params: Vec<Type> = p1
                    .iter()
                    .zip(p2.iter())
                    .map(|(a, b)| self.unify(a, b, span))
                    .collect();
                Type::Generic {
                    name: n1.clone(),
                    params,
                }
            }

            // Mismatch
            _ => {
                self.diagnostics.push(
                    Diagnostic::error(
                        format!("type mismatch: expected {}, found {}", expected, actual),
                        span,
                    ),
                );
                Type::Error
            }
        }
    }

    /// Infer the type of an expression.
    fn infer_expr(&mut self, body: &Body, expr_id: ExprId) -> Type {
        let span = body.span(expr_id);
        let ty = match body.expr(expr_id) {
            ExprData::IntLit(_) => Type::Int,
            ExprData::FloatLit(_) => Type::Float,
            ExprData::StringLit(_) => Type::String,
            ExprData::BoolLit(_) => Type::Bool,

            ExprData::StringInterp(parts) => {
                // Each interpolated expression can be any type (implicitly converted to string).
                // Just infer the sub-expressions.
                for part in parts {
                    if let StringInterpPart::Expr(sub_id) = part {
                        self.infer_expr(body, *sub_id);
                    }
                }
                Type::String
            }

            ExprData::Var(def_id) => {
                let def_id = *def_id;
                // Look up in local types first, then in the lowering context
                if let Some(ty) = self.local_types.get(&def_id) {
                    ty.clone()
                } else if let Some(ty) = self.lower_ctx.def_type(def_id) {
                    ty.clone()
                } else {
                    Type::Error
                }
            }

            ExprData::FieldAccess { expr, field } => {
                let expr_id_inner = *expr;
                let field = field.clone();
                let expr_ty = self.infer_expr(body, expr_id_inner);
                let resolved = self.resolve(&expr_ty);
                match resolved.field_type(&field) {
                    Some(ty) => ty.clone(),
                    None => {
                        if !resolved.is_error() {
                            self.diagnostics.push(
                                Diagnostic::error(
                                    format!(
                                        "no field '{}' on type {}",
                                        field, resolved
                                    ),
                                    span,
                                ),
                            );
                        }
                        Type::Error
                    }
                }
            }

            ExprData::StructLit { fields } => {
                let fields_clone: Vec<(String, ExprId)> = fields.clone();
                let mut struct_fields = Vec::new();
                for (name, val_id) in &fields_clone {
                    let ty = self.infer_expr(body, *val_id);
                    struct_fields.push(StructField {
                        name: name.clone(),
                        ty,
                    });
                }
                Type::Struct(struct_fields)
            }

            ExprData::Call { func, args } => {
                let func_def = *func;
                let args_clone: Vec<(String, ExprId)> = args.clone();

                if let Some(sig) = self.lower_ctx.function_sig(func_def) {
                    let (param_sig, ret_ty) = sig.clone();

                    // Check argument count
                    if args_clone.len() != param_sig.len() {
                        self.diagnostics.push(
                            Diagnostic::error(
                                format!(
                                    "expected {} arguments, found {}",
                                    param_sig.len(),
                                    args_clone.len()
                                ),
                                span,
                            ),
                        );
                        // Still try to type-check what we can
                    }

                    // Check each argument against the expected parameter type
                    for (arg_name, arg_expr_id) in &args_clone {
                        let arg_ty = self.infer_expr(body, *arg_expr_id);
                        // Find the matching parameter by name
                        if let Some((_, param_ty)) = param_sig.iter().find(|(n, _)| n == arg_name)
                        {
                            self.unify(param_ty, &arg_ty, body.span(*arg_expr_id));
                        } else {
                            self.diagnostics.push(
                                Diagnostic::error(
                                    format!("unexpected argument '{}'", arg_name),
                                    body.span(*arg_expr_id),
                                ),
                            );
                        }
                    }

                    // Check that all required params are provided
                    for (param_name, _) in &param_sig {
                        if !args_clone.iter().any(|(n, _)| n == param_name) {
                            self.diagnostics.push(
                                Diagnostic::error(
                                    format!("missing argument '{}'", param_name),
                                    span,
                                ),
                            );
                        }
                    }

                    ret_ty
                } else {
                    // Unknown function (already reported during lowering)
                    for (_, arg_expr_id) in &args_clone {
                        self.infer_expr(body, *arg_expr_id);
                    }
                    Type::Error
                }
            }

            ExprData::If {
                cond,
                then_expr,
                else_expr,
            } => {
                let cond_id = *cond;
                let then_id = *then_expr;
                let else_id = *else_expr;

                let cond_ty = self.infer_expr(body, cond_id);
                self.unify(&Type::Bool, &cond_ty, body.span(cond_id));

                let then_ty = self.infer_expr(body, then_id);

                if let Some(else_id) = else_id {
                    let else_ty = self.infer_expr(body, else_id);
                    // Both branches must have the same type
                    self.unify(&then_ty, &else_ty, span)
                } else {
                    // If without else - the result is the then type
                    // but this is only useful as a statement (we don't enforce void here)
                    then_ty
                }
            }

            ExprData::Block { stmts, tail } => {
                let stmts_clone: Vec<_> = stmts.iter().collect();
                let tail_id = *tail;

                for stmt in stmts_clone {
                    self.infer_stmt(body, stmt);
                }

                if let Some(tail_id) = tail_id {
                    self.infer_expr(body, tail_id)
                } else {
                    // Block with no tail expression produces unit/void
                    // For now, treat as Int (the language doesn't have a unit type)
                    // In practice, blocks without tails are used as statements
                    Type::Int
                }
            }

            ExprData::Binary { op, left, right } => {
                let op = *op;
                let left_id = *left;
                let right_id = *right;

                let left_ty = self.infer_expr(body, left_id);
                let right_ty = self.infer_expr(body, right_id);

                self.check_binary_op(op, &left_ty, &right_ty, span)
            }

            ExprData::Unary { op, expr } => {
                let op = *op;
                let inner_id = *expr;
                let inner_ty = self.infer_expr(body, inner_id);

                match op {
                    UnaryOp::Neg => {
                        if !inner_ty.is_numeric() && !inner_ty.is_error() {
                            self.diagnostics.push(
                                Diagnostic::error(
                                    format!(
                                        "cannot negate type {}; expected int or float",
                                        inner_ty
                                    ),
                                    span,
                                ),
                            );
                            Type::Error
                        } else {
                            inner_ty
                        }
                    }
                    UnaryOp::Not => {
                        self.unify(&Type::Bool, &inner_ty, body.span(inner_id));
                        Type::Bool
                    }
                }
            }

            ExprData::Return { value } => {
                let value_id = *value;
                if let Some(val_id) = value_id {
                    let val_ty = self.infer_expr(body, val_id);
                    self.unify(&self.return_type.clone(), &val_ty, body.span(val_id));
                }
                // Return expressions themselves don't produce a value in the type sense
                // but we give them the return type for consistency
                self.return_type.clone()
            }

            ExprData::Missing => Type::Error,
        };

        let resolved_ty = self.resolve(&ty);
        self.expr_types[expr_id.0 as usize] = resolved_ty.clone();
        resolved_ty
    }

    fn infer_stmt(&mut self, body: &Body, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                def_id,
                ty,
                value,
                span,
                ..
            } => {
                let value_ty = self.infer_expr(body, *value);

                let final_ty = if let Some(declared_ty) = ty {
                    // Explicit type annotation: unify with inferred type
                    self.unify(declared_ty, &value_ty, *span)
                } else {
                    // No type annotation: use inferred type
                    value_ty
                };

                self.local_types.insert(*def_id, final_ty);
            }
            Stmt::Expr { expr, .. } => {
                self.infer_expr(body, *expr);
            }
        }
    }

    /// Check a binary operation and return the result type.
    fn check_binary_op(
        &mut self,
        op: BinOp,
        left_ty: &Type,
        right_ty: &Type,
        span: Span,
    ) -> Type {
        // Skip checks if either side is already an error
        if left_ty.is_error() || right_ty.is_error() {
            return Type::Error;
        }

        match op {
            // Arithmetic: both sides must be same numeric type, result is same type
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                // Special case: string + string is concatenation
                if op == BinOp::Add
                    && matches!(left_ty, Type::String)
                    && matches!(right_ty, Type::String)
                {
                    return Type::String;
                }

                if !left_ty.is_numeric() {
                    self.diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "cannot apply '{}' to type {}; expected int or float",
                                op, left_ty
                            ),
                            span,
                        ),
                    );
                    return Type::Error;
                }

                self.unify(left_ty, right_ty, span)
            }

            // Comparison: both sides must be same type, result is bool
            BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                self.unify(left_ty, right_ty, span);
                Type::Bool
            }

            // Logical: both sides must be bool, result is bool
            BinOp::And | BinOp::Or => {
                self.unify(&Type::Bool, left_ty, span);
                self.unify(&Type::Bool, right_ty, span);
                Type::Bool
            }
        }
    }

    /// After inference is done, resolve all remaining type variables.
    fn finalize(&mut self) {
        let len = self.expr_types.len();
        for i in 0..len {
            let ty = self.expr_types[i].clone();
            self.expr_types[i] = self.resolve(&ty);
        }
        let local_keys: Vec<DefId> = self.local_types.keys().cloned().collect();
        for def_id in local_keys {
            let ty = self.local_types[&def_id].clone();
            self.local_types.insert(def_id, self.resolve(&ty));
        }
    }
}

/// Type-check a module and return the results.
pub fn type_check(module: &Module, lower_ctx: &LoweringContext) -> TypeCheckResult {
    let mut result = TypeCheckResult {
        function_expr_types: HashMap::new(),
        local_types: HashMap::new(),
        diagnostics: Vec::new(),
    };

    for func in &module.functions {
        let expr_count = func.body.len();
        let mut ctx = InferenceContext::new(lower_ctx, func.return_type.clone(), expr_count);

        // Register parameter types in the local context
        for param in &func.params {
            ctx.local_types.insert(param.def_id, param.ty.clone());
        }

        // Infer types for the function body
        let body_ty = ctx.infer_expr(&func.body, func.body.root);

        // The body's type must match the declared return type (for tail expressions)
        // Only check if the body is a block with a tail expression
        if let ExprData::Block { tail: Some(_), .. } = func.body.expr(func.body.root) {
            ctx.unify(&func.return_type, &body_ty, func.span);
        }

        ctx.finalize();

        result
            .function_expr_types
            .insert(func.def_id, ctx.expr_types);
        result.local_types.extend(ctx.local_types);
        result.diagnostics.extend(ctx.diagnostics);
    }

    result
}
