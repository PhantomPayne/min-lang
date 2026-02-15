//! AST → HIR lowering with name resolution.
//!
//! This pass walks the AST and produces HIR with:
//! - All identifiers resolved to DefIds
//! - All type expressions resolved to concrete Types
//! - Type alias expansion
//! - Scope tracking for variable resolution

use std::collections::HashMap;

use min_diagnostics::Diagnostic;
use min_syntax::ast;
use min_types::{StructField, Type};

use crate::hir::*;

/// Built-in function signature: (param_name, param_type) pairs + return type.
struct BuiltinFn {
    params: Vec<(&'static str, Type)>,
    return_type: Type,
}

/// The lowering context that tracks scopes and definitions.
pub struct LoweringContext {
    next_def_id: u32,
    /// Stack of scopes. Each scope maps names to DefIds.
    scopes: Vec<HashMap<String, DefId>>,
    /// Map from DefId to its type (for variables/params).
    def_types: HashMap<DefId, Type>,
    /// Map from type alias name to its resolved type.
    type_aliases: HashMap<String, Type>,
    /// Map from function name to its DefId.
    function_defs: HashMap<String, DefId>,
    /// Map from function DefId to its signature (param types, return type).
    function_sigs: HashMap<DefId, (Vec<(String, Type)>, Type)>,
    /// Built-in function signatures.
    builtins: HashMap<String, BuiltinFn>,
    /// Accumulated diagnostics.
    pub diagnostics: Vec<Diagnostic>,
}

impl LoweringContext {
    pub fn new() -> Self {
        let mut ctx = Self {
            next_def_id: 0,
            scopes: vec![HashMap::new()],
            def_types: HashMap::new(),
            type_aliases: HashMap::new(),
            function_defs: HashMap::new(),
            function_sigs: HashMap::new(),
            builtins: HashMap::new(),
            diagnostics: Vec::new(),
        };
        ctx.register_builtins();
        ctx
    }

    fn register_builtins(&mut self) {
        // Register well-known stdlib functions
        let builtins = vec![
            ("sqrt", BuiltinFn {
                params: vec![("x", Type::Float)],
                return_type: Type::Float,
            }),
            ("to_string", BuiltinFn {
                params: vec![("value", Type::Int)],
                return_type: Type::String,
            }),
            ("concat", BuiltinFn {
                params: vec![("a", Type::String), ("b", Type::String)],
                return_type: Type::String,
            }),
            ("log", BuiltinFn {
                params: vec![("message", Type::String)],
                return_type: Type::String,
            }),
        ];
        for (name, sig) in builtins {
            let def_id = self.fresh_def_id();
            self.builtins.insert(name.to_string(), sig);
            self.function_defs.insert(name.to_string(), def_id);
            // Register builtin signatures
            let params: Vec<(String, Type)> = self.builtins[name]
                .params.iter()
                .map(|(n, t)| (n.to_string(), t.clone()))
                .collect();
            let ret = self.builtins[name].return_type.clone();
            self.function_sigs.insert(def_id, (params, ret));
        }
    }

    fn fresh_def_id(&mut self) -> DefId {
        let id = DefId(self.next_def_id);
        self.next_def_id += 1;
        id
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, def_id: DefId) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), def_id);
        }
    }

    fn resolve_name(&self, name: &str) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(def_id) = scope.get(name) {
                return Some(*def_id);
            }
        }
        None
    }

    /// Look up the type of a defined variable/parameter.
    pub fn def_type(&self, def_id: DefId) -> Option<&Type> {
        self.def_types.get(&def_id)
    }

    /// Look up a function signature by DefId.
    pub fn function_sig(&self, def_id: DefId) -> Option<&(Vec<(String, Type)>, Type)> {
        self.function_sigs.get(&def_id)
    }

    /// Resolve a type expression from the AST to a concrete Type.
    fn resolve_type_expr(&self, type_expr: &ast::TypeExpr) -> Type {
        match type_expr {
            ast::TypeExpr::Named(ident) => self.resolve_type_name(&ident.name),
            ast::TypeExpr::Generic { name, args, .. } => {
                let params: Vec<Type> = args.iter().map(|a| self.resolve_type_expr(a)).collect();
                Type::Generic {
                    name: name.name.clone(),
                    params,
                }
            }
            ast::TypeExpr::Struct { fields, .. } => {
                let resolved_fields: Vec<StructField> = fields
                    .iter()
                    .map(|f| StructField {
                        name: f.name.name.clone(),
                        ty: self.resolve_type_expr(&f.ty),
                    })
                    .collect();
                Type::Struct(resolved_fields)
            }
        }
    }

    /// Resolve a type name (primitive or alias).
    fn resolve_type_name(&self, name: &str) -> Type {
        match name {
            "int" => Type::Int,
            "float" => Type::Float,
            "bool" => Type::Bool,
            "string" => Type::String,
            _ => {
                if let Some(ty) = self.type_aliases.get(name) {
                    ty.clone()
                } else {
                    // Unknown type name, but might be a forward reference
                    // Return Error and report diagnostic later
                    Type::Error
                }
            }
        }
    }

    /// Lower a complete source file to an HIR Module.
    pub fn lower_source_file(&mut self, source: &ast::SourceFile) -> Module {
        // First pass: register all type aliases and function signatures.
        // This allows forward references.
        for item in &source.items {
            match item {
                ast::Item::TypeAlias(alias) => {
                    let ty = self.resolve_type_expr(&alias.ty);
                    self.type_aliases.insert(alias.name.name.clone(), ty);
                }
                ast::Item::Function(_) => {}
            }
        }

        // Second pass: re-resolve type aliases now that all are registered,
        // and register function signatures.
        let alias_names: Vec<String> = self.type_aliases.keys().cloned().collect();
        for name in &alias_names {
            // Re-resolve to pick up aliases that reference other aliases
            let ty = self.type_aliases[name].clone();
            let re_resolved = self.re_resolve_type(&ty);
            self.type_aliases.insert(name.clone(), re_resolved);
        }

        for item in &source.items {
            if let ast::Item::Function(func) = item {
                let def_id = self.fresh_def_id();
                self.function_defs.insert(func.name.name.clone(), def_id);

                let params: Vec<(String, Type)> = func
                    .params
                    .iter()
                    .map(|p| (p.name.name.clone(), self.resolve_type_expr(&p.ty)))
                    .collect();
                let ret = self.resolve_type_expr(&func.return_type);
                self.function_sigs.insert(def_id, (params, ret));
            }
        }

        // Third pass: lower all items.
        let mut functions = Vec::new();
        let mut type_aliases = Vec::new();

        for item in &source.items {
            match item {
                ast::Item::Function(func) => {
                    functions.push(self.lower_function(func));
                }
                ast::Item::TypeAlias(alias) => {
                    type_aliases.push(self.lower_type_alias(alias));
                }
            }
        }

        Module {
            functions,
            type_aliases,
        }
    }

    /// Re-resolve a type to expand aliases that reference other aliases.
    fn re_resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Error => {
                // This might have been an alias that wasn't yet registered
                Type::Error
            }
            Type::Struct(fields) => {
                let new_fields = fields
                    .iter()
                    .map(|f| StructField {
                        name: f.name.clone(),
                        ty: self.re_resolve_type(&f.ty),
                    })
                    .collect();
                Type::Struct(new_fields)
            }
            Type::Generic { name, params } => {
                // Try to resolve the name as an alias
                if let Some(resolved) = self.type_aliases.get(name) {
                    return resolved.clone();
                }
                Type::Generic {
                    name: name.clone(),
                    params: params.iter().map(|p| self.re_resolve_type(p)).collect(),
                }
            }
            other => other.clone(),
        }
    }

    fn lower_type_alias(&mut self, alias: &ast::TypeAlias) -> TypeAlias {
        let def_id = self.fresh_def_id();
        let ty = self.type_aliases.get(&alias.name.name).cloned().unwrap_or(Type::Error);
        TypeAlias {
            def_id,
            name: alias.name.name.clone(),
            ty,
            span: alias.span,
        }
    }

    fn lower_function(&mut self, func: &ast::Function) -> Function {
        let def_id = *self.function_defs.get(&func.name.name).unwrap();
        let sig = self.function_sigs[&def_id].clone();

        let return_type = sig.1.clone();

        let mut body = Body::new();
        self.push_scope();

        // Register parameters in scope
        let mut params = Vec::new();
        for (i, ast_param) in func.params.iter().enumerate() {
            let param_def_id = self.fresh_def_id();
            let ty = sig.0[i].1.clone();
            self.define(&ast_param.name.name, param_def_id);
            self.def_types.insert(param_def_id, ty.clone());
            params.push(Param {
                def_id: param_def_id,
                name: ast_param.name.name.clone(),
                ty,
                span: ast_param.span,
            });
        }

        // Lower the body block
        let root = self.lower_block(&func.body, &mut body);
        body.root = root;

        self.pop_scope();

        Function {
            def_id,
            name: func.name.name.clone(),
            params,
            return_type,
            body,
            span: func.span,
        }
    }

    fn lower_block(&mut self, block: &ast::Block, body: &mut Body) -> ExprId {
        self.push_scope();

        let mut stmts = Vec::new();
        for stmt in &block.stmts {
            stmts.push(self.lower_stmt(stmt, body));
        }

        let tail = block
            .tail_expr
            .as_ref()
            .map(|expr| self.lower_expr(expr, body));

        self.pop_scope();

        body.alloc(ExprData::Block { stmts, tail }, block.span)
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt, body: &mut Body) -> Stmt {
        match stmt {
            ast::Stmt::Let(let_stmt) => {
                let value = self.lower_expr(&let_stmt.value, body);
                let ty = let_stmt.ty.as_ref().map(|te| self.resolve_type_expr(te));
                let def_id = self.fresh_def_id();

                // Register the variable in the current scope
                self.define(&let_stmt.name.name, def_id);
                // If there's an explicit type, record it
                if let Some(ref ty) = ty {
                    self.def_types.insert(def_id, ty.clone());
                }

                Stmt::Let {
                    def_id,
                    name: let_stmt.name.name.clone(),
                    ty,
                    value,
                    span: let_stmt.span,
                }
            }
            ast::Stmt::Expr(expr, span) => {
                let expr_id = self.lower_expr(expr, body);
                Stmt::Expr {
                    expr: expr_id,
                    span: *span,
                }
            }
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr, body: &mut Body) -> ExprId {
        match expr {
            ast::Expr::IntLit(val, span) => {
                body.alloc(ExprData::IntLit(*val), *span)
            }
            ast::Expr::FloatLit(val, span) => {
                body.alloc(ExprData::FloatLit(*val), *span)
            }
            ast::Expr::StringLit(val, span) => {
                body.alloc(ExprData::StringLit(val.clone()), *span)
            }
            ast::Expr::BoolLit(val, span) => {
                body.alloc(ExprData::BoolLit(*val), *span)
            }
            ast::Expr::StringInterp(interp) => {
                let parts: Vec<StringInterpPart> = interp
                    .parts
                    .iter()
                    .map(|part| match part {
                        ast::StringPart::Lit(s, _) => StringInterpPart::Lit(s.clone()),
                        ast::StringPart::Expr(e, _) => {
                            let id = self.lower_expr(e, body);
                            StringInterpPart::Expr(id)
                        }
                    })
                    .collect();
                body.alloc(ExprData::StringInterp(parts), interp.span)
            }
            ast::Expr::Ident(ident) => {
                if let Some(def_id) = self.resolve_name(&ident.name) {
                    body.alloc(ExprData::Var(def_id), ident.span)
                } else {
                    self.diagnostics.push(Diagnostic::error(
                        format!("undefined variable '{}'", ident.name),
                        ident.span,
                    ));
                    body.alloc(ExprData::Missing, ident.span)
                }
            }
            ast::Expr::FieldAccess(fa) => {
                let expr_id = self.lower_expr(&fa.expr, body);
                body.alloc(
                    ExprData::FieldAccess {
                        expr: expr_id,
                        field: fa.field.name.clone(),
                    },
                    fa.span,
                )
            }
            ast::Expr::Struct(s) => {
                let fields: Vec<(String, ExprId)> = s
                    .fields
                    .iter()
                    .map(|f| {
                        let val = self.lower_expr(&f.value, body);
                        (f.name.name.clone(), val)
                    })
                    .collect();
                body.alloc(ExprData::StructLit { fields }, s.span)
            }
            ast::Expr::Call(call) => {
                let func_name = &call.function.name;
                if let Some(&def_id) = self.function_defs.get(func_name.as_str()) {
                    let args: Vec<(String, ExprId)> = call
                        .args
                        .iter()
                        .map(|arg| {
                            let val = self.lower_expr(&arg.value, body);
                            (arg.name.name.clone(), val)
                        })
                        .collect();
                    body.alloc(ExprData::Call { func: def_id, args }, call.span)
                } else {
                    self.diagnostics.push(Diagnostic::error(
                        format!("undefined function '{}'", func_name),
                        call.span,
                    ));
                    // Still lower the arguments
                    for arg in &call.args {
                        self.lower_expr(&arg.value, body);
                    }
                    body.alloc(ExprData::Missing, call.span)
                }
            }
            ast::Expr::If(if_expr) => {
                let cond = self.lower_expr(&if_expr.condition, body);
                let then_expr = self.lower_block(&if_expr.then_block, body);
                let else_expr = if_expr.else_branch.as_ref().map(|eb| match eb {
                    ast::ElseBranch::Block(block) => self.lower_block(block, body),
                    ast::ElseBranch::ElseIf(elif) => {
                        // Desugar `else if` to a block containing an if
                        let nested = self.lower_if_expr(elif, body);
                        nested
                    }
                });
                body.alloc(
                    ExprData::If {
                        cond,
                        then_expr,
                        else_expr,
                    },
                    if_expr.span,
                )
            }
            ast::Expr::Block(block) => self.lower_block(block, body),
            ast::Expr::Binary(bin) => {
                let left = self.lower_expr(&bin.left, body);
                let right = self.lower_expr(&bin.right, body);
                body.alloc(
                    ExprData::Binary {
                        op: bin.op,
                        left,
                        right,
                    },
                    bin.span,
                )
            }
            ast::Expr::Unary(unary) => {
                let expr_id = self.lower_expr(&unary.expr, body);
                let op = match unary.op {
                    ast::UnaryOp::Neg => UnaryOp::Neg,
                    ast::UnaryOp::Not => UnaryOp::Not,
                };
                body.alloc(ExprData::Unary { op, expr: expr_id }, unary.span)
            }
            ast::Expr::Return(ret) => {
                let value = ret.value.as_ref().map(|v| self.lower_expr(v, body));
                body.alloc(ExprData::Return { value }, ret.span)
            }
            ast::Expr::Error(span) => {
                body.alloc(ExprData::Missing, *span)
            }
        }
    }

    fn lower_if_expr(&mut self, if_expr: &ast::IfExpr, body: &mut Body) -> ExprId {
        let cond = self.lower_expr(&if_expr.condition, body);
        let then_expr = self.lower_block(&if_expr.then_block, body);
        let else_expr = if_expr.else_branch.as_ref().map(|eb| match eb {
            ast::ElseBranch::Block(block) => self.lower_block(block, body),
            ast::ElseBranch::ElseIf(elif) => self.lower_if_expr(elif, body),
        });
        body.alloc(
            ExprData::If {
                cond,
                then_expr,
                else_expr,
            },
            if_expr.span,
        )
    }
}
