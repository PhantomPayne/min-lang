//! Type representation for the Min language.
//!
//! Types in Min use structural equality: two types are equal if they have
//! the same structure, regardless of any type alias names.

use std::fmt;

/// A concrete type in the Min language.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// 32-bit signed integer.
    Int,
    /// 32-bit floating point.
    Float,
    /// Boolean value.
    Bool,
    /// UTF-8 string.
    String,
    /// Structural record type with named fields.
    Struct(Vec<StructField>),
    /// Generic type application, e.g. `List<int>`.
    Generic {
        name: std::string::String,
        params: Vec<Type>,
    },
    /// Type variable for inference.
    Var(TypeVar),
    /// Error recovery placeholder.
    Error,
}

/// A field in a structural record type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: std::string::String,
    pub ty: Type,
}

/// A type variable used during type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

impl Type {
    /// Returns true if this type is numeric (int or float).
    pub fn is_numeric(&self) -> bool {
        matches!(self, Type::Int | Type::Float)
    }

    /// Returns true if this type contains the given type variable.
    pub fn contains_var(&self, var: TypeVar) -> bool {
        match self {
            Type::Var(v) => *v == var,
            Type::Struct(fields) => fields.iter().any(|f| f.ty.contains_var(var)),
            Type::Generic { params, .. } => params.iter().any(|p| p.contains_var(var)),
            _ => false,
        }
    }

    /// Returns true if this type is the Error sentinel.
    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }

    /// Look up a field by name in a struct type. Returns the field type if found.
    pub fn field_type(&self, name: &str) -> Option<&Type> {
        match self {
            Type::Struct(fields) => fields.iter().find(|f| f.name == name).map(|f| &f.ty),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Struct(fields) => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.ty)?;
                }
                write!(f, " }}")
            }
            Type::Generic { name, params } => {
                write!(f, "{}<", name)?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ">")
            }
            Type::Var(v) => write!(f, "?{}", v.0),
            Type::Error => write!(f, "<error>"),
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn primitive_equality() {
        assert_eq!(Type::Int, Type::Int);
        assert_eq!(Type::Float, Type::Float);
        assert_eq!(Type::Bool, Type::Bool);
        assert_eq!(Type::String, Type::String);
        assert_ne!(Type::Int, Type::Float);
        assert_ne!(Type::Bool, Type::String);
    }

    #[test]
    fn structural_equality() {
        let point1 = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
            StructField { name: "y".into(), ty: Type::Int },
        ]);
        let point2 = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
            StructField { name: "y".into(), ty: Type::Int },
        ]);
        // Same structure = same type (structural typing)
        assert_eq!(point1, point2);
    }

    #[test]
    fn structural_inequality_different_fields() {
        let point = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
            StructField { name: "y".into(), ty: Type::Int },
        ]);
        let size = Type::Struct(vec![
            StructField { name: "width".into(), ty: Type::Int },
            StructField { name: "height".into(), ty: Type::Int },
        ]);
        assert_ne!(point, size);
    }

    #[test]
    fn structural_inequality_different_types() {
        let int_point = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
            StructField { name: "y".into(), ty: Type::Int },
        ]);
        let float_point = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Float },
            StructField { name: "y".into(), ty: Type::Float },
        ]);
        assert_ne!(int_point, float_point);
    }

    #[test]
    fn nested_struct_equality() {
        let inner = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
        ]);
        let outer1 = Type::Struct(vec![
            StructField { name: "point".into(), ty: inner.clone() },
            StructField { name: "label".into(), ty: Type::String },
        ]);
        let outer2 = Type::Struct(vec![
            StructField { name: "point".into(), ty: inner },
            StructField { name: "label".into(), ty: Type::String },
        ]);
        assert_eq!(outer1, outer2);
    }

    #[test]
    fn field_type_lookup() {
        let ty = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
            StructField { name: "y".into(), ty: Type::Float },
        ]);
        assert_eq!(ty.field_type("x"), Some(&Type::Int));
        assert_eq!(ty.field_type("y"), Some(&Type::Float));
        assert_eq!(ty.field_type("z"), None);
    }

    #[test]
    fn contains_var() {
        let var = TypeVar(0);
        assert!(Type::Var(var).contains_var(var));
        assert!(!Type::Int.contains_var(var));

        let ty = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Var(var) },
        ]);
        assert!(ty.contains_var(var));
        assert!(!ty.contains_var(TypeVar(1)));
    }

    #[test]
    fn display_types() {
        assert_eq!(Type::Int.to_string(), "int");
        assert_eq!(Type::Float.to_string(), "float");
        assert_eq!(Type::Bool.to_string(), "bool");
        assert_eq!(Type::String.to_string(), "string");
        assert_eq!(Type::Error.to_string(), "<error>");

        let point = Type::Struct(vec![
            StructField { name: "x".into(), ty: Type::Int },
            StructField { name: "y".into(), ty: Type::Int },
        ]);
        assert_eq!(point.to_string(), "{ x: int, y: int }");
    }

    #[test]
    fn is_numeric() {
        assert!(Type::Int.is_numeric());
        assert!(Type::Float.is_numeric());
        assert!(!Type::Bool.is_numeric());
        assert!(!Type::String.is_numeric());
    }
}
