//! High-level AST definition
//!
//! Unlike the IR (src/ir), the AST follows the RAM/register model:
//! it allows stateful operations and control flow. The most important
//! types and functions are:
//!
//!    * AST
//!
//! MLFB note: We may to having each enum take a struct for accessor reasons
use crate::ir::term::Sort; 

/// Temporary 
pub type PlainStmt = Stmt<()>;
/// Temporary
pub type TypedStmt = Stmt<Sort>;

/// A statement node
pub enum Stmt<T> {
    /// An assignment. For now, we only support assignment statements.
    /// In the future, we have a few options:
    /// 1. Keep it that way (and let C AST -> generic AST mappers figure it out)
    /// 2. Keep it that way and support some other flexible expression type like sequences
    /// 3. Support both assignment statements and expressions, but have a check that ensures
    ///    you don't intermix both in the same AST (because that seems...concerning?)
    AssignStmt(Var<T>, Expr<T>),
    /// A return
    ReturnStmt(Option<Expr<T>>), 
    /// An If statement, with optional branch for optional else. 
    IfStmt(Expr<T>, Box<Stmt<T>>, Option<Box<Stmt<T>>>),
    /// A while loop.
    WhileStmt(Expr<T>, Box<Stmt<T>>),
    /// A compound statement (i.e., a list of statements)
    CompoundStmt(Vec<Box<Stmt<T>>>), 
}

/// An expression node (e.g., x + 5) 
pub enum Expr<T> {
    /// A constant (e.g., 5)
    ConstExpr(Literal),
    /// A variable (e.g., x)
    VarExpr(Var<T>),
    /// A binary expression (e.g., x + 5)
    BinExpr(BinOp, Box<Expr<T>>, Box<Expr<T>>), 
}

/// A binary operator. Note: distinct from comparison 
pub enum BinOp {
    /// +
    AddOp,
    /// -
    SubOp,
    /// *
    MulOp,
    /// / 
    DivOp, 
}

/// A variable (e.g., x) 
pub struct Var<T> {
    /// A variable name 
    pub name: String,
    /// The variable's metadata (e.g., type)
    pub meta: T,
}

/// A literal
pub enum Literal {
    /// Booleans (e.g., true)
    BoolLit(bool),
}

