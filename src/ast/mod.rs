//! High-level AST definition
//!
//! Unlike the IR (src/ir), the AST follows the RAM/register model:
//! it allows stateful operations and control flow. The most important
//! types and functions are:
//!
//!    * AST
//!
//! MLFB note: We may to having each enum take a struct for accessor reasons

/// A statement node
pub enum Stmt {
    /// An assignment. For now, we only support assignment statements.
    /// In the future, we have a few options:
    /// 1. Keep it that way (and let C AST -> generic AST mappers figure it out)
    /// 2. Keep it that way and support some other flexible expression type like sequences
    /// 3. Support both assignment statements and expressions, but have a check that ensures
    ///    you don't intermix both in the same AST (because that seems...concerning?)
    AssignStmt(Var, Expr),
    /// A return
    ReturnStmt(Option<Expr>), 
    /// An If statement, with optional branch for optional else. 
    IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    /// A while loop.
    WhileStmt(Expr, Box<Stmt>),
    /// A compound statement (i.e., a list of statements)
    CompoundStmt(Vec<Box<Stmt>>), 
}

/// An expression node (e.g., x + 5) 
pub enum Expr {
    /// A constant (e.g., 5)
    ConstExpr(Literal),
    /// A variable (e.g., x)
    VarExpr(Var),
    /// A binary expression (e.g., x + 5)
    BinExpr(BinOp, Box<Expr>, Box<Expr>), 
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
pub struct Var {
    /// A variable name 
    pub name: String, 
}

/// A literal
pub enum Literal {
    /// Booleans (e.g., true)
    BoolLit(bool),
}

