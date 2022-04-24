//! Front-end for directly parsing low-level AST code. 
//! This is intended mostly for (differential) testing.

use pest::error::Error;
use pest::Parser;
use pest_derive::Parser;

/// Pest parser for the AST 
#[derive(Parser)]
#[grammar = "front/ast/grammar.pest"] // relative to src
struct ASTParser;

/// A translation unit. See note below
/// Expect to expand this definition
pub struct TransUnit<T> {
    /// Filename for the translation unit
    pub filename: String,
    /// Contents 
    pub units: Vec<TransUnitPart<T>>,
}

/// Part of a translation unit.
/// MLFB note: probably wrong terminology since this is language-independent and
/// not C or C++ specific, but whatever for now. 
pub enum TransUnitPart<T> {
    /// Global variable declaration
    GlobalDecl(Var<T>),
    /// Function declaration (temporary?)
    FunctionDecl(Var<T>, Vec<Var<T>>),
    /// Function definition
    FunctionDef(Function<T>), 
}

/// A function definition 
pub struct Function<T> {
    /// Name and return type 
    pub name: Var<T>,
    /// Arguments 
    pub args: Vec<Var<T>>,
    /// Function body 
    pub body: Stmt<T>,
}

/// A statement node. (Test will move to high-level AST description)
/// Statements are parameterized over arbitrary metadata---for example, custom types,
/// source spans, analysis information, and more. 
pub enum Stmt<T> {
    /// Declare variable with type 
    DeclStmt(Var<T>), 
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
    UDivOp, 
}

/// A variable (e.g., x) 
pub struct Var<T> {
    /// A variable name 
    pub name: String,
    /// The variable's metadata (e.g., type)
    pub meta: T,
}

pub enum Literal {
    /// Booleans (e.g., true)
    BoolLit(bool),
}


