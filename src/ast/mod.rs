//! Low-level, generic AST definition
//!
//! This generic AST assumes that type checking has already taken place
//! 
//! Unlike the IR (src/ir), the AST follows the RAM/register model:
//! it allows stateful operations and control flow. The most important
//! types and functions are:
//!
//!    * AST
//!
//! MLFB note: We may to having each enum take a struct for accessor reasons
//! MLFB note: Say something about the semantics of operators being lower level
//! (e.g., not wrapping say C shift semantics and insteading following SMT lib)
use crate::circify::{Circify, Embeddable, CirCtx};
use crate::ir::term::{Sort, Term, Op, PartyId, term, check};
use crate::ir::term::{BV_ADD, BV_SUB, BV_MUL, BV_UDIV};

/// Temporary 
pub type PlainStmt = Stmt<()>;
/// Temporary
pub type TypedStmt = Stmt<Sort>;


/// A "typed" AST provides accessors that allow us to automatically extract
/// the types of nodes. This is important for lowering to the IR representation:
/// the IR requires type information for each variable and literal. 
pub trait Typed {
    fn get_type(&self) -> Sort; 
}

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

impl<T: Typed> Typed for Var<T> {
    fn get_type(&self) -> Sort {
	self.meta.get_type()
    }
}

/// A literal
pub enum Literal {
    /// Booleans (e.g., true)
    BoolLit(bool),
}

/// Generic AST langauge definition
/// Ask Alex what this is/should be 
pub struct ASTDef;

impl Embeddable for ASTDef {
    /// Terms in the low-level AST are just IR terms.
    /// It's possible that we'll want a custom term type users define themselves (e.g., in
    /// the C version it would be CTerm), but I'm doing it this way for now. 
    type T = Term;
    /// ...and thus, the types of IR terms are just sorts 
    type Ty = Sort;

    fn declare(
        &self,
        ctx: &mut CirCtx,
        ty: &Self::Ty,	
	raw_name: String,
	user_name: Option<String>,
	visibility: Option<PartyId>,
    ) -> Self::T { panic! () }

    fn ite(&self, _ctx: &mut CirCtx, cond: Term, t: Self::T, f: Self::T) -> Self::T {
	assert!(check(&t) == check(&f));
	// Make sure it's only allowed sorts too
	term![Op::Ite; cond, t, f]
    }

    fn assign(
        &self,
        ctx: &mut CirCtx,
        ty: &Self::Ty,
        name: String,
        t: Self::T,
        visibility: Option<PartyId>,
    ) -> Self::T {
	assert!(check(&t) == *ty);
	ctx.cs.borrow_mut().assign(&name, t, visibility)
    }

    fn values(&self) -> bool { false }

    fn type_of(&self, term: &Self::T) -> Self::Ty { check(term) }

    fn initialize_return(&self, ty: &Self::Ty, _ssa_name: &String) -> Self::T { panic!() }
}


/// Struct for lowering from AST to IR.
/// It lives here for now but all of this will probably move.
pub struct ASTGen {
   circ: Circify<ASTDef>, 
}

impl ASTGen {

    /// Lower a translation unit from AST to IR
    pub fn lower_trans_unit<T: Typed> (trans_unit: TransUnit<T>) -> () {

    }

    pub fn lower_stmt<T: Typed> (stmt: Stmt<T>) -> () {
	match stmt {
	    Stmt::DeclStmt(var) => panic!(),
	    Stmt::AssignStmt(lhs, rhs) => panic!(),
	    Stmt::ReturnStmt(to_ret) => panic!(),
	    Stmt::IfStmt(cond, true_br, false_br) => panic!(),
	    _ => unimplemented!(),
	}
    }
    
    /// Lower an expression from AST to IR
    fn lower_expr<T: Typed>(&self, expr: Expr<T>) -> Term {
	match expr {
	    Expr::BinExpr(op, left, right) => {
		let left_term = self.lower_expr(*left);
		let right_term = self.lower_expr(*right);
		let args = vec![left_term, right_term];
		match op {
		    BinOp::AddOp => term(BV_ADD, args),
		    BinOp::SubOp => term(BV_SUB, args),
		    BinOp::MulOp => term(BV_MUL, args),
		    BinOp::UDivOp => term(BV_UDIV, args), 
		}
	    }
	    Expr::VarExpr(v) => self.lower_var(v), 
	    _ => unimplemented!()
	}
    }

    fn lower_var<T: Typed>(&self, v: Var<T>) -> Term {
	let sort = v.get_type();
	term(Op::Var(v.name, sort), vec![])
    }

}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(3, 3);
    }
    
}
