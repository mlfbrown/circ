//! Lowering from AST to IR 
//! MLFB note: We may to having each enum take a struct for accessor reasons
//! MLFB note: Say something about the semantics of operators being lower level
//! (e.g., not wrapping say C shift semantics and insteading following SMT lib)
use crate::circify::{Circify, Embeddable, CirCtx};
use crate::ir::term::{Sort, Term, Op, PartyId, term, check};
use crate::ir::term::{BV_ADD, BV_SUB, BV_MUL, BV_UDIV};
use crate::front::ast::{TransUnit, TransUnitPart, Function, Stmt, Expr, Var};
use crate::front::ast::{BinOp};

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

/// Ultimately, we just need variable type information 
impl<T: Typed> Typed for Var<T> {
    fn get_type(&self) -> Sort {
	self.meta.get_type()
    }
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
