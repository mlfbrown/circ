//! Front-end for directly parsing low-level AST code. 
//! This is intended mostly for (differential) testing.

use pest::error::Error;
use pest::Parser;
use pest_derive::Parser;

/// Pest parser for the AST 
#[derive(Parser)]
#[grammar = "front/ast/grammar.pest"] // relative to src
struct ASTParser;


