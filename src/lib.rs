#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate rustc;

use std::borrow::ToOwned;
use std::rc::Rc;
use syntax::ast::{
    BinOp_, BiAdd, BiSub, BiMul, Delimited,
    Expr, ExprAssign, ExprAssignOp, ExprBinary, ExprUnary,
    LitInt, UnsuffixedIntLit, Sign, UnNeg, UnNot,
    Ident, Mac, TokenTree, TtDelimited
};
use syntax::codemap::{DUMMY_SP, Span};
use syntax::ext::base::{ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::fold::{self, Folder};
use syntax::parse;
use syntax::parse::token::{self, DelimToken};
use syntax::ptr::P;
use rustc::plugin::Registry;

struct WrappingFolder<'cx> {
    cx: &'cx ExtCtxt<'cx>,
}

impl<'cx> Folder for WrappingFolder<'cx> {
    fn fold_mac(&mut self, mac: Mac) -> Mac {
        self.cx.span_fatal(mac.span, "cannot call nested macros in a `wrapping!` block");
    }

    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> {
        expr.map(|expr| { match expr.node {
            ExprUnary(UnNeg, inner) => {
                // Recurse in sub-expressions
                let inner = self.fold_expr(inner);
                // Rewrite `-a` to `a.wrapping_mul(!0)`
                // This is equivalent to `a.wrapping_mul(-1)`, except it
                // works on unsigned types as well
                let method = token::str_to_ident("wrapping_mul");
                let zero = self.cx.expr_lit(
                    DUMMY_SP, LitInt(0, UnsuffixedIntLit(Sign::Plus)));
                let neg_one = self.cx.expr_unary(DUMMY_SP, UnNot, zero);
                self.cx.expr_method_call(expr.span, inner, method, vec![neg_one])
                    .and_then(|e| e)
            }
            ExprBinary(op, left, right) => {
                // Recurse in sub-expressions
                let left = self.fold_expr(left);
                let right = self.fold_expr(right);
                // Rewrite e.g. `a + b` to `a.wrapping_add(b)`
                match wrapping_method(op.node) {
                    Some(method) => self.cx.expr_method_call(
                        expr.span, left, method, vec![right]).and_then(|e| e),
                    None =>
                        Expr {
                            node: ExprBinary(op, left, right),
                            ..expr
                        },
                }
            },
            ExprAssignOp(op, target, source) => {
                // Recurse in sub-expressions
                let source = self.fold_expr(source);
                // Rewrite e.g. `a += b` to `a = a.wrapping_add(b)`
                Expr {
                    node: match wrapping_method(op.node) {
                        Some(method) => {
                            let call = self.cx.expr_method_call(
                                expr.span, target.clone(), method, vec![source]);
                            ExprAssign(target, call)
                        },
                        None => ExprAssignOp(op, target, source),
                    },
                    ..expr
                }
            },
            _ => fold::noop_fold_expr(expr, self),
        }})
    }
}

/// Returns the wrapping version of an operator, if applicable.
fn wrapping_method(op: BinOp_) -> Option<Ident> {
    Some(token::str_to_ident(match op {
        BiAdd => "wrapping_add",
        BiSub => "wrapping_sub",
        BiMul => "wrapping_mul",
        _ => return None,
    }))
}

fn expand_wrapping<'cx>(cx: &'cx mut ExtCtxt, sp: Span, tts: &[TokenTree]) -> Box<MacResult + 'cx> {
    // Parse the token tree as a block
    let block = TtDelimited(sp, Rc::new(Delimited {
        delim: DelimToken::Brace,
        open_span: DUMMY_SP,
        tts: tts.to_owned(),
        close_span: DUMMY_SP,
    }));
    let mut parser = parse::tts_to_parser(cx.parse_sess, vec![block],
                                          cx.cfg.clone());
    let block = parser.parse_block().unwrap_or_else(|err| panic!(err));
    // Perform the fold
    let block = WrappingFolder { cx: cx }.fold_block(block);
    // Done!
    MacEager::expr(cx.expr_block(block))
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("wrapping", expand_wrapping);
}
