#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate rustc_plugin;
extern crate syntax;

use std::borrow::ToOwned;
use syntax::ast::{BinOpKind, Expr, ExprKind, Ident, Mac, UnOp};
use rustc_plugin::Registry;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::ext::base::{DummyResult, ExtCtxt, MacEager, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::fold::{self, Folder};
use syntax::parse;
use syntax::parse::token::{self, DelimToken};
use syntax::ptr::P;
use syntax::tokenstream::{Delimited, TokenTree};

struct WrappingFolder<'cx, 'a: 'cx> {
    cx: &'cx ExtCtxt<'a>,
}

impl<'cx, 'a> Folder for WrappingFolder<'cx, 'a> {
    fn fold_mac(&mut self, mac: Mac) -> Mac {
        self.cx.span_err(mac.span, "cannot call nested macros in a `wrapping!` block");
        mac
    }

    fn fold_expr(&mut self, expr: P<Expr>) -> P<Expr> {
        expr.map(|expr| { match expr.node {
            ExprKind::Unary(UnOp::Neg, inner) => {
                // Recurse in sub-expressions
                let inner = self.fold_expr(inner);
                // Rewrite `-a` to `a.wrapping_neg()`
                let method = token::str_to_ident("wrapping_neg");
                self.cx.expr_method_call(expr.span, inner, method, vec![])
                    .and_then(|e| e)
            }
            ExprKind::Binary(op, left, right) => {
                // Recurse in sub-expressions
                let left = self.fold_expr(left);
                let right = self.fold_expr(right);
                // Rewrite e.g. `a + b` to `a.wrapping_add(b)`
                match wrapping_method(op.node) {
                    Some(method) => self.cx.expr_method_call(
                        expr.span, left, method, vec![right]).and_then(|e| e),
                    None =>
                        Expr {
                            node: ExprKind::Binary(op, left, right),
                            ..expr
                        },
                }
            },
            ExprKind::AssignOp(op, target, source) => {
                // Recurse in sub-expressions
                let source = self.fold_expr(source);
                // Rewrite e.g. `a += b` to `a = a.wrapping_add(b)`
                Expr {
                    node: match wrapping_method(op.node) {
                        Some(method) => {
                            let call = self.cx.expr_method_call(
                                expr.span, target.clone(), method, vec![source]);
                            ExprKind::Assign(target, call)
                        },
                        None => ExprKind::AssignOp(op, target, source),
                    },
                    ..expr
                }
            },
            _ => fold::noop_fold_expr(expr, self),
        }})
    }
}

/// Returns the wrapping version of an operator, if applicable.
fn wrapping_method(op: BinOpKind) -> Option<Ident> {
    Some(token::str_to_ident(match op {
        BinOpKind::Add => "wrapping_add",
        BinOpKind::Sub => "wrapping_sub",
        BinOpKind::Mul => "wrapping_mul",
        BinOpKind::Div => "wrapping_div",
        BinOpKind::Rem => "wrapping_rem",
        BinOpKind::Shl => "wrapping_shl",
        BinOpKind::Shr => "wrapping_shr",
        _ => return None,
    }))
}

fn expand_wrapping<'cx>(cx: &'cx mut ExtCtxt, sp: Span, tts: &[TokenTree]) -> Box<MacResult + 'cx> {
    // Parse the token tree as a block
    let block = TokenTree::Delimited(sp, Delimited {
        delim: DelimToken::Brace,
        open_span: DUMMY_SP,
        tts: tts.to_owned(),
        close_span: DUMMY_SP,
    });
    let mut parser = parse::tts_to_parser(cx.parse_sess, vec![block],
                                          cx.cfg.clone());
    match parser.parse_block() {
        Ok(block) => {
            // Perform the fold
            let block = WrappingFolder { cx: cx }.fold_block(block);
            MacEager::expr(cx.expr_block(block))
        },
        Err(mut e) => {
            // Emit the parse error and continue
            e.emit();
            DummyResult::expr(sp)
        },
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("wrapping", expand_wrapping);
}
