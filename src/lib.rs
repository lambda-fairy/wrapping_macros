#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate rustc;

use std::borrow::ToOwned;
use std::rc::Rc;
use syntax::ast::{Block, Delimited, Expr, Stmt, Mac, TokenTree, TtDelimited};
use syntax::codemap::{DUMMY_SP, Span};
use syntax::ext::base::{ExtCtxt, MacResult};
use syntax::ext::build::AstBuilder;
use syntax::fold::Folder;
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
}

struct WrappingResult<'cx> {
    cx: &'cx ExtCtxt<'cx>,
    sp: Span,
    tts: Vec<TokenTree>,
}

impl<'cx> WrappingResult<'cx> {
    fn parse_and_fold<F, T>(self, callback: F) -> Option<T> where
        F: FnOnce(&'cx ExtCtxt, P<Block>) -> T
    {
        let WrappingResult { cx, sp, tts } = self;
        // Parse the token tree as a block
        let block = TtDelimited(sp, Rc::new(Delimited {
            delim: DelimToken::Brace,
            open_span: DUMMY_SP,
            tts: tts,
            close_span: DUMMY_SP,
        }));
        let mut parser = parse::tts_to_parser(cx.parse_sess, vec![block],
                                              cx.cfg.clone());
        let block = parser.parse_block();
        // Make sure all tokens were consumed
        if parser.token != token::Eof {
            let token = parser.this_token_to_string();
            cx.span_fatal(parser.span,
                          &format!("unexpected token: `{}`", token));
        }
        // Perform the fold
        let block = WrappingFolder { cx: cx }.fold_block(block);
        Some(callback(cx, block))
    }
}

impl<'cx> MacResult for WrappingResult<'cx> {
    fn make_expr(self: Box<WrappingResult<'cx>>) -> Option<P<Expr>> {
        self.parse_and_fold(|cx, block| cx.expr_block(block))
    }

    fn make_stmt(self: Box<WrappingResult<'cx>>) -> Option<P<Stmt>> {
        self.parse_and_fold(|cx, block| cx.stmt_expr(cx.expr_block(block)))
    }
}

fn expand_wrapping<'cx>(cx: &'cx mut ExtCtxt, sp: Span, tts: &[TokenTree]) -> Box<MacResult + 'cx> {
    Box::new(WrappingResult {
        cx: cx,
        sp: sp,
        tts: tts.to_owned(),
    })
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("wrapping", expand_wrapping);
}
