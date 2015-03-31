#![crate_type = "dylib"]
#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate rustc;

use std::borrow::ToOwned;
use syntax::ast::{Mac, TokenTree};
use syntax::codemap::Span;
use syntax::ext::base::{ExtCtxt, MacResult};
use syntax::fold::Folder;
use rustc::plugin::Registry;

struct WrappingFolder<'cx> {
    cx: &'cx ExtCtxt<'cx>,
}

impl<'cx> Folder for WrappingFolder<'cx> {
    fn fold_mac(&mut self, mac: Mac) -> Mac {
        self.cx.span_fatal(mac.span, "cannot nest macros in a `wrapping!()` block");
    }
}

struct WrappingResult<'cx> {
    cx: &'cx ExtCtxt<'cx>,
    tts: Vec<TokenTree>,
}

impl<'cx> MacResult for WrappingResult<'cx> {}

fn expand_wrapping<'cx>(cx: &'cx mut ExtCtxt, sp: Span, tts: &[TokenTree]) -> Box<MacResult + 'cx> {
    Box::new(WrappingResult {
        cx: cx,
        tts: tts.to_owned(),
    })
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("wrapping", expand_wrapping);
}
