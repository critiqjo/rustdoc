// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Markdown formatting for rustdoc
//!
//! This module implements markdown formatting through pulldown-cmark crate for
//! markdown parsing, hamlet crate for html rendering, and cmark-hamlet crate
//! as the bridge between them! This module exposes all of the functionality
//! through a unit-struct, `Markdown`, which has an implementation of
//! `fmt::Display`. Example usage:
//!
//! ```rust,ignore
//! use rustdoc::html::markdown::Markdown;
//!
//! let s = "My *markdown* _text_";
//! let html = format!("{}", Markdown(s));
//! // ... something using html
//! ```

use std::ascii::AsciiExt;
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::{self, Write};
use syntax::feature_gate::UnstableFeatures;

use cmark::{Event as CmEvent, Options, Parser, Tag};
use hamlet::Token as HmToken;
use cmark_hamlet;

use html::render::derive_id;
//use html::toc::TocBuilder;
use html::highlight;
use test;

/// A unit struct which has the `fmt::Display` trait implemented. When
/// formatted, this struct will emit the HTML corresponding to the rendered
/// version of the contained markdown string.
pub struct Markdown<'a>(pub &'a str);
/// A unit struct like `Markdown`, that renders the markdown with a
/// table of contents.
pub struct MarkdownWithToc<'a>(pub &'a str);

/// Returns Some(code) if `s` is a line that should be stripped from
/// documentation but used in example code. `code` is the portion of
/// `s` that should be used in tests. (None for lines that should be
/// left as-is.)
fn stripped_filtered_line<'a>(s: &'a str) -> Option<&'a str> {
    let trimmed = s.trim();
    if trimmed == "#" {
        Some("")
    } else if trimmed.starts_with("# ") {
        Some(&trimmed[2..])
    } else {
        None
    }
}

/// Returns a new string with all consecutive whitespace collapsed into
/// single spaces.
///
/// Any leading or trailing whitespace will be trimmed.
#[allow(dead_code)]
fn collapse_whitespace(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

// Information about the playground if a URL has been specified, containing an
// optional crate name and the URL.
thread_local!(pub static PLAYGROUND: RefCell<Option<(Option<String>, String)>> = {
    RefCell::new(None)
});

fn is_header(tag_name: &str) -> bool {
    if tag_name.len() == 2 {
        tag_name.char_indices().all(|(i, c)| {
            (i == 0 && c == 'h') || (i == 1 && c >= '1' && c <= '6')
        })
    } else {
        false
    }
}

fn id_from_text(text: &str) -> String {
    let id = text.chars().filter_map(|c| {
        if c.is_alphanumeric() || c == '-' || c == '_' {
            if c.is_ascii() {
                Some(c.to_ascii_lowercase())
            } else {
                Some(c)
            }
        } else if c.is_whitespace() && c.is_ascii() {
            Some('-')
        } else {
            None
        }
    }).collect::<String>();
    derive_id(id)
}

pub fn render(w: &mut fmt::Formatter, md: &str, _: bool) -> fmt::Result {
    let mut rust_block = false;
    let mut header = false;
    let mut header_inner_buf = String::from("");
    let mut header_id_buf = String::from("");
    for hm_tok in cmark_hamlet::Adapter::new(Parser::new_ext(md, Options::all()), true) {
        match hm_tok {
            HmToken::StartTag { ref name, .. } if is_header(name.as_ref()) => {
                header = true;
            }
            HmToken::EndTag { ref name } if is_header(name.as_ref()) => {
                let id = id_from_text(&*header_id_buf);
                try!(write!(w,
                            "{start}<a href=\"#{id}\">{inner}</a>{end}",
                            start = HmToken::start_tag(name.as_ref(),
                                                       attrs!(id = &*id,
                                                              class = "section-header")),
                            id = &*id,
                            inner = header_inner_buf,
                            end = hm_tok));
                header = false;
                header_id_buf.truncate(0);
                header_inner_buf.truncate(0);
            }
            _ if header => {
                if let HmToken::Text(ref text) = hm_tok {
                    try!(write!(header_id_buf, "{}", text));
                }
                try!(write!(header_inner_buf, "{}", hm_tok));
            }
            HmToken::StartTag { ref name, ref attrs, .. } if name.as_ref() == "pre" => {
                let is_rust = attrs.get("data-lang")
                                   .map(|lang| LangString::parse(lang).rust);
                if let Some(true) = is_rust {
                    rust_block = true;
                } else {
                    try!(write!(w, "{}", hm_tok));
                }
            }
            HmToken::StartTag { ref name, .. } |
            HmToken::EndTag { ref name } if rust_block && name.as_ref() == "code" => (),
            HmToken::Text(ref text) if rust_block => {
                let code = text.as_ref();
                // insert newline to clearly separate it from the
                // previous block so we can shorten the html output
                let mut out = String::from("\n");
                PLAYGROUND.with(|play| {
                    let playground_button = play.borrow().as_ref().and_then(|&(ref krate, ref url)| {
                        if url.is_empty() {
                            return None;
                        }
                        let test = code.lines().map(|l| {
                            stripped_filtered_line(l).unwrap_or(l)
                        }).collect::<Vec<&str>>().join("\n");
                        let krate = krate.as_ref().map(|s| &**s);
                        let test = test::maketest(&test, krate, false,
                                                  &Default::default());
                        let channel = if test.contains("#![feature(") {
                            "&amp;version=nightly"
                        } else {
                            ""
                        };
                        // These characters don't need to be escaped in a URI.
                        // FIXME: use a library function for percent encoding.
                        fn dont_escape(c: u8) -> bool {
                            (b'a' <= c && c <= b'z') ||
                            (b'A' <= c && c <= b'Z') ||
                            (b'0' <= c && c <= b'9') ||
                            c == b'-' || c == b'_' || c == b'.' ||
                            c == b'~' || c == b'!' || c == b'\'' ||
                            c == b'(' || c == b')' || c == b'*'
                        }
                        let mut test_escaped = String::new();
                        for b in test.bytes() {
                            if dont_escape(b) {
                                test_escaped.push(char::from(b));
                            } else {
                                write!(test_escaped, "%{:02X}", b).unwrap();
                            }
                        }
                        Some(format!(
                            r#"<a class="test-arrow" target="_blank" href="{}?code={}{}">Run</a>"#,
                            url, test_escaped, channel
                        ))
                    });
                    let filtered_code = code.lines().filter(|l| {
                        stripped_filtered_line(l).is_none()
                    }).collect::<Vec<&str>>().join("\n");
                    out.push_str(&highlight::render_with_highlighting(
                                   &filtered_code,
                                   Some("rust-example-rendered"),
                                   None,
                                   playground_button.as_ref().map(String::as_str)));
                });
                try!(write!(w, "{}", out));
            }
            HmToken::EndTag { name: Cow::Borrowed("pre") } if rust_block => {
                rust_block = false;
            }
            _ => try!(write!(w, "{}", hm_tok)),
        }
    }
    Ok(())
}

pub fn find_testable_code(md: &str, tests: &mut ::test::Collector) {
    let mut block_info = None;
    for hm_tok in cmark_hamlet::Adapter::new(Parser::new(md), true) {
        match hm_tok {
            HmToken::StartTag { name: Cow::Borrowed("pre"), attrs, .. } => {
                block_info = attrs.get("data-lang")
                                  .map(|lang| LangString::parse(lang));
            }
            HmToken::Text(ref text) if block_info.is_some() => {
                let block_info = block_info.as_ref().unwrap();
                if block_info.rust {
                    let lines = text.lines().map(|l| {
                        stripped_filtered_line(l).unwrap_or(l)
                    });
                    let clean_code = lines.collect::<Vec<&str>>().join("\n");
                    tests.add_test(clean_code,
                                   block_info.should_panic, block_info.no_run,
                                   block_info.ignore, block_info.test_harness,
                                   block_info.compile_fail, block_info.error_codes.clone()); // FIXME clone?
                }
            }
            HmToken::EndTag { name: Cow::Borrowed("pre") } if block_info.is_some() => {
                block_info = None;
            }
            _ => (),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct LangString {
    should_panic: bool,
    no_run: bool,
    ignore: bool,
    rust: bool,
    test_harness: bool,
    compile_fail: bool,
    error_codes: Vec<String>,
}

impl LangString {
    fn all_false() -> LangString {
        LangString {
            should_panic: false,
            no_run: false,
            ignore: false,
            rust: true,  // NB This used to be `notrust = false`
            test_harness: false,
            compile_fail: false,
            error_codes: Vec::new(),
        }
    }

    fn parse(string: &str) -> LangString {
        let mut seen_rust_tags = false;
        let mut seen_other_tags = false;
        let mut data = LangString::all_false();
        let mut allow_compile_fail = false;
        let mut allow_error_code_check = false;
        if UnstableFeatures::from_environment().is_nightly_build() {
            allow_compile_fail = true;
            allow_error_code_check = true;
        }

        let tokens = string.split(|c: char|
            !(c == '_' || c == '-' || c.is_alphanumeric())
        );

        for token in tokens {
            match token {
                "" => {},
                "should_panic" => { data.should_panic = true; seen_rust_tags = true; },
                "no_run" => { data.no_run = true; seen_rust_tags = true; },
                "ignore" => { data.ignore = true; seen_rust_tags = true; },
                "rust" => { data.rust = true; seen_rust_tags = true; },
                "test_harness" => { data.test_harness = true; seen_rust_tags = true; },
                "compile_fail" if allow_compile_fail => {
                    data.compile_fail = true;
                    seen_rust_tags = true;
                    data.no_run = true;
                }
                x if allow_error_code_check && x.starts_with("E") && x.len() == 5 => {
                    if let Ok(_) = x[1..].parse::<u32>() {
                        data.error_codes.push(x.to_owned());
                        seen_rust_tags = true;
                    } else {
                        seen_other_tags = true;
                    }
                }
                _ => { seen_other_tags = true }
            }
        }

        data.rust &= !seen_other_tags || seen_rust_tags;

        data
    }
}

impl<'a> fmt::Display for Markdown<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let Markdown(md) = *self;
        // This is actually common enough to special-case
        if md.is_empty() { return Ok(()) }
        render(fmt, md, false)
    }
}

impl<'a> fmt::Display for MarkdownWithToc<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let MarkdownWithToc(md) = *self;
        render(fmt, md, true)
    }
}

pub fn plain_summary_line(md: &str) -> String {
    let events = Parser::new(md).map(|ev| match ev {
        CmEvent::Start(Tag::Code) => CmEvent::Text(Cow::Borrowed("`")),
        CmEvent::End(Tag::Code) => CmEvent::Text(Cow::Borrowed("`")),
        CmEvent::Text(_) => ev,
        //CmEvent::SoftBreak | CmEvent::HardBreak => ev,
        _ => CmEvent::Text(Cow::Borrowed("")),
    });

    let hm_toks = cmark_hamlet::Adapter::new(events, false);
    let mut ret = String::from("");
    for tok in hm_toks {
        write!(ret, "{}", tok).unwrap();
    }
    ret
}

#[cfg(test)]
mod tests {
    use super::{LangString, Markdown};
    use super::plain_summary_line;
    use html::render::reset_ids;

    #[test]
    fn test_lang_string_parse() {
        fn t(s: &str,
            should_panic: bool, no_run: bool, ignore: bool, rust: bool, test_harness: bool,
            compile_fail: bool, error_codes: Vec<String>) {
            assert_eq!(LangString::parse(s), LangString {
                should_panic: should_panic,
                no_run: no_run,
                ignore: ignore,
                rust: rust,
                test_harness: test_harness,
                compile_fail: compile_fail,
                error_codes: error_codes,
            })
        }

        // marker                | should_panic| no_run| ignore| rust | test_harness| compile_fail
        //                       | error_codes
        t("",                      false,        false,  false,  true,  false, false, Vec::new());
        t("rust",                  false,        false,  false,  true,  false, false, Vec::new());
        t("sh",                    false,        false,  false,  false, false, false, Vec::new());
        t("ignore",                false,        false,  true,   true,  false, false, Vec::new());
        t("should_panic",          true,         false,  false,  true,  false, false, Vec::new());
        t("no_run",                false,        true,   false,  true,  false, false, Vec::new());
        t("test_harness",          false,        false,  false,  true,  true,  false, Vec::new());
        t("compile_fail",          false,        true,   false,  true,  false, true,  Vec::new());
        t("E0450",                 false,        false,  false,  true,  false, false,
                                   vec!["E0450".to_owned()]);
        t("{.no_run .example}",    false,        true,   false,  true,  false, false, Vec::new());
        t("{.sh .should_panic}",   true,         false,  false,  true,  false, false, Vec::new());
        t("{.example .rust}",      false,        false,  false,  true,  false, false, Vec::new());
        t("{.test_harness .rust}", false,        false,  false,  true,  true,  false, Vec::new());
    }

    #[test]
    fn issue_17736() {
        let markdown = "# title";
        format!("{}", Markdown(markdown));
        reset_ids(true);
    }

    #[test]
    fn test_header() {
        fn t(input: &str, expect: &str) {
            let output = format!("{}", Markdown(input));
            assert_eq!(output, expect);
            reset_ids(true);
        }

        t("# Foo bar", "\n<h1 id='foo-bar' class='section-header'>\
          <a href='#foo-bar'>Foo bar</a></h1>");
        t("## Foo-bar_baz qux", "\n<h2 id='foo-bar_baz-qux' class=\'section-\
          header'><a href='#foo-bar_baz-qux'>Foo-bar_baz qux</a></h2>");
        t("### **Foo** *bar* baz!?!& -_qux_-%",
          "\n<h3 id='foo-bar-baz--_qux_-' class='section-header'>\
          <a href='#foo-bar-baz--_qux_-'><strong>Foo</strong> \
          <em>bar</em> baz!?!&amp; -_qux_-%</a></h3>");
        t("####**Foo?** & \\*bar?!*  _`baz`_ ❤ #qux",
          "\n<h4 id='foo--bar--baz--qux' class='section-header'>\
          <a href='#foo--bar--baz--qux'><strong>Foo?</strong> &amp; *bar?!*  \
          <em><code>baz</code></em> ❤ #qux</a></h4>");
    }

    #[test]
    fn test_header_ids_multiple_blocks() {
        fn t(input: &str, expect: &str) {
            let output = format!("{}", Markdown(input));
            assert_eq!(output, expect);
        }

        let test = || {
            t("# Example", "\n<h1 id='example' class='section-header'>\
              <a href='#example'>Example</a></h1>");
            t("# Panics", "\n<h1 id='panics' class='section-header'>\
              <a href='#panics'>Panics</a></h1>");
            t("# Example", "\n<h1 id='example-1' class='section-header'>\
              <a href='#example-1'>Example</a></h1>");
            t("# Main", "\n<h1 id='main-1' class='section-header'>\
              <a href='#main-1'>Main</a></h1>");
            t("# Example", "\n<h1 id='example-2' class='section-header'>\
              <a href='#example-2'>Example</a></h1>");
            t("# Panics", "\n<h1 id='panics-1' class='section-header'>\
              <a href='#panics-1'>Panics</a></h1>");
        };
        test();
        reset_ids(true);
        test();
    }

    #[test]
    fn test_plain_summary_line() {
        fn t(input: &str, expect: &str) {
            let output = plain_summary_line(input);
            assert_eq!(output, expect);
        }

        t("hello [Rust](https://www.rust-lang.org) :)", "hello Rust :)");
        t("code `let x = i32;` ...", "code `let x = i32;` ...");
        t("type `Type<'static>` ...", "type `Type<'static>` ...");
        t("# top header", "top header");
        t("## header", "header");
    }
}
