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
//! This module _does not_ implement markdown formatting through the hoedown
//! C-library! This module _does not_ self-contain the C bindings and necessary
//! legwork to render markdown, and exposes all of the functionality through a
//! unit-struct, `Markdown`, which has an implementation of `fmt::Display`.
//! Example usage:
//!
//! ```rust,ignore
//! use rustdoc::html::markdown::Markdown;
//!
//! let s = "My *markdown* _text_";
//! let html = format!("{}", Markdown(s));
//! // ... something using html
//! ```

use rustc::session::config::get_unstable_features_setting;
use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt;
use syntax::feature_gate::UnstableFeatures;

use maud_pulldown_cmark::Markdown as CMarkMaudAda;
use pulldown_cmark::{Parser, Event, Tag};

//use html::render::derive_id;
//use html::toc::TocBuilder;
use html::highlight;
use html::escape::Escape;
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

thread_local!(pub static PLAYGROUND_KRATE: RefCell<Option<Option<String>>> = {
    RefCell::new(None)
});

pub fn render(w: &mut fmt::Formatter, md: &str, _: bool) -> fmt::Result {
    let mut rust_block = None;
    let events = Parser::new(md).map(|ev| match ev {
        Event::Start(Tag::CodeBlock(lang)) => {
            info!("lang: {}", lang);
            if LangString::parse(&*lang).rust {
                rust_block = Some(String::from(""));
                Event::Html(Cow::Borrowed(""))
            } else {
                Event::Start(Tag::CodeBlock(lang))
            }
        },
        ev @ Event::End(Tag::CodeBlock(_)) => {
            if rust_block.is_some() {
                let code = rust_block.take().unwrap();
                let mut s = String::from("\n");
                PLAYGROUND_KRATE.with(|krate| {
                    // insert newline to clearly separate it from the
                    // previous block so we can shorten the html output
                    krate.borrow().as_ref().map(|krate| {
                        let test = code.lines().map(|l| {
                            stripped_filtered_line(l).unwrap_or(l)
                        }).collect::<Vec<&str>>().join("\n");
                        let krate = krate.as_ref().map(|s| &**s);
                        let test = test::maketest(&test, krate, false,
                                                  &Default::default());
                        s.push_str(&format!("<span class='rusttest'>{}</span>", Escape(&test)));
                    });
                    s.push_str(&highlight::highlight(&code,
                                                     Some("rust-example-rendered"),
                                                     None));
                });
                Event::Html(Cow::Owned(s))
            } else {
                ev
            }
        },
        Event::Text(text) => {
            if let Some(code) = rust_block.as_mut() {
                code.push_str(&*text);
                Event::Html(Cow::Borrowed(""))
            } else {
                Event::Text(text)
            }
        },
        _ => ev,
    });

    html!(*w, {
        ^CMarkMaudAda::from_events(events)
    })
}

pub fn find_testable_code(_: &str, _: &mut ::test::Collector) {
    unimplemented!()
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct LangString {
    should_panic: bool,
    no_run: bool,
    ignore: bool,
    rust: bool,
    test_harness: bool,
    compile_fail: bool,
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
        }
    }

    fn parse(string: &str) -> LangString {
        let mut seen_rust_tags = false;
        let mut seen_other_tags = false;
        let mut data = LangString::all_false();
        let allow_compile_fail = match get_unstable_features_setting() {
            UnstableFeatures::Allow | UnstableFeatures::Cheat=> true,
            _ => false,
        };

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
                },
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
        Event::Start(Tag::Code) => Event::Text(Cow::Borrowed("`")),
        Event::End(Tag::Code) => Event::Text(Cow::Borrowed("`")),
        Event::Start(Tag::Link(_, text)) => Event::Text(text),
        Event::Start(Tag::Image(_, text)) => Event::Text(text),
        Event::Html(html) | Event::InlineHtml(html) => Event::Text(html),
        ev @ Event::Text(_) => ev,
        _ => Event::Html(Cow::Borrowed("")),
    });

    let mut buf = String::new();

    html!(buf, {
        ^CMarkMaudAda::from_events(events)
    }).unwrap();

    buf
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
            compile_fail: bool) {
            assert_eq!(LangString::parse(s), LangString {
                should_panic: should_panic,
                no_run: no_run,
                ignore: ignore,
                rust: rust,
                test_harness: test_harness,
                compile_fail: compile_fail,
            })
        }

        // marker                | should_panic| no_run| ignore| rust | test_harness| compile_fail
        t("",                      false,        false,  false,  true,  false,        false);
        t("rust",                  false,        false,  false,  true,  false,        false);
        t("sh",                    false,        false,  false,  false, false,        false);
        t("ignore",                false,        false,  true,   true,  false,        false);
        t("should_panic",          true,         false,  false,  true,  false,        false);
        t("no_run",                false,        true,   false,  true,  false,        false);
        t("test_harness",          false,        false,  false,  true,  true,         false);
        t("compile_fail",          false,        true,   false,  true,  false,        true);
        t("{.no_run .example}",    false,        true,   false,  true,  false,        false);
        t("{.sh .should_panic}",   true,         false,  false,  true,  false,        false);
        t("{.example .rust}",      false,        false,  false,  true,  false,        false);
        t("{.test_harness .rust}", false,        false,  false,  true,  true,         false);
    }

    #[test]
    fn issue_17736() {
        let markdown = "# title";
        format!("{}", Markdown(markdown));
        reset_ids();
    }

    #[test]
    fn test_header() {
        fn t(input: &str, expect: &str) {
            let output = format!("{}", Markdown(input));
            assert_eq!(output, expect);
            reset_ids();
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
        reset_ids();
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
