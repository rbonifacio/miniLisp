package br.ufpe.cin.minilisp

/** Unit tests for [[Parser]]: source text is turned into [[Expr]] trees.
  *
  * The grammar so far recognizes a program as `decl* expr EOF`, where `expr`
  * is one of `Integer`, `String`, `Boolean`, or `Id`; `define` declarations
  * are parsed but not yet reflected in the returned [[Expr]]; and `;`-to-
  * end-of-line comments are skipped like whitespace. [[Parser.visitExpr]]
  * itself only builds an [[Expr]] for `Integer` and `Id` so far, so `String`
  * and `Boolean` tokens parse syntactically but fail in the visitor.
  * Lists, quote, floats, and multiple top-level expressions are not
  * implemented yet and are expected to be rejected.
  */
class ParserSpec extends munit.FunSuite:

  import Expr.*

  private def parse(source: String): Expr = Parser.parse(source)

  // -- integers --------------------------------------------------------

  test("a positive integer becomes IntLit") {
    assertEquals(parse("42"), IntLit(42))
  }

  test("zero is a valid integer") {
    assertEquals(parse("0"), IntLit(0))
  }

  test("a negative integer becomes IntLit") {
    assertEquals(parse("-7"), IntLit(-7))
  }

  test("rejects negative zero") {
    intercept[ParseError](parse("-0"))
  }

  test("rejects a lone minus") {
    intercept[ParseError](parse("-"))
  }

  test("rejects integers with leading zeros") {
    intercept[ParseError](parse("007"))
  }

  // -- symbols -----------------------------------------------------------

  test("an identifier becomes Sym") {
    assertEquals(parse("foo"), Sym("foo"))
    assertEquals(parse("x1"), Sym("x1"))
  }

  test("true and false are ordinary symbols, not booleans, until <bool> is implemented") {
    assertEquals(parse("true"), Sym("true"))
    assertEquals(parse("false"), Sym("false"))
  }

  test("rejects symbol-start characters not yet in the grammar") {
    intercept[ParseError](parse("+"))
    intercept[ParseError](parse("*"))
  }

  test("rejects a symbol starting with a digit") {
    intercept[ParseError](parse("1x"))
  }

  // -- whitespace ----------------------------------------------------------

  test("surrounding whitespace is skipped") {
    assertEquals(parse("  42  "), IntLit(42))
    assertEquals(parse("\t-7\n"), IntLit(-7))
  }

  // -- comments -------------------------------------------------------------

  test("a leading line comment is skipped") {
    assertEquals(parse("; a comment\n10"), IntLit(10))
  }

  test("a trailing line comment is skipped, with or without a preceding space") {
    assertEquals(parse("10 ; trailing comment"), IntLit(10))
    assertEquals(parse("10; trailing comment"), IntLit(10))
  }

  test("consecutive comment lines are skipped") {
    assertEquals(parse("; one\n; two\n10"), IntLit(10))
  }

  test("a comment inside a declaration is skipped") {
    assertEquals(parse("define(f)(1) ; comment\n2"), IntLit(2))
  }

  test("a comment-only program is still rejected, since an expr is still required") {
    intercept[ParseError](parse("; only a comment"))
  }

  // -- program shape: only one trailing expression is supported today -----

  test("rejects more than one top-level expression") {
    intercept[ParseError](parse("1 2"))
    intercept[ParseError](parse("1 x"))
  }

  test("rejects empty input") {
    intercept[ParseError](parse(""))
    intercept[ParseError](parse("   "))
  }

  // -- declarations ---------------------------------------------------------

  test("a leading declaration is parsed but does not appear in the result") {
    assertEquals(parse("define(f)(1) 2"), IntLit(2))
  }

  test("declaration syntax tolerates surrounding whitespace") {
    assertEquals(parse("define (f) (1) 2"), IntLit(2))
  }

  test("a declaration may have multiple parameters") {
    assertEquals(parse("define(f x y)(1) 2"), IntLit(2))
  }

  // -- string and boolean literals: lexed, but not yet built by the visitor --

  test("strings are accepted by the grammar but not yet handled by visitExpr") {
    intercept[IllegalArgumentException](parse("\"hi\""))
  }

  test("booleans are accepted by the grammar but not yet handled by visitExpr") {
    intercept[IllegalArgumentException](parse("True"))
    intercept[IllegalArgumentException](parse("False"))
  }

  // -- constructs from the BNF that aren't implemented yet -------------------

  test("rejects lists") {
    intercept[ParseError](parse("(1 2)"))
  }

  test("rejects quote") {
    intercept[ParseError](parse("'1"))
  }

  test("rejects floats") {
    intercept[ParseError](parse("1.5"))
  }

  // -- error reporting --------------------------------------------------------

  test("ParseError reports the source location for lexer errors") {
    val err = intercept[ParseError](parse("+"))
    assert(err.message.contains("line 1:"), clue = err.message)
  }
