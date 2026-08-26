package br.ufpe.cin.minilisp

import scala.io.Source

/** Unit tests for [[Parser]]: source text is turned into [[Expr]] trees.
  *
  * The grammar recognizes a program as `decl* expr EOF`: zero or more
  * `define` declarations followed by exactly one trailing expression.
  * [[Parser.parse]] validates the declarations syntactically but only the
  * trailing expression is reflected in the returned [[Expr]] -- see
  * [[Parser.from]].
  */
class ParserSpec extends munit.FunSuite:

  import Expr.*

  private def parse(source: String): Expr = Parser.parse(source)

  private def parseResource(name: String): Expr =
    val source = Source.fromResource(s"programs/$name")
    try parse(source.mkString)
    finally source.close()

  // -- integers ---------------------------------------------------------

  test("a positive integer becomes IntLit") {
    assertEquals(parse("42"), IntLit(42))
  }

  test("zero is a valid integer") {
    assertEquals(parse("0"), IntLit(0))
  }

  test("a negative integer becomes IntLit") {
    assertEquals(parse("-7"), IntLit(-7))
  }

  test("negative zero is accepted, same as zero") {
    assertEquals(parse("-0"), IntLit(0))
  }

  test("leading zeros are accepted") {
    assertEquals(parse("007"), IntLit(7))
  }

  test("rejects a lone minus with nothing to negate") {
    intercept[ParseError](parse("-"))
  }

  // -- floats -------------------------------------------------------------

  test("a float becomes FloatLit") {
    assertEquals(parse("3.14"), FloatLit(3.14))
  }

  test("a negative float becomes FloatLit") {
    assertEquals(parse("-0.5"), FloatLit(-0.5))
  }

  test("rejects a float with no fractional digits") {
    intercept[ParseError](parse("3."))
  }

  // -- strings --------------------------------------------------------------

  test("a string literal becomes StringLit without its surrounding quotes") {
    assertEquals(parse("\"hello world\""), StringLit("hello world"))
  }

  test("a string may contain characters that are otherwise syntax") {
    assertEquals(parse("\"(1 + 2)\""), StringLit("(1 + 2)"))
  }

  test("an empty string is accepted") {
    assertEquals(parse("\"\""), StringLit(""))
  }

  test("rejects an unterminated string") {
    intercept[ParseError](parse("\"unterminated"))
  }

  // -- booleans -------------------------------------------------------------

  test("True and False become BoolLit") {
    assertEquals(parse("True"), BoolLit(true))
    assertEquals(parse("False"), BoolLit(false))
  }

  test("lowercase true/false are ordinary symbols, not booleans") {
    assertEquals(parse("true"), Symbol("true"))
    assertEquals(parse("false"), Symbol("false"))
  }

  // -- symbols -----------------------------------------------------------

  test("an identifier becomes Symbol") {
    assertEquals(parse("foo"), Symbol("foo"))
    assertEquals(parse("x1"), Symbol("x1"))
  }

  test("rejects a symbol starting with a digit") {
    intercept[ParseError](parse("1x"))
  }

  // -- whitespace and comments ----------------------------------------------

  test("surrounding whitespace is skipped") {
    assertEquals(parse("  42  "), IntLit(42))
    assertEquals(parse("\t-7\n"), IntLit(-7))
  }

  test("a leading line comment is skipped") {
    assertEquals(parse("// a comment\n10"), IntLit(10))
  }

  test("a trailing line comment is skipped, with or without a preceding space") {
    assertEquals(parse("10 // trailing comment"), IntLit(10))
    assertEquals(parse("10// trailing comment"), IntLit(10))
  }

  test("consecutive comment lines are skipped") {
    assertEquals(parse("// one\n// two\n10"), IntLit(10))
  }

  test("rejects the older ';' comment style, which this grammar does not support") {
    intercept[ParseError](parse("; a comment\n10"))
  }

  test("a comment-only program is still rejected, since a trailing expr is required") {
    intercept[ParseError](parse("// only a comment"))
  }

  test("rejects empty input") {
    intercept[ParseError](parse(""))
    intercept[ParseError](parse("   "))
  }

  // -- binary and unary arithmetic/relational expressions --------------------

  test("a binary arithmetic expression becomes BinExpr") {
    assertEquals(parse("(+ 1 2)"), BinExpr("+", IntLit(1), IntLit(2)))
    assertEquals(parse("(* 3 4)"), BinExpr("*", IntLit(3), IntLit(4)))
    assertEquals(parse("(/ 8 2)"), BinExpr("/", IntLit(8), IntLit(2)))
  }

  test("a binary relational expression becomes BinExpr") {
    assertEquals(parse("(> 3 2)"), BinExpr(">", IntLit(3), IntLit(2)))
    assertEquals(parse("(<= a b)"), BinExpr("<=", Symbol("a"), Symbol("b")))
    assertEquals(parse("(/= a b)"), BinExpr("/=", Symbol("a"), Symbol("b")))
  }

  test("minus with two operands, separated by whitespace, is a binary BinExpr") {
    assertEquals(parse("(- 5 3)"), BinExpr("-", IntLit(5), IntLit(3)))
  }

  test("minus with a single operand is a unary NegExpr") {
    assertEquals(parse("(- 5)"), NegExpr(IntLit(5)))
    assertEquals(parse("(- x)"), NegExpr(Symbol("x")))
  }

  test("minus glued to a digit lexes as a single negative-integer atom, not an operator") {
    // "-5" is itself a valid Integer token, so this is a two-element list,
    // not a subtraction: leave a space after '-' to get BinExpr/NegExpr.
    assertEquals(parse("(-5 3)"), SList(List(IntLit(-5), IntLit(3))))
  }

  test("binary expressions nest") {
    assertEquals(
      parse("(+ (* 2 3) (- 10 4))"),
      BinExpr("+", BinExpr("*", IntLit(2), IntLit(3)), BinExpr("-", IntLit(10), IntLit(4)))
    )
  }

  test("rejects a binary operator with only one operand") {
    intercept[ParseError](parse("(+ 1)"))
  }

  test("rejects a binary operator with three operands") {
    intercept[ParseError](parse("(+ 1 2 3)"))
  }

  // -- let --------------------------------------------------------------

  test("let becomes LetExpr") {
    assertEquals(parse("(let (x 1) (+ x 1))"), LetExpr("x", IntLit(1), BinExpr("+", Symbol("x"), IntLit(1))))
  }

  test("let bindings may nest") {
    assertEquals(
      parse("(let (x 1) (let (y 2) (+ x y)))"),
      LetExpr("x", IntLit(1), LetExpr("y", IntLit(2), BinExpr("+", Symbol("x"), Symbol("y"))))
    )
  }

  test("rejects let missing a body") {
    intercept[ParseError](parse("(let (x 1))"))
  }

  // -- if -----------------------------------------------------------------

  test("if becomes IfExpr") {
    assertEquals(
      parse("(if (> x 0) x (- x))"),
      IfExpr(BinExpr(">", Symbol("x"), IntLit(0)), Symbol("x"), NegExpr(Symbol("x")))
    )
  }

  test("rejects if with a missing branch") {
    intercept[ParseError](parse("(if True 1)"))
  }

  // -- lists ----------------------------------------------------------------

  test("an empty list is accepted") {
    assertEquals(parse("()"), SList(Nil))
  }

  test("a list of atoms becomes SList") {
    assertEquals(parse("(1 2 3)"), SList(List(IntLit(1), IntLit(2), IntLit(3))))
  }

  test("lists may hold mixed atom types") {
    assertEquals(
      parse("(1 2.5 \"hi\" True sym)"),
      SList(List(IntLit(1), FloatLit(2.5), StringLit("hi"), BoolLit(true), Symbol("sym")))
    )
  }

  test("a list can look like a function call, since application has no dedicated syntax") {
    assertEquals(parse("(fact 5)"), SList(List(Symbol("fact"), IntLit(5))))
  }

  test("lists nest") {
    assertEquals(
      parse("((1 2) (3 4))"),
      SList(List(SList(List(IntLit(1), IntLit(2))), SList(List(IntLit(3), IntLit(4)))))
    )
  }

  // -- declarations -----------------------------------------------------------

  test("a leading declaration is parsed but does not appear in the result") {
    assertEquals(parse("define (f) (1) 2"), IntLit(2))
  }

  test("declaration syntax tolerates surrounding whitespace") {
    assertEquals(parse("define(f)(1) 2"), IntLit(2))
  }

  test("a declaration may have multiple parameters") {
    // the decl's own '(' ')' wrap the body expr, so a BinExpr body needs
    // its own extra pair: define NAME(ARGS) ( BODYEXPR )
    assertEquals(parse("define (f x y) ((+ x y)) 2"), IntLit(2))
  }

  test("a declaration may have zero parameters") {
    assertEquals(parse("define (const) (42) True"), BoolLit(true))
  }

  test("multiple declarations may precede the trailing expression") {
    assertEquals(parse("define (a) (1) define (b) (2) (+ a b)"), BinExpr("+", Symbol("a"), Symbol("b")))
  }

  test("rejects a declaration with no trailing expression") {
    intercept[ParseError](parse("define (f) (1)"))
  }

  // -- program shape: exactly one trailing expression -----------------------

  test("rejects more than one top-level expression") {
    intercept[ParseError](parse("1 2"))
    intercept[ParseError](parse("1 x"))
  }

  // -- constructs not in this grammar -----------------------------------------

  test("rejects quote, which this grammar does not support") {
    intercept[ParseError](parse("'1"))
  }

  test("rejects an unmatched opening parenthesis") {
    intercept[ParseError](parse("(1 2"))
  }

  test("rejects an unmatched closing parenthesis") {
    intercept[ParseError](parse("1 2)"))
  }

  // -- error reporting --------------------------------------------------------

  test("ParseError reports the source location for parse errors") {
    val err = intercept[ParseError](parse("(+ 1)"))
    assert(err.message.contains("line 1:"), clue = err.message)
  }

  // -- sample files written in the dialect ------------------------------------

  test("parses factorial.mlisp: declarations are skipped, the call remains") {
    assertEquals(parseResource("factorial.mlisp"), SList(List(Symbol("fact"), IntLit(5))))
  }

  test("parses max-of-two.mlisp: declarations are skipped, the call remains") {
    assertEquals(parseResource("max-of-two.mlisp"), SList(List(Symbol("max2"), IntLit(3), IntLit(7))))
  }

  test("parses circle-area.mlisp: a let expression over floats") {
    assertEquals(
      parseResource("circle-area.mlisp"),
      LetExpr("radius", FloatLit(2.5), BinExpr("*", Symbol("radius"), Symbol("radius")))
    )
  }

  test("parses mixed-list.mlisp: a heterogeneous list literal") {
    assertEquals(
      parseResource("mixed-list.mlisp"),
      SList(List(IntLit(1), FloatLit(2.5), StringLit("hello"), BoolLit(true), Symbol("done")))
    )
  }

  test("parses multi-arg-decl.mlisp: a three-parameter declaration, still skipped") {
    assertEquals(
      parseResource("multi-arg-decl.mlisp"),
      SList(List(Symbol("sum3"), IntLit(1), IntLit(2), IntLit(3)))
    )
  }
