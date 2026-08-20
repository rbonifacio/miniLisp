package br.ufpe.cin.minilisp

/** Unit tests for [[Parser]]: source text is turned into [[Expr]] trees, and
  * constructs outside the current integer-only grammar are rejected.
  */
class ParserSpec extends munit.FunSuite:

  override def munitIgnore: Boolean = true

  import Expr.*

  private def parse(source: String): List[Expr] = Parser.parse(source)

  test("single integer becomes IntLit") {
    assertEquals(parse("42"), List(IntLit(42)))
  }

  test("zero is a valid integer") {
    assertEquals(parse("0"), List(IntLit(0)))
    assertEquals(parse("-0"), List(IntLit(0)))
  }

  test("a program is a sequence of integers") {
    assertEquals(parse("0 1 2"), List(IntLit(0), IntLit(1), IntLit(2)))
  }

  test("negative integers follow the BNF optional minus") {
    assertEquals(parse("-7"), List(IntLit(-7)))
    assertEquals(parse("-1 0 1"), List(IntLit(-1), IntLit(0), IntLit(1)))
  }

  test("integers may have leading zeros") {
    assertEquals(parse("007"), List(IntLit(7)))
  }

  test("empty input and comment-only input are empty programs") {
    assertEquals(parse(""), Nil)
    assertEquals(parse("   "), Nil)
    assertEquals(parse("  ; just a comment\n"), Nil)
  }

  test("whitespace between integers is skipped") {
    assertEquals(parse("1\t2\n3"), List(IntLit(1), IntLit(2), IntLit(3)))
  }

  test("line comments are skipped") {
    assertEquals(
      parse("1 ; this is ignored\n  2"),
      List(IntLit(1), IntLit(2))
    )
  }

  test("a comment may appear before the first integer") {
    assertEquals(parse("; header\n10"), List(IntLit(10)))
  }

  test("rejects lists") {
    intercept[ParseError](parse("(1 2)"))
  }

  test("rejects quote") {
    intercept[ParseError](parse("'1"))
  }

  test("rejects symbols") {
    intercept[ParseError](parse("foo"))
    intercept[ParseError](parse("+"))
  }

  test("rejects floats") {
    intercept[ParseError](parse("1.5"))
  }

  test("rejects strings") {
    intercept[ParseError](parse("\"hi\""))
  }

  test("rejects booleans") {
    intercept[ParseError](parse("#t"))
    intercept[ParseError](parse("#f"))
  }

  test("rejects a lone minus") {
    intercept[ParseError](parse("-"))
  }

  test("ParseError reports the source location") {
    val err = intercept[ParseError](parse("1 x"))
    assert(err.message.contains("line 1:"), clue = err.message)
  }
