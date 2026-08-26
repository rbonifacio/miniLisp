package br.ufpe.cin.minilisp

/** Unit tests for [[Interpreter]].
  *
  * Programs are built directly as [[Expr]]/[[Decl]]/[[Program]] trees rather
  * than parsed from source, since [[Parser.parse]] does not yet assemble
  * `decl*` into a [[Program]] (see [[ParserSpec]]).
  */
class InterpreterSpec extends munit.FunSuite:

  import Expr.*

  private def evalExpr(expr: Expr): Expr =
    Interpreter.evalProgram(Program(List(), expr))

  private def evalWith(decls: List[Decl], expr: Expr): Expr =
    Interpreter.evalProgram(Program(decls, expr))

  // -- literals -------------------------------------------------------------

  test("integer literals evaluate to themselves") {
    assertEquals(evalExpr(IntLit(42)), IntLit(42))
  }

  test("float literals evaluate to themselves") {
    assertEquals(evalExpr(FloatLit(3.14)), FloatLit(3.14))
  }

  test("string literals evaluate to themselves") {
    assertEquals(evalExpr(StringLit("hi")), StringLit("hi"))
  }

  test("boolean literals evaluate to themselves") {
    assertEquals(evalExpr(BoolLit(true)), BoolLit(true))
  }

  // -- symbol / decl lookup --------------------------------------------------

  test("a bare symbol resolves to its bound declaration's value") {
    val decls = List(Decl("x", List(), IntLit(42)))
    assertEquals(evalWith(decls, Symbol("x")), IntLit(42))
  }

  test("a single-symbol list resolves the same as a bare symbol") {
    val decls = List(Decl("x", List(), IntLit(42)))
    assertEquals(evalWith(decls, SList(List(Symbol("x")))), IntLit(42))
  }

  test("an unbound symbol raises EvalError") {
    intercept[EvalError](evalExpr(Symbol("nope")))
  }

  // -- function calls ---------------------------------------------------------

  test("calling a user-defined function substitutes its argument") {
    val double = Decl("double", List("n"), BinExpr("*", Symbol("n"), IntLit(2)))
    val call = SList(List(Symbol("double"), IntLit(21)))
    assertEquals(evalWith(List(double), call), IntLit(42))
  }

  test("calling with the wrong number of arguments raises EvalError") {
    val double = Decl("double", List("n"), BinExpr("*", Symbol("n"), IntLit(2)))
    val call = SList(List(Symbol("double"), IntLit(1), IntLit(2)))
    intercept[EvalError](evalWith(List(double), call))
  }

  test("arguments are evaluated in the caller's environment before binding") {
    // (f n) = (g (+ n 1)); (g n) = (* n n).  Both f and g name their
    // parameter `n` -- this only gives the right answer if evaluating the
    // actual argument to g does not get reinterpreted against g's own `n`.
    val f = Decl("f", List("n"), SList(List(Symbol("g"), BinExpr("+", Symbol("n"), IntLit(1)))))
    val g = Decl("g", List("n"), BinExpr("*", Symbol("n"), Symbol("n")))
    val call = SList(List(Symbol("f"), IntLit(3)))
    assertEquals(evalWith(List(f, g), call), IntLit(16))
  }

  test("a call frame does not leak its bindings into the caller's environment") {
    val id = Decl("id", List("n"), Symbol("n"))
    val first = Decl("first", List("a", "b"), Symbol("a"))
    // Both arguments to `first` are evaluated in the same (top-level)
    // environment. If evaluating `(id 5)` ever defined `n` on that shared
    // environment instead of a fresh child frame, the sibling argument
    // `n` would wrongly resolve to 5 instead of raising EvalError.
    val call = SList(List(Symbol("first"), SList(List(Symbol("id"), IntLit(5))), Symbol("n")))
    intercept[EvalError](evalWith(List(id, first), call))
  }

  // -- list literals ----------------------------------------------------------

  test("a parenthesized list with a non-symbol head evaluates to a list value") {
    val list = SList(List(IntLit(1), IntLit(2), IntLit(3)))
    assertEquals(evalExpr(list), SList(List(IntLit(1), IntLit(2), IntLit(3))))
  }

  test("elements of a list literal are themselves evaluated") {
    val list = SList(List(BinExpr("+", IntLit(1), IntLit(2)), IntLit(3)))
    assertEquals(evalExpr(list), SList(List(IntLit(3), IntLit(3))))
  }

  // -- BinExpr: arithmetic ------------------------------------------------------

  test("integer + integer stays an integer") {
    assertEquals(evalExpr(BinExpr("+", IntLit(1), IntLit(2))), IntLit(3))
  }

  test("integer - integer stays an integer") {
    assertEquals(evalExpr(BinExpr("-", IntLit(5), IntLit(2))), IntLit(3))
  }

  test("integer * integer stays an integer") {
    assertEquals(evalExpr(BinExpr("*", IntLit(4), IntLit(5))), IntLit(20))
  }

  test("exact integer division stays an integer") {
    assertEquals(evalExpr(BinExpr("/", IntLit(6), IntLit(3))), IntLit(2))
  }

  test("inexact integer division produces a float") {
    assertEquals(evalExpr(BinExpr("/", IntLit(7), IntLit(2))), FloatLit(3.5))
  }

  test("mixing an integer and a float promotes the result to float") {
    assertEquals(evalExpr(BinExpr("+", IntLit(1), FloatLit(2.5))), FloatLit(3.5))
  }

  test("float + float stays a float") {
    assertEquals(evalExpr(BinExpr("+", FloatLit(1.5), FloatLit(2.5))), FloatLit(4.0))
  }

  test("division by zero raises EvalError") {
    intercept[EvalError](evalExpr(BinExpr("/", IntLit(1), IntLit(0))))
  }

  test("arithmetic on a non-numeric operand raises EvalError") {
    intercept[EvalError](evalExpr(BinExpr("+", IntLit(1), StringLit("x"))))
  }

  // -- NegExpr ------------------------------------------------------------------

  test("negating an integer literal") {
    assertEquals(evalExpr(NegExpr(IntLit(3))), IntLit(-3))
  }

  test("negating a float literal") {
    assertEquals(evalExpr(NegExpr(FloatLit(1.5))), FloatLit(-1.5))
  }

  test("the operand of a negation is evaluated first") {
    assertEquals(evalExpr(NegExpr(BinExpr("+", IntLit(1), IntLit(2)))), IntLit(-3))
  }

  test("negation resolves a bound symbol in its operand") {
    val decls = List(Decl("x", List(), IntLit(7)))
    assertEquals(evalWith(decls, NegExpr(Symbol("x"))), IntLit(-7))
  }

  test("negating a non-number raises EvalError") {
    intercept[EvalError](evalExpr(NegExpr(BoolLit(true))))
  }

  // -- NotExpr ------------------------------------------------------------------

  test("not inverts true") {
    assertEquals(evalExpr(NotExpr(BoolLit(true))), BoolLit(false))
  }

  test("not inverts false") {
    assertEquals(evalExpr(NotExpr(BoolLit(false))), BoolLit(true))
  }

  test("the operand of not is evaluated first") {
    assertEquals(evalExpr(NotExpr(BinExpr("=", IntLit(1), IntLit(2)))), BoolLit(true))
  }

  test("not is its own inverse") {
    assertEquals(evalExpr(NotExpr(NotExpr(BoolLit(true)))), BoolLit(true))
  }

  test("not of a non-boolean raises EvalError") {
    intercept[EvalError](evalExpr(NotExpr(IntLit(0))))
  }

  // -- LetExpr ------------------------------------------------------------------

  test("let binds a name over its body") {
    assertEquals(evalExpr(LetExpr("x", IntLit(2), BinExpr("*", Symbol("x"), IntLit(3)))), IntLit(6))
  }

  test("the bound expression is evaluated in the enclosing environment") {
    // The inner `x` in the initializer must resolve to the outer x (10),
    // not to the x being defined -- let is not recursive.
    val decls = List(Decl("x", List(), IntLit(10)))
    val expr = LetExpr("x", BinExpr("+", Symbol("x"), IntLit(1)), Symbol("x"))
    assertEquals(evalWith(decls, expr), IntLit(11))
  }

  test("a let binding does not escape its body") {
    intercept[EvalError](
      evalExpr(SList(List(LetExpr("x", IntLit(1), Symbol("x")), Symbol("x"))))
    )
  }

  // -- IfExpr -------------------------------------------------------------------

  test("if selects the then-branch when the condition is true") {
    assertEquals(evalExpr(IfExpr(BoolLit(true), IntLit(1), IntLit(2))), IntLit(1))
  }

  test("if selects the else-branch when the condition is false") {
    assertEquals(evalExpr(IfExpr(BoolLit(false), IntLit(1), IntLit(2))), IntLit(2))
  }

  test("the condition of an if is evaluated") {
    val expr = IfExpr(BinExpr("<", IntLit(1), IntLit(2)), StringLit("yes"), StringLit("no"))
    assertEquals(evalExpr(expr), StringLit("yes"))
  }

  test("the branch not taken is never evaluated") {
    // The else-branch would raise EvalError if it were evaluated.
    val expr = IfExpr(BoolLit(true), IntLit(1), BinExpr("/", IntLit(1), IntLit(0)))
    assertEquals(evalExpr(expr), IntLit(1))
  }

  test("a non-boolean condition raises EvalError") {
    intercept[EvalError](evalExpr(IfExpr(IntLit(1), IntLit(1), IntLit(2))))
  }

  test("recursion terminates when a base case guards the recursive call") {
    // (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
    val fact = Decl("fact", List("n"),
      IfExpr(
        BinExpr("=", Symbol("n"), IntLit(0)),
        IntLit(1),
        BinExpr("*", Symbol("n"), SList(List(Symbol("fact"), BinExpr("-", Symbol("n"), IntLit(1)))))
      ))
    assertEquals(evalWith(List(fact), SList(List(Symbol("fact"), IntLit(5)))), IntLit(120))
  }

  // -- BinExpr: comparisons -----------------------------------------------------

  test("= compares equal numbers as true") {
    assertEquals(evalExpr(BinExpr("=", IntLit(2), IntLit(2))), BoolLit(true))
  }

  test("= compares unequal numbers as false") {
    assertEquals(evalExpr(BinExpr("=", IntLit(2), IntLit(3))), BoolLit(false))
  }

  test("< holds for a smaller left-hand side") {
    assertEquals(evalExpr(BinExpr("<", IntLit(1), IntLit(2))), BoolLit(true))
  }

  test("> holds for a larger left-hand side") {
    assertEquals(evalExpr(BinExpr(">", IntLit(3), IntLit(2))), BoolLit(true))
  }

  test("<= holds when equal") {
    assertEquals(evalExpr(BinExpr("<=", IntLit(2), IntLit(2))), BoolLit(true))
  }

  test(">= holds when equal") {
    assertEquals(evalExpr(BinExpr(">=", IntLit(2), IntLit(2))), BoolLit(true))
  }

  test("/= holds for unequal numbers") {
    assertEquals(evalExpr(BinExpr("/=", IntLit(1), IntLit(2))), BoolLit(true))
  }

  test("comparisons work across int/float") {
    assertEquals(evalExpr(BinExpr("<", IntLit(1), FloatLit(1.5))), BoolLit(true))
  }
