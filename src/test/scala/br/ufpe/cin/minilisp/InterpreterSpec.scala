package br.ufpe.cin.minilisp

/** Exercises the interpreter directly against hand-built [[Expr]] trees.
  * No parser is involved yet — these ASTs stand in for what a MiniLisp
  * parser will eventually produce from source text.
  */
class InterpreterSpec extends munit.FunSuite:

  import Expr.*

  // small builders to keep the trees below readable
  private def int(v: Long): Expr = IntLit(v)
  private def sym(name: String): Expr = Sym(name)
  private def sl(items: Expr*): Expr = SList(items.toList)

  private def run(exprs: Expr*): Value =
    val env = Builtins.newGlobalEnv()
    Interpreter.evalProgram(exprs.toList, env)

  test("integer arithmetic") {
    assertEquals(run(sl(sym("+"), int(1), int(2), int(3))), Value.IntV(6))
    assertEquals(run(sl(sym("*"), int(2), int(3), int(4))), Value.IntV(24))
    assertEquals(run(sl(sym("-"), int(10), int(3), int(2))), Value.IntV(5))
  }

  test("mixed int/float arithmetic promotes to double") {
    assertEquals(run(sl(sym("+"), int(1), FloatLit(2.5))), Value.DoubleV(3.5))
  }

  test("comparisons") {
    assertEquals(run(sl(sym("<"), int(1), int(2), int(3))), Value.BoolV(true))
    assertEquals(run(sl(sym("<"), int(1), int(3), int(2))), Value.BoolV(false))
    assertEquals(run(sl(sym("="), int(2), int(2))), Value.BoolV(true))
  }

  test("if") {
    // (if (> 3 2) 1 2)
    assertEquals(run(sl(sym("if"), sl(sym(">"), int(3), int(2)), int(1), int(2))), Value.IntV(1))
    // (if (> 2 3) 1 2)
    assertEquals(run(sl(sym("if"), sl(sym(">"), int(2), int(3)), int(1), int(2))), Value.IntV(2))
  }

  test("define and lookup") {
    // (define x 5) (+ x 1)
    assertEquals(
      run(sl(sym("define"), sym("x"), int(5)), sl(sym("+"), sym("x"), int(1))),
      Value.IntV(6)
    )
  }

  test("lambda and application") {
    // ((lambda (x y) (+ x y)) 3 4)
    val lambda = sl(sym("lambda"), sl(sym("x"), sym("y")), sl(sym("+"), sym("x"), sym("y")))
    assertEquals(run(sl(lambda, int(3), int(4))), Value.IntV(7))
  }

  test("named function via define sugar, recursive") {
    // (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
    val factDef = sl(
      sym("define"),
      sl(sym("fact"), sym("n")),
      sl(
        sym("if"),
        sl(sym("="), sym("n"), int(0)),
        int(1),
        sl(sym("*"), sym("n"), sl(sym("fact"), sl(sym("-"), sym("n"), int(1))))
      )
    )
    assertEquals(run(factDef, sl(sym("fact"), int(5))), Value.IntV(120))
  }

  test("let and let*") {
    // (let ((x 1) (y 2)) (+ x y))
    val letExpr = sl(sym("let"), sl(sl(sym("x"), int(1)), sl(sym("y"), int(2))), sl(sym("+"), sym("x"), sym("y")))
    assertEquals(run(letExpr), Value.IntV(3))

    // (let* ((x 1) (y (+ x 1))) (+ x y))
    val letStarExpr = sl(
      sym("let*"),
      sl(sl(sym("x"), int(1)), sl(sym("y"), sl(sym("+"), sym("x"), int(1)))),
      sl(sym("+"), sym("x"), sym("y"))
    )
    assertEquals(run(letStarExpr), Value.IntV(3))
  }

  test("cond") {
    // (cond ((= 1 2) 'no) ((= 1 1) 'yes) (else 'never))
    val condExpr = sl(
      sym("cond"),
      sl(sl(sym("="), int(1), int(2)), Quoted(sym("no"))),
      sl(sl(sym("="), int(1), int(1)), Quoted(sym("yes"))),
      sl(sym("else"), Quoted(sym("never")))
    )
    assertEquals(run(condExpr), Value.SymV("yes"))
  }

  test("quote and lists") {
    assertEquals(run(Quoted(sl(int(1), int(2), int(3)))), Value.ListV(List(Value.IntV(1), Value.IntV(2), Value.IntV(3))))
    assertEquals(run(sl(sym("car"), Quoted(sl(int(1), int(2), int(3))))), Value.IntV(1))
    assertEquals(
      run(sl(sym("cdr"), Quoted(sl(int(1), int(2), int(3))))),
      Value.ListV(List(Value.IntV(2), Value.IntV(3)))
    )
    assertEquals(
      run(sl(sym("cons"), int(1), Quoted(sl(int(2), int(3))))),
      Value.ListV(List(Value.IntV(1), Value.IntV(2), Value.IntV(3)))
    )
  }

  test("closures capture their defining environment") {
    // (define (make-adder n) (lambda (x) (+ x n)))
    val makeAdder = sl(
      sym("define"),
      sl(sym("make-adder"), sym("n")),
      sl(sym("lambda"), sl(sym("x")), sl(sym("+"), sym("x"), sym("n")))
    )
    val addDef = sl(sym("define"), sym("add5"), sl(sym("make-adder"), int(5)))
    assertEquals(run(makeAdder, addDef, sl(sym("add5"), int(10))), Value.IntV(15))
  }

  test("and/or short-circuit") {
    assertEquals(run(sl(sym("and"), int(1), int(2), int(3))), Value.IntV(3))
    assertEquals(run(sl(sym("and"), int(1), BoolLit(false), int(3))), Value.BoolV(false))
    assertEquals(run(sl(sym("or"), BoolLit(false), BoolLit(false), int(3))), Value.IntV(3))
  }

  test("set! mutates enclosing scope") {
    assertEquals(
      run(sl(sym("define"), sym("x"), int(1)), sl(sym("set!"), sym("x"), int(2)), sym("x")),
      Value.IntV(2)
    )
  }

  test("unbound variable raises EvalError") {
    intercept[EvalError](run(sym("undefined-variable")))
  }
