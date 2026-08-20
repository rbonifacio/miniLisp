package br.ufpe.cin.minilisp

import Expr.*

/** Demonstrates that the AST + interpreter work end to end, *without* going
  * through the ANTLR parser yet. Building a bridge from the generated parse
  * tree to this same `Expr` AST (replacing this hand-built tree with the
  * output of a real parser) is left as the follow-up exercise.
  *
  * Equivalent source:
  *   (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
  *   (fact 5)
  */
object Main:

  def main(args: Array[String]): Unit =
    val factDefinition =
      SList(List(
        Sym("define"),
        SList(List(Sym("fact"), Sym("n"))),
        SList(List(
          Sym("if"),
          SList(List(Sym("="), Sym("n"), IntLit(0))),
          IntLit(1),
          SList(List(Sym("*"), Sym("n"), SList(List(Sym("fact"), SList(List(Sym("-"), Sym("n"), IntLit(1)))))))
        ))
      ))

    val factCall = SList(List(Sym("fact"), IntLit(5)))

    val env = Builtins.newGlobalEnv()
    Interpreter.eval(factDefinition, env)
    val result = Interpreter.eval(factCall, env)

    println(s"(fact 5) = ${result.render}")
