package br.ufpe.cin.minilisp

import scala.collection.mutable

/** Thrown for any evaluation-time failure: unbound symbols, wrong arity,
  * type mismatches, etc.
  */
case class EvalError(message: String) extends RuntimeException(message)

/** A lexical environment: maps function names to their declarations. */
final class Env():
  private val functions = mutable.Map.empty[String, Decl]

  def define(name: String, decl: Decl): Unit =
    functions(name) = decl

  def lookup(name: String): Decl =
    functions.get(name) match
      case Some(decl) => decl
      case None       => throw EvalError(s"unbound function: $name")



/** Evaluates MiniLisp [[Expr]] trees against an [[Env]]. */
object Interpreter:

  def evalProgram(program: Program): Value =
    val env: Env = new Env()
    program.decls.map(d => env.define(d.name, d))
    eval(program.expr, env)


  def eval(expr: Expr, env: Env): Value = ???
