package br.ufpe.cin.minilisp

import scala.collection.mutable

/** Runtime values produced by evaluating an [[Expr]]. */
enum Value:
  case IntV(value: Long)
  case DoubleV(value: Double)
  case StringV(value: String)
  case BoolV(value: Boolean)
  case SymV(name: String)
  case ListV(items: List[Value])
  case Closure(params: List[String], varargs: Option[String], body: List[Expr], env: Env)
  case Builtin(name: String, fn: List[Value] => Value)
  case NilV

  def isTruthy: Boolean = this match
    case Value.BoolV(false) => false
    case _                  => true

  def render: String = this match
    case Value.IntV(v)      => v.toString
    case Value.DoubleV(v)   => v.toString
    case Value.StringV(v)   => "\"" + v + "\""
    case Value.BoolV(v)     => if v then "#t" else "#f"
    case Value.SymV(n)      => n
    case Value.NilV         => "()"
    case Value.ListV(items) => items.map(_.render).mkString("(", " ", ")")
    case _: Value.Closure   => "#<closure>"
    case Value.Builtin(n, _) => s"#<builtin:$n>"

/** Thrown for any evaluation-time failure: unbound symbols, wrong arity,
  * type mismatches, etc.
  */
final case class EvalError(message: String) extends RuntimeException(message)

/** A lexical environment: a mutable variable frame chained to an optional parent. */
final class Env(parent: Option[Env] = None):
  private val bindings = mutable.Map.empty[String, Value]

  def define(name: String, value: Value): Unit =
    bindings(name) = value

  def set(name: String, value: Value): Unit =
    if bindings.contains(name) then bindings(name) = value
    else
      parent match
        case Some(p) => p.set(name, value)
        case None    => throw EvalError(s"unbound variable: $name")

  def lookup(name: String): Value =
    bindings.get(name) match
      case Some(v) => v
      case None =>
        parent match
          case Some(p) => p.lookup(name)
          case None    => throw EvalError(s"unbound variable: $name")

  def child(): Env = Env(Some(this))
