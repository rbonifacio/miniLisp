package br.ufpe.cin.minilisp

import scala.collection.mutable
import Expr.*

/** Thrown for any evaluation-time failure: unbound symbols, wrong arity,
  * type mismatches, etc.
  */
case class EvalError(message: String) extends RuntimeException(message)

/** An [[Expr]] already in normal form -- that is, the *result* of evaluation
  * rather than syntax still awaiting it.
  *
  * MiniLisp reuses the AST as its value domain, so this alias carries no
  * extra type safety; it exists to record, in each signature, which side of
  * evaluation that parameter sits on. A function taking an `Expr` expects
  * raw syntax and will evaluate it; one taking a `Value` expects its caller
  * to have evaluated it already.
  */
type Value = Expr

/** A lexical environment: maps names to their bindings.
  *
  * A binding is always a [[Decl]]: a top-level function keeps its
  * parameters and body, while a bound parameter or `let` name is stored as
  * a parameterless [[Decl]] whose body is the already-evaluated [[Value]].
  */
final class Env(parent: Option[Env] = None):
  private val bindings = mutable.Map.empty[String, Decl]

  /** Binds `name` to an already-evaluated value. */
  def define(name: String, value: Value): Unit =
    bindings(name) = Decl(name, List(), value)

  /** Binds `name` to a function declaration. */
  def define(name: String, decl: Decl): Unit =
    bindings(name) = decl

  /** Every name visible here, including those inherited from enclosing
    * environments. Used to offer completions in the interactive session.
    */
  def names: Set[String] =
    bindings.keySet.toSet ++ parent.map(_.names).getOrElse(Set.empty)

  def lookup(name: String): Decl =
    bindings.get(name) match
      case Some(decl) => decl
      case None => parent match
        case Some(p) => p.lookup(name)
        case None    => throw EvalError(s"unbound symbol: $name")

/** Evaluates MiniLisp programs. */
object Interpreter:

  def evalProgram(program: Program): Value =
    val env: Env = new Env()
    program.decls.foreach(d => env.define(d.name, d))
    eval(program.body, env)

  def eval(expr: Expr, env: Env): Value = expr match
    case IntLit(v)                   => IntLit(v)
    case FloatLit(v)                 => FloatLit(v)
    case StringLit(v)                => StringLit(v)
    case BoolLit(v)                  => BoolLit(v)
    case Symbol(name)                => evalCall(name, List(), env)
    case SList(List(Symbol(name)))   => evalCall(name, List(), env)
    case SList(Symbol(name) :: args) => evalCall(name, args.map(a => eval(a, env)), env)
    case SList(items)                => SList(items.map(i => eval(i, env)))
    case BinExpr(op, lhs, rhs)       => evalBinOp(op, eval(lhs, env), eval(rhs, env))
    case NegExpr(operand)            => evalNeg(eval(operand, env))
    case NotExpr(operand)            => evalNot(eval(operand, env))
    case LetExpr(name, init, body)   => evalLet(name, init, body, env)
    case IfExpr(cond, thenBranch, elseBranch) => evalIf(cond, thenBranch, elseBranch, env)

  /** Applies the function bound to `name`. The `args` are already evaluated,
    * in the *caller's* environment; they are bound to the declaration's
    * formal parameters in a fresh frame whose parent is `env`.
    */
  def evalCall(name: String, args: List[Value], env: Env): Value =
    val decl: Decl = env.lookup(name)
    if decl.params.length != args.length then
      throw EvalError(s"invalid call to function $name: expected ${decl.params.length} argument(s), got ${args.length}")
    val frame: Env = new Env(Some(env))
    decl.params.zip(args).foreach((param, arg) => frame.define(param, arg))
    eval(decl.body, frame)

  private def evalBinOp(op: String, lhs: Value, rhs: Value): Value = op match
    case "+" | "-" | "*" | "/"                => evalArith(op, lhs, rhs)
    case ">" | "<" | "=" | ">=" | "<=" | "/=" => evalCompare(op, lhs, rhs)
    case _                                     => throw EvalError(s"unknown operator: $op")

  /** Arithmetic negation: `(- e)`. Numeric operands only -- boolean
    * complement is [[evalNot]], spelled `(not e)`.
    */
  private def evalNeg(operand: Value): Value =
    asNumber(operand).fold(i => IntLit(-i), d => FloatLit(-d))

  /** Boolean complement: `(not e)`. */
  private def evalNot(operand: Value): Value = operand match
    case BoolLit(v) => BoolLit(!v)
    case other      => throw EvalError(s"expected a boolean, got ${other.render}")

  /** `init` is evaluated in the enclosing environment -- `let` is not
    * recursive, so its initializer cannot see the name being bound.
    */
  private def evalLet(name: String, init: Expr, body: Expr, env: Env): Value =
    val bodyEnv: Env = new Env(Some(env))
    bodyEnv.define(name, eval(init, env))
    eval(body, bodyEnv)

  /** Only the branch selected by `cond` is evaluated. */
  private def evalIf(cond: Expr, thenBranch: Expr, elseBranch: Expr, env: Env): Value =
    eval(cond, env) match
      case BoolLit(true)  => eval(thenBranch, env)
      case BoolLit(false) => eval(elseBranch, env)
      case other          => throw EvalError(s"expected a boolean condition, got ${other.render}")

  private def asNumber(value: Value): Either[Long, Double] = value match
    case IntLit(v)   => Left(v)
    case FloatLit(v) => Right(v)
    case other       => throw EvalError(s"expected a number, got ${other.render}")

  private def evalArith(op: String, lhs: Value, rhs: Value): Value =
    (asNumber(lhs), asNumber(rhs)) match
      case (Left(a), Left(b)) =>
        op match
          case "+" => IntLit(a + b)
          case "-" => IntLit(a - b)
          case "*" => IntLit(a * b)
          case "/" =>
            if b == 0 then throw EvalError("/: division by zero")
            else if a % b == 0 then IntLit(a / b)
            else FloatLit(a.toDouble / b.toDouble)
      case (a, b) =>
        val x = a.fold(_.toDouble, identity)
        val y = b.fold(_.toDouble, identity)
        op match
          case "+" => FloatLit(x + y)
          case "-" => FloatLit(x - y)
          case "*" => FloatLit(x * y)
          case "/" =>
            if y == 0.0 then throw EvalError("/: division by zero")
            else FloatLit(x / y)

  private def evalCompare(op: String, lhs: Value, rhs: Value): Value =
    val x = asNumber(lhs).fold(_.toDouble, identity)
    val y = asNumber(rhs).fold(_.toDouble, identity)
    val result = op match
      case ">"  => x > y
      case "<"  => x < y
      case "="  => x == y
      case ">=" => x >= y
      case "<=" => x <= y
      case "/=" => x != y
    BoolLit(result)
