package br.ufpe.cin.minilisp

/** Evaluates MiniLisp [[Expr]] trees against an [[Env]]. */
object Interpreter:

  def evalProgram(exprs: List[Expr], env: Env): Value =
    exprs.foldLeft(Value.NilV: Value)((_, e) => eval(e, env))

  def eval(expr: Expr, env: Env): Value = expr match
    case Expr.IntLit(v)    => Value.IntV(v)
    case Expr.FloatLit(v)  => Value.DoubleV(v)
    case Expr.StringLit(v) => Value.StringV(v)
    case Expr.BoolLit(v)   => Value.BoolV(v)
    case Expr.Sym(name)    => env.lookup(name)
    case Expr.Quoted(e)    => quote(e)
    case Expr.SList(Nil)   => Value.NilV
    case Expr.SList(Expr.Sym(op) :: rest) if specialForms.contains(op) =>
      evalSpecialForm(op, rest, env)
    case Expr.SList(fnExpr :: argExprs) =>
      val fn = eval(fnExpr, env)
      apply(fn, argExprs.map(eval(_, env)))

  private val specialForms: Set[String] =
    Set("quote", "if", "define", "lambda", "let", "let*", "cond", "begin", "and", "or", "set!")

  private def evalSpecialForm(op: String, args: List[Expr], env: Env): Value = op match
    case "quote" =>
      args match
        case List(e) => quote(e)
        case _       => throw EvalError("quote: expects exactly 1 argument")

    case "if" =>
      args match
        case List(c, t, f) => if eval(c, env).isTruthy then eval(t, env) else eval(f, env)
        case List(c, t)    => if eval(c, env).isTruthy then eval(t, env) else Value.NilV
        case _             => throw EvalError("if: expects 2 or 3 arguments")

    case "define" =>
      args match
        // (define name expr)
        case List(Expr.Sym(name), valueExpr) =>
          env.define(name, eval(valueExpr, env))
          Value.SymV(name)
        // (define (name params...) body...)
        case Expr.SList(Expr.Sym(name) :: params) :: body if body.nonEmpty =>
          val (fixed, varargs) = parseParams(params)
          env.define(name, Value.Closure(fixed, varargs, body, env))
          Value.SymV(name)
        case _ => throw EvalError("define: malformed definition")

    case "lambda" =>
      args match
        case Expr.SList(params) :: body if body.nonEmpty =>
          val (fixed, varargs) = parseParams(params)
          Value.Closure(fixed, varargs, body, env)
        case _ => throw EvalError("lambda: expects (lambda (params...) body...)")

    case "let" =>
      args match
        case Expr.SList(bindings) :: body if body.nonEmpty =>
          val letEnv = env.child()
          bindings.foreach {
            case Expr.SList(List(Expr.Sym(name), valueExpr)) =>
              letEnv.define(name, eval(valueExpr, env))
            case _ => throw EvalError("let: malformed binding")
          }
          evalBody(body, letEnv)
        case _ => throw EvalError("let: expects (let (bindings...) body...)")

    case "let*" =>
      args match
        case Expr.SList(bindings) :: body if body.nonEmpty =>
          val letEnv = env.child()
          bindings.foreach {
            case Expr.SList(List(Expr.Sym(name), valueExpr)) =>
              letEnv.define(name, eval(valueExpr, letEnv))
            case _ => throw EvalError("let*: malformed binding")
          }
          evalBody(body, letEnv)
        case _ => throw EvalError("let*: expects (let* (bindings...) body...)")

    case "cond" =>
      def go(clauses: List[Expr]): Value = clauses match
        case Nil => Value.NilV
        case Expr.SList(Expr.Sym("else") :: body) :: _ => evalBody(body, env)
        case Expr.SList(test :: body) :: rest =>
          if eval(test, env).isTruthy then evalBody(body, env) else go(rest)
        case _ => throw EvalError("cond: malformed clause")
      go(args)

    case "begin" => evalBody(args, env)

    case "and" =>
      def go(exprs: List[Expr]): Value = exprs match
        case Nil          => Value.BoolV(true)
        case last :: Nil  => eval(last, env)
        case head :: rest => val v = eval(head, env); if v.isTruthy then go(rest) else v
      go(args)

    case "or" =>
      def go(exprs: List[Expr]): Value = exprs match
        case Nil          => Value.BoolV(false)
        case last :: Nil  => eval(last, env)
        case head :: rest => val v = eval(head, env); if v.isTruthy then v else go(rest)
      go(args)

    case "set!" =>
      args match
        case List(Expr.Sym(name), valueExpr) =>
          env.set(name, eval(valueExpr, env))
          Value.NilV
        case _ => throw EvalError("set!: expects (set! name expr)")

    case _ => throw EvalError(s"unknown special form: $op")

  private def parseParams(params: List[Expr]): (List[String], Option[String]) =
    params match
      case fixed :+ Expr.Sym("&rest") :+ Expr.Sym(rest) =>
        (fixed.map { case Expr.Sym(n) => n; case _ => throw EvalError("lambda: params must be symbols") }, Some(rest))
      case _ =>
        (params.map { case Expr.Sym(n) => n; case _ => throw EvalError("lambda: params must be symbols") }, None)

  private def evalBody(body: List[Expr], env: Env): Value =
    body match
      case Nil          => Value.NilV
      case last :: Nil  => eval(last, env)
      case head :: rest => eval(head, env); evalBody(rest, env)

  def apply(fn: Value, args: List[Value]): Value = fn match
    case Value.Builtin(_, f) => f(args)
    case Value.Closure(params, varargs, body, closureEnv) =>
      val callEnv = closureEnv.child()
      bindParams(params, varargs, args, callEnv)
      evalBody(body, callEnv)
    case other => throw EvalError(s"not callable: ${other.render}")

  private def bindParams(params: List[String], varargs: Option[String], args: List[Value], env: Env): Unit =
    (params, varargs) match
      case (Nil, Some(restName)) => env.define(restName, Value.ListV(args))
      case (Nil, None) =>
        if args.nonEmpty then throw EvalError(s"too many arguments, expected 0 got ${args.length}")
      case (p :: ps, _) =>
        args match
          case a :: as => env.define(p, a); bindParams(ps, varargs, as, env)
          case Nil     => throw EvalError(s"too few arguments: missing $p")

  /** Converts a quoted syntax tree into a plain data [[Value]] (symbols and lists
    * are not evaluated).
    */
  private def quote(expr: Expr): Value = expr match
    case Expr.IntLit(v)      => Value.IntV(v)
    case Expr.FloatLit(v)    => Value.DoubleV(v)
    case Expr.StringLit(v)   => Value.StringV(v)
    case Expr.BoolLit(v)     => Value.BoolV(v)
    case Expr.Sym(name)      => Value.SymV(name)
    case Expr.SList(Nil)     => Value.NilV
    case Expr.SList(items)   => Value.ListV(items.map(quote))
    case Expr.Quoted(inner)  => Value.ListV(List(Value.SymV("quote"), quote(inner)))
