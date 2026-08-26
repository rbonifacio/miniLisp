package br.ufpe.cin.minilisp

case class Program(decls: List[Decl], body: Expr)

/** A top-level function declaration: `params` are the *formal* parameter
  * names, bound to the actual arguments supplied at each call site.
  */
case class Decl(name: String, params: List[String], body: Expr)

enum Expr:
  case IntLit(value: Long)
  case FloatLit(value: Double)
  case StringLit(value: String)
  case BoolLit(value: Boolean)
  case Symbol(name: String)
  case SList(items: List[Expr])
  case BinExpr(op: String, lhs: Expr, rhs: Expr)
  case NegExpr(operand: Expr)
  case NotExpr(operand: Expr)
  case LetExpr(name: String, init: Expr, body: Expr)
  case IfExpr(cond: Expr, thenBranch: Expr, elseBranch: Expr)

  /** Renders this expression back as MiniLisp source. */
  def render: String = this match
    case IntLit(v)                     => v.toString
    case FloatLit(v)                   => v.toString
    case StringLit(v)                  => "\"" + v + "\""
    case BoolLit(v)                    => if v then "True" else "False"
    case Symbol(n)                     => n
    case SList(items)                  => items.map(_.render).mkString("(", " ", ")")
    case BinExpr(op, lhs, rhs)         => s"($op ${lhs.render} ${rhs.render})"
    case NegExpr(operand)              => s"(- ${operand.render})"
    case NotExpr(operand)              => s"(not ${operand.render})"
    case LetExpr(name, init, body)     => s"(let ($name ${init.render}) ${body.render})"
    case IfExpr(cond, thenB, elseB)    => s"(if ${cond.render} ${thenB.render} ${elseB.render})"
