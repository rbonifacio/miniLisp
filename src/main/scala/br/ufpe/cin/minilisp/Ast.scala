package br.ufpe.cin.minilisp

case class Program(decls: List[Decl], expr: Expr)

case class Decl(name: String, args: List[String], body: Expr)

enum Expr:
  case IntLit(value: Long)
  case FloatLit(value: Double)
  case StringLit(value: String)
  case BoolLit(value: Boolean)
  case Symbol(name: String)
  case SList(items: List[Expr])
  case BinExpr(op: String, left: Expr, right: Expr)
  case NegExpr(operand: Expr)
  case LetExpr(name: String, valueExpr: Expr, body: Expr)
  case IfExpr(cond: Expr, thenBranch: Expr, elseBranch: Expr)
