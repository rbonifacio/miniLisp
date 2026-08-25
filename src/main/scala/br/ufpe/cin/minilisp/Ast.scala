package br.ufpe.cin.minilisp

case class Program(decls: List[Decl], expr: Expr)

case class Decl(name: String, args: List[String], body: Expr)

enum Expr:
  case IntLit(value: Long)
  case FloatLit(value: Double)
  case StringLit(value: String)
  case BoolLit(value: Boolean)
  case Sym(name: String)
  case SList(items: List[Expr])
  case Quoted(expr: Expr)
