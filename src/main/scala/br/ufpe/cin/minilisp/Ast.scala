package br.ufpe.cin.minilisp

/** The abstract syntax tree for miniLisp, produced from the ANTLR parse tree
  * by [[Parser]] and consumed by [[Interpreter]].
  */
enum Expr:
  case IntLit(value: Long)
  case FloatLit(value: Double)
  case StringLit(value: String)
  case BoolLit(value: Boolean)
  case Sym(name: String)
  case SList(items: List[Expr])
  case Quoted(expr: Expr)
