package br.ufpe.cin.minilisp

/** Runtime values produced by evaluating an [[Expr]]. */
enum Value:
  case IntV(value: Long)
  case DoubleV(value: Double)
  case StringV(value: String)
  case BoolV(value: Boolean)
  case SymV(name: String)
  case ListV(items: List[Value])
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
    case Value.Builtin(n, _) => s"#<builtin:$n>"

