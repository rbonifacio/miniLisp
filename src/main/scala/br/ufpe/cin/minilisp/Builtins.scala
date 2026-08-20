package br.ufpe.cin.minilisp

import Value.*

/** Constructs the top-level [[Env]] pre-populated with the builtin procedures. */
object Builtins:

  def newGlobalEnv(): Env =
    val env = Env()
    numeric.foreach((name, f) => env.define(name, Builtin(name, f)))
    comparisons.foreach((name, f) => env.define(name, Builtin(name, f)))
    lists.foreach((name, f) => env.define(name, Builtin(name, f)))
    misc.foreach((name, f) => env.define(name, Builtin(name, f)))
    env

  private def asNum(v: Value): Either[Long, Double] = v match
    case IntV(i)    => Left(i)
    case DoubleV(d) => Right(d)
    case other      => throw EvalError(s"expected a number, got ${other.render}")

  private def toDouble(v: Value): Double = asNum(v) match
    case Left(i)  => i.toDouble
    case Right(d) => d

  private def asDouble(n: Either[Long, Double]): Double = n.fold(_.toDouble, d => d)

  private def numFold(name: String, args: List[Value], identityValue: Long)(
      intOp: (Long, Long) => Long,
      dblOp: (Double, Double) => Double
  ): Value =
    if args.isEmpty then IntV(identityValue)
    else
      args.map(asNum).reduce {
        case (Left(a), Left(b)) => Left(intOp(a, b))
        case (a, b)             => Right(dblOp(asDouble(a), asDouble(b)))
      } match
        case Left(i)  => IntV(i)
        case Right(d) => DoubleV(d)

  private val numeric: Map[String, List[Value] => Value] = Map(
    "+" -> (args => numFold("+", args, 0)(_ + _, _ + _)),
    "*" -> (args => numFold("*", args, 1)(_ * _, _ * _)),
    "-" -> {
      case Nil         => throw EvalError("-: expects at least 1 argument")
      case List(a)     => asNum(a) match
        case Left(i)  => IntV(-i)
        case Right(d) => DoubleV(-d)
      case a :: rest => numFold("-", a :: rest, 0)(_ - _, _ - _)
    },
    "/" -> {
      case Nil     => throw EvalError("/: expects at least 1 argument")
      case List(a) => DoubleV(1.0 / toDouble(a))
      case a :: rest =>
        rest.foldLeft(a) { (acc, b) =>
          (asNum(acc), asNum(b)) match
            case (Left(x), Left(y)) if y != 0 && x % y == 0 => IntV(x / y)
            case _                                          => DoubleV(toDouble(acc) / toDouble(b))
        }
    },
    "modulo" -> {
      case List(a, b) => (asNum(a), asNum(b)) match
        case (Left(x), Left(y)) => IntV(Math.floorMod(x, y))
        case _                  => DoubleV(toDouble(a) % toDouble(b))
      case _ => throw EvalError("modulo: expects 2 arguments")
    }
  )

  private def chainCompare(args: List[Value], p: (Double, Double) => Boolean): Value =
    Value.BoolV(args.sliding(2).forall {
      case Seq(a, b) => p(toDouble(a), toDouble(b))
      case _         => true
    })

  private val comparisons: Map[String, List[Value] => Value] = Map(
    "=" -> (args => chainCompare(args, _ == _)),
    "<" -> (args => chainCompare(args, _ < _)),
    ">" -> (args => chainCompare(args, _ > _)),
    "<=" -> (args => chainCompare(args, _ <= _)),
    ">=" -> (args => chainCompare(args, _ >= _))
  )

  private def isEqual(a: Value, b: Value): Boolean = (a, b) match
    case (IntV(x), IntV(y))       => x == y
    case (DoubleV(x), DoubleV(y)) => x == y
    case (IntV(x), DoubleV(y))    => x.toDouble == y
    case (DoubleV(x), IntV(y))    => x == y.toDouble
    case (StringV(x), StringV(y)) => x == y
    case (BoolV(x), BoolV(y))     => x == y
    case (SymV(x), SymV(y))       => x == y
    case (NilV, NilV)             => true
    case (ListV(xs), ListV(ys))   => xs.length == ys.length && xs.zip(ys).forall(isEqual)
    case _                        => false

  private val lists: Map[String, List[Value] => Value] = Map(
    "cons" -> {
      case List(h, ListV(t)) => ListV(h :: t)
      case List(h, NilV)     => ListV(List(h))
      case List(_, other)    => throw EvalError(s"cons: second argument must be a list, got ${other.render}")
      case _                 => throw EvalError("cons: expects 2 arguments")
    },
    "car" -> {
      case List(ListV(h :: _)) => h
      case List(ListV(Nil))    => throw EvalError("car: empty list")
      case List(NilV)          => throw EvalError("car: empty list")
      case _                   => throw EvalError("car: expects 1 list argument")
    },
    "cdr" -> {
      case List(ListV(_ :: t)) => if t.isEmpty then NilV else ListV(t)
      case List(ListV(Nil))    => throw EvalError("cdr: empty list")
      case List(NilV)          => throw EvalError("cdr: empty list")
      case _                   => throw EvalError("cdr: expects 1 list argument")
    },
    "list" -> (args => if args.isEmpty then NilV else ListV(args)),
    "length" -> {
      case List(ListV(items)) => IntV(items.length)
      case List(NilV)         => IntV(0)
      case _                  => throw EvalError("length: expects 1 list argument")
    },
    "append" -> (args =>
      val items = args.flatMap {
        case ListV(xs) => xs
        case NilV      => Nil
        case other     => throw EvalError(s"append: expected a list, got ${other.render}")
      }
      if items.isEmpty then NilV else ListV(items)
    ),
    "null?" -> {
      case List(NilV)         => BoolV(true)
      case List(ListV(Nil))   => BoolV(true)
      case List(_)            => BoolV(false)
      case _                  => throw EvalError("null?: expects 1 argument")
    },
    "pair?" -> {
      case List(ListV(items)) => BoolV(items.nonEmpty)
      case List(_)            => BoolV(false)
      case _                  => throw EvalError("pair?: expects 1 argument")
    }
  )

  private val misc: Map[String, List[Value] => Value] = Map(
    "not" -> { case List(v) => BoolV(!v.isTruthy); case _ => throw EvalError("not: expects 1 argument") },
    "eq?" -> { case List(a, b) => BoolV(isEqual(a, b)); case _ => throw EvalError("eq?: expects 2 arguments") },
    "equal?" -> { case List(a, b) => BoolV(isEqual(a, b)); case _ => throw EvalError("equal?: expects 2 arguments") },
    "number?" -> { case List(IntV(_) | DoubleV(_)) => BoolV(true); case List(_) => BoolV(false); case _ => throw EvalError("number?: expects 1 argument") },
    "string?" -> { case List(StringV(_)) => BoolV(true); case List(_) => BoolV(false); case _ => throw EvalError("string?: expects 1 argument") },
    "symbol?" -> { case List(SymV(_)) => BoolV(true); case List(_) => BoolV(false); case _ => throw EvalError("symbol?: expects 1 argument") },
    "procedure?" -> { case List(_: Closure | _: Builtin) => BoolV(true); case List(_) => BoolV(false); case _ => throw EvalError("procedure?: expects 1 argument") },
    "display" -> {
      case List(v) => print(v match { case StringV(s) => s; case other => other.render }); NilV
      case _       => throw EvalError("display: expects 1 argument")
    },
    "print" -> {
      case List(v) => println(v match { case StringV(s) => s; case other => other.render }); NilV
      case _       => throw EvalError("print: expects 1 argument")
    },
    "newline" -> { case Nil => println(); NilV; case _ => throw EvalError("newline: expects 0 arguments") }
  )
