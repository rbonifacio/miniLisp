package br.ufpe.cin.minilisp

import br.ufpe.cin.minilisp.Expr.*

// Palavras que a gramática trata como literais e que, por isso, não podem
// ser usadas como identificadores.
val reserved = Set("define", "let", "if", "not", "True", "False")

// ===========================================================================
// program : decl* expr EOF
// ===========================================================================
def program: Parser[Program] =
  parseAll(
    many(decl)           >>= { ds =>
    expr                 >>= { e  => pure(Program(ds, e)) } })

// decl : 'define' '(' name params* ')' '(' body ')'
//
// Conforme MiniLisp.g4:11, `define` fica FORA dos parênteses:
//     define (sum3 a b c) ((+ a (+ b c)))
def decl: Parser[Decl] =
  keyword("define")      >>= {    _ =>
  symb('(')              >>= {    _ =>
  identifier             >>= { name =>
  many(identifier)       >>= { args =>
  symb(')')              >>= {    _ =>
  symb('(')              >>= {    _ =>
  expr                   >>= { body =>
  symb(')')              >>= {    _ => pure(Decl(name, args, body)) } } } } } } } }

// ===========================================================================
// expr
//
// A ORDEM DAS ALTERNATIVAS É SIGNIFICATIVA. Todas as formas parentizadas
// começam com '(', então `|` (escolha ordenada) decide qual vence. `slist`
// aceita QUALQUER coisa entre parênteses, logo tem de ficar por último --
// caso contrário engole as formas especiais antes que elas sejam tentadas.
//
// >>> EXERCÍCIO: `letExpr` e `ifExpr` entram AQUI, antes de `slist`. <<<
// ===========================================================================
def expr: Parser[Expr]
  = atom
  | binExpr
  | negExpr
  | notExpr
  | slist

// ---------------------------------------------------------------------------
// Formas especiais.
//
// `notExpr` e `negExpr` são o mesmo padrão que `if` e `let` vão seguir:
// abre parêntese, consome a palavra/símbolo que identifica a forma, lê os
// operandos, fecha parêntese. A única diferença é a quantidade de operandos.
// ---------------------------------------------------------------------------

// '(' 'not' operand ')'
def notExpr: Parser[Expr] =
  symb('(')              >>= {    _ =>
  keyword("not")         >>= {    _ =>
  expr                   >>= { op =>
  symb(')')              >>= {    _ => pure(NotExpr(op)) } } } }

// '(' (BinArithOp | BinRelOp | MinusOp) lhs rhs ')'
//
// Vem antes de `negExpr` porque `(- a b)` e `(- a)` compartilham o prefixo:
// tentando a forma binária primeiro, a unária só é alcançada quando de fato
// há um único operando.
def binExpr: Parser[Expr] =
  symb('(')              >>= {    _ =>
  operator               >>= { op =>
  expr                   >>= { lhs =>
  expr                   >>= { rhs =>
  symb(')')              >>= {    _ => pure(BinExpr(op, lhs, rhs)) } } } } }

// '(' MinusOp operand ')'
def negExpr: Parser[Expr] =
  symb('(')              >>= {    _ =>
  token(string("-"))     >>= {    _ =>
  expr                   >>= { op =>
  symb(')')              >>= {    _ => pure(NegExpr(op)) } } } }

// '(' expr* ')' -- a forma genérica, tentada por último.
def slist: Parser[Expr] =
  symb('(')              >>= {    _ =>
  many(expr)             >>= { items =>
  symb(')')              >>= {    _ => pure(SList(items)) } } }

// Operadores, ordenados do mais longo para o mais curto: `string` não faz
// maximal munch sozinho, então ">=" tem de ser tentado antes de ">", e "/="
// antes de "/". Sem essa ordem, `(>= a b)` seria lido como `(> a b)` com um
// "=" sobrando.
def operator: Parser[String] =
  List(">=", "<=", "/=", "+", "*", "/", ">", "<", "=", "-")
    .map(op => token(string(op)))
    .reduce(_ | _)

// ---------------------------------------------------------------------------
// Átomos.
// ---------------------------------------------------------------------------

def atom: Parser[Expr] = float | integer | stringLit | boolean | symbolAtom

// O sinal usa `symbol` cru, e não `symb`: por MiniLisp.g4:33-34 o '-' faz
// parte do literal, logo não pode haver espaço entre ele e os dígitos.
def sign: Parser[String] =
  (symbol('-')           >>= { _ => pure("-") }) | pure("")

// Float vem antes de Integer: sobre "3.14", `integer` casaria com "3" e
// deixaria ".14" para trás.
def float: Parser[Expr] = token(
  sign                   >>= { s  =>
  many1(digit)           >>= { ip =>
  symbol('.')            >>= { _  =>
  many1(digit)           >>= { fp =>
    pure(FloatLit(s"$s${ip.mkString}.${fp.mkString}".toDouble)) } } } })

// `toLongOption` em vez de `toLong`: um literal grande demais deve fazer o
// parser falhar, e não deixar uma NumberFormatException escapar da monad.
def integer: Parser[Expr] = token(
  sign                   >>= { s  =>
  many1(digit)           >>= { ds =>
    s"$s${ds.mkString}".toLongOption match
      case Some(n) => pure(IntLit(n))
      case None    => failure } })

// '"' ~["]* '"'
def stringLit: Parser[Expr] = token(
  symbol('"')            >>= { _  =>
  many(noneof(List('"'))) >>= { cs =>
  symbol('"')            >>= { _  => pure(StringLit(cs.mkString)) } } })

def boolean: Parser[Expr] = pTrue | pFalse

def pTrue: Parser[Expr]  = keyword("True")  >>= { _ => pure(BoolLit(true))  }
def pFalse: Parser[Expr] = keyword("False") >>= { _ => pure(BoolLit(false)) }

def symbolAtom: Parser[Expr] =
  identifier             >>= { n => pure(Symbol(n)) }

// Um identificador é um `name` completo (maximal munch, definido na
// biblioteca) que não seja palavra reservada.
def identifier: Parser[String] =
  name                   >>= { n => if reserved.contains(n) then failure else pure(n) }

def parseExpr: Parser[Expr] = parseAll(expr)
def parseDecl: Parser[Decl] = parseAll(decl)
