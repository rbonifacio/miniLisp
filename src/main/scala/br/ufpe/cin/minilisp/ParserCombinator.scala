package br.ufpe.cin.minilisp

import br.ufpe.cin.minilisp.Expr.*

// ---------------------------------------------------------------------------
// ESTRATÉGIA 2: convenção de espaço à direita (`token`).
//
// Nenhuma regra abaixo menciona espaço em branco. Cada parser primitivo é
// embrulhado em `token`, que consome o espaçamento à direita; a partir daí
// a composição preserva o invariante sozinha.
// ---------------------------------------------------------------------------

def expr: Parser[Expr] = identifier | integer | slist

def identifier: Parser[Expr] = token(
  alpha >>= { first =>
    many(alpha | digit) >>= { rest =>
      pure(Symbol((first :: rest).mkString)) } })

def integer: Parser[Expr] = token(
  many1(digit) >>= { ds => pure(IntLit(ds.mkString.toLong)) })

// Compare com a versão anterior: as quatro chamadas a `spaces` sumiram, e a
// regra auxiliar `items` deu lugar a um `many(expr)` direto. `many` é seguro
// aqui porque todo `expr` consome ao menos um caracter.
def slist: Parser[Expr] =
  symb('(') >>= { _ =>
    many(expr) >>= { items =>
      symb(')') >>= { _ => pure(SList(items)) } } }

def parseExpr: Parser[Expr] = parseAll(expr)
