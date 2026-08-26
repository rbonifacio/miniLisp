package br.ufpe.cin.minilisp

import br.ufpe.cin.minilisp.Expr.*

// ---------------------------------------------------------------------------
// ESTRATÉGIA 1: espaço em branco tratado explicitamente em cada regra.
//
// Não há convenção alguma: cada regra que precisa aceitar espaço chama
// `spaces` na mão, no ponto exato em que o espaço pode aparecer. É o ponto
// de partida natural, e serve para expor o problema que as próximas
// estratégias resolvem.
// ---------------------------------------------------------------------------

def expr: Parser[Expr] = identifier | integer | slist

def identifier: Parser[Expr] =
  alpha >>= { first =>
    many(alpha | digit) >>= { rest =>
      pure(Symbol((first :: rest).mkString)) } }

def integer: Parser[Expr] =
  many1(digit) >>= { ds => pure(IntLit(ds.mkString.toLong)) }

// Repare no ruído: são quatro chamadas a `spaces` para reconhecer uma
// construção com apenas três elementos significativos.
def slist: Parser[Expr] =
  symbol('(') >>= { _ =>
    spaces >>= { _ =>
      items >>= { es =>
        spaces >>= { _ =>
          symbol(')') >>= { _ => pure(SList(es)) } } } } }

def items: Parser[List[Expr]] =
  (expr >>= { e =>
    spaces >>= { _ =>
      items >>= { es => pure(e :: es) } } }) | pure(List())

// E o espaço à esquerda do programa inteiro é mais um caso especial,
// que só existe porque não há invariante global.
def parseExpr: Parser[Expr] =
  spaces >>= { _ => expr }
