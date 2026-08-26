package br.ufpe.cin.minilisp

// Um Parser[A] representa uma computação que:
//   - recebe uma String como entrada;
//   - pode produzir zero ou mais resultados;
//   - cada resultado contém um valor do tipo A e a entrada restante.
//
// Parser, juntamente com as operações pure e >>= definidas abaixo,
// fornece uma estrutura monádica:
//   - pure  : A => Parser[A]
//   - >>=   : (Parser[A], A => Parser[B]) => Parser[B]
//
// pure cria um parser sem consumir a entrada, enquanto >>= permite
// sequenciar parsers, fazendo com que o resultado de um parser determine
// o próximo parser a ser executado.
//
type Parser[T] = (String) => List[(T, String)]

def pure[A](a: A): Parser[A] = input =>
  List((a, input))

extension [A](p: Parser[A])
  def >>=[B](f: A => Parser[B]): Parser[B] = input =>
    p(input) match {
      case List() => List()
      case List((v, newInput)) => (f(v))(newInput)
      case other => List()
    }

  def |(q: Parser[A]): Parser[A] = input =>
    p(input) match {
      case List() => q(input)
      case res  => res
    }

def failure[A]: Parser[A] = _ => List.empty

// Um parser que reconhece um único caracter lido
// do input. Caso 'input' seja uma lista vazia,
// o parser falha.
def char: Parser[Char] = input =>
  input match {
    case "" => List()
    case s => List((s.head, s.tail))
  }

def symbol(a: Char): Parser[Char] =
  char >>= { v => if a == v then pure(v) else failure }

def string(str: String): Parser[String] =
  if str.isEmpty
   then pure("")
  else symbol(str.head) >>= { v => string(str.tail) >>= {vs => pure(s"$v$vs")} }

def digit: Parser[Char] =
  char >>= { v => if v.isDigit then pure(v) else failure }

def alpha: Parser[Char] =
  char >>= { v => if v.isLetter then pure(v) else failure }

def many[A](p: Parser[A]): Parser[List[A]] =
  (p >>= { v => many(p) >>= { vs => pure(v :: vs) } }) | pure(List())

def many1[A](p: Parser[A]): Parser[List[A]] =
  p >>= { v => many(p) >>= { vs => pure(v :: vs) } }

def ~(options: List[Char]): Parser[Char] =
  char >>= { v => if options.contains(v) then failure else pure(v) }

def oneof(options: List[Char]): Parser[Char] =
  char >>= { v => if options.contains(v) then pure(v) else failure }

// ---------------------------------------------------------------------------
// ESTRATÉGIA 2: convenção de espaço à direita.
//
// O invariante estabelecido aqui é:
//
//     todo parser deixa a entrada posicionada no início do próximo token.
//
// Ele é preservado automaticamente por sequenciamento (>>=), alternativa (|)
// e repetição (many), de modo que basta garanti-lo nos parsers primitivos.
// ---------------------------------------------------------------------------

// Ao menos um caracter de espaçamento.
// IMPORTANTE: many1, e não many. Um parser que tem sucesso consumindo zero
// caracteres faz `many` sobre ele divergir.
def whitespace: Parser[Unit] =
  many1(oneof(List(' ', '\t', '\n', '\r'))) >>= { _ => pure(()) }

// Comentário de linha, conforme a regra COMMENT da gramática ANTLR:
// '//' seguido de tudo até o fim da linha.
//
// Os backticks em `~` são necessários: em posição de prefixo o compilador
// leria ~(xs) como xs.unary_~.
def comment: Parser[Unit] =
  string("//") >>= { _ =>
    many(`~`(List('\n', '\r'))) >>= { _ => pure(()) } }

// Tudo que deve ser descartado entre dois tokens. Espaço e comentário são
// exatamente a mesma coisa do ponto de vista do parser -- ambos separam
// tokens sem contribuir com nada -- então pertencem ao mesmo lugar.
//
// `many` continua seguro: whitespace consome ao menos um caracter, comment
// consome ao menos dois.
def junk: Parser[Unit] =
  many(whitespace | comment) >>= { _ => pure(()) }

// O combinador central da convenção: aplica `p` e descarta o que vier depois.
def token[A](p: Parser[A]): Parser[A] =
  p >>= { v => junk >>= { _ => pure(v) } }

def symb(c: Char): Parser[Char] =
  token(symbol(c))

// Único ponto do sistema em que o espaço à esquerda é descartado.
def parseAll[A](p: Parser[A]): Parser[A] =
  junk >>= { _ => p }
