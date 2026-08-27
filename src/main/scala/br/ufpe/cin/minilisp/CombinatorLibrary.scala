package br.ufpe.cin.minilisp

// Um Parser[A] representa uma computação que:
//   - recebe uma String como entrada;
//   - ou falha, ou produz exatamente um resultado;
//   - o resultado contém um valor do tipo A e a entrada restante.
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
// O tipo é Option, e não List, porque este parser é determinístico: `|` é
// escolha ordenada, devolvendo o primeiro ramo que tem sucesso, de modo que
// uma entrada nunca produz mais de uma análise. Com List, o caso de dois ou
// mais resultados existiria no tipo sem existir no comportamento, e >>=
// precisaria decidir o que fazer com ele -- a versão anterior o descartava
// em silêncio, o que quebrava a lei `m >>= pure == m`. Com Option esse caso
// não é sequer representável.
//
type Parser[T] = (String) => Option[(T, String)]

def pure[A](a: A): Parser[A] = input =>
  Some((a, input))

extension [A](p: Parser[A])
  def >>=[B](f: A => Parser[B]): Parser[B] = input =>
    p(input).flatMap((v, newInput) => f(v)(newInput))

  // `orElse` recebe o argumento por nome, então `q` só é executado se `p`
  // falhar -- a escolha continua preguiçosa, como no match anterior.
  def |(q: Parser[A]): Parser[A] = input =>
    p(input).orElse(q(input))

def failure[A]: Parser[A] = _ => None

// Um parser que reconhece um único caracter lido
// do input. Caso 'input' seja uma lista vazia,
// o parser falha.
def char: Parser[Char] = input =>
  input match {
    case "" => None
    case s  => Some((s.head, s.tail))
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

// Reconhece um caracter que NÃO esteja em `options`. Simétrico de `oneof`.
//
// Chamava-se `~`, mas esse nome era inutilizável: em posição de prefixo o
// compilador lê ~(xs) como xs.unary_~, obrigando a escrever `~`(xs) com
// backticks em todo uso.
def noneof(options: List[Char]): Parser[Char] =
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
def comment: Parser[Unit] =
  string("//") >>= { _ =>
    many(noneof(List('\n', '\r'))) >>= { _ => pure(()) } }

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

// Só tem sucesso no fim da entrada, sem consumir nada.
def eof: Parser[Unit] = input =>
  if input.isEmpty then Some(((), input)) else None

// Ponto de entrada do sistema. Faz as duas coisas que nenhuma regra da
// gramática deve ter de lembrar:
//
//   - descarta o espaço à esquerda (único lugar onde isso acontece, já que
//     a convenção `token` só cuida do lado direito);
//   - exige que `p` tenha consumido TODA a entrada.
//
// Sem o `eof`, "define (f x) (x) LIXO" era aceito devolvendo " LIXO" como
// resto, e cabia a cada chamador lembrar de conferir -- exatamente o tipo
// de obrigação implícita que a convenção `token` existe para eliminar.
def parseAll[A](p: Parser[A]): Parser[A] =
  junk >>= { _ => p >>= { v => eof >>= { _ => pure(v) } } }

// Um nome: a maior sequência de letras e dígitos disponível.
// Este é o ponto de decisão da fronteira de átomo -- `many` é guloso, então
// o nome sempre é consumido por inteiro antes de qualquer comparação.
def name: Parser[String] = token(
  alpha >>= { first =>
    many(alpha | digit) >>= { rest =>
      pure((first :: rest).mkString) } })

// Uma palavra reservada é um nome *completo* igual a `s`, e não um prefixo.
def keyword(s: String): Parser[String] =
  name >>= { n => if n == s then pure(n) else failure }
