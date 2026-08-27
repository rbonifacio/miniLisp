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
// O tipo é Either, e não List: este parser é determinístico -- `|` é escolha
// ordenada, então uma entrada nunca produz mais de uma análise -- e a falha
// carrega o motivo. Com List, o caso de dois ou mais resultados existiria no
// tipo sem existir no comportamento, e >>= o descartava em silêncio,
// quebrando a lei `m >>= pure == m`.
//
type Parser[T] = (String) => Either[Failure, (T, String)]

/** Uma falha: o que o parser esperava, e o trecho de entrada onde parou.
  *
  * A posição não é guardada como número. Todo parser desta biblioteca apenas
  * descarta prefixos, então `remaining` é sempre um sufixo da entrada
  * original e o deslocamento é a diferença de tamanhos. É assim que
  * `describe` recupera linha e coluna sem que o estado precise carregá-las, e
  * é assim que `merge` decide qual de duas falhas chegou mais longe: a que
  * deixou MENOS entrada por consumir.
  */
case class Failure(remaining: String, expected: List[String])

/** Funde duas falhas: vence a que chegou mais longe; empatadas, as
  * expectativas se somam.
  */
def merge(a: Failure, b: Failure): Failure =
  if a.remaining.length < b.remaining.length then a
  else if b.remaining.length < a.remaining.length then b
  else Failure(a.remaining, (a.expected ++ b.expected).distinct)

def pure[A](a: A): Parser[A] = input =>
  Right((a, input))

def failure[A](expected: String): Parser[A] = input =>
  Left(Failure(input, List(expected)))

extension [A](p: Parser[A])
  def >>=[B](f: A => Parser[B]): Parser[B] = input =>
    p(input).flatMap((v, newInput) => f(v)(newInput))

  // Escolha ordenada. Quando os DOIS ramos falham, sobrevive o diagnóstico
  // que chegou mais longe. Sem isso o erro reportado seria sempre o do
  // último ramo tentado, que costuma ser o menos informativo: em "(iZ", o
  // ramo que reconhece "(if" falhou após dois caracteres e sabia o que
  // esperava, enquanto um ramo que falha no primeiro caracter não sabe nada.
  def |(q: Parser[A]): Parser[A] = input =>
    p(input) match
      case ok @ Right(_) => ok
      case Left(e1) =>
        q(input) match
          case ok @ Right(_) => ok
          case Left(e2)      => Left(merge(e1, e2))

/** Reconhece um caracter que satisfaz `pred`, ou falha dizendo o que esperava.
  *
  * O teste vem ANTES do consumo, de propósito. A forma anterior era
  * `char >>= { v => if pred(v) then pure(v) else failure }`, que já havia
  * avançado um caracter quando decidia falhar -- e a falha apontaria para a
  * posição seguinte à do erro.
  */
def satisfy(expected: String)(pred: Char => Boolean): Parser[Char] = input =>
  if input.nonEmpty && pred(input.head) then Right((input.head, input.tail))
  else Left(Failure(input, List(expected)))

/** Reporta a falha de `p` na posição em que `p` COMEÇOU, e não onde parou.
  *
  * Necessário para parsers que só descobrem o erro depois de consumir. Sem
  * isso, um casamento parcial apareceria mais à frente que as alternativas e
  * venceria a fusão do `|` indevidamente. É o `try` do Parsec.
  */
def attempt[A](p: Parser[A]): Parser[A] = input =>
  p(input).left.map(f => Failure(input, f.expected))

/** Como `attempt`, mas também troca a expectativa por uma descrição de mais
  * alto nível -- "a comment" em vez de "'/'".
  */
def label[A](expected: String)(p: Parser[A]): Parser[A] = input =>
  p(input).left.map(_ => Failure(input, List(expected)))

private def quoted(c: Char): String = c match
  case '\n'  => "'\\n'"
  case '\r'  => "'\\r'"
  case '\t'  => "'\\t'"
  case other => s"'$other'"

// Um parser que reconhece um único caracter lido do input.
// Caso 'input' esteja vazio, o parser falha.
def char: Parser[Char] = satisfy("any character")(_ => true)

def symbol(a: Char): Parser[Char] = satisfy(quoted(a))(_ == a)

// `label` por duas razões: um casamento parcial ("//" contra "/x") não pode
// reportar a falha no meio do literal, e a expectativa útil é o literal
// inteiro, não o caracter em que ele divergiu.
def string(str: String): Parser[String] = label("\"" + str + "\"")(
  if str.isEmpty
   then pure("")
  else symbol(str.head) >>= { v => string(str.tail) >>= {vs => pure(s"$v$vs")} })

def digit: Parser[Char] = satisfy("a digit")(_.isDigit)

def alpha: Parser[Char] = satisfy("a letter")(_.isLetter)

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
  satisfy(s"any character except ${options.map(quoted).mkString(", ")}")(
    !options.contains(_))

def oneof(options: List[Char]): Parser[Char] =
  satisfy(s"one of ${options.map(quoted).mkString(", ")}")(options.contains)

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
def whitespace: Parser[Unit] = label("whitespace")(
  many1(oneof(List(' ', '\t', '\n', '\r'))) >>= { _ => pure(()) })

// Comentário de linha, conforme a regra COMMENT da gramática ANTLR:
// '//' seguido de tudo até o fim da linha.
def comment: Parser[Unit] = label("a comment")(
  string("//") >>= { _ =>
    many(noneof(List('\n', '\r'))) >>= { _ => pure(()) } })

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
  if input.isEmpty then Right(((), input))
  else Left(Failure(input, List("end of input")))

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
def name: Parser[String] = label("a name")(token(
  alpha >>= { first =>
    many(alpha | digit) >>= { rest =>
      pure((first :: rest).mkString) } }))

// Uma palavra reservada é um nome *completo* igual a `s`, e não um prefixo.
def keyword(s: String): Parser[String] = label(quotedWord(s))(
  name >>= { n => if n == s then pure(n) else failure(quotedWord(s)) })

private def quotedWord(s: String): String = s"'$s'"

// ---------------------------------------------------------------------------
// Relato de erros.
// ---------------------------------------------------------------------------

/** Formata a falha com linha e coluna, no mesmo formato do parser ANTLR
  * ("line L:C"), de modo que as duas frentes reportem erros da mesma forma.
  */
def describe(original: String, f: Failure): String =
  val offset = original.length - f.remaining.length
  val before = original.take(offset)
  val line   = before.count(_ == '\n') + 1
  val column = offset - (before.lastIndexOf('\n') + 1)
  val found  = f.remaining.headOption.map(quoted).getOrElse("end of input")
  s"line $line:$column expected ${f.expected.distinct.mkString(" or ")}, found $found"

/** Executa um parser já fechado por `parseAll`, devolvendo o valor ou a
  * mensagem de erro formatada.
  */
def run[A](p: Parser[A], input: String): Either[String, A] =
  p(input) match
    case Right((v, _)) => Right(v)
    case Left(f)       => Left(describe(input, f))
