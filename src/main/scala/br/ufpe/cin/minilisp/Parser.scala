package br.ufpe.cin.minilisp

import br.ufpe.cin.minilisp.parser.{MiniLispBaseVisitor, MiniLispLexer, MiniLispParser}
import org.antlr.v4.runtime.{
  BaseErrorListener,
  BailErrorStrategy,
  CharStreams,
  CommonTokenStream,
  RecognitionException,
  Recognizer
}
import org.antlr.v4.runtime.misc.ParseCancellationException

import scala.jdk.CollectionConverters.*

/** Thrown when MiniLisp source cannot be parsed (or uses syntax not yet in the grammar). */
final case class ParseError(message: String) extends RuntimeException(message)

/** Scala front-end for the MiniLisp parser: lexes source with ANTLR, walks the
  * generated parse tree, and produces the [[Expr]] AST consumed by [[Interpreter]].
  *
  * The grammar currently recognizes only integer literals, so every resulting
  * [[Expr]] is an [[Expr.IntLit]].
  */
object Parser extends MiniLispBaseVisitor[Expr]:

  /** Parse MiniLisp source into a program (a sequence of expressions). */
  def parse(source: String): Expr =
    val antlrParser = parserFor(source)
    try from(antlrParser.program())
    catch
      case e: ParseError                 => throw e
      case e: ParseCancellationException =>
        throw ParseError(Option(e.getCause).map(_.getMessage).getOrElse("parse failed"))

  /** Translate an already-built `program` parse tree. */
  def from(ctx: MiniLispParser.ProgramContext): Expr =
    ctx.expr().accept(this)

  override def visitExpr(ctx: MiniLispParser.ExprContext): Expr =
    if ctx.Integer() != null then Expr.IntLit(ctx.Integer().getText.toLong)
    else if ctx.Id() != null then Expr.Sym(ctx.Id().getText)
    else throw new IllegalArgumentException(s"Unknown expression: ${ctx.getText}")


  private def parserFor(source: String): MiniLispParser =
    val lexer = MiniLispLexer(CharStreams.fromString(source))
    lexer.removeErrorListeners()
    lexer.addErrorListener(ErrorListener)
    val antlrParser = MiniLispParser(CommonTokenStream(lexer))
    antlrParser.removeErrorListeners()
    antlrParser.addErrorListener(ErrorListener)
    antlrParser.setErrorHandler(BailErrorStrategy())
    antlrParser

  private object ErrorListener extends BaseErrorListener:
    override def syntaxError(
        recognizer: Recognizer[?, ?],
        offendingSymbol: Any,
        line: Int,
        charPositionInLine: Int,
        msg: String,
        e: RecognitionException
    ): Unit =
      throw ParseError(s"line $line:$charPositionInLine $msg")
