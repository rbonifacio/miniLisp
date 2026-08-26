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

  override def visitAtomExpr(ctx: MiniLispParser.AtomExprContext): Expr =
    ctx.atom().accept(this)

  override def visitAtom(ctx: MiniLispParser.AtomContext): Expr =
    if ctx.Integer() != null then Expr.IntLit(ctx.Integer().getText.toLong)
    else if ctx.Float() != null then Expr.FloatLit(ctx.Float().getText.toDouble)
    else if ctx.String() != null then
      val text = ctx.String().getText
      Expr.StringLit(text.substring(1, text.length - 1))
    else if ctx.Boolean() != null then Expr.BoolLit(ctx.Boolean().getText == "True")
    else Expr.Symbol(ctx.Symbol().getText)

  override def visitBinExpr(ctx: MiniLispParser.BinExprContext): Expr =
    val op =
      if ctx.BinArithOpr() != null then ctx.BinArithOpr().getText
      else if ctx.BinRelOpr() != null then ctx.BinRelOpr().getText
      else ctx.MinusOpr().getText
    Expr.BinExpr(op, ctx.expr(0).accept(this), ctx.expr(1).accept(this))

  override def visitNegExpr(ctx: MiniLispParser.NegExprContext): Expr =
    Expr.NegExpr(ctx.expr().accept(this))

  override def visitLetExpr(ctx: MiniLispParser.LetExprContext): Expr =
    Expr.LetExpr(ctx.Symbol().getText, ctx.expr(0).accept(this), ctx.expr(1).accept(this))

  override def visitIfThenElseExpr(ctx: MiniLispParser.IfThenElseExprContext): Expr =
    Expr.IfExpr(ctx.expr(0).accept(this), ctx.expr(1).accept(this), ctx.expr(2).accept(this))

  override def visitListOfExpr(ctx: MiniLispParser.ListOfExprContext): Expr =
    Expr.SList(ctx.expr().asScala.map(_.accept(this)).toList)


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
