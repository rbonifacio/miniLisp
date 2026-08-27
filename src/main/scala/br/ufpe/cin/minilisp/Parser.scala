package br.ufpe.cin.minilisp

import br.ufpe.cin.minilisp.parser.{MiniLispBaseVisitor, MiniLispLexer, MiniLispParser}
import org.antlr.v4.runtime.{
  BaseErrorListener,
  BailErrorStrategy,
  CharStreams,
  CommonTokenStream,
  RecognitionException,
  Recognizer,
  Token
}
import org.antlr.v4.runtime.misc.ParseCancellationException

import scala.jdk.CollectionConverters.*

/** Thrown when MiniLisp source cannot be parsed (or uses syntax not yet in the grammar). */
final case class ParseError(message: String) extends RuntimeException(message)

/** Scala front-end for the MiniLisp parser: lexes source with ANTLR, walks the
  * generated parse tree, and produces the [[Expr]] AST consumed by [[Interpreter]].
  */
object Parser extends MiniLispBaseVisitor[Expr]:

  /** Parse MiniLisp source into its trailing expression, discarding any
    * leading declarations. Use [[parseProgram]] to keep them.
    */
  def parse(source: String): Expr =
    withParseErrors(source)(from)

  /** Parse MiniLisp source into a complete [[Program]]: the leading
    * declarations plus the trailing expression they are in scope for.
    */
  def parseProgram(source: String): Program =
    withParseErrors(source)(programFrom)

  /** Translate an already-built `program` parse tree, keeping only its
    * trailing expression.
    */
  def from(ctx: MiniLispParser.ProgramContext): Expr =
    ctx.expr().accept(this)

  /** Translate an already-built `program` parse tree in full. */
  def programFrom(ctx: MiniLispParser.ProgramContext): Program =
    Program(ctx.decl().asScala.map(declFrom).toList, ctx.expr().accept(this))

  private def declFrom(ctx: MiniLispParser.DeclContext): Decl =
    Decl(
      ctx.name.getText,
      ctx.params.asScala.map(_.getText).toList,
      ctx.body.accept(this)
    )

  /** Parse a single REPL entry: either a declaration to add to the session
    * (`Left`) or an expression to evaluate (`Right`).
    */
  def parseReplEntry(source: String): Either[Decl, Expr] =
    val antlrParser = parserFor(source)
    try
      antlrParser.replEntry() match
        case ctx: MiniLispParser.ReplDeclContext => Left(declFrom(ctx.decl()))
        case ctx: MiniLispParser.ReplExprContext => Right(ctx.expr().accept(this))
        case other => throw ParseError(s"unrecognized input: ${other.getText}")
    catch
      case e: ParseError                 => throw e
      case e: ParseCancellationException => throw parseFailure(e)

  private def withParseErrors[A](source: String)(f: MiniLispParser.ProgramContext => A): A =
    val antlrParser = parserFor(source)
    try f(antlrParser.program())
    catch
      case e: ParseError                 => throw e
      case e: ParseCancellationException => throw parseFailure(e)

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
      if ctx.BinArithOp() != null then ctx.BinArithOp().getText
      else if ctx.BinRelOp() != null then ctx.BinRelOp().getText
      else ctx.MinusOp().getText
    Expr.BinExpr(op, ctx.lhs.accept(this), ctx.rhs.accept(this))

  override def visitNegExpr(ctx: MiniLispParser.NegExprContext): Expr =
    Expr.NegExpr(ctx.operand.accept(this))

  override def visitNotExpr(ctx: MiniLispParser.NotExprContext): Expr =
    Expr.NotExpr(ctx.operand.accept(this))

  override def visitLetExpr(ctx: MiniLispParser.LetExprContext): Expr =
    Expr.LetExpr(ctx.name.getText, ctx.init.accept(this), ctx.body.accept(this))

  override def visitIfExpr(ctx: MiniLispParser.IfExprContext): Expr =
    Expr.IfExpr(ctx.cond.accept(this), ctx.thenBranch.accept(this), ctx.elseBranch.accept(this))

  override def visitListOfExpr(ctx: MiniLispParser.ListOfExprContext): Expr =
    Expr.SList(ctx.expr().asScala.map(_.accept(this)).toList)


  /** Turns ANTLR's bail-out into a [[ParseError]] carrying a source location.
    *
    * Which path an error takes depends on where it is detected.  A failure
    * inside a rule's generated `catch` goes through `reportError` first, so
    * [[ErrorListener]] runs and already raises a located [[ParseError]].  A
    * token mismatch inside `match()` instead reaches
    * `BailErrorStrategy.recoverInline`, which throws without reporting, so
    * the listener never sees it and the wrapped `RecognitionException`
    * carries no message.  This recovers the location from the offending
    * token for that second path.
    */
  private def parseFailure(e: ParseCancellationException): ParseError =
    val located = e.getCause match
      case r: RecognitionException =>
        Option(r.getOffendingToken).map { tok =>
          val what =
            if tok.getType == Token.EOF then "end of input" else s"'${tok.getText}'"
          s"line ${tok.getLine}:${tok.getCharPositionInLine} unexpected $what"
        }
      case _ => None

    // Note the flatMap: `Option(cause).map(_.getMessage)` yields Some(null)
    // when the cause has no message, and `getOrElse` then never fires --
    // which is how this used to surface as "parse error: null".
    val fromCause = Option(e.getCause).flatMap(c => Option(c.getMessage))

    ParseError(located.orElse(fromCause).getOrElse("parse failed"))

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
