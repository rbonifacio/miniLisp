package br.ufpe.cin.minilisp

import org.jline.reader.{Candidate, Completer, EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.reader.impl.DefaultParser
import org.jline.terminal.TerminalBuilder

import java.nio.file.Paths
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Entry point for MiniLisp.
  *
  *   - `run <file.mlisp>` evaluates a whole program: the leading `define`
  *     declarations followed by the trailing expression.
  *   - with no argument, starts an interactive session where declarations
  *     accumulate and expressions are evaluated as they are entered.
  */
object Main:

  private val Prompt = "minilisp> "
  private val Continuation = "      ... "

  def main(args: Array[String]): Unit =
    args.toList match
      case Nil                            => repl()
      case "-h" :: _ | "--help" :: _      => println(usage)
      case path :: Nil                    => runFile(path)
      case _                              =>
        Console.err.println(usage)
        sys.exit(2)

  private def usage: String =
    """usage: minilisp [file.mlisp]
      |
      |  with a file    evaluate the program and print its result
      |  with no file   start an interactive session""".stripMargin

  // -- running a file ---------------------------------------------------------

  private def runFile(path: String): Unit =
    val source =
      try Using.resource(Source.fromFile(path))(_.mkString)
      catch
        case e: java.io.IOException =>
          Console.err.println(s"cannot read $path: ${e.getMessage}")
          sys.exit(1)

    try println(Interpreter.evalProgram(Parser.parseProgram(source)).render)
    catch
      case e: ParseError =>
        Console.err.println(s"parse error: ${e.message}")
        sys.exit(1)
      case e: EvalError =>
        Console.err.println(s"evaluation error: ${e.message}")
        sys.exit(1)

  // -- interactive session ------------------------------------------------------

  /** Syntax that is always available, offered alongside whatever the
    * session has defined so far.
    */
  private val Keywords =
    List("define", "let", "if", "not", "True", "False", ":help", ":quit")

  private def repl(): Unit =
    val env = new Env()
    val terminal = TerminalBuilder.builder().dumb(true).build()

    // Leaving a bracket or quote open makes JLine ask for another line
    // rather than handing us an incomplete entry.
    val parser = DefaultParser()
    parser.setEofOnUnclosedBracket(DefaultParser.Bracket.ROUND)
    parser.setEofOnUnclosedQuote(true)
    parser.setLineCommentDelims(Array("//"))
    parser.setEscapeChars(null)

    val completer: Completer = (_, line, candidates) =>
      val word = line.word()
      (Keywords ++ env.names)
        .filter(_.startsWith(word))
        .foreach(n => candidates.add(Candidate(n)))

    val reader = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .parser(parser)
      .completer(completer)
      .variable(LineReader.SECONDARY_PROMPT_PATTERN, Continuation)
      .variable(LineReader.HISTORY_FILE, Paths.get(sys.props("user.home"), ".minilisp_history"))
      .build()

    terminal.writer().println("MiniLisp -- :help for commands, :quit to exit")

    var running = true
    while running do
      try
        reader.readLine(Prompt) match
          case ":quit" | ":q"            => running = false
          case ":help"                   => terminal.writer().println(replHelp)
          case entry if hasNoCode(entry) => ()
          case entry                     => evalEntry(entry, env)
      catch
        case _: UserInterruptException => ()          // Ctrl-C: abandon the line
        case _: EndOfFileException     => running = false // Ctrl-D: exit

    reader.getHistory.save()
    terminal.close()

  private def replHelp: String =
    """  <expr>              evaluate an expression, e.g. (+ 1 2)
      |  define (f x) (...)  add a declaration to the session
      |  :help               show this message
      |  :quit, :q           exit (or Ctrl-D)
      |
      |  Tab completes names, up/down browses history, and an unclosed
      |  parenthesis continues onto the next line.""".stripMargin

  /** True when an entry holds nothing to evaluate -- it is empty, or every
    * line of it is a `//` comment.
    */
  private def hasNoCode(entry: String): Boolean =
    entry.linesIterator.forall(line => line.isBlank || line.trim.startsWith("//"))

  private def evalEntry(entry: String, env: Env): Unit =
    try
      Parser.parseReplEntry(entry) match
        case Left(decl) =>
          env.define(decl.name, decl)
          println(s"defined ${decl.name}")
        case Right(expr) =>
          println(Interpreter.eval(expr, env).render)
    catch
      case e: ParseError => Console.err.println(s"parse error: ${e.message}")
      case e: EvalError  => Console.err.println(s"evaluation error: ${e.message}")
