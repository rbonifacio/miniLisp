# miniLisp

A small Lisp interpreter written in Scala 3. This is teaching material for a course
on parser generators (ANTLR 4): the interpreter (AST, evaluator, builtins) is fully
implemented and unit-tested, but **the grammar itself is the exercise** —
`MiniLisp.g4` currently holds only a plain-BNF comment and a placeholder rule.

## Layout

```
src/main/antlr4/MiniLisp.g4                                   BNF comment + placeholder rule — write the real grammar here
src/main/scala/br/ufpe/cin/minilisp/Ast.scala                 the AST the interpreter evaluates
src/main/scala/br/ufpe/cin/minilisp/Values.scala               runtime Value type + Env (lexical scoping)
src/main/scala/br/ufpe/cin/minilisp/Interpreter.scala          tree-walking evaluator + special forms
src/main/scala/br/ufpe/cin/minilisp/Builtins.scala             builtin procedures (+, car, cons, display, ...)
src/main/scala/br/ufpe/cin/minilisp/Main.scala                 demo: hand-builds an Expr tree and evaluates it
src/test/scala/br/ufpe/cin/minilisp/InterpreterSpec.scala      interpreter tests, built directly on Expr trees
examples/factorial.lisp                                        sample MiniLisp source, for testing the grammar once it parses
```

Base package: `br.ufpe.cin.minilisp` (Centro de Informática, UFPE). The ANTLR-generated
lexer/parser lands in `br.ufpe.cin.minilisp.parser`.

## Requirements

JDK 11+ (a `.sdkmanrc` pinning JDK 17 is included; run `sdk env` if you use sdkman).

## Usage

```
sbt compile   # generates a lexer/parser from MiniLisp.g4 (currently just the placeholder rule)
sbt run       # runs Main's hand-built (fact 5) demo — no parsing involved
sbt test      # runs the interpreter test suite (Expr trees built by hand)
```

## The exercise

`MiniLisp.g4` contains the MiniLisp grammar written as plain BNF, in a comment — not ANTLR
syntax. The task is two-fold:

1. Translate that BNF into real ANTLR parser/lexer rules (replacing the `start: EOF ;`
   placeholder), so `MiniLispParser.program()` can parse the files in `examples/`.
2. Write an `AstBuilder` that walks the resulting parse tree (a `MiniLispVisitor`/
   `MiniLispBaseVisitor` is generated automatically since `antlr4GenVisitor` is on) and
   produces a `List[Expr]` — the same AST shape `InterpreterSpec` already builds
   by hand.

Once both exist, `Main` and a REPL can read real `.lisp` source instead of hard-coded trees.

## Language subset (already implemented in the interpreter)

- Types: integers, floats, strings, booleans (`#t` / `#f`), symbols, lists
- Special forms: `quote` (`'x`), `if`, `define` (value and function sugar), `lambda`,
  `let`, `let*`, `cond`, `begin`, `and`, `or`, `set!`
- Builtins: `+ - * / modulo`, `= < > <= >=`, `cons car cdr list length append`,
  `null? pair? not eq? equal? number? string? symbol? procedure?`, `display print newline`

Not implemented: tail-call optimization, macros, vectors, continuations, the full numeric tower.
