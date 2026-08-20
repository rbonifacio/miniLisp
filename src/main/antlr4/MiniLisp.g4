grammar MiniLisp;

// ---------------------------------------------------------------------------
// EXERCISE: implement the grammar below.
//
// MiniLisp BNF (plain BNF, not ANTLR syntax — translate it into ANTLR rules):
//
//   <program> ::= <expr>*
//
//   <expr>    ::= <atom>
//               | "(" <expr>* ")"
//               | "'" <expr>
//
//   <atom>    ::= <int> | <float> | <string> | <bool> | <symbol>
//
//   <int>     ::= "-"? <digit>+
//   <float>   ::= "-"? <digit>+ "." <digit>+
//   <string>  ::= '"' (any character except '"', or an escaped '\' + character)* '"'
//   <bool>    ::= "#t" | "#f"
//   <symbol>  ::= <symbol-start> <symbol-char>*
//   <symbol-start> ::= letter | "_" | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "!" | "?"
//   <symbol-char>  ::= <symbol-start> | <digit>
//   <digit>   ::= "0" | "1" | ... | "9"
//
//   whitespace and ';'-to-end-of-line comments are insignificant (skipped).
// ---------------------------------------------------------------------------


program : expr* EOF ;

expr : ' ';
// passo 0: 
//   - especificar whitespaces and comments
//   - especificar digitos
//   - especificar Integer
//   
// em seguida, seguir com os alunos. 
