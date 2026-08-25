grammar MiniLisp;

// ---------------------------------------------------------------------------
// EXERCISE: implement the grammar below.
//
// MiniLisp BNF (plain BNF, not ANTLR syntax — translate it into ANTLR rules):
//
//   <program> ::= <decl>* <expr>*
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
//   <bool>    ::= "true" | "false"
//   <symbol>  ::= <symbol-start> <symbol-char>*
//   <symbol-start> ::= letter | "_" | "+" | "-" | "*" | "/" | "<" | ">" | "=" | "!" | "?"
//   <symbol-char>  ::= <symbol-start> | <digit>
//   <digit>   ::= "0" | "1" | ... | "9"
//
//   whitespace and ';'-to-end-of-line comments are insignificant (skipped).
// ---------------------------------------------------------------------------

program : decl* expr EOF ;

decl : 'define' '(' name = Id args = Id* ')' '(' body = expr ')' ;

expr : Integer
     | String
     | Boolean
     | Id
     ;

Integer : ('0' | '-'? ([1-9] DIGIT*)) ;
String : '"' ~["]* '"' ;
Boolean : 'True' | 'False' ;

Id : ALPHA ALPHA_NUM* ;

WS : [ \t\n\r]+ -> skip ;

COMMENT : ';' ~[\r\n]* -> skip ;

fragment ALPHA : ('a' .. 'z') | ('A' .. 'Z') ;

fragment ALPHA_NUM : ALPHA | DIGIT ;

fragment DIGIT : [0-9] ;
