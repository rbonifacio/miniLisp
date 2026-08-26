grammar MiniLisp;

program : decl* expr EOF ;

// A single REPL entry: either a declaration to add to the session, or an
// expression to evaluate. Unlike `program`, a declaration may stand alone.
replEntry : decl EOF   # ReplDecl
          | expr EOF   # ReplExpr
          ;

decl : 'define' '(' name = Symbol params += Symbol* ')' '(' body = expr ')' ;

expr : atom                                                  # AtomExpr
     | '(' (BinArithOp | BinRelOp | MinusOp) lhs = expr rhs = expr ')'    # BinExpr
     | '(' MinusOp operand = expr ')'                        # NegExpr
     | '(' 'not' operand = expr ')'                          # NotExpr
     | '(' 'let' '(' name = Symbol init = expr ')' body = expr ')'        # LetExpr
     | '(' 'if' cond = expr thenBranch = expr elseBranch = expr ')'       # IfExpr
     | '(' expr* ')'                                         # ListOfExpr
     ;

atom : Integer
     | Float
     | String
     | Boolean
     | Symbol
     ;

MinusOp : '-' ;
BinArithOp : ('+' | '*' | '/') ;
BinRelOp : ('>' | '<' | '=' | '>=' | '<=' | '/=');

Integer : '-'? DIGIT+ ;
Float : '-'? DIGIT+ '.' DIGIT+ ;
String : '"' ~["]* '"' ;
Boolean : 'True' | 'False' ;

Symbol : ALPHA ALPHA_NUM* ;

WS : [ \t\n\r]+ -> skip ;

COMMENT : '//' ~[\r\n]* -> skip ;

fragment ALPHA : ('a' .. 'z') | ('A' .. 'Z') ;

fragment ALPHA_NUM : ALPHA | DIGIT ;

fragment DIGIT : [0-9] ;
