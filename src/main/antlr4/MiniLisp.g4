grammar MiniLisp;

program : decl* expr EOF ;

decl : 'define' '(' name = Symbol args = Symbol* ')' '(' body = expr ')' ;

expr : atom                                                # AtomExpr
     | '(' (BinArithOpr | BinRelOpr | MinusOpr) expr expr ')' # BinExpr
     | '(' MinusOpr expr ')'                                # NegExpr
     | '(' 'let' '(' Symbol expr ')' expr ')'                # LetExpr
     | '(' 'if' expr expr expr ')'                           # IfThenElseExpr
     | '(' expr* ')'                                         # ListOfExpr
     ;

atom : Integer
     | Float
     | String
     | Boolean
     | Symbol
     ;

MinusOpr : '-' ;
BinArithOpr : ('+' | '*' | '/') ;
BinRelOpr : ('>' | '<' | '=' | '>=' | '<=' | '/=');

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
