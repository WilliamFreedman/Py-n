type token =
  | NEWLINE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | DOT
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LCURL
  | RCURL
  | COMMA
  | EXPONENT
  | FLOORDIVIDE
  | MOD
  | LSHIFT
  | RSHIFT
  | BITAND
  | BITOR
  | BITXOR
  | BITNOT
  | WALRUS
  | GT
  | LT
  | GEQ
  | LEQ
  | EQ
  | NEQ
  | ASSIGN
  | PLUSASSIGN
  | MINUSASSIGN
  | TIMESASSIGN
  | DIVIDEASSIGN
  | MODASSIGN
  | FLOORDIVASSIGN
  | EXPASSIGN
  | ANDASSIGN
  | ORASSIGN
  | XORASSIGN
  | RSHIFTASSIGN
  | LSHIFTASSIGN
  | DEDENT
  | OR
  | AND
  | FLOAT
  | BOOL
  | STR
  | INT
  | VOID
  | INDENT
  | NONE
  | FALSE
  | TRUE
  | FOR
  | WHILE
  | FROM
  | DEL
  | NOT
  | IS
  | IN
  | PASS
  | CONTINUE
  | BREAK
  | ELIF
  | ELSE
  | IF
  | RETURN
  | DEF
  | COLON
  | ARROW
  | LIST
  | DICT
  | IMPLEMENTS
  | EOF
  | BOOLLIT of (bool)
  | INTLIT of (int)
  | FLOATLIT of (float)
  | STRINGLIT of (string)
  | VARIABLE of (string)

val program_rule :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
