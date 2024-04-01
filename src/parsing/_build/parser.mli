type token =
  | NEWLINE
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
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
  | INDENT
  | NONE
  | FALSE
  | TRUE
  | CLASS
  | INTERFACE
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
  | DOT
  | EOF
  | INTLIT of (
# 15 "parser.mly"
        int
# 77 "parser.mli"
)
  | FLOATLIT of (
# 16 "parser.mly"
        float
# 82 "parser.mli"
)
  | STRINGLIT of (
# 17 "parser.mly"
        string
# 87 "parser.mli"
)
  | VARIABLE of (
# 18 "parser.mly"
        string
# 92 "parser.mli"
)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
