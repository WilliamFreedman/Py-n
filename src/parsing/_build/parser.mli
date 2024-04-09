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
  | ARROW
  | LIST
  | DICT
  | IMPLEMENTS
  | EOF
  | BOOLLIT of (
# 15 "parser.mly"
        bool
# 83 "parser.mli"
)
  | INTLIT of (
# 16 "parser.mly"
        int
# 88 "parser.mli"
)
  | FLOATLIT of (
# 17 "parser.mly"
        float
# 93 "parser.mli"
)
  | STRINGLIT of (
# 18 "parser.mly"
        string
# 98 "parser.mli"
)
  | VARIABLE of (
# 20 "parser.mly"
        string
# 103 "parser.mli"
)

val program_rule :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
