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
# 77 "parser.ml"
)
  | FLOATLIT of (
# 16 "parser.mly"
        float
# 82 "parser.ml"
)
  | STRINGLIT of (
# 17 "parser.mly"
        string
# 87 "parser.ml"
)
  | VARIABLE of (
# 18 "parser.mly"
        string
# 92 "parser.ml"
)

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
open Ast
# 99 "parser.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
  258 (* PLUS *);
  259 (* MINUS *);
  260 (* TIMES *);
  261 (* DIVIDE *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* LBRACK *);
  265 (* RBRACK *);
  266 (* LCURL *);
  267 (* RCURL *);
  268 (* COMMA *);
  269 (* EXPONENT *);
  270 (* FLOORDIVIDE *);
  271 (* MOD *);
  272 (* LSHIFT *);
  273 (* RSHIFT *);
  274 (* BITAND *);
  275 (* BITOR *);
  276 (* BITXOR *);
  277 (* BITNOT *);
  278 (* WALRUS *);
  279 (* GT *);
  280 (* LT *);
  281 (* GEQ *);
  282 (* LEQ *);
  283 (* EQ *);
  284 (* NEQ *);
  285 (* ASSIGN *);
  286 (* PLUSASSIGN *);
  287 (* MINUSASSIGN *);
  288 (* TIMESASSIGN *);
  289 (* DIVIDEASSIGN *);
  290 (* FLOORDIVASSIGN *);
  291 (* EXPASSIGN *);
  292 (* ANDASSIGN *);
  293 (* ORASSIGN *);
  294 (* XORASSIGN *);
  295 (* RSHIFTASSIGN *);
  296 (* LSHIFTASSIGN *);
  297 (* DEDENT *);
  298 (* OR *);
  299 (* AND *);
  300 (* FLOAT *);
  301 (* BOOL *);
  302 (* STR *);
  303 (* INT *);
  304 (* INDENT *);
  305 (* NONE *);
  306 (* FALSE *);
  307 (* TRUE *);
  308 (* CLASS *);
  309 (* INTERFACE *);
  310 (* FOR *);
  311 (* WHILE *);
  312 (* FROM *);
  313 (* DEL *);
  314 (* NOT *);
  315 (* IS *);
  316 (* IN *);
  317 (* PASS *);
  318 (* CONTINUE *);
  319 (* BREAK *);
  320 (* ELIF *);
  321 (* ELSE *);
  322 (* IF *);
  323 (* RETURN *);
  324 (* DEF *);
  325 (* COLON *);
  326 (* DOT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  327 (* INTLIT *);
  328 (* FLOATLIT *);
  329 (* STRINGLIT *);
  330 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\001\000\000\000"

let yylen = "\002\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\002\000"

let yydgoto = "\002\000\
\004\000"

let yysindex = "\255\255\
\001\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000"

let yytablesize = 1
let yytable = "\001\000\
\003\000"

let yycheck = "\001\000\
\000\000"

let yynames_const = "\
  NEWLINE\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  LCURL\000\
  RCURL\000\
  COMMA\000\
  EXPONENT\000\
  FLOORDIVIDE\000\
  MOD\000\
  LSHIFT\000\
  RSHIFT\000\
  BITAND\000\
  BITOR\000\
  BITXOR\000\
  BITNOT\000\
  WALRUS\000\
  GT\000\
  LT\000\
  GEQ\000\
  LEQ\000\
  EQ\000\
  NEQ\000\
  ASSIGN\000\
  PLUSASSIGN\000\
  MINUSASSIGN\000\
  TIMESASSIGN\000\
  DIVIDEASSIGN\000\
  FLOORDIVASSIGN\000\
  EXPASSIGN\000\
  ANDASSIGN\000\
  ORASSIGN\000\
  XORASSIGN\000\
  RSHIFTASSIGN\000\
  LSHIFTASSIGN\000\
  DEDENT\000\
  OR\000\
  AND\000\
  FLOAT\000\
  BOOL\000\
  STR\000\
  INT\000\
  INDENT\000\
  NONE\000\
  FALSE\000\
  TRUE\000\
  CLASS\000\
  INTERFACE\000\
  FOR\000\
  WHILE\000\
  FROM\000\
  DEL\000\
  NOT\000\
  IS\000\
  IN\000\
  PASS\000\
  CONTINUE\000\
  BREAK\000\
  ELIF\000\
  ELSE\000\
  IF\000\
  RETURN\000\
  DEF\000\
  COLON\000\
  DOT\000\
  EOF\000\
  "

let yynames_block = "\
  INTLIT\000\
  FLOATLIT\000\
  STRINGLIT\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
     ( Eof )
# 296 "parser.ml"
               : Ast.program))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
