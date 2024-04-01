%{
open Ast
%}

%token NEWLINE PLUS MINUS TIMES DIVIDE LPAREN RPAREN LBRACK RBRACK LCURL RCURL COMMA
%token EXPONENT FLOORDIVIDE MOD LSHIFT RSHIFT BITAND BITOR BITXOR BITNOT WALRUS

%token GT LT GEQ LEQ EQ NEQ ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN
%token FLOORDIVASSIGN EXPASSIGN ANDASSIGN ORASSIGN XORASSIGN RSHIFTASSIGN LSHIFTASSIGN 

%token DEDENT OR AND FLOAT BOOL STR INT INDENT NONE FALSE TRUE CLASS INTERFACE FOR WHILE 
%token FROM DEL NOT IS IN PASS CONTINUE BREAK ELIF ELSE IF RETURN DEF COLON DOT
%token EOF

%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT
%token <string> VARIABLE

(* https://www.geeksforgeeks.org/precedence-and-associativity-of-operators-in-python/ *)
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN FLOORDIVASSIGN EXPASSIGN ANDASSIGN ORASSIGN XORASSIGN RSHIFTASSIGN LSHIFTASSIGN 
%right IF ELIF ELSE
%left OR
%left AND
%left NOT
%left GT LT GEQ LEQ EQ NEQ
%left BITOR
%left BITXOR
%left BITAND
%left LSHIFT RSHIFT
%left PLUS MINUS
%left TIMES DIVIDE FLOORDIVIDE MOD
%right EXPONENT
%left OPENPARAM CLOSEPARAM

(** List indexing? (identifier (for an int) | int) optional:, comma separated**)

%start program
%type <Ast.program> program
//%type <Ast.expr list> expr_list

%%

program:
	EOF { Eof }