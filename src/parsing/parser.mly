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
%token <bool> BOOLLIT

// %start program
// %type <Ast.program> program
%start program_rule
%type <Ast.program> program_rule

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
%left LPAREN RPAREN

(** List indexing? (identifier (for an int) | int) optional:, comma separated**)

%start program_rule
%type <Ast.program> program_rule
//%type <Ast.expr list> expr_list

%%

program_rule:
	| block_list EOF {}

block_list:
	/* nothing */ { [] }
	| block block_list {$1 :: $2}  

block:
	| declaration;
  	| assignment; 
	| class_definition
	| interface_definition
	| conditional
	| assert
	| while_loop
	| for_loop
	| BREAK
	| CONTINUE
	| PASS
	| function_definition
	| function_call

declaration:
	| identifier COLON identifier EQUALS expr
	| identifier COLON identifier

(* TODO: Implement the nonstandard assigns *)
assignment:
	| identifier ASSIGN expr {Assign {$1, $3}}
	| identifier PLUSASSIGN expr {Assign {$1, $3}}
	| identifier MINUSASSIGN expr {Assign {$1, $3}}
	| identifier TIMESASSIGN expr {Assign {$1, $3}}
	| identifier EXPASSIGN expr {Assign {$1, $3}}
	| identifier DIVIDEASSIGN expr {Assign {$1, $3}}
	| identifier FLOORDIVASSIGN expr {Assign {$1, $3}}
	| identifier MODASSIGN expr {Assign {$1, $3}}
	| identifier ANDASSIGN expr {Assign {$1, $3}}
	| identifier ORASSIGN expr {Assign {$1, $3}}
	| identifier XORASSIGN expr {Assign {$1, $3}}
	| identifier RSHIFTASSIGN expr {Assign {$1, $3}}
	| identifier LSHIFTASSIGN expr {Assign {$1, $3}}

class_definition:
	| CLASS identifier (identifier|ε): NEWLINE INDENT block* DEDENT
	| CLASS identifier (identifier|ε) implements (identifier+): NEWLINE INDENT block* DEDENT

interface_definition:
	INTERFACE identifier COLON NEWLINE INDENT func_header* DEDENT

func_header:
	| DEF identifier identifier LPAREN identifier COMMA * identifier RPAREN
	| DEF identifier identifier RPAREN LPAREN

conditional:
	| IF value COLON NEWLINE INDENT block+ DEDENT (else_block | ε)
	| IF value COLON NEWLINE INDENT block+ DEDENT elif_block (else_block | ε)

elif_block:
	| ELIF expr: NEWLINE INDENT block+ DEDENT elif_block* (else_block | ε)
else_block:
	| ELSE: NEWLINE INDENT block+ DEDENT

while_loop:
	| while expr: NEWLINE INDENT block+ DEDENT

for_loop:
	| for identifier in value NEWLINE INDENT block* DEDENT

function_definition:
	| DEF identifier LPAREN args RPAREN ARROW type COLON NEWLINE INDENT block+ RETURN value DEDENT

args:
	| ε
	| (expr COLON identifier COMMA )* expr COLON identifier

function_call:
	| identifier LPAREN args RPAREN

assert:
	| ASSERT LPAREN expr RPAREN { Assert ($3)}

//list Expressions

iterable:
	| list
	| dict

list:
	| list_literal
	| list_comprehension

list_literal:
LBRACK list_contents RBRACK
LBRACKist_contents:
R	| ε
	| (value COMMA)* + value

list_comprehension:
	| LBRACK value FOR identifier IN value IF value? RBRACK

//Dict 

dict:
	| dict_literal
	| dict_comprehension

dict_literal:
LBRACE dict_contents RBRACE
dict_contents:
	| ε
	| value COLON value,* value COLON expr
dict_comprehension:
	| LBRACK expr:value FOR identifier IN value (IF value)? RBRACK



LBRACK
R// program_rule:
//   vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} }
// x:int,y:int,z:int = 1,2,3?
// vdecl_list_rule:
//   /*nothing*/                   { []       }
//   | vdecl_rule vdecl_list_rule  { $1 :: $2 }

// vdecl_rule:
//   typ_rule ID SEMI { ($1, $2) }

// vdecl_rule:
// 	VARIABLE COLON typ_rule NEWLINE { ($3, $1) }


// typ_rule:
//   INT       { Int  }
//   | BOOL    { Bool }

// stmt_list_rule:
//     /* nothing */               { []     }
//     | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule NEWLINE                                        { Expr $1         }
//   | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }

expr_rule:
  | BOOLLIT                       { BoolLit $1            }
  | INTLIT                        { IntLit $1            }
  | VARIABLE                      { Id $1                 }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | VARIABLE ASSIGN expr_rule     { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }