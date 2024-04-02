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

// idenitifier LBRACKET expr RBRACKET
// ArrayGet identifier expr

program_rule:
	| block_list EOF {}

block_list:
	/* nothing */ { [] }
	| block NEWLINE block_list {$1 :: $2}  

block:
	| declaration {$1}
  	| assignment {$1}
	| class_definition {$1}
	| interface_definition {$1}
	| conditional {$1}
	| assert {$1}
	| while_loop {$1}
	| for_loop {$1}
	| BREAK { Break }
	| CONTINUE { Continue }
	| PASS { Pass }
	| function_definition {$1}
	| function_call {$1}

declaration:
	| identifier COLON identifier EQUALS expr { VarAssign{$3, $1, $5} } 
	| identifier COLON identifier { VarDec{$3, $1}}

(* TODO: Implement the nonstandard assigns probably later on *)
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
	| CLASS identifier LPAREN identifier RPAREN COLON NEWLINE INDENT block_list DEDENT
	| CLASS identifier LPAREN RPAREN COLON NEWLINE INDENT block_list DEDENT
	| CLASS identifier LPAREN identifier RPAREN implements LPAREN identifier_list RPAREN COLON NEWLINE INDENT block_list DEDENT
	| CLASS identifier LPAREN RPAREN implements LPAREN identifier_list RPAREN COLON NEWLINE INDENT block_list DEDENT

identifier_list:
	identifier { $1 }
	|identifier COMMA identifier_list { $1 :: $3}

interface_definition:
	INTERFACE identifier COLON NEWLINE INDENT func_header_list DEDENT

func_header_list:
	func_header
	| func_header_list NEWLINE func_header

func_header:
	| DEF identifier identifier LPAREN args_list RPAREN ARROW IDENTIFIER
	| DEF identifier identifier RPAREN LPAREN

conditional:
	| IF value COLON NEWLINE INDENT block_list DEDENT else_block
	| IF value COLON NEWLINE INDENT block_list DEDENT
	| IF value COLON NEWLINE INDENT block_list DEDENT elif_block else_block
	| IF value COLON NEWLINE INDENT block_list DEDENT elif_block


elif_block:
	ELIF expr COLON NEWLINE INDENT block_list DEDENT elif_block else_block
	| ELIF expr COLON NEWLINE INDENT block_list DEDENT else_block
	| ELIF expr COLON NEWLINE INDENT block_list DEDENT elif_block
	| ELIF expr COLON NEWLINE INDENT block_list DEDENT

else_block:
	| ELSE COLON NEWLINE INDENT block_list DEDENT {Else {}}

while_loop:
	| WHILE expr COLON NEWLINE INDENT block_list DEDENT {While {$2, $6} }

(* Value doesn't exist right now *)
for_loop:
	| FOR identifier IN value NEWLINE INDENT block_list DEDENT {For {$2, $4, $7}}

function_definition:
	| DEF identifier LPAREN args RPAREN ARROW type COLON NEWLINE INDENT block_list RETURN value DEDENT

args:
	| LPAREN args_list RPAREN

args_list:
	arg COMMA args_list
	| arg

arg:
	identifier COLON identifier

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

list_contents:
	| value
	| list_contents COMMA value

list_comprehension:
	LBRACK value FOR identifier IN value RBRACK
	|LBRACK value FOR identifier IN value IF value RBRACK

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


// stmt_rule:
//   expr_rule NEWLINE                                        { Expr $1         }
// //   | LBRACE stmt_list_rule RBRACE                          { Block $2        }
//   | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
//   | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }

// expr_rule:
//   | BOOLLIT                       { BoolLit $1            }
//   | INTLIT                        { IntLit $1            }
//   | VARIABLE                      { Id $1                 }
//   | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
//   | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
//   | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
//   | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
//   | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
//   | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
//   | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
//   | VARIABLE ASSIGN expr_rule     { Assign ($1, $3)       }
//   | LPAREN expr_rule RPAREN       { $2                    }