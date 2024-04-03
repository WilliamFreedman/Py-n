%{
open Ast
%}

%token NEWLINE PLUS MINUS TIMES DIVIDE DOT LPAREN RPAREN LBRACK RBRACK LCURL RCURL COMMA
%token EXPONENT FLOORDIVIDE MOD LSHIFT RSHIFT BITAND BITOR BITXOR BITNOT WALRUS

%token GT LT GEQ LEQ EQ NEQ ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN
%token FLOORDIVASSIGN EXPASSIGN ANDASSIGN ORASSIGN XORASSIGN RSHIFTASSIGN LSHIFTASSIGN 

%token DEDENT OR AND FLOAT BOOL STR INT INDENT NONE FALSE TRUE CLASS INTERFACE FOR WHILE 
%token FROM DEL NOT IS IN PASS CONTINUE BREAK ELIF ELSE IF RETURN DEF COLON ARROW LIST DICT
%token EOF

%token <bool> BOOLLIT
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT

%token <string> VARIABLE
%token <string> TYPENAME
%token <string> FUNCNAME

// %start program
// %type <Ast.program> program
%start program_rule
%type <Ast.program> program_rule

(* https://www.geeksforgeeks.org/precedence-and-associativity-of-operators-in-python/ *)
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN FLOORDIVASSIGN EXPASSIGN ANDASSIGN ORASSIGN XORASSIGN RSHIFTASSIGN LSHIFTASSIGN MODASSIGN 
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

// identifier LBRACKET expr RBRACKET
// ArrayGet identifier expr

program_rule:
    block_list EOF { {body=$1}  }

block_list:
	/* nothing */               { [] }
	| block NEWLINE block_list  { $1 :: $3 }  

block:
    declaration     { $1 }
  	| assignment    { $1 }
	(** | class_definition {$1} **)
	(** | interface_definition {$1} **)
	(** | conditional {$1} **)
	(** | assert {$1} **)
	(** | while_loop {$1} **)
	(** | for_loop {$1} **)
	(** | BREAK { Break } **)
	(** | CONTINUE { Continue } **)
	(** | PASS { Pass } **)
	(** | function_definition {$1} **)
	(** | function_call {$1} **)

declaration:
    VARIABLE COLON typename ASSIGN expr { VarDec($3, Var($1), $5) } 
	(*| VARIABLE COLON VARIABLE { VarDec(TypeVariable($3), Var($1)) }*)
	
typename:
	INT	{ TypeVariable("int") }
	| BOOL { TypeVariable("bool") }
	| FLOAT { TypeVariable("float") }
	| STR { TypeVariable("str") }
  	| VARIABLE { TypeVariable($1) }
	| LIST LBRACK typename RBRACK { List($3) }
	| DICT LBRACK typename COMMA typename RBRACK { Dict($3, $5) }

assignment:
    VARIABLE ASSIGN expr    		{ BlockAssign(Var($1), IdentityAssign, $3) }
	| VARIABLE PLUSASSIGN expr 		{ BlockAssign(Var($1), PlusAssign,     $3) }
	| VARIABLE MINUSASSIGN expr 	{ BlockAssign(Var($1), MinusAssign,    $3) }
	| VARIABLE TIMESASSIGN expr 	{ BlockAssign(Var($1), TimesAssign,    $3) }
	| VARIABLE EXPASSIGN expr 		{ BlockAssign(Var($1), ExpAssign,      $3) }
	| VARIABLE DIVIDEASSIGN expr 	{ BlockAssign(Var($1), DivideAssign,   $3) }
	| VARIABLE FLOORDIVASSIGN expr 	{ BlockAssign(Var($1), FloorDivAssign,  $3) }
	| VARIABLE MODASSIGN expr 		{ BlockAssign(Var($1), ModAssign,      $3) }
	| VARIABLE ANDASSIGN expr 		{ BlockAssign(Var($1), AndAssign,      $3) }
	| VARIABLE ORASSIGN expr 		{ BlockAssign(Var($1), OrAssign,       $3) }
	| VARIABLE XORASSIGN expr 		{ BlockAssign(Var($1), XorAssign,      $3) }
	| VARIABLE RSHIFTASSIGN expr 	{ BlockAssign(Var($1), RShiftAssign,   $3) }
	| VARIABLE LSHIFTASSIGN expr 	{ BlockAssign(Var($1), LShiftAssign,   $3) }

expr:
	 BOOLLIT                        { BoolLit($1)           }
    | INTLIT                        { IntLit($1)            }
    | FLOATLIT                      { FloatLit($1)          }
    | STRINGLIT                     { StringLit($1)         }
    | VARIABLE                      { VarExpr(Var($1))      }
	| list							{ $1 }    
	| dict							{ $1 }
	| expr PLUS expr                { Binop($1, Add, $3)    }
    | expr MINUS expr               { Binop($1, Sub, $3)    }
    | expr EQ expr                  { Binop($1, Eq, $3)  	}
    | expr NEQ expr                 { Binop($1, Neq, $3)    }
    | expr LT expr                  { Binop($1, Less, $3)   }
    | expr AND expr                 { Binop($1, And, $3)    }
    | expr OR expr                  { Binop($1, Or, $3)     }
    | VARIABLE ASSIGN expr          { Assign(Var($1), IdentityAssign, $3)  }
    | LPAREN expr RPAREN            { $2                    }

list:
	| list_literal {$1}
	// | list_comprehension


list_literal:
	LBRACK list_contents RBRACK { List($2)}

list_contents:
	/* nothing */               { [] }
	| expr                 { [$1] }
	| expr COMMA list_contents  { $1 :: $3 }

dict:
	| dict_literal {$1}
	// | dict_comprehension


dict_literal:
	LCURL dict_contents RCURL { Dict($2)}

dict_contents:
	/* nothing */               { [] }
	| dict_element               { [$1]  }
	| dict_element COMMA dict_contents  { $1 :: $3 }

dict_element:
	expr COLON expr                { ($1, $3)  }


// while_loop:
// 	WHILE expr COLON NEWLINE INDENT block_list DEDENT { While($2, $6) }

(** TODO: Update AST to implement actions for class definitions **)
(**
class_definition:
    CLASS identifier LPAREN identifier RPAREN COLON NEWLINE INDENT block_list DEDENT
	| CLASS identifier LPAREN RPAREN COLON NEWLINE INDENT block_list DEDENT
	| CLASS identifier LPAREN identifier RPAREN implements LPAREN identifier_list RPAREN COLON NEWLINE INDENT block_list DEDENT
	| CLASS identifier LPAREN RPAREN implements LPAREN identifier_list RPAREN COLON NEWLINE INDENT block_list DEDENT
    **)

(**
identifier_list:
	identifier { $1 }
	|identifier COMMA identifier_list { $1 :: $3}

interface_definition:
	INTERFACE identifier COLON NEWLINE INDENT func_header_list DEDENT
    **)

(** TODO: Update AST to implement actions for function headers **)
    (**
func_header_list:
	func_header
	| func_header_list NEWLINE func_header

func_header:
	| DEF identifier identifier LPAREN args_list RPAREN ARROW IDENTIFIER
	| DEF identifier identifier RPAREN LPAREN
    **)

   (** TODO: Update AST to implement actions for conditionals and loops **)
   (**
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




(* Value doesn't exist right now *)
for_loop:
	| FOR identifier IN value NEWLINE INDENT block_list DEDENT {For {$2, $4, $7}}
    **)

(** TODO: Update AST to implement actions for function definitions **)
(**
function_definition:
	| DEF identifier LPAREN args RPAREN ARROW type COLON NEWLINE INDENT block_list RETURN value DEDENT
    **)

(** TODO: Update AST to implement actions for arguments **)
    (**
args:
	| LPAREN args_list RPAREN

args_list:
	arg COMMA args_list
	| arg

arg:
	identifier COLON identifier
**)

(**
function_call:
	| identifier LPAREN args RPAREN

assert:
	| ASSERT LPAREN expr RPAREN { Assert ($3)}

list Expressions
**)

(**
iterable:
	| list
	| dict

list_comprehension:
	LBRACK value FOR identifier IN value RBRACK
	|LBRACK value FOR identifier IN value IF value RBRACK
**)

(**
//Dict 

dict:
	| dict_literal
	| dict_comprehension

dict_literal:
	LCURL dict_contents RCURL

dict_contents:
	| ε
	| value COLON value, *value COLON expr
dict_comprehension:
	| LBRACK expr:value FOR identifier IN value (IF value)? RBRACK
**)

(**
stmt_rule:
    expr_rule NEWLINE                                        { Expr $1         }
    | LBRACE stmt_list_rule RBRACE                          { Block $2        }
    | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
    | WHILE LPAREN expr_rule RPAREN stmt_rule               { While ($3,$5)   }
**) 


