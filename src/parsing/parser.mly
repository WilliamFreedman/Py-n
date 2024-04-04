%{
open Ast
%}

%token NEWLINE PLUS MINUS TIMES DIVIDE DOT LPAREN RPAREN LBRACK RBRACK LCURL RCURL COMMA
%token EXPONENT FLOORDIVIDE MOD LSHIFT RSHIFT BITAND BITOR BITXOR BITNOT WALRUS

%token GT LT GEQ LEQ EQ NEQ ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN
%token FLOORDIVASSIGN EXPASSIGN ANDASSIGN ORASSIGN XORASSIGN RSHIFTASSIGN LSHIFTASSIGN 

%token DEDENT OR AND FLOAT BOOL STR INT VOID INDENT NONE FALSE TRUE CLASS INTERFACE FOR WHILE 
%token FROM DEL NOT IS IN PASS CONTINUE BREAK ELIF ELSE IF RETURN DEF COLON ARROW LIST DICT IMPLEMENTS
%token EOF

%token <bool> BOOLLIT
%token <int> INTLIT
%token <float> FLOATLIT
%token <string> STRINGLIT

%token <string> VARIABLE

%start program_rule
%type <Ast.program> program_rule

(* https://www.geeksforgeeks.org/precedence-and-associativity-of-operators-in-python/ *)
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN FLOORDIVASSIGN EXPASSIGN ANDASSIGN ORASSIGN XORASSIGN RSHIFTASSIGN LSHIFTASSIGN MODASSIGN 
%right IF ELIF ELSE
%right WALRUS
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

%%

program_rule:
    block_list EOF { { body = $1 } }

block_list:
	/* nothing */                       { []        }
	| block newline_list block_list     { $1 :: $3  }  

newline_list:
	NEWLINE {}
	//| newline_list NEWLINE {}

block:
    declaration             { $1        }
  	| assignment            { $1        }
	| interface_definition  { $1        }
	| while_loop            { $1        }
	| BREAK                 { Break     }
	| CONTINUE              { Continue  }
	| PASS                  { Pass      }
	| function_definition   { $1        }
 	| function_block_call   { $1        }
	| return_exit           { $1        }
	| return_val            { $1        }
	| class_definition      { $1        } 
	| conditional           { $1        }
	| for_loop              { $1        }
	// | expr {$1} should this be allowed?

return_exit: 
	RETURN { ReturnVoid }

return_val:
	RETURN expr { ReturnVal($2) }

declaration:
    VARIABLE COLON typename ASSIGN expr { VarDec($3, Var($1), $5) } 
	(*| VARIABLE COLON VARIABLE { VarDec(TypeVariable($3), Var($1)) }*)

typename:
	INT	                                            { TypeVariable("int")   }
	| BOOL                                          { TypeVariable("bool")  }
	| FLOAT                                         { TypeVariable("float") }
	| STR                                           { TypeVariable("str")   }
	| VOID                                          { TypeVariable("void")  }
  	| VARIABLE                                      { TypeVariable($1)      }
	| LIST LBRACK typename RBRACK                   { List($3)              }
	| DICT LBRACK typename COMMA typename RBRACK    { Dict($3, $5)          }

(** VARIABLE: Just a string, variable: string.string string[blah blah] **)
variable:
    VARIABLE                        { Var($1)           }
    | variable DOT variable         { VarDot($1, $3)    }
    | variable LBRACK expr RBRACK   { VarIndex($1, $3)  }

assignment:
    variable ASSIGN expr            { BlockAssign($1, IdentityAssign, $3) }
	| variable PLUSASSIGN expr      { BlockAssign($1, PlusAssign,     $3) }
	| variable MINUSASSIGN expr     { BlockAssign($1, MinusAssign,    $3) }
	| variable TIMESASSIGN expr     { BlockAssign($1, TimesAssign,    $3) }
	| variable EXPASSIGN expr       { BlockAssign($1, ExpAssign,      $3) }
	| variable DIVIDEASSIGN expr    { BlockAssign($1, DivideAssign,   $3) }
	| variable FLOORDIVASSIGN expr  { BlockAssign($1, FloorDivAssign, $3) }
	| variable MODASSIGN expr       { BlockAssign($1, ModAssign,      $3) }
	| variable ANDASSIGN expr       { BlockAssign($1, AndAssign,      $3) }
	| variable ORASSIGN expr        { BlockAssign($1, OrAssign,       $3) }
	| variable XORASSIGN expr       { BlockAssign($1, XorAssign,      $3) }
	| variable RSHIFTASSIGN expr    { BlockAssign($1, RShiftAssign,   $3) }
	| variable LSHIFTASSIGN expr    { BlockAssign($1, LShiftAssign,   $3) }

expr:
	 BOOLLIT                        { BoolLit($1)                   }
    | INTLIT                        { IntLit($1)                    }
    | FLOATLIT                      { FloatLit($1)                  }
    | STRINGLIT                     { StringLit($1)                 }
    | variable                      { VarExpr($1)                   }
	| list							{ $1                            }    
	| dict							{ $1                            }
	| STRINGLIT LBRACK expr RBRACK  { IndexingStringLit($1, $3) 	}
	| list LBRACK expr RBRACK       { IndexingExprList($1, $3) 		}
	(** variable should eat this up | VARIABLE LBRACK expr RBRACK   { IndexingVar(Var($1), $3) 		} **)
	(** | expr DOT expr                 { Binop($1, Dot, $3)    } **)
	| expr PLUS expr                { Binop($1, Add, $3)            }
    | expr MINUS expr               { Binop($1, Sub, $3)            }
    | expr EQ expr                  { Binop($1, Eq, $3)  	        }
    | expr NEQ expr                 { Binop($1, Neq, $3)            }
    | expr LT expr                  { Binop($1, Less, $3)           }
    | expr AND expr                 { Binop($1, And, $3)            }
    | expr OR expr                  { Binop($1, Or, $3)             }
	| expr BITAND expr              { Binop($1, BitAnd, $3)         }
	| expr BITOR expr               { Binop($1, BitOr, $3)          }
    | expr LSHIFT expr              { Binop($1, LShift, $3)         }
    | expr RSHIFT expr              { Binop($1, RShift, $3)  	    }
    | expr MOD expr                 { Binop($1, Mod, $3)            }
    | expr BITXOR expr              { Binop($1, Less, $3)           }
    | variable WALRUS expr          { Walrus($1, $3)                }
    | LPAREN expr RPAREN            { $2                            }
	| function_call					{ $1                            }

function_call:
	variable LPAREN list_contents RPAREN    { FuncCall($1, $3)      }

function_block_call:
	variable LPAREN list_contents RPAREN    { FuncBlockCall($1, $3) }

list:
	list_literal            { $1    }
	| list_comprehension    { $1    }

list_comprehension:
	LBRACK expr FOR VARIABLE IN expr RBRACK             { ListCompUnconditional($2, Var($4), $6)    }
 	| LBRACK expr FOR VARIABLE IN expr IF expr RBRACK   { ListCompConditional($2, Var($4), $6, $8)  }

dict_comprehension:
	LCURL expr COLON expr FOR LPAREN VARIABLE COMMA VARIABLE RPAREN IN expr RCURL           { DictCompUnconditional($2, $4, Var($7), Var($9), $12)  }
 	| LCURL expr COLON expr FOR LPAREN VARIABLE COMMA VARIABLE RPAREN IN expr IF expr RCURL { DictCompConditional($2, $4, Var($7), Var($9), $12, $14)   }

list_literal:
	LBRACK list_contents RBRACK { List($2) }

list_contents:
	/* nothing */               { []        }
	| expr                      { [$1]      }
	| expr COMMA list_contents  { $1 :: $3  }

dict:
	 dict_literal           { $1    }
	| dict_comprehension    { $1    }


dict_literal:
	LCURL dict_contents RCURL   { Dict($2)    }

dict_contents:
	/* nothing */                       { []        }
	| dict_element                      { [$1]      }
	| dict_element COMMA dict_contents  { $1 :: $3  }

dict_element:
	expr COLON expr         { ($1, $3)  }


while_loop:
	WHILE expr COLON newline_list INDENT block_list DEDENT  { While($2, $6) }

for_loop:
	FOR VARIABLE IN expr COLON NEWLINE INDENT block_list    { For(Var($2), $4, $8)  }
	
interface_definition:
	INTERFACE VARIABLE COLON newline_list INDENT func_signature_list DEDENT     { InterfaceDefinition(Var($2), $6)  }

class_definition:
	CLASS VARIABLE COLON newline_list INDENT block_list DEDENT                              { ClassDefinition(Var($2), $6)                  }
	| CLASS VARIABLE IMPLEMENTS variable_list COLON newline_list INDENT block_list DEDENT   { ClassDefinitionImplements(Var($2), $4, $8)    }


variable_list:
	/* nothing */                   { []            }
	| VARIABLE                      { [Var($1)]     }
	| VARIABLE COMMA variable_list  { Var($1) :: $3 }

func_signature_list:
	/* nothing */                                       { []        }
	| func_signature                                    { [$1]      }
	| func_signature newline_list func_signature_list   { $1 :: $3  }

func_signature:
	DEF VARIABLE LPAREN args_list RPAREN ARROW typename { (Var($2), $4, $7) }

function_definition:
	func_signature COLON newline_list INDENT block_list DEDENT { FunctionDefinition($1, $5) }

args_list:
	| /* nothing */         { []        }
	| arg                   { [$1]      }
	| arg COMMA args_list   { $1 :: $3  }

arg:
	VARIABLE COLON typename { (Var($1), $3) }

conditional:
	IF expr COLON NEWLINE INDENT block_list DEDENT elif { IfNonEnd($2, $6, $8)  }
	| IF expr COLON NEWLINE INDENT block_list DEDENT    { IfEnd($2, $6)         }

elif:
	ELIF expr COLON NEWLINE INDENT block_list DEDENT elif   { ElifNonEnd($2, $6, $8)    }
	| ELIF expr COLON NEWLINE INDENT block_list DEDENT      { ElifEnd($2, $6)           }
	| ELSE COLON NEWLINE INDENT block_list DEDENT           { ElseEnd($5)               }
