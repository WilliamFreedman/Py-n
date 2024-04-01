(* OH: how to do literals, array indexing*)

{ open Parser } 

let digits = ['0'-'9']

let alphabet = ['a'-'z' 'A'-'Z' '0'-'9' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '=' '+' '{' '}' '\\' '|' ':' ';' ',' '<' '.' '>' '/' '?']

rule tokenize = parse
  [' ' '\t' '\r'] { tokenize lexbuf }
| '\n' { NEWLINE }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' {LBRACK}
| ']' {RBRACK}
| '{' {LCURL}
| '}' {RCURL}
| ',' {COMMA}
| "**" { EXPONENT }
| "//" {FLOORDIVIDE}
| '%' { MOD }
| "<<" { LSHIFT }
| ">>" { RSHIFT }
| '&' { BITAND }
| '|' { BITOR }
| '^' { BITXOR }
| '~' { BITNOT }
| ":=" { WALRUS }
| '<' { GT }
| '>' { LT }
| ">=" { GEQ }
| "<=" { LEQ }
| "==" { EQ }
| "!=" { NEQ }
| '=' { ASSIGN }
| "+=" { PLUSASSIGN }
| "-=" { MINUSASSIGN }
| "*=" { TIMESASSIGN }
| "/=" { DIVIDEASSIGN }
| "//=" { FLOORDIVASSIGN }
| "**=" { EXPASSIGN }
| "&=" { ANDASSIGN }
| "|=" { ORASSIGN }
| "^=" { XORASSIGN }
| ">>=" { RSHIFTASSIGN } 
| "<<=" { LSHIFTASSIGN }
| '.' { DOT }
| ':' {COLON}
| "def" { DEF }
| "return" { RETURN }
| "if" { IF }
| "else" { ELSE }
| "elif" { ELIF }
| "break" { BREAK }
| "continue" { CONTINUE }
| "pass" { PASS }
| "in" { IN }
| "is" { IS }
| "not" { NOT }
| "del" { DEL }
| "from" {  FROM  }
| "while" { WHILE  }
| "for" { FOR }
| "interface" { INTERFACE }
| "class" { CLASS }
| "True"  { TRUE  }
| "False" { FALSE }
| "None" { NONE }
| "INDENT" { INDENT }
| "int" { INT }
| "str" { STR }
| "bool" {  BOOL  }
| "float" { FLOAT }
| "and" { AND } 
| "or" {OR}
| "DEDENT" { DEDENT }
| ['+' '-']? ('0'|(['1'-'9'] digits*))? '.' digits* (['e' 'E'] ['+' '-']? digits+)? as lit { FLOATLIT(float_of_string lit) }
| ['+' '-']? ('0'|(['1'-'9'] digits*)) ['e' 'E'] ['+' '-']? digits+ as lit { FLOATLIT(float_of_string lit) }
| ['+' '-']? ('0'|['1'-'9'] digits*) as lit { INTLIT(int_of_string lit) }
| '\"'alphabet*'\"' as lit { STRINGLIT(lit) }
| '\''alphabet*'\'' as lit { STRINGLIT(lit) }
| ['a'-'z''A'-'Z''_']+['a'-'z''A'-'Z''_' '0'-'9']+ as id { VARIABLE(id) }
| eof { EOF }

{
let buf = Lexing . from_channel stdin in
let f = tokenize buf in
print_endline f
}

(*
rule lex_float = parse
   | digit+('E'|'e')('+'|'-')?digit+ as out {out}
   | digit+'.' as out {out}
   | digit+'.'('E'|'e')('+'|'-')?digit+ as out {out}
   | ('E'|'e')('+'|'-')?digit+ as out {out}
   | '.'digit+ as out {out}
   | '.'digit+('E'|'e')('+'|'-')?digit+ as out {out}
   | digit+('E'|'e')('+'|'-')?digit+ as out {out}
   | digit+'.'digit+ as out {out}
   | digit+'.'digit+('E'|'e')('+'|'-')?digit+ as out {out}
*)


