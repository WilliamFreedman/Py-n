type bop = Add | Sub | Eq | Neq | Less | And | Or (**| ArrayGet **)

(** Types in our language **)
type typevar = TypeVariable of string

(** Indicates a variable **)
type variable = Var of string

type special_assignment = 
  IdentityAssign
  | PlusAssign
  | MinusAssign
  | TimesAssign
  | DivideAssign
  | FloorDivAssign
  | ExpAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | RShiftAssign
  | LShiftAssign
  | ModAssign


type expr =
  BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | VarExpr of variable
  | Binop of expr * bop * expr
  | Assign of variable * special_assignment * expr
  (** | Return of expr **)

type block =
    BlockAssign of variable * special_assignment * expr
  | Break
  | Continue
  | Pass
  | VarDec of typevar * variable * expr (** Mostly for assignment expressions (w/ no type decl) **)
  | Return of expr
  (**| If of expr * block * block **)
  | While of expr * block list
  | For of expr * expr * block list

(** A block can be a sequence of statements, an if ... elif ... else, or a loop **)


(** type bind = typ * string **)

type program = {
  body: block list
}

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Eq -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"

let string_of_special_assignment = function
IdentityAssign -> "="
  | PlusAssign -> "+="
  | MinusAssign -> "-=" 
  | TimesAssign -> "*="
  | DivideAssign -> "/="
  | FloorDivAssign -> "//="
  | ExpAssign -> "**="
  | AndAssign -> "&="
  | OrAssign -> "|="
  | XorAssign -> "^=" 
  | RShiftAssign -> ">>="
  | LShiftAssign -> "<<="
  | ModAssign -> "%="
  
let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(Var(v), assign_type, e) -> v ^ string_of_special_assignment assign_type ^ string_of_expr e
  | VarExpr(Var(v)) -> v


let rec string_of_block = function
  BlockAssign(Var(v_string), spec_assign, expr) -> 
  v_string ^ " " ^ string_of_special_assignment spec_assign ^ " " ^ string_of_expr expr ^ "\n"
  | VarDec(TypeVariable(t_string), Var(v_string), expr) ->
  v_string ^ ": " ^ t_string ^ " = " ^ string_of_expr expr ^ "\n"
  | _ -> "x_x"
  (* "\n" ^ String.concat "" (List.map string_of_block stmts) ^ "}\n" *)
  (* | Expr(expr) -> string_of_expr expr ^ ";\n"; *)
  (* | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s *)

let string_of_typevar = function
  TypeVariable(v) -> v

(* 
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n" *)

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_block fdecl.body) ^
  "\n"

