type bop = Add | Sub | Eq | Neq | Less | And | Or (**| ArrayGet **)

(** Types in our language **)
type typevar = Int | Bool | String | TypeVariable of string

(** Indicates a variable **)
type variable = Var of string

type expr =
  BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | VarExpr of variable
  | Binop of expr * bop * expr
  | Assign of variable * expr
  (** | Return of expr **)

type stmt =
  VarAssign of typevar * variable * expr
  | VarDec of typevar * variable
  | Break
  | Continue
  | Pass
  | Return of expr

(**
type block =
  | Expr of expr
  | If of expr * block * block
  | While of expr * block
  | For of expr * expr * block
  **)

(** type bind = typ * string **)

type program = {
  body: block list;
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

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(f) -> string_of_float
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(Var(v), e) -> v ^ " = " ^ string_of_expr e
  | VarExpr(Var(v)) -> v

(**
let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  **)

let string_of_typevar = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Str -> "str"
  | TypeVariable(v) -> v

(**
let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "\n"
  **)
