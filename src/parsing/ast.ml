type bop = Add | Sub | Eq | Neq | Less | And | Or | BitAnd | BitOr | LShift | RShift | Mod | BitXor (*| ArrayGet | Dot *)

(** Types in our language **)
type typevar = Dict of typevar * typevar
  | List of typevar
  | TypeVariable of string

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

(** Variable / term indicator, with dots and indexing **)
type variable = Var of string | VarDot of variable * variable | VarIndex of variable * expr
and expr =
  BoolLit of bool
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | VarExpr of variable
  | List of expr list
  | ListCompUnconditional of expr * variable * expr
  | ListCompConditional of expr * variable * expr * expr
  | DictCompConditional of expr * expr * variable * variable * expr * expr
  | DictCompUnconditional of expr * expr * variable * variable * expr
  | Dict of (expr * expr) list
  | Binop of expr * bop * expr
  | Walrus of variable * expr
  | FuncCall of variable * expr list
  (** | IndexingVar of variable * expr **)
  | IndexingStringLit of string * expr
  | IndexingExprList of expr * expr
  (** | Return of expr **)

(*{DictCompUnconditional($2, $4, Var(6), Var($8), $10, 12$)}*)
type arg = variable * typevar

type function_signature = variable * arg list * typevar

type block =
    BlockAssign of variable * special_assignment * expr
  | Break
  | Continue
  | Pass
  | VarDec of typevar * variable * expr (** Mostly for assignment expressions (w/ no type decl) **)
  | ReturnVal of expr
  | ReturnVoid
  | While of expr * block list
  | For of variable * expr * block list
  | FuncBlockCall of variable * expr list
  | FunctionSignature of function_signature
  | FunctionDefinition of function_signature * block list 
  | InterfaceDefinition of variable * function_signature list
  | ClassDefinition of variable * block list
  | ClassDefinitionImplements of variable * variable list * block list
  | IfEnd of expr * block list
  | IfNonEnd of expr * block list * block
  | ElifEnd of expr * block list
  | ElifNonEnd of expr * block list * block
  | ElseEnd of block list

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
  (* | Dot -> "." *)
  | Mod -> "%"
  | LShift -> "<<"
  | RShift -> ">>"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"

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

let rec string_of_var = function
    Var(v) -> v
    | VarDot(v1, v2) -> string_of_var v1 ^ "." ^ string_of_var v2
    | VarIndex(v, e) -> string_of_var v ^ "[" ^ string_of_expr e ^ "]"
and string_of_expr = function
    IntLit(l) -> string_of_int l
  | BoolLit(true) -> "True"
  | BoolLit(false) -> "False"
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Walrus(v, e) -> string_of_var v ^ " := " ^ string_of_expr e
  | VarExpr(v) -> string_of_var v
  | List(list) -> "[" ^ String.concat ", " (List.map string_of_expr list) ^ "]"
  | Dict(d) -> let expr_expr_printer = function
    (e1, e2) -> string_of_expr e1 ^ " : " ^ string_of_expr e2 in
  "{" ^ String.concat ", " (List.map expr_expr_printer d) ^ "}"
  | FuncCall(v, e) -> string_of_var v ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")"
  | ListCompUnconditional(e1, v, e2) -> "[" ^ string_of_expr e1 ^ " for " ^ string_of_var v ^ " in " ^ string_of_expr e2 ^ "]"
  | ListCompConditional(e1, v, e2, e3) -> "[" ^ string_of_expr e1 ^ " for " ^ string_of_var v ^ " in " ^ string_of_expr e2 ^ " if " ^ string_of_expr e3 ^ "]"
  (** Shouldn't be needed anymore: | IndexingVar(v, e) -> v ^ "[" ^ string_of_expr e ^ "]" **)
  | IndexingStringLit(s, e) -> s ^ "[" ^ string_of_expr e ^ "]"
  | IndexingExprList(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | DictCompConditional(e1,e2,v1,v2,e3,e4) -> "{" ^ string_of_expr e1 ^ " : " ^ string_of_expr e2 ^ " for (" ^ string_of_var v1 ^ ", " ^ string_of_var v2 ^ ") in " ^ string_of_expr e3 ^ "if" ^ string_of_expr e4 ^ "}" 
  | DictCompUnconditional(e1,e2,v1,v2,e3) -> "{" ^ string_of_expr e1 ^ " : " ^ string_of_expr e2 ^ " for (" ^ string_of_var v1 ^ ", " ^ string_of_var v2 ^ ") in " ^ string_of_expr e3 ^ "}" 

  (* | _ -> raise (Failure "Invalid expr") *)



let rec string_of_typevar = function
  TypeVariable(v) -> v
  | List(t) -> "list[" ^ string_of_typevar t ^ "]"
  | Dict(t1, t2) -> "dict[" ^ string_of_typevar t1 ^ ", " ^ string_of_typevar t2 ^ "]"

let string_of_arg = function
(v, t) -> string_of_var v ^ " : " ^ string_of_typevar t

let string_of_func_sig = function
  (v, args, ret_type) ->
    "def " ^ string_of_var v ^ "(" ^ String.concat ", " (List.map string_of_arg args) ^ ") -> " ^ string_of_typevar ret_type
    
let rec string_of_block = function
  BlockAssign(v, spec_assign, expr) -> 
  string_of_var v ^ " " ^ string_of_special_assignment spec_assign ^ " " ^ string_of_expr expr ^ "\n"
  | VarDec(t, v, expr) ->
  string_of_var v ^ ": " ^ string_of_typevar t ^ " = " ^ string_of_expr expr ^ "\n"
  | While(e, block_list) ->
  "while " ^ string_of_expr e ^ ":\n" ^ String.concat "" (List.map string_of_block block_list) ^ "\n"
  | For(v, e, block_list) ->
  "for " ^ string_of_var v ^ " in " ^ string_of_expr e ^ ":\n" ^ String.concat "" (List.map string_of_block block_list) ^ "\n"
  | FuncBlockCall(v, e) -> string_of_var v ^ "(" ^ String.concat ", " (List.map string_of_expr e) ^ ")" ^ "\n"
  | ReturnVal(e) -> "return " ^ string_of_expr e ^ "\n"
  | ReturnVoid -> "return" ^ "\n"
  | FunctionSignature(signature) -> string_of_func_sig signature ^ "\n"
  | FunctionDefinition(signature, block_list) -> 
    string_of_func_sig signature ^ "\n" ^ String.concat "" (List.map string_of_block block_list) ^ "\n"
  | Break -> "break\n"
  | Continue -> "continue\n"
  | Pass -> "pass\n"
  | InterfaceDefinition(v, sig_list) ->
  "interface " ^ string_of_var v ^ ":\n" ^  String.concat "\n" (List.map string_of_func_sig sig_list) ^ "\n"
  | ClassDefinition(v, block_list) ->
    "class " ^ string_of_var v ^ ":\n" ^  String.concat "\n" (List.map string_of_block block_list) ^ "\n"
  | ClassDefinitionImplements(v, interfaces, block_list) ->
    "class " ^ string_of_var v ^ " implements " ^ String.concat ", " (List.map string_of_var interfaces) ^ ":\n" ^  String.concat "\n" (List.map string_of_block block_list) ^ "\n"
  | IfEnd(e, bl) -> "if " ^ string_of_expr e ^ ":\n" ^ String.concat "\n" (List.map string_of_block bl) ^ "\n"
  | IfNonEnd(e, bl, nbl) -> "if " ^ string_of_expr e ^ ":\n" ^ String.concat "\n" (List.map string_of_block bl) ^ string_of_block nbl
  | ElifEnd(e, bl) -> "elif " ^ string_of_expr e ^ ":\n" ^ String.concat "\n" (List.map string_of_block bl) ^ "\n"
  | ElifNonEnd(e, bl, nbl) -> "elif " ^ string_of_expr e ^ ":\n" ^ String.concat "\n" (List.map string_of_block bl) ^ string_of_block nbl
  | ElseEnd(bl) -> "else:\n" ^ String.concat "\n" (List.map string_of_block bl) ^ "\n"

let string_of_program fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_block fdecl.body) ^
  "\n"
