open Ast

(** (should be semantically checked) types in our language **)

type sassignment = 
  SAssign

(** Variable / term indicator, with dots and indexing **)
type svariable = variable (*| SVarDot of svariable * svariable | SVarIndex of svariable * sexpr *)
and sexpr = typevar * sx
and sx = 
  SBoolLit of bool
  | SIntLit of int
  | SFloatLit of float
  | SStringLit of string
  | SVarExpr of svariable
  | SList of sexpr list
  | SListCompUnconditional of sexpr * svariable * sexpr
  | SListCompConditional of sexpr * svariable * sexpr * sexpr
  | SDictCompConditional of sexpr * sexpr * svariable * svariable * sexpr * sexpr
  | SDictCompUnconditional of sexpr * sexpr * svariable * svariable * sexpr
  | SDict of (sexpr * sexpr) list
  | SBinop of sexpr * bop * sexpr
  | SWalrus of svariable * sexpr
  | SFuncCall of svariable * sexpr list
  (** | IndexingVar of svariable * sexpr **)
  | SIndexingStringLit of string * sexpr
  | SIndexingExprList of sexpr * sexpr
  (** | Return of sexpr **)

(*{DictCompUnconditional($2, $4, Var(6), Var($8), $10, 12$)}*)
type sarg = svariable * typevar

type sfunction_signature = svariable * sarg list * typevar


type sblock =
    SBlockAssign of svariable * sassignment * sexpr
  | SBreak
  | SContinue
  | SPass
  | SVarDec of typevar * svariable * sexpr (** Mostly for assignment expressions (w/ no type decl) **)
  | SReturnVal of sexpr
  | SReturnVoid
  | SWhile of sexpr * sblock list
  | SFor of svariable * sexpr * sblock list
  | SFuncBlockCall of svariable * sexpr list
  | SFunctionSignature of sfunction_signature
  | SFunctionDefinition of sfunction_signature * sblock list 
  | SInterfaceDefinition of svariable * sfunction_signature list
  | SClassDefinition of svariable * sblock list
  | SClassDefinitionImplements of svariable * svariable list * sblock list
  | SIfEnd of sexpr * sblock list
  | SIfNonEnd of sexpr * sblock list * sblock
  | SElifEnd of sexpr * sblock list
  | SElifNonEnd of sexpr * sblock list * sblock
  | SElseEnd of sblock list

type sprogram = {
  body: sblock list
}

(* Pretty-printing functions *)
let string_of_sspecial_assignment = function
SIdentityAssign -> "="
  | SPlusAssign -> "+="
  | SMinusAssign -> "-=" 
  | STimesAssign -> "*="
  | SDivideAssign -> "/="
  | SFloorDivAssign -> "//="
  | SExpAssign -> "**="
  | SAndAssign -> "&="
  | SOrAssign -> "|="
  | SXorAssign -> "^=" 
  | SRShiftAssign -> ">>="
  | SLShiftAssign -> "<<="
  | SModAssign -> "%="

let rec string_of_svar = function
  | (t, var) -> "(" ^ string_of_typevar t ^ "," ^ string_of_var var ^ ")"
    SVar(v) -> v
    (* | SVarDot(v1, v2) -> string_of_svar v1 ^ "." ^ string_of_svar v2 *)
    | SVarIndex(v, e) -> string_of_svar v ^ "[" ^ string_of_sexpr e ^ "]"
and string_of_sexpr (t, e) =
  "(" ^ string_of_typevar t ^ " : " ^ (match e with
    SIntLit(l) -> string_of_int l
  | SBoolLit(true) -> "True"
  | SBoolLit(false) -> "False"
  | SFloatLit(f) -> string_of_float f
  | SStringLit(s) -> s
  | SBinop(e1, o, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SWalrus(v, e) -> string_of_svar v ^ " := " ^ string_of_sexpr e
  | SVarExpr(v) -> string_of_svar v
  | SList(list) -> "[" ^ String.concat ", " (List.map string_of_sexpr list) ^ "]"
  | SDict(d) -> let expr_expr_printer = function
    (e1, e2) -> string_of_sexpr e1 ^ " : " ^ string_of_sexpr e2 in
  "{" ^ String.concat ", " (List.map expr_expr_printer d) ^ "}"
  | SFuncCall(v, e) -> string_of_svar v ^ "(" ^ String.concat ", " (List.map string_of_sexpr e) ^ ")"
  | SListCompUnconditional(e1, v, e2) -> "[" ^ string_of_sexpr e1 ^ " for " ^ string_of_svar v ^ " in " ^ string_of_sexpr e2 ^ "]"
  | SListCompConditional(e1, v, e2, e3) -> "[" ^ string_of_sexpr e1 ^ " for " ^ string_of_svar v ^ " in " ^ string_of_sexpr e2 ^ " if " ^ string_of_sexpr e3 ^ "]"
  (** Shouldn't be needed anymore: | IndexingVar(v, e) -> v ^ "[" ^ string_of_expr e ^ "]" **)
  | SIndexingStringLit(s, e) -> s ^ "[" ^ string_of_sexpr e ^ "]"
  | SIndexingExprList(e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
  | SDictCompConditional(e1,e2,v1,v2,e3,e4) -> "{" ^ string_of_sexpr e1 ^ " : " ^ string_of_sexpr e2 ^ " for (" ^ string_of_svar v1 ^ ", " ^ string_of_svar v2 ^ ") in " ^ string_of_sexpr e3 ^ "if" ^ string_of_sexpr e4 ^ "}" 
  | SDictCompUnconditional(e1,e2,v1,v2,e3) -> "{" ^ string_of_sexpr e1 ^ " : " ^ string_of_sexpr e2 ^ " for (" ^ string_of_svar v1 ^ ", " ^ string_of_svar v2 ^ ") in " ^ string_of_sexpr e3 ^ "}" 
  ) ^ ")"

  (* | _ -> raise (Failure "Invalid expr") *)

let string_of_sarg = function
(v, t) -> string_of_svar v ^ " : " ^ string_of_typevar t

let string_of_sfunc_sig = function
  (v, args, ret_type) ->
    "def " ^ string_of_svar v ^ "(" ^ String.concat ", " (List.map string_of_sarg args) ^ ") -> " ^ string_of_typevar ret_type
    
let rec string_of_sblock = function
  SBlockAssign(v, spec_assign, expr) -> 
    string_of_svar v ^ " " ^ string_of_sspecial_assignment spec_assign ^ " " ^ string_of_sexpr expr ^ "\n"
  | SVarDec(t, v, expr) ->
    string_of_svar v ^ ": " ^ string_of_typevar t ^ " = " ^ string_of_sexpr expr ^ "\n"
  | SWhile(e, block_list) ->
  "while " ^ string_of_sexpr e ^ ":\n" ^ String.concat "" (List.map string_of_sblock block_list) ^ "\n"
  | SFor(v, e, block_list) ->
  "for " ^ string_of_svar v ^ " in " ^ string_of_sexpr e ^ ":\n" ^ String.concat "" (List.map string_of_sblock block_list) ^ "\n"
  | SFuncBlockCall(v, e) -> string_of_svar v ^ "(" ^ String.concat ", " (List.map string_of_sexpr e) ^ ")" ^ "\n"
  | SReturnVal(e) -> "return " ^ string_of_sexpr e ^ "\n"
  | SReturnVoid -> "return" ^ "\n"
  | SFunctionSignature(signature) -> string_of_sfunc_sig signature ^ "\n"
  | SFunctionDefinition(signature, block_list) -> 
    string_of_sfunc_sig signature ^ "\n" ^ String.concat "" (List.map string_of_sblock block_list) ^ "\n"
  | SBreak -> "break\n"
  | SContinue -> "continue\n"
  | SPass -> "pass\n"
  | SInterfaceDefinition(v, sig_list) ->
  "interface " ^ string_of_svar v ^ ":\n" ^  String.concat "\n" (List.map string_of_sfunc_sig sig_list) ^ "\n"
  | SClassDefinition(v, block_list) ->
    "class " ^ string_of_svar v ^ ":\n" ^  String.concat "\n" (List.map string_of_sblock block_list) ^ "\n"
  | SClassDefinitionImplements(v, interfaces, block_list) ->
    "class " ^ string_of_svar v ^ " implements " ^ String.concat ", " (List.map string_of_svar interfaces) ^ ":\n" ^  String.concat "\n" (List.map string_of_sblock block_list) ^ "\n"
  | SIfEnd(e, bl) -> "if " ^ string_of_sexpr e ^ ":\n" ^ String.concat "\n" (List.map string_of_sblock bl) ^ "\n"
  | SIfNonEnd(e, bl, nbl) -> "if " ^ string_of_sexpr e ^ ":\n" ^ String.concat "\n" (List.map string_of_sblock bl) ^ string_of_block nbl
  | SElifEnd(e, bl) -> "elif " ^ string_of_sexpr e ^ ":\n" ^ String.concat "\n" (List.map string_of_sblock bl) ^ "\n"
  | SElifNonEnd(e, bl, nbl) -> "elif " ^ string_of_sexpr e ^ ":\n" ^ String.concat "\n" (List.map string_of_sblock bl) ^ string_of_block nbl
  | SElseEnd(bl) -> "else:\n" ^ String.concat "\n" (List.map string_of_sblock bl) ^ "\n"

let string_of_sprogram fdecl =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_sblock fdecl.body) ^
  "\n"
