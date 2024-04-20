(*TODO: add binops to scanner, no-op binop*)

open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

(* Verify a list of bindings has no duplicate names
let check_binds (kind : string) (binds : (typ * string) list) =
  let rec dups = function
      [] -> ()
    |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
      raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
    | _ :: t -> dups t
  in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
in

(* Make sure no blocks duplicate *)
check_binds "block" blocks; *)


(* Collect all function names into one symbol table *)
let function_decls = List.fold_left add_func StringMap.empty functions
in

(* Return a function from our symbol table *)
let find_func s =
  try StringMap.find s function_decls
  with Not_found -> raise (Failure ("unrecognized function " ^ s))
in

let _ = find_func "main" in (* Ensure "main" is defined *)

let check_func func =
  (* Make sure no formals or locals are void or duplicates *)
  check_binds "formal" func.formals;
  check_binds "local" func.locals;

  (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  (* Build local symbol table of variables for this function *)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty (globals @ func.formals @ func.locals )
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s symbol_table =
    try StringMap.find s symbol_table
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a tuple of a fun_table and a var_table from our class table *)
  (* let class_details s class_table =
    try StringMap.find s class_table
    with Not_found -> raise (Failure ("undeclared class " ^ s))
  in *)

  (* List.fold_left (fun ) (StringMap.empty StringMap.empty StringMap.empty) *)
  
  

  (* var_table contains a key of the variable name which points to a typevar *)
  (* func_table contains a key of the function name which points to a tuple of a list of args and the return type *)
  (* Class table contains a key of the class name which points to a tuple of a function table and variable table *)
  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr var_table func_table  = function
    | IntLit l -> (TypeVariable("int"), SIntLit l)
    | FloatLit l -> (TypeVariable("Float"), SFloatLit l)
    | BoolLit l -> (TypeVariable("bool"), SBoolLit l)
    | StringLit l -> (TypeVariable("string") , SStringLit l)
    (*| SVarExpr var -> (check the type of var, SVarExpr (convert var to svar type))*)
    | VarExpr var -> match var with
      | Var(v) -> (StringMap.find v var_table, SVar v)
      | VarDot(v1, v2) -> let (v1_typevar, v1_sem) = check_expr v1 ar_table func_table  in
        let (_, var_table) = class_details (string_of_typevar v1_typevar)  in 
        let typ = StringMap.find v2 var_table
        in (typ, SVarDot((v1_typevar, v1_sem), check_expr v2 var_table func_table ))
      | VarIndex(v1, e) -> let (t, sx) = check_expr v1 var_table func_table  in match t with
        | Dict(key_type, value_type) ->
        let (typvar, _) = check_expr e var_table func_table  in 
        if key_type = typvar then (value_type, SVarIndex(t, sx))
        else raise (Failure ("Invalid dictionary indexing"))
        | List(list_type) ->
        let (typvar, _) = check_expr e var_table func_table  in 
        if typvar = TypeVariable("int") then (list_type, SVarIndex(t, sx))
        else raise (Failure ("Invalid indexing: " ^ string_of_var VarIndex(v1, e)))
    | List (e) -> list_check e symbol_table func_table
    | ListCompUnconditional (expr, loop_var, loop_list) -> 
      let (loop_type, _) = check_expr symbol_table func_table  loop_list in
      match loop_type with
      | List (elem_type) -> (** Only doing lists **)
        let (expr_type, _) = check_expr (StringMap.add loop_var elem_type symbol_table) func_table expr in
        (List(expr_type), SListCompUnconditional(check_expr expr, check_expr symbol_table loop_var, check_expr symbol_table loop_list))
      | _ -> raise (Failure ("List comprehension depends on non-list object"))
    | ListCompConditional (expr, loop_var, loop_list, condition) -> 
      let (loop_type, _) = check_expr symbol_table  func_table loop_list in
      match loop_type with
      | List (elem_type) -> (** Only doing lists **)
        let (expr_type, _) = check_expr (StringMap.add loop_var elem_type symbol_table) func_table expr in
        let (condition_type, _) = check_expr (StringMap.add loop_var elem_type symbol_table) func_table condition in
        if (condition_type != TypeVariable("bool")) then raise (Failure ("List comprehension condition depends on non-boolean object"))
        else
        (List(expr_type), SListCompConditional(check_expr expr, check_expr symbol_table loop_var, check_expr symbol_table loop_list, check_expr symbol_table condition))
      | _ -> raise (Failure ("List comprehension depends on non-list object"))
    (*| Id var -> (type_of_identifier var, SId var)*) (*todo*)
    | Walrus(var, e) as ex ->
      let lt = type_of_identifier var symbol_table
      and (rt, e') = check_expr e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))
    | Binop(e1, op, e2) as e -> check_binop e1 op e2 symbol_table func_table
    | FuncCall(fname, args) -> check_func_call symbol_table func_table fname args

let rec list_to_sexpr (list_elems: expr list) (symbol_table: StringMap) (func_table: StringMap) (head_type: typevar) (list_so_far: sexpr_list) = 
  match list_elems with
  | [] -> list_so_far
  | head :: tail -> 
    let head_sexpr = check_expr symbol_table func_table head in
    match head_sexpr with
    (t,_) -> 
      (if (t != head_type) then raise (Failure "Type mismatch in list")
      else
        list_to_sexpr tail symbol_table func_table func_table head_type (list_so_far :: head_sexpr))
in
  (*will return an option type, the calling function should check if its None because thnen any type is valid*)
let check_list (list_elems: expr list) (symbol_table: StringMap) (func_table: StringMap) = 
  match list_elems with
  |[] -> None
  | head :: tail -> 
      let head_sexpr = check_expr symbol_table func_table head in
        match head_sexpr with
        (t,_) -> (List(t), SList(list_to_sexpr tail symbol_table func_table t [head_sexpr]));;

let binop_return_type t1 op t2= 
  match op with
  | Add | Sub  ->
    (match (t1,t2) with 
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("float")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | Eq | Neq | Less ->
    (match (t1,t2) with 
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("bool")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | Mod | LShif | RShift -> 
    (match (t1,t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | And | Or | Xor->
   ( match (t1,t2) with
    | (TypeVariable("bool"), TypeVariable("bool")) -> true
    | (_,_) -> raise (Failure "Invalid binop types"))
  | BitAnd | BitOr | BitXor-> 
   ( match (t1,t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | Iden | Mult | Div | FDiv | Exp |  ->
    ( match (t1,t2) with
    | (t,t) -> t
    | (_,_) -> raise (Failure "Invalid binop types"))
  | _ -> raise (Failure "Unknown binop found") in 
 
    

let check_binop (e1: expr) (operation: bop) (e2: expr) (symbol_table: StringMap) (func_table: StringMap) = 
  let (t1,_) = check_expr symbol_table func_table e1 in
  let s1 = SExpr(t1,e1) in
  let (t2,_) = check_expr symbol_table func_table e2 in
  let s2 = SExpr(t2,e2) in
  let rtype = binop_return_type t1 operation t2 in
  (rtype, SBinop(s1,bop,s2));;


let assignment_to_bop special_assignment = 
  match special_assignment with 
  | IdentityAssign -> Iden
  | PlusAssign -> Add
  | MinusAssign -> Sub
  | TimesAssign -> Mult
  | DivideAssign -> Div
  | FloorDivAssign -> FDiv
  | ExpAssign -> Exp
  | AndAssign -> And
  | OrAssign -> Or
  | XorAssign -> Xor
  | RShiftAssign -> RShift
  | LShiftAssign -> LShift
  | ModAssign -> Mod
  | _ -> raise (Failure "Unknown binop found");;

let check_variable = (*WILLIAM WILL DO THIS IF I FORGET YELL AT ME*)

  (*BlockAssign of variable * special_assignment * expr*)
  let check_assign (lvalue: variable) (assign_type: special_assignment) (rvalue: expr) (symbol_table: StringMap) (func_table: StringMap) =
    let bop = assignment_to_bop assign_type in
    let rvalue_sexpr = check_expr symbol_table func_table rvalue in 
    let lvalue_type = check_variable symbol_table func_table lvalue in
    let binop_sexpr = check_binop lvalue bop rvalue symbol_table func_table in
    match binop_sexpr with 
    | (t1,_) -> ((t1 != lvalue_type) then raise (Failure "Improperly typed assignment")
     else 
        SBlockAssign(lvalue, SAssign, binop_sexpr));;
    
  let check_list_comprehension (comp: expr) (symbol_table: StringMap.t) (func_table: StringMap.t)  =
  match comp with
  | ListCompUnconditional (expr, loop_var, loop_list) ->
      let (loop_list_type, loop_list_sexpr) = check_expr symbol_table func_table loop_list in
      begin match loop_list_type with
      | ListType elem_type ->
          let extended_symbol_table = StringMap.add loop_var elem_type symbol_table in
          let (elem_sexpr_type, elem_sexpr) = check_expr extended_symbol_table func_table expr in 
          (ListType elem_sexpr_type, SListCompUnconditional (elem_sexpr, loop_var, loop_list_sexpr))
      | _ -> raise (Failure "List comprehension requires a list type for iteration")
      end
  | ListCompConditional (expr, loop_var, loop_list, condition) ->
      (* Similar to unconditional, but with a condition that must be a boolean *)
      let (loop_list_type, loop_list_sexpr) = check_expr symbol_table func_table loop_list in
      begin match loop_list_type with
      | ListType elem_type ->
          let extended_symbol_table = StringMap.add loop_var elem_type symbol_table in
          let (elem_sexpr_type, elem_sexpr) = check_expr extended_symbol_table func_table expr in
          let (cond_type, cond_sexpr) = check_expr extended_symbol_table func_table condition in
          if cond_type != BoolType then raise (Failure "Condition in list comprehension must be a boolean")
          else
          (ListType elem_sexpr_type, SListCompConditional (elem_sexpr, loop_var, loop_list_sexpr, cond_sexpr))
      | _ -> raise (Failure "List comprehension requires a list type for iteration")
      end
  | _ -> raise (Failure "Invalid expression type for list comprehension");;

let check_func_types (func_table: StringMap.t) (func_name: variable) = 
    try StringMap.find func_name func_table
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

let arg_list_helper (symbol_table: StringMap.t) (func_table: StringMap.t) (arg_types: string list) (params: expr list) (so_far: sexpr list) =
  match (params, arg_types) with
  | ([],[]) -> so_far
  | ((phead :: ptail),(thead :: ttail)) -> 
    ( let phead_sexpr = check_expr symbol_table func_table phead in
      match phead_sexpr with 
      | (t,_) -> if t != thead then raise (Failure "Improperly typed parameter")
      else arg_list_helper symbol_table func_table ptail ttail (so_far :: phead_sexpr)
    )
  | (_,_) -> raise (Failure "Wrong number of arguments") in


let check_func_call (symbol_table: StringMap.t) (func_table: StringMap.t) (func_name: variable) (params: expr list) = 
  let func_tuple = check_func func_table func_name in
  let params_sexprs = List.map (check_expr symbol_table func_table) params in 
  match func_tuple with 
  | (return_type,arg_types) -> arg_list_helper symbol_table func_table arg_types params_sexprs []

let variable_declaration_helper (symbol_table: StringMap.t) (func_table: StringMap.t) (var: variable) (vartype: typevar)=
  if StringMap.mem var symbol_table
  then raise (Failure "Variable " ^ var  ^ " already declared")
  else 
  if StringMap.mem var func_table
    then raise (Failure "Variable " ^ var  ^ " already declared as function")
  else
    StringMap.add var typevar symbol_table in
    
let arg_to_type (arg_types: (variable * typevar) list) =
  let (_,t) = arg_types in
    t in



let function_declaration_helper (symbol_table: StringMap.t) (func_table: StringMap.t) (func_name: variable) (return_type: typevar) (arg_types: (variable * typevar) list)=
  if StringMap.mem func_name func_table
  then raise (Failure "Function " ^ func_name  ^ " already declared")
  else 
  if StringMap.mem func_name symbol_table
    then raise (Failure "Function " ^ var  ^ " already declared as function")
  else
    StringMap.add func_name (return_type, List.map arg_to_type arg_types) func_table
    
(* check_block takes the following args: *)

(* allowed_block_set: a StringSet indicating whether a particular block type can be used
   (only used for Break / Continue / Pass / ReturnVal / ReturnVoid) *)
(* var_table contains a key of the variable name which points to a typevar *)
(* func_table contains a key of the function name which points to a tuple of a list of args and the return type *)
(* func_return_type: typevar option for the return type of the current function (None if we aren't in a function) *)
(* and the Ast block type *)


(* Return list of sblocks given blocks *)
let check_block_list allowed_block_set var_table func_table func_return_type = function
  | [] -> []
  | x :: xs -> let (v_t, f_t, sblock) = check_block allowed_block_set var_table func_table func_return_type x in sblock :: check_block_list v_t f_t xs

(* It returns a tuple of the var_table * func_table * Sast block *)

in let rec check_block allowed_block_set var_table func_table func_return_type = function
  BlockAssign(v, s, e) -> check_assign v s e symbol_table func_table
  | Break ->
    match StringSet.find_opt "Break" allowed_block_set with
    | Some _ -> (var_table, func_table, SBreak)
    | None -> raise (Failure ("Break must be specified within a loop."))
  | Continue ->
    match StringSet.find_opt "Continue" allowed_block_set with
    | Some _ -> (var_table, func_table, SContinue)
    | None -> raise (Failure ("Continue must be specified within a loop."))
  | Pass ->
    match StringSet.find_opt "Pass" allowed_block_set with
    | Some _ -> (var_table, func_table, SPass)
    | None -> raise (Failure ("Pass must be specified within a function / loop / conditional."))
  | ReturnVal(Expr(expr)) -> 
    match StringSet.find_opt "ReturnVal" allowed_block_set with
    | None | func_return_type = None -> raise (Failure ("Return must be specified within a function."))
    | Some _ when let (t, e) = check_expr var_table func_table expr in t = func_return_type -> (var_table, func_table, SReturnVal(SExpr(t, e)))
  | ReturnVoid -> 
    match StringSet.find_opt "ReturnVoid" allowed_block_set with
    | None | func_return_type = None -> raise (Failure ("Return must be specified within a function."))
    | Some _ when func_return_type = TypeVariable("void") -> (var_table, func_table, SReturnVoid)
  | VarDec(typ, var, expr) -> 
    let new_map = variable_declaration_helper symbol_table func_table in
    let rvalue_sexpr = check_expr var_able func_table expr in
    let var_dec_to_return = SVarDec (typ,Svar(var),rvalue_sexpr)
    (var_table, func_table, var_dec_to_return)

  | While(loop_expr, loop_block_list) -> 
    (* Make sure the expression is a bool type, 
       then recurse in the block list w/ breaks + continues + passes *)
      let loop_sexpr = check_expr loop_expr in
      match loop_sexpr with
      | (TypeVariable("bool"), loop_sx) ->
      let new_allowed_block_set = StringSet.add "Break" (StringSet.add "Continue" (StringSet.add "Pass" allowed_block_set)) in 
      let sblock_list = check_block_list new_allowed_block_set var_table func_table func_return_type loop_block_list in
      (var_table, func_table, SWhile(loop_sexpr, sblock_list))
      | _ -> raise (Failure ((string_of_expr loop_expr) ^ "is not of type bool."))

  | For(loop_var, loop_expr, loop_block_list) -> 
    (* Make sure the loop_expr is a list wrapper type, 
       then sequentially eval the block list w/ breaks + continues + passes + loop_var in scope *)
    let loop_sexpr = check_expr loop_expr in
    match loop_sexpr with 
      | (List(sub_type), loop_sx) ->
      let new_var_table = StringMap.add loop_var sub_type var_table in
      let new_allowed_block_set = StringSet.add "Break" (StringSet.add "Continue" (StringSet.add "Pass" allowed_block_set)) in
      let sblock_list = check_block_list new_allowed_block_set new_var_table func_table func_return_type loop_block_list in
      (var_table, func_table, SFor(, loop_sexpr, sblock_list))
      | _ -> raise (Failure ((string_of_expr loop_expr) ^ " is not a list type."))
    
  | FuncBlockCall(fname, args) -> check_func_call symbol_table func_table fname args
  (* | FunctionSignature *)
  | FunctionDefinition(func_sig, block_list) -> 
    let (variable, arg_list, typevar) = func_sig
    let sfunc_sig = (check_expr var_table func_table variable, )
    
  | IfEnd(expr, blocks) -> 
    let (t, e) = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type in
    SIfEnd(SExpr(t, e), sblock_list)
  | IfNonEnd(expr, block_list, block) ->
    let (t, e) = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type in
    SIfNonEnd(SExpr(t, e), sblock_list, check_block new_allowed_block_set var_table func_table func_return_type block)
  | ElifEnd(expr, blocks) -> 
    let (t, e) = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type in
    SElifEnd(SExpr(t, e), sblock_list)
  | ElifNonEnd(expr, block_list, block) ->
    let (t, e) = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type in
    SElifNonEnd(SExpr(t, e), sblock_list, check_block new_allowed_block_set var_table func_table func_return_type block)
  | ElseEnd(expr, block_list) ->
    let (t, e) = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type in
    SElseEnd(SExpr(t, e), sblock_list)
  | _ as c -> raise (Failure ("Unsupported block type " ^ c))
(*| InterfaceDefinition
| ClassDefinition
| ClassDefinitionImplements *)

in let check var_table func_table block_list = check_block_list StringSet.empty StringMap.empty StringMap.empty None block_list
