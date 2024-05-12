(*TODO: add binops to scanner, no-op binop*)

open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

  (* var_table contains a key of the variable name which points to a typevar *)
  (* func_table contains a key of the function name which points to a tuple of a list of args and the return type *)
  (* Class table contains a key of the class name which points to a tuple of a function table and variable table *)
  (* Return a semantically-checked expression, i.e., with a type *)

let rec check_variable symbol_table s = 
    try (StringMap.find (string_of_var s) symbol_table)
    with Not_found -> raise (Failure ("undeclared identifier " ^ string_of_var s))

and check_expr var_table func_table  = function
  | IntLit l -> (TypeVariable("int"), SIntLit l)
  | FloatLit l -> (TypeVariable("float"), SFloatLit l)
  | BoolLit l -> (TypeVariable("bool"), SBoolLit l)
  | StringLit l -> (TypeVariable("string") , SStringLit l)
  (*| SVarExpr var -> (check the type of var, SVarExpr (convert var to svar type))*)
  | VarExpr var -> (match var with
    | Var(v) -> (check_variable var_table (Var(v)), SVarExpr(Var(v)))
    (* | VarDot(v1, v2) -> let (v1_typevar, v1_sem) = check_expr v1 ar_table func_table  in
      let (_, var_table) = class_details (string_of_typevar v1_typevar)  in 
      let typ = StringMap.find v2 var_table
      in (typ, SVarDot((v1_typevar, v1_sem), check_expr v2 var_table func_table )) *)
    | VarIndex(v1, e) -> (let (t, sx) = check_expr var_table func_table (VarExpr(v1)) in match t with
      | Dict(key_type, value_type) -> 
        (let (typvar, _) = check_expr var_table func_table e  in 
        if key_type = typvar then (value_type, SVarExpr(var))
        else raise (Failure ("Invalid dictionary indexing")))
      | List(list_type) ->
        (let (typvar, _) = check_expr var_table func_table e  in 
        if typvar = TypeVariable("int") then (list_type, SVarExpr(var))
        else raise (Failure ("Invalid indexing: " ^ string_of_var (VarIndex(v1, e)))))
      | _ -> raise (Failure ("Indexing not supported for this variable"))))
    | Dict(dict_elems) -> 
    
    (match dict_elems with
    | [] -> raise (Failure "Empty dictionaries not allowed")
    | (k, v) :: _ ->
      let (key_type, _) = check_expr var_table func_table k in
      let (value_type, _) = check_expr var_table func_table v in
      let sdict_elems = check_dict var_table func_table dict_elems key_type value_type in
      (Dict(key_type, value_type), SDict(sdict_elems)))
  | ListCompUnconditional(expr, loop_var, loop_list) -> 
    let (loop_type, _) = check_expr var_table func_table loop_list in 
    (match loop_type with
    | List (elem_type) -> (** Only doing lists **)
      let (expr_type, _) = check_expr (StringMap.add (string_of_var loop_var) elem_type var_table) func_table expr in
      (List(expr_type), SListCompUnconditional(check_expr var_table func_table expr, loop_var, check_expr var_table func_table loop_list))
    | _ -> raise (Failure ("List comprehension depends on non-list object")))
  | ListCompConditional(expr, loop_var, loop_list, condition) -> 
    (let (loop_type, _) = check_expr var_table  func_table loop_list in
    match loop_type with
    | List (elem_type) -> (** Only doing lists **)
      (let (expr_type, _) = check_expr (StringMap.add (string_of_var loop_var) elem_type var_table) func_table expr in
      let (condition_type, _) = check_expr (StringMap.add (string_of_var loop_var) elem_type var_table) func_table condition in
      if (condition_type != TypeVariable("bool")) then raise (Failure ("List comprehension condition depends on non-boolean object"))
      else
      (List(expr_type), SListCompConditional(check_expr var_table func_table expr, loop_var, check_expr var_table func_table loop_list, check_expr var_table func_table condition)))
    | _ -> raise (Failure ("List comprehension depends on non-list object")))
  | DictCompUnconditional(key_expr, value_expr, key_var, value_var, dict_expr) ->
    let (key_type, key_sx) = check_expr var_table func_table key_expr in
    let (value_type, value_sx) = check_expr var_table func_table value_expr in
    let updated_var_table = StringMap.add (string_of_var key_var) key_type var_table in
    let updated_var_table = StringMap.add (string_of_var value_var) value_type updated_var_table in
    let (dict_expr_type, dict_sexpr_sx) = check_expr updated_var_table func_table dict_expr in
    let dict_sexpr = (dict_expr_type, dict_sexpr_sx) in
    (Dict(key_type, value_type), SDictCompUnconditional((key_type, key_sx), (value_type, value_sx), key_var, value_var, dict_sexpr))

  | DictCompConditional(key_expr, value_expr, key_var, value_var, dict_expr, condition) ->
    let (key_type, key_sx) = check_expr var_table func_table key_expr in
    let (value_type, value_sx) = check_expr var_table func_table value_expr in
    let updated_var_table = StringMap.add (string_of_var key_var) key_type var_table in
    let updated_var_table = StringMap.add (string_of_var value_var) value_type updated_var_table in
    let (dict_expr_type, dict_sexpr_sx) = check_expr updated_var_table func_table dict_expr in
    let (condition_type, condition_sexpr_sx) = check_expr updated_var_table func_table condition in
    if condition_type != TypeVariable("bool") then
        raise (Failure "Dictionary comprehension condition must be of type bool");
    
    let key_sexpr = (key_type, key_sx) in
    let value_sexpr = (value_type, value_sx) in
    let dict_sexpr = (dict_expr_type, dict_sexpr_sx) in
    let condition_sexpr = (condition_type, condition_sexpr_sx) in
    (Dict(key_type, value_type), SDictCompConditional(key_sexpr, value_sexpr, key_var, value_var, dict_sexpr, condition_sexpr))
  
  
  (*| Id var -> (type_of_identifier var, SId var)*) (*todo*)
  | Walrus(var, e) -> 
    let lt = check_variable var_table var in
    let (rt, e') = check_expr var_table func_table e in
    if (lt = rt) then (lt, SWalrus(var, (rt, e')))
    else raise (Failure ("Type mismatch in walrus"))
  | Binop(e1, op, e2) -> check_binop e1 op e2 var_table func_table
  | FuncCall(fname, args) -> let (return_type, func_name, sexpr_list) = check_func_call var_table func_table fname args in
    (return_type, SFuncCall(func_name, sexpr_list))
  | List(l) -> check_list var_table func_table l
  | _ -> raise (Failure ("Unsupported expr type (for now?)"))
  
and

list_to_sexpr list_elems symbol_table func_table head_type = 
  match list_elems with
  | [] -> []
  | head :: tail -> 
    let head_sexpr = check_expr symbol_table func_table head in
    match head_sexpr with
    (t,_) -> 
      (if (t != head_type) then raise (Failure "Type mismatch in list")
      else (head_sexpr :: list_to_sexpr tail symbol_table func_table head_type))

and
  (* Will return an sexpr *)
  (* If the type is TypeVariable("none"), then the list is empty and this can correspond to any list variable type *)
 check_list symbol_table func_table list_elems = 
  match list_elems with
  |[] -> (TypeVariable("none"), SList([]))
  | head :: tail -> 
      let head_sexpr = check_expr symbol_table func_table head in
        match head_sexpr with
        (t, _) -> (List(t), SList(list_to_sexpr list_elems symbol_table func_table t))
and check_dict var_table func_table dict_elems key_type value_type =
  List.fold_left (fun acc (k, v) ->
    let (k_type, k_sexpr) = check_expr var_table func_table k in
    let (v_type, v_sexpr) = check_expr var_table func_table v in
    if k_type != key_type then
      raise (Failure ("Key type mismatch. Expected " ^ string_of_typevar key_type ^ ", found " ^ string_of_typevar k_type))
    else if v_type != value_type then
      raise (Failure ("Value type mismatch. Expected " ^ string_of_typevar value_type ^ ", found " ^ string_of_typevar v_type))
    else acc @ [((k_type, k_sexpr), (v_type, v_sexpr))]
  ) [] dict_elems
        


and binop_return_type t1 op t2= 
  match op with
  | Add | Sub  ->
    (match (t1, t2) with 
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("float")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("float")
    | (_, _) -> raise (Failure "Invalid binop types"))
  | Eq -> 
    (match (t1, t2) with 
    | (TypeVariable("bool"), TypeVariable("bool")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("bool")
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("bool")
    | (_, _) -> raise (Failure "Invalid binop types"))
  | Neq | Less | More | Geq | Leq ->
    (match (t1, t2) with 
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("bool")
    | (_, _) -> raise (Failure "Invalid binop types"))
  | Mod | LShift | RShift -> 
    (match (t1, t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")
    | (_, _) -> raise (Failure "Invalid binop types"))
  | And | Or | Xor->
   ( match (t1, t2) with
    | (TypeVariable("bool"), TypeVariable("bool")) -> TypeVariable("bool")
    | (_, _) -> raise (Failure "Invalid binop types"))
  | BitAnd | BitOr | BitXor-> 
   ( match (t1, t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | Iden | Mult | FDiv ->
    ( match (t1, t2) with
    | (t1, t2) when t1 = t2 -> t1    
    | (_, _) -> raise (Failure "Invalid binop types"))
  | Div -> 
    ( match (t1,t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("float")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("float")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("float")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | Exp -> 
    ( match (t1, t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")   
    | (_, _) -> raise (Failure "Invalid binop types"))
  (* | _ -> raise (Failure "Unknown binop found") *)
 
and

 check_binop e1 operation e2 symbol_table func_table = 
  let (t1, e1') = check_expr symbol_table func_table e1 in
  let s1 = (t1, e1') in
  let (t2, e2') = check_expr symbol_table func_table e2 in
  let s2 = (t2, e2') in
  let rtype = binop_return_type t1 operation t2 in
  (rtype, SBinop(s1, operation, s2))

and

 assignment_to_bop special_assignment = 
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
  (* | _ -> raise (Failure "Unknown binop found") *)

and

  (*BlockAssign of variable * special_assignment * expr*)
 check_assign symbol_table func_table lvalue assign_type rvalue  =
    if (assign_type != IdentityAssign) then
      (let bop = assignment_to_bop assign_type in
      let lvalue_type = check_variable symbol_table lvalue in
      let binop_sexpr = check_binop (VarExpr(lvalue)) bop rvalue symbol_table func_table in 
      match binop_sexpr with 
      | (t1, _) -> if ( t1 <>  lvalue_type) then raise (Failure ("Improperly typed assignment, lvalue: " ^ string_of_typevar lvalue_type ^ ", rvalue: " ^ string_of_typevar t1))
      else 
          SBlockAssign(lvalue, SAssign, binop_sexpr))
    else
      (let bop = assignment_to_bop assign_type in
      let lvalue_type = check_variable symbol_table lvalue in
      let rvalue_sexpr = check_expr symbol_table func_table rvalue in 
      (* let binop_sexpr = check_binop (VarExpr(lvalue)) bop rvalue symbol_table func_table in  *)
      match rvalue_sexpr with 
      | (t1, _) -> if ( t1 <>  lvalue_type) then raise (Failure ("Improperly typed assignment, lvalue: " ^ string_of_typevar lvalue_type ^ ", rvalue: " ^ string_of_typevar t1))
      else 
          SBlockAssign(lvalue, SAssign, rvalue_sexpr))


and

check_func_types func_table func_name = 
    try StringMap.find func_name func_table
    with Not_found -> raise (Failure ("undeclared identifier " ^ func_name))

and

arg_list_helper symbol_table func_table params arg_types = 
  match (params, arg_types) with
  | ([], []) -> []
  | ((phead :: ptail), (thead :: ttail)) -> 
    ( let phead_sexpr = check_expr symbol_table func_table phead in
      match phead_sexpr with 
      | (t, _) -> if (string_of_typevar t) <> (string_of_typevar thead) then raise (Failure ("Improperly typed parameter, should be: " ^ string_of_typevar t ^ " got: " ^ string_of_typevar thead))
      else (phead_sexpr :: arg_list_helper symbol_table func_table ptail ttail))
  | (_, _) -> raise (Failure "Wrong number of arguments")

and

check_func_call symbol_table func_table func_name params = 
  let func_tuple = check_func_types func_table (string_of_var func_name) in
  match func_tuple with 
  | (return_type, arg_types) -> (return_type, func_name, arg_list_helper symbol_table func_table params arg_types)
and 

variable_declaration_helper symbol_table func_table var vartype =
  if StringMap.mem (string_of_var var) symbol_table
  then raise (Failure ("Variable " ^ string_of_var var  ^ " already declared"))
  else 
  if StringMap.mem (string_of_var var) func_table
    then raise (Failure ("Variable " ^ string_of_var var  ^ " already declared as function"))
  else
    StringMap.add (string_of_var var) vartype symbol_table 
    
and
    
arg_to_type (arg_types: (variable * typevar)) =
  let (_,t) = arg_types in
    t and



function_declaration_helper symbol_table func_table func_name return_type args =
  if StringMap.mem (string_of_var func_name) func_table
  then raise (Failure ("Function " ^ (string_of_var func_name)  ^ " already declared"))
  else 
  if StringMap.mem (string_of_var func_name) symbol_table
    then raise (Failure ("Function " ^ (string_of_var func_name)  ^ " already declared as function"))
  else
    StringMap.add (string_of_var func_name) (return_type, List.map arg_to_type args) func_table

(* check_block takes the following args: *)

(* allowed_block_set: a StringSet indicating whether a particular block type can be used
   (only used for Break / Continue / Pass / ReturnVal / ReturnVoid) *)
(* var_table contains a key of the variable name which points to a typevar *)
(* func_table contains a key of the function name which points to a tuple of a list of args and the return type *)
(* func_return_type: typevar option for the return type of the current function (None if we aren't in a function) *)
(* and the Ast block type *)

and
(* Return list of sblocks given blocks *)
check_block_list allowed_block_set var_table func_table func_return_type = function
  | [] -> []
  | x :: xs -> let (v_t, f_t, new_block_set, new_func_return_type, sblock) = check_block allowed_block_set var_table func_table func_return_type x in sblock :: check_block_list new_block_set v_t f_t new_func_return_type xs

(* It returns a tuple of the var_table * func_table * Sast block *)
and add_func_params_to_var_table var_table func_table arg_list = (match arg_list with
  | [] -> var_table
  | x :: xs -> let (var_name, typ) = x in let new_var_table = (variable_declaration_helper var_table func_table var_name typ) in add_func_params_to_var_table new_var_table func_table xs)
  
  
and check_block allowed_block_set var_table func_table func_return_type = function
  BlockAssign(v, s, e) -> (var_table, func_table, allowed_block_set, func_return_type, check_assign var_table func_table v s e)
  | Break -> 
    (match StringSet.find_opt "Break" allowed_block_set with
    | Some _ -> (var_table, func_table, allowed_block_set, func_return_type, SBreak)
    | None -> raise (Failure ("Break must be specified within a loop.")))
  | Continue ->     
    (match StringSet.find_opt "Continue" allowed_block_set with
  | Some _ -> (var_table, func_table, allowed_block_set, func_return_type, SContinue)
  | None -> raise (Failure ("Continue must be specified within a loop.")))
  | Pass -> (var_table, func_table, allowed_block_set, func_return_type, SPass)
  | ReturnVal(expr) -> 
    if (func_return_type = None) then raise (Failure ("Return must be specified within a function.")) else
    (match StringSet.find_opt "ReturnVal" allowed_block_set with
    | None -> raise (Failure ("Return must be specified within a function.")) 
    | Some _ -> 
      (let (t, e) = check_expr var_table func_table expr in 
      (match func_return_type with 
      | None -> raise (Failure "ReturnVal statement not in a function")
      | Some func_return_type ->
        if t = func_return_type then (var_table, func_table, allowed_block_set, Some(func_return_type), SReturnVal(t, e))
        else raise (Failure ("Expected return type " ^ (string_of_typevar func_return_type) ^ " got " ^ (string_of_typevar t))))))
  | ReturnVoid -> 
    if (func_return_type = None) then raise (Failure ("Return must be specified within a function.")) else
    (match StringSet.find_opt "ReturnVoid" allowed_block_set with
    | None -> raise (Failure ("Return statement not in a function."))
    | Some _ -> 
      if func_return_type = Some(TypeVariable("void")) then (var_table, func_table, allowed_block_set, func_return_type, SReturnVoid)
      else raise (Failure ("Expected no return expression in a void function")))
  | VarDec(typ, var, expr) -> 
    let (t, e) = check_expr var_table func_table expr in
    if t = typ then (
    let new_var_table = variable_declaration_helper var_table func_table var typ in
    let rvalue_sexpr = check_expr var_table func_table expr in
    let var_dec_to_return = SVarDec(typ, var, rvalue_sexpr) in 
    (new_var_table, func_table, allowed_block_set, func_return_type, var_dec_to_return)
    ) else raise (Failure ("Type mismatch, LHS: " ^ string_of_typevar typ ^ " RHS: " ^ string_of_typevar t))

  | While(loop_expr, loop_block_list) -> 
    (* Make sure the expression is a bool type, 
       then recurse in the block list w/ breaks + continues + passes *)
      let loop_sexpr = check_expr var_table func_table loop_expr in
      (match loop_sexpr with
      | (TypeVariable("bool"), loop_sx) ->
      let new_allowed_block_set = StringSet.add "Break" (StringSet.add "Continue" allowed_block_set) in 
      let sblock_list = check_block_list new_allowed_block_set var_table func_table func_return_type loop_block_list in
      (var_table, func_table, allowed_block_set, func_return_type, SWhile(loop_sexpr, sblock_list))
      | _ -> raise (Failure ((string_of_expr loop_expr) ^ "is not of type bool.")))

  | For(loop_var, loop_expr, loop_block_list) -> 
    (* Make sure the loop_expr is a list wrapper type, 
       then sequentially eval the block list w/ breaks + continues + passes + loop_var in scope *)
    let loop_sexpr = check_expr var_table func_table loop_expr in
    (match loop_sexpr with 
      | (List(sub_type), loop_sx) ->
      let new_var_table = StringMap.add (string_of_var loop_var) sub_type var_table in
      let new_allowed_block_set = StringSet.add "Break" (StringSet.add "Continue" allowed_block_set) in
      let sblock_list = check_block_list new_allowed_block_set new_var_table func_table func_return_type loop_block_list in
      (var_table, func_table, allowed_block_set, func_return_type, SFor(loop_var, loop_sexpr, sblock_list))
      | _ -> raise (Failure ((string_of_expr loop_expr) ^ " is not a list type."))) 
  | FuncBlockCall(fname, args) -> 
    let (_, func_name, sexpr_list) = check_func_call var_table func_table fname args in
    (var_table, func_table, allowed_block_set, func_return_type, SFuncBlockCall(func_name, sexpr_list))
  (* | FunctionSignature *) 
  | FunctionDefinition(func_sig, block_list) -> 
    let (variable, arg_list, return_type) = func_sig in
    let new_func_table = function_declaration_helper var_table func_table variable return_type arg_list in
    let new_var_table = add_func_params_to_var_table var_table func_table arg_list in 
    let new_allowed_block_set = 
      (if return_type = TypeVariable("void") then StringSet.add "ReturnVoid" allowed_block_set
      else StringSet.add "ReturnVal" allowed_block_set) in
    let sblock_list = check_block_list new_allowed_block_set new_var_table new_func_table (Some(return_type)) block_list in
    (var_table, new_func_table, allowed_block_set, func_return_type, SFunctionDefinition(func_sig, sblock_list))
    
  | IfEnd(expr, block_list) -> 
    let sexpr = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type block_list in
    (var_table, func_table, allowed_block_set, func_return_type, SIfEnd(sexpr, sblock_list))

  | IfNonEnd(expr, block_list, block) ->
    let sexpr = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type block_list in
    let (_, _, _, _, sblock) = check_block allowed_block_set var_table func_table func_return_type block in
    (var_table, func_table, allowed_block_set, func_return_type, SIfNonEnd(sexpr, sblock_list, sblock))

  | ElifEnd(expr, block_list) -> 
    let sexpr = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type block_list in
    (var_table, func_table, allowed_block_set, func_return_type, SElifEnd(sexpr, sblock_list))

  | ElifNonEnd(expr, block_list, block) ->
    let sexpr = check_expr var_table func_table expr in
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type block_list in
    let (_, _, _, _, sblock) = check_block allowed_block_set var_table func_table func_return_type block in
    (var_table, func_table, allowed_block_set, func_return_type, SElifNonEnd(sexpr, sblock_list, sblock))

  | ElseEnd(block_list) ->
    let sblock_list = check_block_list allowed_block_set var_table func_table func_return_type block_list in
    (var_table, func_table, allowed_block_set, func_return_type, SElseEnd(sblock_list))

  | _ -> raise (Failure ("Unsupported block type"))
(*| InterfaceDefinition
| ClassDefinition
| ClassDefinitionImplements *)



and check block_list =  let builtin_entries =
  [
    ("print_bool", (TypeVariable "void", [TypeVariable "bool"]));
    ("print_float", (TypeVariable "void", [TypeVariable "float"]));
    ("print_int", (TypeVariable "void", [TypeVariable "int"]));
  ] 
  in


  let add_entry map (key, value) =
    StringMap.add key value map 
  
  in
  
  let builtin_map =
    List.fold_left add_entry StringMap.empty builtin_entries
  in check_block_list StringSet.empty StringMap.empty builtin_map None block_list

