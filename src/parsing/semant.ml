(*TODO: add binops to scanner, no-op binop*)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (blocks) =

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

  (* Collect function declarations for built-in functions: no bodies *)
  (* let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(Int, "x")];
      locals = []; body = [] } StringMap.empty
  in *)

  (* Add function name to symbol table *)
  let add_func map fd =
    let (fname, args, ret_type) = fd in
    let built_in_err = "function " ^ fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fname
    and make_err er = raise (Failure er)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
     (n, _, _) when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

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
    let class_details s class_table =
      try StringMap.find s class_table
      with Not_found -> raise (Failure ("undeclared class " ^ s))
    in

    List.fold_left (fun ) (StringMap.empty StringMap.empty StringMap.empty)
    let rec check_block var_table func_table class_table = function
      BlockAssign(v, s, e) -> SBlockAssign(check_expr v, s, e)
      | Break
      | Continue
      | Pass
      | VarDec
      | ReturnVal
      | ReturnVoid
      | While
      | For
      | FuncBlockCall
      | FunctionSignature
      | FunctionDefinition
      | InterfaceDefinition
      | ClassDefinition
      | ClassDefinitionImplements
      | IfEnd
      | IfNonEnd
      | ElifEnd
      | ElifNonEnd
      | ElseEnd

    (* Function table contains a key of the function name which points to a tuple of a list of args and the return type *)
    (* Variable table contains a key of the variable name which points to a typevar *)
    (* Class table contains a key of the class name which points to a tuple of a function table and variable table *)
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr var_table func_table class_table = function
        IntLit l -> (TypeVariable("int"), SIntLit l)
      | BoolLit l -> (TypeVariable("bool"), SBoolLit l)
      | StringLit l -> (TypeVariable("string") , SStringLit l)
      (*| SVarExpr var -> (check the type of var, SVarExpr (convert var to svar type))*)
      | VarExpr var -> match var with
        | Var(v) -> (StringMap.find v var_table, SVar v)
        | VarDot(v1, v2) -> let (v1_typevar, v1_sem) = check_expr v1 ar_table func_table class_table in
          let (_, var_table) = class_details (string_of_typevar v1_typevar) class_table in 
          let typ = StringMap.find v2 var_table
          in (typ, SVarDot((v1_typevar, v1_sem), check_expr v2 var_table func_table class_table))
        | VarIndex(v1, e) -> let (t, sx) = check_expr v1 var_table func_table class_table in match t with
          | Dict(key_type, value_type) ->
          let (typvar, _) = check_expr e var_table func_table class_table in 
          if key_type = typvar then (value_type, SVarIndex(t, sx))
          else raise (Failure ("Invalid dictionary indexing"))
          | List(list_type) ->
          let (typvar, _) = check_expr e var_table func_table class_table in 
          if typvar = TypeVariable("int") then (list_type, SVarIndex(t, sx))
          else raise (Failure ("Invalid indexing: " ^ string_of_var VarIndex(v1, e)))
      | List (e) -> list_check e symbol_table func_table
      | ListCompUnconditional (expr, loop_var, loop_list) -> 
        let (loop_type, _) = check_expr symbol_table func_table class_table loop_list in
        match loop_type with
        List (elem_type) -> (** Only doing lists **)
          let (expr_type, _) = check_expr (StringMap.add loop_var elem_type symbol_table) func_table class_table expr in
          (List(expr_type), SListCompUnconditional(check_expr expr, check_expr symbol_table loop_var, check_expr symbol_table loop_list))
        _ -> raise (Failure ("List comprehension depends on non-list object"))
      | ListCompConditional (expr, loop_var, loop_list, condition) -> 
        let (loop_type, _) = check_expr symbol_table  func_table class_table loop_list in
        match loop_type with
        List (elem_type) -> (** Only doing lists **)
          let (expr_type, _) = check_expr (StringMap.add loop_var elem_type symbol_table) func_table class_table expr in
          let (condition_type, _) = check_expr (StringMap.add loop_var elem_type symbol_table) func_table class_table condition in
          if (condition_type != TypeVariable("bool")) then raise (Failure ("List comprehension condition depends on non-boolean object"))
          else
          (List(expr_type), SListCompConditional(check_expr expr, check_expr symbol_table loop_var, check_expr symbol_table loop_list, check_expr symbol_table condition))
        _ -> raise (Failure ("List comprehension depends on non-list object"))
      | Id var -> (type_of_identifier var, SId var)
      | Walrus(var, e) as ex ->
        let lt = type_of_identifier var symbol_table
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))
      | Binop(e1, op, e2) as e -> binop_check e1 op e2 symbol_table func_table class_table: StringMap
      | FuncCall(fname, args) as call ->
      (* fname can be just a string (in which case we look up func_table)
      VarDot(variable * variable) ?
          *)

        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in
  (globals, List.map check_func functions)

let rec list_to_sexpr (list_elems: expr list) (symbol_table: StringMap) (func_table: StringMap) (head_type: typevar) (list_so_far: sexpr_list) = 
  match list_elems with
  | [] -> list_so_far
  | head :: tail -> 
    let head_sexpr = check_expr symbol_table func_table class_table head in
    match head_sexpr with
    (t,_) -> 
      (if (t != head_type) then raise (Failure "Type mismatch in list")
      else
        list_to_sexpr tail symbol_table func_table func_table head_type (list_so_far :: head_sexpr))
in
  (*will return an option type, the calling function should check if its None because thnen any type is valid*)
let check_list (list_elems: expr list) (symbol_table: StringMap) (func_table: StringMap) = 
  match list_elems with
  |[] -> return None
  | head :: tail -> 
      let head_sexpr = check_expr symbol_table func_table class_table head in
        match head_sexpr with
        (t,_) -> SList(list_to_sexpr tail symbol_table func_table t [head_sexpr]);;

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
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("bool")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("bool")
    | (TypeVariable("int"), TypeVariable("float")) -> TypeVariable("bool")
  | Mod | LShif | RShift ->    
    (match (t1,t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("int")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | And | Or ->
   ( match (t1,t2) with
    | (TypeVariable("bool"), TypeVariable("bool")) -> true
    | (_,_) -> raise (Failure "Invalid binop types"))
  | BitAnd | BitOr | BitXor-> 
   ( match (t1,t2) with
    | (TypeVariable("int"), TypeVariable("int")) -> TypeVariable("int")
    | (TypeVariable("float"), TypeVariable("float")) -> TypeVariable("float")
    | (_,_) -> raise (Failure "Invalid binop types"))
  | _ -> raise (Failure "Unknown binop found") in

let check_binop (e1: expr) (operation: bop) (e2: expr) (symbol_table: StringMap) (func_table: StringMap) (class_table: StringMap) = 
  let (t1,_) = check_expr symbol_table func_table class_table e1 in
  let s1 = SExpr(t1,e1) in
  let (t2,_) = check_expr symbol_table func_table class_table e2 in
  let s2 = SExpr(t2,e2) in
  let rtype = binop_return_type t1 bop t2 in
  SBinop(s1,bop,s2);;


let assignment_to_bop special_assignment = 
  match special_assignment with 
  | IdentityAssign -> (*TODO: add a no-op binop*)
  | PlusAssign -> Add
  | MinusAssign -> Sub
  | TimesAssign -> (*TODO: add a mult binop*)
  | DivideAssign -> (*TODO: add a div binop*)
  | FloorDivAssign -> (*TODO: add a floor div binop*)
  | ExpAssign -> (*TODO: add a exp binop*)
  | AndAssign -> And
  | OrAssign -> Or
  | XorAssign -> (*TODO: add a xor binop*)
  | RShiftAssign -> RShift
  | LShiftAssign -> LShift
  | ModAssign -> Mod
  | _ -> raise (Failure "Unknown binop found");;


  (*BlockAssign of variable * special_assignment * expr*)
  let check_assign (lvalue: variable) (assign_type: special_assignment) (rvalue: expr) (symbol_table: StringMap) (func_table: StringMap) (class_table: StringMap) =
    let bop = assignment_to_bop assign_type in
    let rvalue_sexpr = check_expr symbol_table func_table class_table rvalue in
    let lvalue_sexpr = check_variable symbol_table func_table class_table lvalue in
    let binop_sexpr = check_binop lvalue bop rvalue symbol_table func_table class_table in
    match binop_sexpr with 
    | (t1,_) -> (match lvalue_sexpr with 
    | (t2,_) -> if (t1 != t2) then raise (Failure "Improperly typed assignment")
     else
        SBlockAssign(lvalue_sexpr, SAssign, binop_sexpr));;