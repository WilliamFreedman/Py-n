
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (block_list) = 
  let context = L.global_context () in
  let the_module = L.create_module context "Pyn" in

  let int_t = L.i32_type context
  and float_t = L.double_type context
  and bool_t = L.i1_type context
  and char_t = L.i8_type context in
  
  let char_pt = L.pointer_type char_t in
  
  let printf_t : L.lltype =
    L.var_arg_function_type int_t [| L.pointer_type char_t |] 
  in 
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
  let rec ltype_of_typ = function
    A.TypeVariable("int") -> int_t
    | A.TypeVariable("bool")  -> bool_t
    | A.TypeVariable("float") -> float_t
    | A.TypeVariable("string") -> char_pt
    | _ -> raise (Failure (" Not implemented yet"))

  and

  (* Assigns an expression (rvalue's) llvalue to the variable's llvalue (w/ name var_name) *)
  (* Returns a builder! *)
  variable_assignment_helper var_map func_map var_name rvalue builder =
    let var_llvalue = StringMap.find var_name var_map in
    let rvalue_llvalue = build_expr var_map func_map builder rvalue in
    ignore(L.build_store rvalue_llvalue var_llvalue builder); builder

  and

  (* Declares a variable on the stack using alloca *)
  (* Also evaluates the assigned expression using var_assignment_helper *)
  (* Updates and returns the var_map with the new var_name -> llvalue mapping *)
  variable_declaration_helper var_map func_map var_type var_name rvalue builder =
    let var_llvalue = L.build_alloca (ltype_of_typ var_type) var_name builder in
    let new_var_map = StringMap.add var_name var_llvalue var_map in
    ignore(variable_assignment_helper new_var_map func_map var_name rvalue builder); new_var_map

  and
  
  (*
  let function_definition_helper = function_name return_type param_list func_body map builder context module_name =
    let params_array = (Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) param_list)) in
    let params_array = (Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) param_list)) in
    let new_function_type = L.function_type (ltype_of_typ return_type) params_array in
    let new_function = L.declare_function function_name new_function_type module_name in
    let new_map = List.fold_left (fun m (k, v) -> StringMap.add k v m) map param_list in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    
    variable_declaration_helper var_type var_name rvalue builder in

    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    *)

    (*
    let params_array = (Array.of_list (List.map ltype_of_typ arg_types_list))

    let new_function_type = L.function_type (ltype_of_typ return_type) params_array in
    let new_function = L.declare_function function_name new_function_type module_name in
    let end_of_block = L.position_at_end entry_block builder in

    let arg_values = List.map (fun a -> L.param new_function a) arg_types_list in

    List.map (build_code builder )
    *)
    

  (* we only do division with floating points *)
  (* look at the types of e1 and e2, call L.build_sitofp on the llvalues if they're ints, and then do floating point division on these new llvalues *)
  (* div_helper e1 e2 name builder = 
      let (t1, e1x) = e1 and (t2, e2x) = e2 in

      (* let e1' = (if t1 = A.TypeVariable then (L.build_sitofp e1 float_t name builder) else e1 in
      let e2' = if ()
         ))*)
      if t1 = A.TypeVariable("int") then let e1' = (L.build_sitofp e1 float_t name builder)
      else let e1' = e1 in
      if t2 = A.TypeVariable("int") then let e2' = (L.build_sitofp e2 float_t name builder)
      else let e2' = e2 in *)
      (* L.build_fdiv e1' e2' name builder *)

    (* div_helper e1 e2 name builder = 
      let (t1, e1x) = e1 and (t2, e2x) = e2 in
      let e1' = 
        if t1 = A.TypeVariable("int") then (L.build_sitofp e1x float_t name builder)
      else 
        e1 in
      let e2' = 
        if t2 = A.TypeVariable("int") then (L.build_sitofp e2x float_t name builder)
      else 
        e2 in
      L.build_fdiv e1' e2' name builder      
    and      

  (* floor division - we do regular floating point division and convert the result into a signed integer *)
  fdiv_helper e1 e2 name builder = 
    let result_llvalue = div_helper e1 e2 name builder in
    L.build_fptosi result_llvalue int_t name builder
  and *)

  (*this might not work*)
  (* exp_helper e1 e2 name builder =
    let float_t = double_type (context builder) in
    let (t1, e1x) = e1 and (t2, e2x) = e2 in
    let e1' =
      if t1 = TypeVariable("int") then
        build_sitofp e1 float_t name builder
      else e1x
    in
    let e2' =
      (if t2 = TypeVariable("int") then
        build_sitofp e2 float_t name builder
      else e2x)
    in
    let rec pow base exp acc =
      if exp = 0 then acc
      else pow base (exp - 1) (build_fmul base acc name builder)
    in
    let result = pow e1' e2' (const_float float_t 1.0) in
    result
  
  and *)

  add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore (instr builder) 
  
  and



  build_expr var_map func_map builder ((_, e) : sexpr) = match e with
      SIntLit i -> L.const_int int_t i
    | SBoolLit b -> L.const_int bool_t (if b then 1 else 0)
    | SFloatLit f -> L.const_float float_t f
    (* | SStringLit s -> TODO *)
    | SVarExpr(Var(v)) -> L.build_load (StringMap.find v var_map) v builder
    | SBinop (e1, op, e2) ->
        let e1' = build_expr var_map func_map builder e1
        and e2' = build_expr var_map func_map builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> L.build_fdiv
          (* div_helper  *)
         | A.FDiv    ->  raise (Failure (" Not implemented yet"))

          (* fdiv_helper *)
         (* | A.Exp -> I think we need to manually do this *)
         | A.Mod     -> L.build_srem
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Xor     -> L.build_xor
         | A.Eq   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Geq  -> L.build_icmp L.Icmp.Sge
         | A.Leq  -> L.build_icmp L.Icmp.Sle
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.LShift -> L.build_shl
         | A.RShift -> L.build_ashr
         | A.BitAnd -> L.build_and
         | A.BitOr -> L.build_or
         | A.BitXor -> L.build_xor
         | _ as c-> raise (Failure ("Binop error " ^ A.string_of_op c))
        ) e1' e2' "tmp" builder
    (* | SList l -> todo *)
    (* | SDict d ->  todo *)
    (* | SListCompUnconditional lc ->  
    | SListCompConditional lc -> 
    | SDictCompUnconditional dc -> 
    | SDictCompConditional dc ->  *)
    | SWalrus(var, expr) -> raise (Failure (" Walrus Not implemented yet"))
    | SFuncCall (var, expr) -> 
      (* let (fdef, fast) = StringMap.find *)
      raise (Failure ("Not implemented yet"))
    (* | SIndexingExprList *)
    | SIndexingStringLit(e1, e2) -> raise (Failure (" Not implemented yet"))
    and 

  (* Takes in a var_map, func_map, builder, and block and returns new var_map, func_map builder *)
  (* var_map is a mapping from variable names (strings) to their corresponding llvalues *)
  (* func_map is a mapping from function names (strings) to their corresponding llvalues *)
  build_block var_map func_map builder cur_function = function
    SBlockAssign(Var(var_name), s_assign, s_expr) ->
      (* Evaluate the expression and store it in the variable *)
      (* raise (Failure("in sblock assign")) *)
      (var_map, func_map, (variable_assignment_helper var_map func_map var_name s_expr builder))

    | SBreak -> raise (Failure (" Not implemented yet"))
    | SContinue -> raise (Failure (" Not implemented yet"))
    | SPass -> raise (Failure (" Not implemented yet"))

    | SVarDec (typ, Var(var_name), s_expr) -> 
      (* Declare the variable, evaluate the expression and store it in the variable *)
      (* define_global is never used, at least for now *) 
      let new_var_map = variable_declaration_helper var_map func_map typ var_name s_expr builder in
      (new_var_map, func_map, builder)

    | SReturnVal(sexpr) -> raise (Failure (" Not implemented yet"))
    | SReturnVoid -> raise (Failure (" Not implemented yet"))
    | SWhile(condition, block_list) -> raise (Failure (" Not implemented yet"))
    | SFor(svariable, sexpr, block_list) -> raise (Failure (" Not implemented yet"))
    | SFuncBlockCall (Var("print_int"), args) -> ignore(L.build_call printf_func [| (L.build_global_stringptr "%d\n" "fmt" builder) ; (build_expr var_map func_map builder (List.hd args)) |]
      "print_int" builder); (var_map, func_map, builder)
    | SFuncBlockCall(svariable, args) -> raise (Failure (" googoo ahaha"))
    (* | SFunctionSignature of sfunction_signature -> *)
    | SFunctionDefinition (sfunction_signature, block_list) -> raise (Failure (" Not implemented yet"))
    (* | SInterfaceDefinition of svariable * sfunction_signature list -> *)
    (* | SClassDefinition of svariable * sblock list -> *)
    (* | SClassDefinitionImplements of svariable * svariable list * sblock list -> *)
    (* | SIfEnd(condition, block_list) | SElifEnd(condition, block_list)->
      let bool_val = build_expr var_map func_map builder condition in
      let then_bb = L.append_block context "then" cur_function in
      let (new_var_map, new_func_map, _) = (build_block_list var_map func_map (L.builder_at_end context then_bb) cur_function block_list) in

      let end_bb = L.append_block context "if_end" cur_function in
      let build_br_end = L.build_br end_bb in (* partial function *)
      add_terminal (L.builder_at_end context then_bb) build_br_end;
      
      ignore(L.build_cond_br bool_val then_bb end_bb builder);
      (new_var_map, new_func_map, L.builder_at_end context end_bb)
    | SIfNonEnd(condition, block_list, else_block) | SElifNonEnd(condition, block_list, else_block) ->
      let bool_val = build_expr var_map func_map builder condition in
      let then_bb = L.append_block context "then" cur_function in
      ignore (build_block_list var_map func_map (L.builder_at_end context then_bb) cur_function block_list);
      let else_bb = L.append_block context "else" cur_function in
      ignore (build_block var_map func_map (L.builder_at_end context else_bb) cur_function else_block);

      let end_bb = L.append_block context "if_end" cur_function in
      let build_br_end = L.build_br end_bb in (* partial function *)
      ignore (add_terminal (L.builder_at_end context then_bb) build_br_end);
      (add_terminal (L.builder_at_end context else_bb) build_br_end);
            
      (* (match else_block with 
      | SElifNonEnd -> build_block var_map func_map builder cur_function else_block
      | SElifEnd -> build_block var_map func_map builder cur_function else_block
      | SElseEnd -> ) *)
    | SElseEnd(block_list) ->
      let then_bb = L.append_block context "then" cur_function in
      ignore (build_block_list var_map func_map (L.builder_at_end context then_bb) cur_function block_list); *)
  and

      

  build_block_list var_table func_table builder cur_function = function 
    | [] -> []
    | x :: xs -> let (new_var_table, new_func_table, new_builder) = build_block var_table func_table builder cur_function x in build_block_list new_var_table new_func_table new_builder cur_function xs
    in 
  
  (* Create a "main" function that all code will be inside *)
  let prog_func = L.define_function "main" (L.function_type (L.void_type context) [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block prog_func) in

  build_block_list StringMap.empty StringMap.empty builder None block_list;
  L.build_ret_void builder;
  the_module