module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let translate block_list =
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
    L.declare_function "printf" printf_t the_module
  in
  let rec ltype_of_typ = function
    | A.TypeVariable "int" -> int_t
    | A.TypeVariable "bool" -> bool_t
    | A.TypeVariable "float" -> float_t
    | A.TypeVariable "string" -> char_pt
    | _ -> raise (Failure " Not implemented yet")
  and
      (* Assigns an expression (rvalue's) llvalue to the variable's llvalue (w/ name var_name) *)
      (* Returns a builder! *)
      variable_assignment_helper var_map func_map var_name rvalue builder =
    let var_llvalue = StringMap.find var_name var_map in
    let rvalue_llvalue = build_expr var_map func_map builder rvalue in
    ignore (L.build_store rvalue_llvalue var_llvalue builder);
    builder
    (*
    let rvalue_llvalue = build_expr var_map func_map builder rvalue in
    ignore(L.build_store rvalue_llvalue var_llvalue builder); builder
    *)
  and
      (* Declares a variable on the stack using alloca *)
      (* Also evaluates the assigned expression using var_assignment_helper *)
      (* Updates and returns the var_map with the new var_name -> llvalue mapping *)
      variable_declaration_helper var_map func_map var_type var_name rvalue
      builder =
    let var_llvalue = L.build_alloca (ltype_of_typ var_type) var_name builder in
    let new_var_map = StringMap.add var_name var_llvalue var_map in
    ignore
      (variable_assignment_helper new_var_map func_map var_name rvalue builder);
    new_var_map
  and function_definition_helper function_name return_type param_list func_body
      var_map func_map block_map original_builder =
    let params_array =
      Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) param_list)
    in
    let new_function_type =
      L.function_type (ltype_of_typ return_type) params_array
    in

    let new_function =
      L.define_function function_name new_function_type the_module
    in
    let builder = L.builder_at_end context (L.entry_block new_function) in

    (* Take function arguments and add them to the var map *)
    let add_args map (var, sast_type) value =
      let name = string_of_svar var in
      L.set_value_name name value;
      let local = L.build_alloca (ltype_of_typ sast_type) name builder in
      ignore (L.build_store value local builder);
      StringMap.add name local map
    in
    (* Add function arg names -> their llvalues to the var_map, getting new_var_map *)
    let new_var_map =
      List.fold_left2 add_args var_map param_list
        (Array.to_list (L.params new_function))
    in

    (* Add function to function map so that we can also call it recursively *)
    let new_func_map = StringMap.add function_name new_function func_map in

    (* Take function arguments and actually do stuff with them, go block by block in the func body*)
    let final_var_map, final_func_map, new_builder =
      build_block_list new_var_map new_func_map builder new_function block_map
        func_body
    in

    (var_map, final_func_map, original_builder)
  and add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  and build_expr var_map func_map builder ((_, e) : sexpr) =
    match e with
    | SIntLit i -> L.const_int int_t i
    | SBoolLit b -> L.const_int bool_t (if b then 1 else 0)
    | SFloatLit f -> L.const_float float_t f
    (* | SStringLit s -> TODO *)
    | SVarExpr (Var v) -> L.build_load (StringMap.find v var_map) v builder
    | SBinop (e1, op, e2) ->
        let e1' = build_expr var_map func_map builder e1
        and e2' = build_expr var_map func_map builder e2 in
        if op <> A.Div then
          (match op with
          | A.Add -> L.build_add
          | A.Sub -> L.build_sub
          | A.Mult -> L.build_mul
          | A.Div ->
              if L.type_of e1' = float_t && L.type_of e2' = float_t then
                L.build_fdiv
              else if L.type_of e1' = int_t && L.type_of e2' = int_t then
                L.build_sdiv
              else raise (Failure " Not implemented yet") (* div_helper  *)
          | A.FDiv -> raise (Failure " Not implemented yet")
          (* fdiv_helper *)
          (* | A.Exp -> I think we need to manually do this *)
          | A.Mod -> L.build_srem
          | A.And -> L.build_and
          | A.Or -> L.build_or
          | A.Xor -> L.build_xor
          | A.Eq -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | A.Geq -> L.build_icmp L.Icmp.Sge
          | A.Leq -> L.build_icmp L.Icmp.Sle
          | A.Less -> L.build_icmp L.Icmp.Slt
          | A.More -> L.build_icmp L.Icmp.Sgt
          | A.LShift -> L.build_shl
          | A.RShift -> L.build_ashr
          | A.BitAnd -> L.build_and
          | A.BitOr -> L.build_or
          | A.BitXor -> L.build_xor
          | _ as c -> raise (Failure ("Binop error " ^ A.string_of_op c)))
            e1' e2' "tmp" builder
        else if L.type_of e1' = float_t && L.type_of e2' = float_t then
          L.build_fdiv e1' e2' "tmp" builder
        else if L.type_of e1' = int_t && L.type_of e2' = int_t then
          let e1f = L.build_sitofp e1' float_t "c1" builder in
          let e2f = L.build_sitofp e2' float_t "c2" builder in
          L.build_fdiv e1f e2f "tmp" builder
        else if L.type_of e1' = int_t then
          let e1f = L.build_sitofp e1' float_t "c1" builder in
          L.build_fdiv e1f e2' "tmp" builder
        else
          let e2f = L.build_sitofp e2' float_t "c2" builder in
          L.build_fdiv e1' e2f "tmp" builder
    (* | SList l -> todo *)
    (* | SDict d ->  todo *)
    (* | SListCompUnconditional lc ->
       | SListCompConditional lc ->
       | SDictCompUnconditional dc ->
       | SDictCompConditional dc -> *)
    | SWalrus (var, expr) -> raise (Failure " Walrus Not implemented yet")
    | SFuncCall (svariable, args) ->
        (* let (fdef, fast) = StringMap.find *)
        let func_ll = StringMap.find (string_of_svar svariable) func_map in
        let llargs =
          List.rev
            (List.map (build_expr var_map func_map builder) (List.rev args))
        in
        let result = string_of_svar svariable ^ "_result" in
        L.build_call func_ll (Array.of_list llargs) result builder
    (* | SIndexingExprList *)
    | SIndexingStringLit (e1, e2) -> raise (Failure " Not implemented yet")
    | _ -> raise (Failure " Not implemented yet")
  and
      (* Takes in a var_map, func_map, builder, and block and returns new var_map, func_map builder *)
      (* var_map is a mapping from variable names (strings) to their corresponding llvalues *)
      (* func_map is a mapping from function names (strings) to their corresponding llvalues *)
      (* block_map is a mapping from special block type (SContinue, SBreak, conditional statements) to llbasicblock *)
      (* in the case of elif / else statements, block_map maps 'if_end' to llbasicblock for the end of the original if *)
      build_block var_map func_map builder cur_function block_map = function
    | SBlockAssign (Var var_name, s_assign, s_expr) ->
        (* Evaluate the expression and store it in the variable *)
        (* raise (Failure("in sblock assign")) *)
        ( var_map,
          func_map,
          variable_assignment_helper var_map func_map var_name s_expr builder )
    | SBreak -> (
        try
          let block = StringMap.find "break" block_map in
          ignore (L.build_br block builder);
          (var_map, func_map, builder)
        with Not_found ->
          raise (Failure "Break cannot be placed outside of a loop"))
    | SContinue -> (
        try
          let block = StringMap.find "continue" block_map in
          ignore (L.build_br block builder);
          (var_map, func_map, builder)
        with Not_found ->
          raise (Failure "Continue cannot be placed outside of a loop"))
    | SPass -> (var_map, func_map, builder)
    | SVarDec (typ, Var var_name, s_expr) ->
        (* Declare the variable, evaluate the expression and store it in the variable *)
        (* define_global is never used, at least for now *)
        let new_var_map =
          variable_declaration_helper var_map func_map typ var_name s_expr
            builder
        in
        (new_var_map, func_map, builder)
    | SReturnVal sexpr ->
        let ret_value = build_expr var_map func_map builder sexpr in
        ignore (L.build_ret ret_value builder);
        (var_map, func_map, builder)
    | SReturnVoid ->
        ignore (L.build_ret_void builder);
        (var_map, func_map, builder)
    | SWhile (condition, block_list) ->
        (* raise (Failure (" Not implemented yet")) *)
        let while_bb = L.append_block context "while" cur_function in
        let build_br_while = L.build_br while_bb in
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr var_map func_map while_builder condition in

        let body_bb = L.append_block context "while_body" cur_function in

        (* add_terminal (build_block_list var_map func_map (L.builder_at_end context body_bb) cur_function block_list) build_br_while; *)
        let end_bb = L.append_block context "while_end" cur_function in
        let new_block_map =
          StringMap.add "break" end_bb
            (StringMap.add "continue" while_bb block_map)
        in
        let _, _, new_builder =
          build_block_list var_map func_map
            (L.builder_at_end context body_bb)
            cur_function new_block_map block_list
        in
        add_terminal new_builder build_br_while;

        ignore (L.build_cond_br bool_val body_bb end_bb while_builder);
        (var_map, func_map, L.builder_at_end context end_bb)
    | SFor (svariable, sexpr, block_list) ->
        raise (Failure " Not implemented yet")
    | SFuncBlockCall (Var "print_int", args) ->
        ignore
          (L.build_call printf_func
             [|
               L.build_global_stringptr "%d\n" "fmt" builder;
               build_expr var_map func_map builder (List.hd args);
             |]
             "print_int" builder);
        (var_map, func_map, builder)
    | SFuncBlockCall (Var "print_float", args) ->
        ignore
          (L.build_call printf_func
             [|
               L.build_global_stringptr "%f\n" "fmt" builder;
               build_expr var_map func_map builder (List.hd args);
             |]
             "print_float" builder);
        (var_map, func_map, builder)
    | SFuncBlockCall (Var "print_bool", args) ->
        ignore
          (L.build_call printf_func
             [|
               L.build_global_stringptr "%d\n" "fmt" builder;
               build_expr var_map func_map builder (List.hd args);
             |]
             "print_bool" builder);
        (var_map, func_map, builder)
    | SFuncBlockCall (svariable, args) ->
        let func_ll = StringMap.find (string_of_svar svariable) func_map in
        let llargs =
          List.rev
            (List.map (build_expr var_map func_map builder) (List.rev args))
        in
        let result = string_of_svar svariable ^ "_result" in
        ignore (L.build_call func_ll (Array.of_list llargs) result builder);
        (var_map, func_map, builder)
    (* | SFunctionSignature of sfunction_signature -> *)
    | SFunctionDefinition (sfunction_signature, block_list) ->
        let function_name, param_list, return_type = sfunction_signature in
        function_definition_helper
          (string_of_svar function_name)
          return_type param_list block_list var_map func_map block_map builder
    (* | SInterfaceDefinition of svariable * sfunction_signature list -> *)
    (* | SClassDefinition of svariable * sblock list -> *)
    (* | SClassDefinitionImplements of svariable * svariable list * sblock list -> *)
    | SIfEnd (condition, block_list) ->
        let bool_val = build_expr var_map func_map builder condition in
        let then_bb = L.append_block context "then" cur_function in
        ignore
          (build_block_list var_map func_map
             (L.builder_at_end context then_bb)
             cur_function block_map block_list);

        (* label for if we don't hit the condition, now we go to the original builder and add in the conditional branch *)
        let next_else_bb = L.append_block context "next_else" cur_function in
        ignore (L.build_cond_br bool_val then_bb next_else_bb builder);

        (* now we jump to next_else at the end of the then_bb! (this is where the stuff after the if will start) *)
        ignore (L.build_br next_else_bb (L.builder_at_end context then_bb));

        (* no block map changes because IfEnd *)
        (* we return the builder at the end of the context of this next_else label *)
        (var_map, func_map, L.builder_at_end context next_else_bb)
    | SElifEnd (condition, block_list) ->
        let jump_block =
          try StringMap.find "if_end" block_map
          with Not_found ->
            raise (Failure "No if_end to match elif_end to: SHOULDN'T HAPPEN!")
        in
        let bool_val = build_expr var_map func_map builder condition in
        let then_bb = L.append_block context "then" cur_function in
        ignore
          (build_block_list var_map func_map
             (L.builder_at_end context then_bb)
             cur_function block_map block_list);

        (* have to jump to the jump_block here, since the condition was true and we executed the statements here *)
        ignore (L.build_br jump_block (L.builder_at_end context then_bb));

        (* label for the case where the elif isn't true, now we go to the original builder and add in the conditional branch *)
        let next_else_bb = L.append_block context "next_else" cur_function in
        ignore (L.build_cond_br bool_val then_bb next_else_bb builder);

        (* the next_else_bb just has a jump to the jump_block though! *)
        ignore (L.build_br jump_block (L.builder_at_end context next_else_bb));

        (* Does it really matter what gets returned here? We stack up into a previous if / elif after all... *)
        (var_map, func_map, builder)
    | SIfNonEnd (condition, block_list, else_block) ->
        let bool_val = build_expr var_map func_map builder condition in
        let then_bb = L.append_block context "then" cur_function in
        ignore
          (build_block_list var_map func_map
             (L.builder_at_end context then_bb)
             cur_function block_map block_list);

        (* have to jump to if_end from then_bb, since the condition was true and we executed the statements here *)
        (* but we haven't created the if_end yet (this is the first if), so we create it now and pass it on in the else_block eval *)
        (* the idea is that future elifs (if successful) & the else should be able to jump to this if_end *)
        let if_end_bb = L.append_block context "if_end" cur_function in
        ignore (L.build_br if_end_bb (L.builder_at_end context then_bb));

        (* label for future elifs / else, now we go to the original builder and add in the conditional branch *)
        let next_else_bb = L.append_block context "next_else" cur_function in
        ignore (L.build_cond_br bool_val then_bb next_else_bb builder);

        (* we build the subsequent block at the correct place (the next_else of the if), also passing in the if_end_bb
           into the block map *)
        let new_block_map = StringMap.add "if_end" if_end_bb block_map in
        ignore
          (build_block var_map func_map
             (L.builder_at_end context next_else_bb)
             cur_function new_block_map else_block);
        (var_map, func_map, L.builder_at_end context if_end_bb)
    | SElifNonEnd (condition, block_list, else_block) ->
        let jump_block =
          try StringMap.find "if_end" block_map
          with Not_found ->
            raise
              (Failure "No if_end to match elif_non_end to: SHOULDN'T HAPPEN!")
        in
        let bool_val = build_expr var_map func_map builder condition in
        let then_bb = L.append_block context "then" cur_function in
        ignore
          (build_block_list var_map func_map
             (L.builder_at_end context then_bb)
             cur_function block_map block_list);

        (* have to jump to the jump_block here, since the condition was true and we executed the statements here *)
        ignore (L.build_br jump_block (L.builder_at_end context then_bb));

        (* label for future elifs / else, now we go to the original builder and add in the conditional branch *)
        let next_else_bb = L.append_block context "next_else" cur_function in
        ignore (L.build_cond_br bool_val then_bb next_else_bb builder);

        (* we build the subsequent block at the correct place (the next_else of the elif) *)
        ignore
          (build_block var_map func_map
             (L.builder_at_end context next_else_bb)
             cur_function block_map else_block);

        (* Does it really matter what gets returned here? We stack up into a previous if / elif after all... *)
        (var_map, func_map, builder)
    | SElseEnd block_list ->
        let jump_block =
          try StringMap.find "if_end" block_map
          with Not_found ->
            raise (Failure "No if_end to match else to: SHOULDN'T HAPPEN!")
        in
        (* build the blocks; we're at the right place *)
        ignore
          (build_block_list var_map func_map builder cur_function block_map
             block_list);

        (* then jump to the jump_block *)
        ignore (L.build_br jump_block builder);

        (* Does it really matter what gets returned here? We stack up into a previous if / elif after all... *)
        (var_map, func_map, builder)
    | _ -> raise (Failure "Not an implemented build_block rule.")
  and build_block_list var_table func_table builder cur_function block_map =
    function
    | [] -> (var_table, func_table, builder)
    | x :: xs ->
        let new_var_table, new_func_table, new_builder =
          build_block var_table func_table builder cur_function block_map x
        in
        build_block_list new_var_table new_func_table new_builder cur_function
          block_map xs
  in

  (* Create a "main" function that all code will be inside *)
  let prog_func =
    L.define_function "main"
      (L.function_type (L.void_type context) [||])
      the_module
  in
  let builder = L.builder_at_end context (L.entry_block prog_func) in

  let var_table, func_table, new_builder =
    build_block_list StringMap.empty StringMap.empty builder prog_func
      StringMap.empty block_list
  in
  ignore (L.build_ret_void new_builder);
  the_module
