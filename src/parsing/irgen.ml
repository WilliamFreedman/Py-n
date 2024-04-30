
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (block_list) = 
  let context = L.global_context () in
  let the_module = L.create_module context "Pyn" in

  let int_t = L.i32_type context
  and float_t = L.double_type context
  and bool_t - L.i1_type context
  and char_t = L.i8_type context in

  let char_pt = L.pointer_type char_t in

  (* we only do division with floating points *)
  (* look at the types of e1 and e2, call L.build_sitofp on the llvalues if they're ints, and then do floating point division on these new llvalues *)
  let div_helper e1 e2 name builder = 
      let (t1, e1x) = e1 and (t2, e2x) = e2 in
      if t1 = TypeVariable("int") then e1' = (L.build_sitofp e1 float_t name builder)
      else e1' = e1 in
      if t2 = TypeVariable("int") then e2' = (L.build_sitofp e2 float_t name builder)
      else e2' = e2 in
      L.build_fdiv e1' e2' name builder
  in      

  (* floor division - we do regular floating point division and convert the result into a signed integer *)
  let fdiv_helper e1 e2 name builder = 
    let result_llvalue = div_helper e1 e2 name builder in
    L.build_fptosi result_llvalue int_t name builder
  in

  let rec build_expr builder ((_, e) : sexpr) = match e with
      SIntLit i -> L.const_int int_t i
    | SBoolLit b -> L.const_int bool_t (if b then 1 else 0)
    | SFloatLit f -> L.const_float float_t f
    | SStringLit s -> (* TODO *)
    | SVarExpr v -> 
    | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mult    -> L.build_mul
         | A.Div     -> div_helper 
         | A.FDiv    -> fdiv_helper
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
         | _ raise (Failure (""))
        ) e1' e2' "tmp" builder
    | SList l ->
    | SDict d -> 
    | SListCompUnconditional lc -> 
    | SListCompConditional lc -> 
    | SDictCompUnconditional dc -> 
    | SDictCompConditional dc -> 
    | SWalrus
    | SFuncCall (var, expr) -> 
    | SIndexingExprList
    | SIndexingStringLit
    in 
  let rec build_block builder = function
    SBlockAssign of svariable * sassignment * sexpr -> 
    | SBreak ->
    | SContinue -> 
    | SPass -> 
    | SVarDec of typevar * svariable * sexpr -> 
      (* in the global case, define_global *) (* variable_name -> Llvalue mapping in microc do we scope it out like ocaml? *)
    | SReturnVal of sexpr ->
    | SReturnVoid ->
    | SWhile of sexpr * sblock list ->
    | SFor of svariable * sexpr * sblock list ->
    | SFuncBlockCall of svariable * sexpr list ->
    | SFunctionSignature of sfunction_signature ->
    | SFunctionDefinition of sfunction_signature * sblock list ->
    | SInterfaceDefinition of svariable * sfunction_signature list ->
    | SClassDefinition of svariable * sblock list ->
    | SClassDefinitionImplements of svariable * svariable list * sblock list ->
    | SIfEnd of sexpr * sblock list ->
    | SIfNonEnd of sexpr * sblock list * sblock ->
    | SElifEnd of sexpr * sblock list ->
    | SElifNonEnd of sexpr * sblock list * sblock ->
    | SElseEnd of sblock list ->
  in

let variable_declaration_helper var_type var_name builder =
  let local = L.build_alloca (type_to_string var_type) var_name builder in

the_module