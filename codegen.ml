(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "TeXy" in
  let i8_pt   = L.pointer_type i8_t in

  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t

    | A.Word  -> i8_pt
    | A.Char -> i8_t
    | A.File -> i8_pt
    | A.Array t  -> match t with 
          A.Int -> L.pointer_type i32_t
        | A.Bool -> L.pointer_type i1_t
        | A.Float -> L.pointer_type float_t
        | A.Word -> L.pointer_type i8_pt
        | A.Char -> L.pointer_type i8_t
        | t -> raise (Failure ("Array of " ^ A.string_of_typ t ^ " not implemented yet"))
  in


  (* Declare each global variable; remember its value in a map *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in
    
  (* Declare the built-in open() function *)
  let open_t = L.function_type i8_pt [| L.pointer_type i8_t; L.pointer_type i8_t |] in    
  let open_func = L.declare_function "fopen" open_t the_module in

  (* Declare the built-in close() function *)
  let close_t = L.function_type i32_t [| i8_pt |] in
  let close_func = L.declare_function "fclose" close_t the_module in
   
  (* Declare the built-in fputs() function as write() *)
  let write_t = L.function_type i32_t [| L.pointer_type i8_t; i8_pt |] in 
  let write_func = L.declare_function "fputs" write_t the_module in

  (* Declare the built-in fread() function as read() *)
  let read_t = L.function_type i32_t [| i8_pt; i32_t; i32_t; i8_pt |] in 
  let read_func = L.declare_function "fread" read_t the_module in

  (* Declare heap storage function *)
  let calloc_t = L.function_type i8_pt [| i32_t ; i32_t|] in 
  let calloc_func = L.declare_function "calloc" calloc_t the_module in

  (* Declare free from heap *)
  let free_t = L.function_type i8_pt [| i8_pt |] in 
  let free_func = L.declare_function "free" free_t the_module in

  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  let conbin_t = L.function_type i8_pt [| i8_pt |] in
  let conbin_func = L.declare_function "conbin" conbin_t the_module in

  let bincon_t = L.function_type i8_pt [| i8_pt |] in
  let bincon_func = L.declare_function "bincon" bincon_t the_module in

  let bitflip_t = L.function_type i8_pt [| i8_pt |] in
  let bitflip_func = L.declare_function "bitflip" bitflip_t the_module in

  let concat_t = L.function_type i8_pt [| i8_pt ; i8_pt |] in 
  let concat_func = L.declare_function "concat" concat_t the_module in

  (* Define each function (arguments and return type) so we can 
   * define it's body and call it later *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder 
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
	let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	StringMap.add n local m 
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SLiteral i -> L.const_int i32_t i
      | SWordLit s -> L.build_global_stringptr s "wrd" builder
      | SFliteral l -> L.const_float_of_string float_t l
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c -> L.const_int i8_t (Char.code c)
      | SNoexpr -> L.const_int i32_t 0
      | SConbin (e) -> L.build_call conbin_func [| (expr builder e) |] "conbin" builder
      | SBincon (e) -> L.build_call bincon_func [| (expr builder e) |] "bincon" builder
      | SBitflip e -> L.build_call bitflip_func [| (expr builder e) |] "bitflip" builder
      | SConcat (e1, e2) -> L.build_call concat_func [| (expr builder e1) ; (expr builder e2) |] "concat" builder
      | SId s -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                          let _  = L.build_store e' (lookup s) builder in e'
      | SBinop (e1, op, e2) ->
	  let (t, _) = e1
	  and e1' = expr builder e1
    and e2' = expr builder e2 in
    if t = A.Float then (match op with 
    A.Add     -> L.build_fadd
  | A.Sub     -> L.build_fsub
  | A.Mult    -> L.build_fmul
  | A.Div     -> L.build_fdiv 
  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
  | A.Neq     -> L.build_fcmp L.Fcmp.One
  | A.Less    -> L.build_fcmp L.Fcmp.Olt
  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
  | A.And | A.Or ->
      raise (Failure "internal error: semant should have rejected and/or on float")
  ) e1' e2' "tmp" builder 
  else (match op with
	  | A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mult    -> L.build_mul
    | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Equal   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Less    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Greater -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder
      | SUnop(op, e) ->
	  let (t, _) = e in
          let e' = expr builder e in
	  (match op with
    A.Neg when t = A.Float -> L.build_fneg 
  | A.Neg                  -> L.build_neg
  | A.Not                  -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
      | SCall ("printf", [e]) -> 
	  L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall ("printbig", [e]) ->
    L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("conbin", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call conbin_func (Array.of_list x) "conbin" builder
      | SCall ("bincon", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call bincon_func (Array.of_list x) "bincon" builder
      | SCall ("bitflip", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call bitflip_func (Array.of_list x) "bitflip" builder
      | SCall ("concat", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call concat_func (Array.of_list x) "concat" builder
      | SCall ("printword", [e]) -> 
	  L.build_call printf_func [| str_format_str ; (expr builder e) |]
      "printf" builder
      | SCall ("open", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call open_func (Array.of_list x) "fopen" builder
      | SCall ("close", [e]) -> 
    L.build_call close_func [| (expr builder e) |] "fclose" builder
      | SCall ("read", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call read_func (Array.of_list x) "fread" builder
      | SCall ("write", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call write_func (Array.of_list x) "fputs" builder
      | SCall ("calloc", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
    L.build_call calloc_func (Array.of_list x) "calloc" builder
      | SCall ("free", [e]) -> 
	  L.build_call free_func [| (expr builder e) |] "free" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | SArrAcc (s, e) ->
          let idx = expr builder e in
          let arr = lookup s in
          let arr = L.build_load arr "" builder in
          let s' = L.build_gep arr [| idx |] "" builder in
          let s' = L.build_load s' "" builder in
          s' 

      | SArrayLit sel ->
          let al = List.map (expr builder) sel in
          let ty = L.type_of (List.hd al) in
          let sz = List.length al in
          let aty = L.array_type ty sz in
          let arrp = L.build_alloca aty "" builder in
          let fill i v = 
            let vp = L.build_gep arrp [|L.const_int i32_t 0; L.const_int i32_t i|] "" builder in
            ignore(L.build_store v vp builder);
            in List.iteri fill al; 
          let arrp' = L.build_gep arrp [|L.const_int i32_t 0; L.const_int i32_t 0|] "" builder in
          arrp'
    in
    
    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "instr builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | SExpr e -> let _ = expr builder e in builder 
      | SReturn e -> let _ = match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder 
                     in builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         (* Add "merge" basic block to our function's list of blocks *)
	 let merge_bb = L.append_block context "merge" the_function in
         (* Partial function used to generate branch to merge block *) 
         let branch_instr = L.build_br merge_bb in

         (* Same for "then" basic block *)
	 let then_bb = L.append_block context "then" the_function in
         (* Position builder in "then" block and build the statement *)
         let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
         (* Add a branch to the "then" block (to the merge block) 
           if a terminator doesn't already exist for the "then" block *)
	 let () = add_terminal then_builder branch_instr in

         (* Identical to stuff we did for "then" *)
	 let else_bb = L.append_block context "else" the_function in
         let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
	 let () = add_terminal else_builder branch_instr in

         (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
	 let _ = L.build_cond_br bool_val then_bb else_bb builder in
         (* Move to the merge block for further instruction building *)
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
	  let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
	  let _ = L.build_br pred_bb builder in

          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
	  let body_bb = L.append_block context "while_body" the_function in
          let while_builder = stmt (L.builder_at_end context body_bb) body in
	  let () = add_terminal while_builder (L.build_br pred_bb) in

          (* Generate the predicate code in the predicate block *)
	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

          (* Hook everything up *)
	  let merge_bb = L.append_block context "merge" the_function in
	  let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | SFor (e1, e2, e3, body) -> stmt builder
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
