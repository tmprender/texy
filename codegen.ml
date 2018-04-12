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
let translate (_, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context 
  and void_t     = L.void_type   context in

  let string_t   = L.pointer_type i8_t 
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "Texy" in

  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Void  -> void_t
    | A.Word  -> string_t
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  (* declare i32 @printf(i8*, ...) *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in 

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  (* Generate the LLVM instructions to define a MicroC function *)
  let build_function fdecl =

    (* int main() {}  ----->  define i32 @main() {}  *)
    let main_ty = L.function_type (ltype_of_typ fdecl.styp) [||] in
    let the_function = L.define_function "main" main_ty the_module in

    (* An LLVM "instruction builder" points to a basic block. 
     * Adding a new instruction mutates both the_module and builder. *) 
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

    let rec expr builder ((_, e) : sexpr) = match e with
        SWordLit i -> L.build_global_stringptr i "str" builder          
      | SLiteral i -> L.const_int i32_t i  
      | SCall ("print", [e]) ->  
	  L.build_call printf_func [| str_format_str ; (expr builder e) |]
      "printf" builder 
      (* Throw an exception for any other expressions *)
      | _ -> to_imp (string_of_sexpr (A.Word,e))  
    in

    let rec stmt builder = function
        SExpr e -> let _ = expr builder e in builder 
      | SBlock sl -> List.fold_left stmt builder sl
      (* return 0;  ----->  ret i32 0 *)
      | SReturn e -> let _ = match fdecl.styp with
                              A.Int -> L.build_ret (expr builder e) builder 
                            | _ -> to_imp (A.string_of_typ fdecl.styp)
                     in builder
      | s -> to_imp (string_of_sstmt s)

    in ignore (stmt builder (SBlock fdecl.sbody))
  (* Build each function (there should only be one for Hello World), 
     and return the final module *)
  in List.iter build_function functions; the_module
