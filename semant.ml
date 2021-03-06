(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check program =
  let globals = program.var_decls in
  let functions = program.func_decls in
  let structs = program.struct_decls in

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  let check_binds (kind : string) (to_check : bind list) = 
    let check_it checked binding = 
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding
      in match binding with
        (* No void bindings *)
        (Void, _) -> raise (Failure void_err)
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check) 
       in to_check
  in 

  (**** Checking Global Variables ****)

  let globals' = check_binds "global" globals in

  (**** Checking Structs ****)

  let add_struct map sd = 
    let dup_err = "duplicate struct " ^ sd.sname
    and make_err er = raise (Failure er)
    and n = sd.sname (* Name of the struct *)
    in match sd with (* No duplicate structs *)
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n sd map 
  in

  let struct_decls = List.fold_left add_struct StringMap.empty structs
  in

  (* Return struct from symbol table *)
  let find_struct s = 
    try StringMap.find s struct_decls
    with Not_found -> raise (Failure ("unrecognized struct " ^ s))
  in

  (* Checking the specifics of the struct *)
  let check_struct struc =
    let vars' = check_binds "local" struc.vars in
    
    (* body of check_struct *)
    { ssname = struc.sname;
      svars = vars';
    }
  in

  (**** Checking Functions ****)


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name ty map
    in List.fold_left add_bind StringMap.empty 
    [ ("print", {typ = Void; fname = "print"; locals = []; body = [];
      formals = [(Int, "x")] });
      ("printb", {typ = Void; fname = "printb"; locals = []; body = [];
      formals = [(Bool, "x")] });
      ("printf", {typ = Void; fname = "printf"; locals = []; body = [];
      formals = [(Float, "x")] });
      ("printword", {typ = Void; fname = "printword"; locals = []; body = [];
      formals = [(Word, "x")] });
      ("printbig", {typ = Void; fname = "printbig"; locals = []; body = [];
      formals = [(Int, "x")] });
      ("open", {typ = File; fname = "open"; locals = []; body = [];
      formals = [(Word,"x"); (Word,"y")] }); 
      ("close", {typ = Int; fname = "close"; locals = []; body = [];
      formals = [(File,"x")] });
      ("read", {typ = Int; fname = "read"; locals = []; body = [];
      formals = [(Word,"x");(Int,"y");(Int,"z");(File,"w")] });
      ("write", {typ = Int; fname = "write"; locals = []; body = [];
      formals = [(Word,"x");(File,"y")] });
      ("calloc", {typ = Word; fname = "calloc"; locals = []; body = [];
      formals = [(Int,"x");(Int,"y")] });
      ("free", {typ = Void; fname = "free"; locals = []; body = [];
      formals = [(Word,"x")] });
      ("conbin", {typ = Word; fname = "conbin"; locals = []; body = [];
      formals = [(Word,"x")] });
      ("concat", {typ = Word; fname = "concat"; locals = []; body = [];
      formals = [(Word,"x"); (Word, "y")] });      
      ("bincon", {typ = Word; fname = "bincon"; locals = []; body = [];
      formals = [(Word,"x")] });
      ("bitflip", {typ = Word; fname = "bitflip"; locals = []; body = [];
      formals = [(Word,"x")] });
      ("binshift", {typ = Word; fname = "binshift"; locals = []; body = [];
      formals = [(Word,"x"); (Int,"y")] });
      ("bitflip", {typ = Word; fname = "shiftdown"; locals = []; body = [];
      formals = [(Word,"x"); (Int,"y")] });
      ("strcmp", {typ = Int; fname = "strcmp"; locals = []; body = [];
      formals = [(Word,"x"); (Word,"y")] });
      ("strlen", {typ = Word; fname = "strlen"; locals = []; body = [];
      formals = [(Word,"x")] });
      ("strncpy", {typ = Word; fname = "strncpy"; locals = []; body = [];
      formals = [(Word,"x"); (Word,"y"); (Int,"z")] })
    ]

  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in
    let locals' = check_binds "local" func.locals in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err = 
      let ltyp = match lvaluet with
           Array(olt,_) -> olt
         | _ -> lvaluet
         in
      let rtyp = match rvaluet with
      Array(ort,_) -> ort
    | _ -> rvaluet
    in
      if ltyp = rtyp then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals' @ formals' @ locals' )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in


    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | WordLit l -> (Word, SWordLit l)
      | CharLit l -> (Char, SCharLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Concat (e1, e2) -> let (ty1, _) = expr e1 in
          let (ty2, _) = expr e2 in
          if ty1 != Word then raise(Failure("Can only concat words")) else
          if ty2 != Word then raise(Failure("Can only concat words"))
          else (Word, SConcat((expr e1), (expr e2)))
      | Conbin e -> let (ty, _) = expr e in
          if ty != Word then raise(Failure("# can only be applied to words"))
          else (Word, SConbin (expr e))
      | Bincon (e) -> let (ty, _) = expr e in
          if ty != Word then raise(Failure("% can only be applied to words"))
          else (Word, SBincon (expr e)) 
      | Bitflip e -> let (ty, _) = expr e in
          if ty != Word then raise(Failure("~ can only be applied to words"))
          else (Word, SBitflip (expr e)) 
      | Shiftup (e1,e2) -> let (ty1, _) = expr e1 in
          if ty1 != Word then raise(Failure("Use #+ as words #+ int"))
          else let (ty2, _) = expr e2 in
          if ty2 != Int then raise(Failure("Use #+ as words #+ int"))
          else (Word, SShiftup (expr e1, expr e2))
      | Shiftdown (e1,e2) -> let (ty1, _) = expr e1 in
          if ty1 != Word then raise(Failure("Use #- as words #- int"))
          else let (ty2, _) = expr e2 in
          if ty2 != Int then raise(Failure("Use #- as words #- int"))
          else (Word, SShiftdown (expr e1, expr e2))
      | ArrAcc (s, e) -> let (ty,_) = expr e in
          if ty != Int then raise(Failure("Array index must be integer"))
          else let aty = type_of_identifier s in 
          let accty = match aty with
          Array(Int,_)    -> Int
        | Array(Float,_)  -> Float
        | Array(Word,_) -> Word
        | Array(Bool,_)   -> Bool
        | Array(Struct(i),_) -> Struct(i)
        | _	-> raise(Failure (s^" is not a valid array ID")) in
           (accty, SArrAcc(s, expr e))
      | StructVar(e, var) as str ->
          let e' = expr e in
          let typ = fst e' in
          (match typ with
              Struct s ->
                  let stype = find_struct s in
                  (try
                    fst (List.find (fun b -> snd b = var) stype.vars)
                  with Not_found ->
                    raise (Failure ("struct "^s^ " does not contain " ^ 
                      var ^ " in " ^ string_of_expr str)))
            | _ -> raise (Failure ("illegal struct access of type " ^ 
              string_of_typ typ ^ " in " ^ string_of_expr str))
          ), (SStructVar(e', var))
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex -> 
          let (lt, var') = expr var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign((lt, var'), (rt, e')))
      | Unop(op, e) as ex -> 
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
      | ArrayLit l ->
          let ty_arr = List.map expr l in
          let chk_arr_elem chked elem = 
        (* check if the elements in an array literal are of the same type *)
          let ty_err = "Array elements must be the same type" in
            match elem with 
              (arrt1, _) -> match chked with 
                  ((arrt2, _) :: _) when arrt1 != arrt2 -> raise (Failure ty_err)
                | _ -> elem :: chked
            in let _ = List.fold_left chk_arr_elem [] (List.sort compare ty_arr) in
          let (aty,_) = List.hd ty_arr in
          (Array(aty,1), SArrayLit(ty_arr))
      | ArrayAssign (v,i) -> let (ty,_) = expr i in
          if ty != Int then raise(Failure("Array index must be integer"))
          else let arrty = type_of_identifier v in 
          let lt = match arrty with
          Array(Int,_)    -> Int
        | Array(Float,_)  -> Float
        | Array(Word,_) -> Word
        | Array(Bool,_)   -> Bool
        | Array(Struct(n),_) -> Struct n
        | _	-> raise(Failure (v^" is not a valid array ID")) in
          (lt, SArrayAssign(v, expr i))

    in

    let check_bool_expr e = 
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      slocals  = locals';
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (globals', List.map check_struct structs, List.map check_function functions)
