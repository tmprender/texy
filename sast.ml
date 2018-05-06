(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SWordLit of string
  | SCharLit of char
  | SBoolLit of bool
  | SConbin of sexpr
  | SConcat of sexpr * sexpr
	| SArrAcc of string * sexpr
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SArrayLit of sexpr list
  | SStructVar of expr * string
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    slocals : bind list;
    sbody : sstmt list;
  }

type sstruct_decl = {
    ssname: string;
    svars: bind list;
  }

type sprogram = {
    svar_decls: bind list;
    sstruct_decls: sstruct_decl list;
    sfunc_decls: sfunc_decl list;
}

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SWordLit(l) -> l
  | SFliteral(l) -> l
  | SCharLit(l) -> Char.escaped l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SConbin(e) -> "#" ^ string_of_sexpr e
  | SConcat(e1, e2) -> string_of_sexpr e1 ^ "+^" ^ string_of_sexpr e2
  | SArrAcc(n, e) ->
      n ^ "[" ^ string_of_sexpr e ^ "]"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SArrayLit(arr) -> "[" ^ String.concat ", " (List.map string_of_sexpr arr) ^ "]"
  | SStructVar(s, v) -> string_of_expr s ^ "." ^ v
  | SNoexpr -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_ssdecl sdecl =
  "struct " ^ sdecl.ssname ^  " {\n" ^ String.concat "" (List.map string_of_vdecl sdecl.svars) ^ "};\n"

let string_of_sprogram sprogram =
  String.concat "" (List.map string_of_ssdecl sprogram.sstruct_decls) ^ "\n"  ^
  String.concat "\n" (List.map string_of_vdecl sprogram.svar_decls) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl sprogram.sfunc_decls)
