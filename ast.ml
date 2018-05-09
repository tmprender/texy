(* Abstract Syntax Tree and functions for printing it  - based off MicroC ast*)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Word | Char | Float | File | Void | Array of typ * int | Struct of string

type bind = typ * string

type expr =
    Literal of int
  | Fliteral of string
  | WordLit of string
  | CharLit of char
  | BoolLit of bool
  | ArrAcc of string * expr
  | Conbin of expr
  | Bincon of expr
  | Bitflip of expr
  | Shiftup of expr * expr
  | Shiftdown of expr * expr
  | Concat of expr * expr
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | ArrayAssign of string * expr
  | Call of string * expr list
  | ArrayLit of expr list
  | StructVar of expr * string
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type struct_decl = {
    sname: string;
    vars: bind list;
  }

type program = {
    var_decls: bind list;
    struct_decls: struct_decl list;
    func_decls: func_decl list;
}


(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | CharLit(l) -> Char.escaped l
  | WordLit(w) -> w
  | ArrayLit(arr) -> "[" ^ String.concat ", " (List.map string_of_expr arr) ^ "]"
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Conbin(e) -> "#" ^ string_of_expr e
  | Bincon(e) -> "%" ^ string_of_expr e
  | Bitflip(e) -> "~" ^ string_of_expr e
  | Shiftup(e1,e2) -> string_of_expr e1 ^ "#+" ^ string_of_expr e2
  | Shiftdown(e1,e2) -> string_of_expr e1 ^ "#-" ^ string_of_expr e2
  | Concat(e1, e2) -> string_of_expr e1 ^ "^" ^ string_of_expr e2
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | ArrayAssign(v,i) -> v ^ "[" ^ string_of_expr i ^ "]"
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ArrAcc(n, e) ->
      n ^ "[" ^ string_of_expr e ^ "]"
  | StructVar(s, v) -> string_of_expr s ^ "." ^ v
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Word -> "word"
  | Float -> "float"
  | Char -> "char"
  | File -> "file"
  | Void -> "void"
  | Array(t,i) -> string_of_typ t ^ "[" ^ string_of_int i ^"]"
  | Struct(s) -> "struct " ^ s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_sdecl sdecl =
  "struct " ^ sdecl.sname ^  " {\n" ^ String.concat "" (List.map string_of_vdecl sdecl.vars) ^ "};\n"


let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program program =
  String.concat "" (List.map string_of_vdecl program.var_decls) ^ "\n"  ^
  String.concat "\n" (List.map string_of_sdecl program.struct_decls) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl program.func_decls)
