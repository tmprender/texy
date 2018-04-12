type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 42 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* RETURN *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* FOR *);
  281 (* WHILE *);
  282 (* INT *);
  283 (* BOOL *);
  284 (* FLOAT *);
  285 (* VOID *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  286 (* LITERAL *);
  287 (* BLIT *);
  288 (* ID *);
  289 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\007\000\007\000\003\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\012\000\012\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\000\000\002\000\003\000\
\000\000\002\000\002\000\003\000\003\000\005\000\007\000\009\000\
\005\000\000\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\004\000\003\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\053\000\000\000\010\000\011\000\012\000\013\000\
\001\000\003\000\004\000\000\000\000\000\016\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\014\000\000\000\000\000\
\009\000\015\000\000\000\000\000\000\000\000\000\017\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\030\000\
\000\000\029\000\018\000\000\000\000\000\000\000\044\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\000\021\000\020\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\023\000\000\000\000\000\
\024\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\011\000\012\000\017\000\024\000\028\000\
\018\000\043\000\044\000\050\000\076\000\077\000"

let yysindex = "\017\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\255\067\255\000\000\061\255\011\255\
\046\255\050\255\000\000\057\255\061\255\000\000\038\255\061\255\
\000\000\000\000\045\255\042\255\080\255\147\255\000\000\000\000\
\147\255\147\255\147\255\090\255\098\255\100\255\000\000\000\000\
\037\255\000\000\000\000\200\255\128\000\074\255\000\000\000\000\
\178\000\102\255\147\255\147\255\147\255\147\255\147\255\000\000\
\147\255\147\255\147\255\147\255\147\255\147\255\147\255\147\255\
\147\255\147\255\147\255\147\255\000\000\000\000\000\000\146\000\
\107\255\164\000\178\000\106\255\118\255\178\000\075\255\075\255\
\000\000\000\000\155\255\155\255\108\255\108\255\108\255\108\255\
\205\000\192\000\136\255\147\255\136\255\000\000\147\255\104\255\
\220\255\000\000\178\000\136\255\147\255\000\000\109\255\136\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\125\255\000\000\
\000\000\127\255\000\000\000\000\000\000\000\000\000\000\089\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\130\255\000\000\000\000\000\000\000\000\000\000\
\180\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\013\255\000\000\000\000\130\255\000\000\129\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\255\000\000\131\255\054\255\240\255\004\000\
\000\000\000\000\039\255\110\000\030\000\050\000\070\000\090\000\
\114\000\006\255\000\000\000\000\000\000\000\000\000\000\121\255\
\000\000\000\000\077\255\000\000\132\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\112\000\000\000\252\255\000\000\000\000\106\000\
\000\000\171\255\226\255\206\255\000\000\000\000"

let yytablesize = 479
let yytable = "\045\000\
\009\000\073\000\047\000\048\000\049\000\096\000\043\000\098\000\
\043\000\051\000\016\000\043\000\051\000\027\000\102\000\027\000\
\023\000\001\000\105\000\027\000\072\000\049\000\074\000\075\000\
\078\000\043\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\054\000\036\000\
\013\000\036\000\019\000\030\000\036\000\031\000\032\000\055\000\
\020\000\033\000\103\000\036\000\036\000\034\000\046\000\021\000\
\046\000\036\000\036\000\046\000\022\000\097\000\035\000\036\000\
\099\000\037\000\038\000\014\000\015\000\025\000\049\000\039\000\
\040\000\041\000\042\000\030\000\029\000\031\000\070\000\052\000\
\014\000\033\000\052\000\059\000\060\000\034\000\005\000\006\000\
\007\000\008\000\017\000\051\000\017\000\017\000\035\000\036\000\
\017\000\037\000\038\000\052\000\017\000\053\000\071\000\039\000\
\040\000\041\000\042\000\092\000\094\000\017\000\017\000\104\000\
\017\000\017\000\057\000\058\000\059\000\060\000\017\000\017\000\
\017\000\017\000\022\000\095\000\022\000\022\000\100\000\006\000\
\022\000\007\000\026\000\049\000\022\000\050\000\026\000\026\000\
\046\000\030\000\000\000\031\000\000\000\022\000\022\000\033\000\
\022\000\022\000\000\000\034\000\030\000\000\000\022\000\022\000\
\022\000\022\000\033\000\000\000\035\000\036\000\034\000\037\000\
\038\000\057\000\058\000\059\000\060\000\039\000\040\000\041\000\
\042\000\063\000\064\000\065\000\066\000\000\000\000\000\000\000\
\039\000\040\000\041\000\042\000\031\000\000\000\031\000\000\000\
\000\000\031\000\031\000\031\000\031\000\031\000\000\000\000\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
\056\000\000\000\000\000\000\000\000\000\000\000\057\000\058\000\
\059\000\060\000\000\000\000\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\101\000\000\000\000\000\000\000\
\000\000\000\000\057\000\058\000\059\000\060\000\000\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\032\000\000\000\032\000\000\000\000\000\032\000\032\000\032\000\
\000\000\000\000\000\000\000\000\032\000\032\000\032\000\032\000\
\032\000\032\000\032\000\032\000\033\000\000\000\033\000\000\000\
\000\000\033\000\033\000\033\000\000\000\000\000\000\000\000\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\000\000\000\000\005\000\006\000\007\000\008\000\038\000\000\000\
\038\000\000\000\000\000\038\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\039\000\000\000\039\000\000\000\000\000\039\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\040\000\000\000\
\040\000\000\000\000\000\040\000\000\000\000\000\000\000\000\000\
\000\000\000\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\041\000\000\000\041\000\000\000\000\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\037\000\000\000\
\037\000\000\000\042\000\037\000\042\000\000\000\000\000\042\000\
\000\000\000\000\037\000\037\000\000\000\000\000\000\000\000\000\
\037\000\037\000\069\000\000\000\042\000\042\000\057\000\058\000\
\059\000\060\000\000\000\000\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\091\000\000\000\000\000\000\000\
\057\000\058\000\059\000\060\000\000\000\000\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\093\000\000\000\
\000\000\000\000\057\000\058\000\059\000\060\000\000\000\000\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\057\000\058\000\059\000\060\000\000\000\000\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\057\000\058\000\
\059\000\060\000\000\000\000\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\057\000\058\000\059\000\060\000\000\000\
\000\000\061\000\062\000\063\000\064\000\065\000\066\000"

let yycheck = "\030\000\
\000\000\052\000\033\000\034\000\035\000\091\000\001\001\093\000\
\003\001\003\001\015\000\006\001\006\001\001\001\100\000\003\001\
\021\000\001\000\104\000\024\000\051\000\052\000\053\000\054\000\
\055\000\020\001\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\002\001\001\001\
\032\001\003\001\032\001\002\001\006\001\004\001\005\001\011\001\
\003\001\008\001\101\000\013\001\014\001\012\001\001\001\006\001\
\003\001\019\001\020\001\006\001\004\001\092\000\021\001\022\001\
\095\000\024\001\025\001\001\001\002\001\032\001\101\000\030\001\
\031\001\032\001\033\001\002\001\032\001\004\001\005\001\003\001\
\001\001\008\001\006\001\009\001\010\001\012\001\026\001\027\001\
\028\001\029\001\002\001\002\001\004\001\005\001\021\001\022\001\
\008\001\024\001\025\001\002\001\012\001\002\001\001\001\030\001\
\031\001\032\001\033\001\001\001\003\001\021\001\022\001\003\001\
\024\001\025\001\007\001\008\001\009\001\010\001\030\001\031\001\
\032\001\033\001\002\001\006\001\004\001\005\001\023\001\003\001\
\008\001\003\001\001\001\003\001\012\001\003\001\003\001\024\000\
\031\000\002\001\255\255\004\001\255\255\021\001\022\001\008\001\
\024\001\025\001\255\255\012\001\002\001\255\255\030\001\031\001\
\032\001\033\001\008\001\255\255\021\001\022\001\012\001\024\001\
\025\001\007\001\008\001\009\001\010\001\030\001\031\001\032\001\
\033\001\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\030\001\031\001\032\001\033\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\255\255\255\255\255\255\255\255\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\255\255\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\255\255\255\255\006\001\007\001\008\001\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\003\001\255\255\
\255\255\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\255\255\026\001\027\001\028\001\029\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\255\255\003\001\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\255\255\003\001\255\255\255\255\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\255\255\
\003\001\255\255\001\001\006\001\003\001\255\255\255\255\006\001\
\255\255\255\255\013\001\014\001\255\255\255\255\255\255\255\255\
\019\001\020\001\003\001\255\255\019\001\020\001\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\007\001\008\001\
\009\001\010\001\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\007\001\008\001\009\001\010\001\255\255\
\255\255\013\001\014\001\015\001\016\001\017\001\018\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 33 "parser.mly"
            ( _1 )
# 329 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                 ( ([], [])               )
# 335 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 37 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 343 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 38 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 351 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 42 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 366 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                  ( [] )
# 372 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                  ( _1 )
# 379 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)]     )
# 387 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                             ( (_3,_4) :: _1 )
# 396 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "parser.mly"
          ( Int   )
# 402 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
          ( Bool  )
# 408 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
          ( Float )
# 414 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
          ( Void  )
# 420 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                     ( [] )
# 426 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 64 "parser.mly"
                     ( _2 :: _1 )
# 434 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "parser.mly"
               ( (_1, _2) )
# 442 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                   ( [] )
# 448 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "parser.mly"
                   ( _2 :: _1 )
# 456 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
                                            ( Expr _1               )
# 463 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 75 "parser.mly"
                                            ( Return _2             )
# 470 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 76 "parser.mly"
                                            ( Block(List.rev _2)    )
# 477 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 485 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 78 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 494 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 80 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 81 "parser.mly"
                                            ( While(_3, _5)         )
# 512 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                  ( Noexpr )
# 518 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                  ( _1 )
# 525 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
                     ( Literal(_1)            )
# 532 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
              ( Fliteral(_1)           )
# 539 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 90 "parser.mly"
                     ( BoolLit(_1)            )
# 546 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 91 "parser.mly"
                     ( Id(_1)                 )
# 553 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Binop(_1, Add,   _3)   )
# 561 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Binop(_1, Sub,   _3)   )
# 569 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( Binop(_1, Mult,  _3)   )
# 577 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( Binop(_1, Div,   _3)   )
# 585 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Equal, _3)   )
# 593 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Neq,   _3)   )
# 601 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Less,  _3)   )
# 609 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( Binop(_1, Leq,   _3)   )
# 617 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( Binop(_1, Greater, _3) )
# 625 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( Binop(_1, Geq,   _3)   )
# 633 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( Binop(_1, And,   _3)   )
# 641 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( Binop(_1, Or,    _3)   )
# 649 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                         ( Unop(Neg, _2)      )
# 656 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Unop(Not, _2)          )
# 663 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Assign(_1, _3)         )
# 671 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 107 "parser.mly"
                              ( Call(_1, _3)  )
# 679 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                       ( _2                   )
# 686 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                  ( [] )
# 692 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 112 "parser.mly"
               ( List.rev _1 )
# 699 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                            ( [_1] )
# 706 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                         ( _3 :: _1 )
# 714 "parser.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
