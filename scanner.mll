(* Ocamllex scanner for Texy, based off of the MicroC scanner *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| '"'      { wordlit (Buffer.create 80) lexbuf }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "char"   { CHAR }
| "Word"   { WORD }
| "File"   { FILE }
| "float"  { FLOAT }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| '''([' '-'!' '#'-'[' ']'-'~' ]|['0'-'9'])''' as lxm {CHARLIT( String.get lxm 1)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

 (* https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html 
 *)


and wordlit wbuf =
parse
|        '"' { WLIT (Buffer.contents wbuf) }
|	'\\' '/' 	{ Buffer.add_char wbuf '/'; wordlit wbuf lexbuf }
|	'\\' '\\'	{ Buffer.add_char wbuf '\\'; wordlit wbuf lexbuf }
|	'\\' 'b' 	{ Buffer.add_char wbuf '\b'; wordlit wbuf lexbuf }
|	'\\' 'n' 	{ Buffer.add_char wbuf '\n'; wordlit wbuf lexbuf }
|       '\\' 'f'        { Buffer.add_char wbuf '\012'; wordlit wbuf lexbuf }
|	'\\' 'r' 	{ Buffer.add_char wbuf '\r'; wordlit wbuf lexbuf }
|	'\\' 't'	{ Buffer.add_char wbuf '\t'; wordlit wbuf lexbuf }
|	[^ '"' '\\']+	{ Buffer.add_string wbuf (Lexing.lexeme lexbuf); wordlit wbuf lexbuf}
| 	_ { raise (Failure("Illegal word character " ^ Lexing.lexeme lexbuf)) }
| 	eof { raise (Failure("Unterminated word ")) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
