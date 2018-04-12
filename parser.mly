/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET COMMA DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LT LEQ GT GEQ AND OR CONBIN CONCAT
%token RETURN IF ELSE FOR WHILE INT BOOL CHAR WORD FILE ARRAY VOID STRUCT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { {var_decls=[]; struct_decls=[]; func_decls=[]} }
 | decls vdecl { { $1 with var_decls = $2 :: $1.var_decls; } }
 | decls fdecl { { $1 with func_decls = $2 :: $1.func_decls; } }
 | decls sdecl { { $1 with struct_decls = $2 :: $1.struct_decls; } }


fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

sdecl:
   STRUCT ID LBRACE vdecl_list RBRACE SEMI
    { { sname = $2;
        vars = $4; } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | CHAR  { Char  }
  | WORD  { Word  }
  | FILE  { File  }
  | FLOAT { Float }
  | VOID  { Void  }
  | ARRAY { Array }
  | STRUCT ID { Struct($2) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }
  | typ ID LBRACKET LITERAL RBRACKET SEMI { (Array, $2) }

arry_opt:
    /* empty */ { [||] }
    | arry { Array.of_list (List.rev $1) }

arry:
    expr              { [$1] }
    | arry COMMA expr { $3 :: $1 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

access_expr:
    LITERAL          { Literal($1)            }
  | FLIT             { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | ID               { Id($1)                 }
  | WLIT             { WordLit($1)            }
  | LBRACKET arry_opt RBRACKET { ArrayLit($2) }
  | access_expr DOT ID { StructVar($1, $3)    }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }

binary_expr:
    expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr CONCAT expr { Binop($1, Concat, $3)  }

unary_expr:
    MINUS expr %prec NEG { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | CONBIN expr      { Unop(Conbin, $2)       }

assign_expr:
    expr ASSIGN expr   { Assign($1, $3)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    access_expr        { $1 }
  | binary_expr        { $1 }
  | unary_expr         { $1 }
  | assign_expr        { $1 }
  | LPAREN expr RPAREN { $2 }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
