/* Ocamlyacc parser for Comma */

%{
open Ast
open Helpers
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE MULTIPLY DIVIDE MODULO PLUS MINUS ASSIGN LBRACK RBRACK BAR
%token EQ NEQ LT GT LTE GTE AND OR 
%token IF ELSE EIF
%token WHILE FOR IN
%token FUNC RETURN COMMA
%token ROW COL
%token DOUBLE INT BOOL CHAR ARRAY MATRIX NUL
%token VOID
%token COMMA LAMBDA
%token <string> FLIT
%token <int> INTLIT
%token <char> CHLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%nonassoc NOELSEEIF
%nonassoc EIF
%nonassoc ELSE
%right EXP_NUL
%right ASSIGN
%left OR 
%left AND
%left EQ NEQ 
%left LT GT LTE GTE 
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULO


%%

program_rule:
  decls EOF { $1}

/* first array is for variable declarations, and second array is for function declarations */
decls:                   { ([], [])                     }
 | vdecl_rule SEMI decls { (($1 :: get_first_item_in_tuple $3), get_second_item_in_tuple $3)  }
 | fdecl_rule decls      { (get_first_item_in_tuple $2), ($1 :: get_second_item_in_tuple $2)  }

vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule SEMI vdecl_list_rule  { $1 :: $3 }

vdecl_rule:
  | typ_rule ID ASSIGN expr_rule               { AssignBind ($1, $2, $4) }
  | typ_rule ID                                { NoAssignBind ($1, $2)   }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_rule:
  | typ_rule ID { NoAssignBind($1, $2) }

formals_list:
  formals_rule { [$1] }
  | formals_rule COMMA formals_list { $1::$3 }

fdecl_rule:
  FUNC typ_rule ID LPAREN formals_opt RPAREN LBRACE vdecl_list_rule stmt_list_rule RBRACE
  {
    {
      rtyp=$2;
      fname=$3;
      formals=$5;
      locals=$8;
      body=$9;
    }
  }

typ_rule:
  INT       { Int    }
  | BOOL    { Bool   }
  | DOUBLE  { Double }
  | CHAR    { Char   } 
  | typ_rule ARRAY { Array($1)  }
  | MATRIX  { Matrix }
  | VOID    { Void   }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule %prec NOELSEEIF  { If ($3, $5, Block([])) }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) }
  | RETURN expr_rule SEMI                                 { Return $2       }
  | WHILE LPAREN expr_rule RPAREN stmt_rule				        { While ($3, $5)  }
  | FOR LPAREN expr_rule COMMA expr_rule RPAREN stmt_rule { For ($3, $5, $7) }

array_decl_rule:
  /*nothing*/        { [] }
  | array_elems_rule { $1 }

array_elems_rule:
  expr_rule                            { [$1]   }
  | expr_rule COMMA array_elems_rule   { $1::$3 }

matrix_decl_rule:
  /*nothing*/        { [] }
  | matrix_elems_rule { $1 }

matrix_elems_rule:
  LBRACK array_decl_rule RBRACK   { [$2] }
  | LBRACK array_decl_rule RBRACK COMMA matrix_elems_rule { $2::$5 }

expr_list_rule:
  expr_rule            { [$1] }
  | expr_rule SEMI expr_list_rule { $1::$3 }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | INTLIT                        { IntLit  $1            }
  | FLIT                          { DoubLit $1            }
  | CHLIT                         { CharLit $1            }
  | ID                            { Id $1                 }
  | NUL	                          { NulLit 		            }
  | LBRACK array_decl_rule RBRACK typ_rule { ArrayLit ($4, $2)           }
  | BAR matrix_decl_rule BAR      { MatrixLit ($2)          }
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule MULTIPLY expr_rule  { Binop ($1, Multiply, $3)   }
  | expr_rule DIVIDE expr_rule    { Binop ($1, Divide, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule GT expr_rule        { Binop ($1, Great, $3) }
  | expr_rule LTE expr_rule 	    { Binop ($1, LessEqual, $3)  }
  | expr_rule GTE expr_rule 	    { Binop ($1, GreatEqual, $3) }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | expr_rule MODULO expr_rule    { Binop ($1, Mod, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }
  | LPAREN LAMBDA typ_rule ID LBRACE expr_list_rule RBRACE expr_rule RPAREN { Lambda($3, $4, $6, $8) }
  | ID LPAREN args_opt RPAREN     { Call ($1, $3)         }

args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr_rule  { [$1] }
  | expr_rule COMMA args { $1::$3 }

