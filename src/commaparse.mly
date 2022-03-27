/* Ocamlyacc parser for Comma */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN LBRACK RBRACK
%token EQ NEQ LT GT LTE GTE AND OR 
%token IF ELSE EIF
%token WHILE FOR IN
%token FUNC RETURN 
%token ROW COL
%token DOUBLE INT BOOL CHAR LIST ARRAY MATRIX NUL
%token COMMA LAMBDA
%token <float> FLIT
%token <int> INTLIT
%token <char> CHLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%nonassoc NOELSEEIF
%nonassoc ELSE
%nonassoc EIF
%left SEMI
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
  vdecl_list_rule stmt_list_rule EOF { {locals=$1; body=$2} }

vdecl_list_rule:
  /*nothing*/                   { []       }
  | vdecl_rule vdecl_list_rule  { $1 :: $2 }

vdecl_rule:
  | typ_rule ID ASSIGN expr_rule SEMI { ($1, $2, $4) }
  | typ_rule LBRACK RBRACK ID ASSIGN expr_rule SEMI { ($1, $4, $6) }

typ_rule:
  INT       { Int    }
  | BOOL    { Bool   }
  | DOUBLE  { Double }
  | CHAR    { Char   } 
  | LIST    { List   }

stmt_list_rule:
    /* nothing */               { []     }
    | stmt_rule stmt_list_rule  { $1::$2 }

stmt_rule:
  expr_rule SEMI                                          { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                          { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule %prec NOELSEEIF  { If ($3, $5, Block([])) }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule   { If ($3, $5, $7) } 
  | WHILE LPAREN expr_rule RPAREN stmt_rule				        { While ($3, $5)  }
  | FOR LPAREN expr_rule COMMA expr_rule COMMA expr_rule RPAREN stmt_rule { For ($3, $5, $7, $9) }

list_decl_rule:
  /*nothing*/ { [] }
  | list_elems_rule  { $1 }

list_elems_rule:
    expr_rule                         { [$1]   }
  | expr_rule COMMA list_elems_rule   { $1::$3 }

expr_rule:
  | BLIT                          { BoolLit $1            }
  | INTLIT                        { IntLit  $1            }
  | FLIT                          { DoubLit $1            }
  | CHLIT                         { CharLit $1            }
  | ID                            { Id $1                 }
  | LBRACK list_decl_rule RBRACK  { ListLit $2            } 
  | expr_rule PLUS expr_rule      { Binop ($1, Add, $3)   }
  | expr_rule MINUS expr_rule     { Binop ($1, Sub, $3)   }
  | expr_rule EQ expr_rule        { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule       { Binop ($1, Neq, $3)   }
  | expr_rule LT expr_rule        { Binop ($1, Less, $3)  }
  | expr_rule GT expr_rule        { Binop ($1, Great, $3) }
  | expr_rule LTE expr_rule 	  { Binop ($1, LessEqual, $3)  }
  | expr_rule GTE expr_rule 	  { Binop ($1, GreatEqual, $3) }
  | expr_rule AND expr_rule       { Binop ($1, And, $3)   }
  | expr_rule OR expr_rule        { Binop ($1, Or, $3)    }
  | ID ASSIGN expr_rule           { Assign ($1, $3)       }
  | LPAREN expr_rule RPAREN       { $2                    }
