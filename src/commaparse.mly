/* Ocamlyacc parser for Comma */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN
%token EQ NEQ LT AND OR
%token IF ELSE EIF
%token WHILE FOR IN
%token FUNC RETURN 
%token ROW COL
%token DOUBLE INT BOOL CHAR LIST ARRAY MATRIX NUL
%token COMMA LAMBDA
%token <float> FLIT
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token EOF

%start program_rule
%type <Ast.program> program_rule

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT
%left PLUS MINUS

%%

program_rule:
	