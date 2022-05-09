(* Ocamllex scanner for Comma *)


{ open Commaparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let char = ''' ( letter | digit ) '''
let escape = '\\' [''' '"']

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACK }
| ']'      { RBRACK }
| ';'      { SEMI }
| ','      { COMMA }
| '@'      { LAMBDA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MULTIPLY }
| '/'      { DIVIDE }
| '%'      { MODULO }
| '='      { ASSIGN }
| '|'      { BAR }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LTE }
| '>'	     { GT } 
| ">="	   { GTE }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR  }
| "while"  { WHILE }
| "return" { RETURN }
| "double" { DOUBLE }
| "int"    { INT }
| "bool"   { BOOL }
| "char"   { CHAR }
| "array"  { ARRAY }
| "matrix" { MATRIX }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "def"    { FUNC }
| "nul"    { NUL  }
| "void"   { VOID }
| "in"     { IN   }
| "row"    { ROW  } 
| "col"    { COL  }
| digit+ as lem  { INTLIT(int_of_string lem) }
| digit+ '.' digit+ as lem { FLIT(lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| char as lem { CHLIT(lem.[1]) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { tokenize lexbuf }
| _    { comment lexbuf }



