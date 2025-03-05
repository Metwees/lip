{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let digits_hex = ['0'-'9' 'A'-'F' 'a'-'f']
let hex = "0" ('x'|'X') digits_hex+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "*" { MULTIPLY }
  | "/" { DIVIDE }
  | "+" { PLUS }
  | "-" { MINUS }
  | hex { CONST (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
