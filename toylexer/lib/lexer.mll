{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let atok = ['A'-'Z'] chr*
let lvowels = ['a' 'e' 'i' 'o' 'u']
let btok = lvowels*
let vowels = ['a' 'e' 'i' 'o' 'u' 'A' 'E' 'I' 'O' 'U']
let consonants = letter#vowels
let ctok = consonants* vowels? consonants*
let dtok = ['-']? num? ['.']? num?
let hex = ['0'-'9' 'A'-'F' 'a'-'f']
let etok = '0'('X'|'x') hex+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | atok {ATOK(Lexing.lexeme lexbuf)}
  | btok {BTOK(Lexing.lexeme lexbuf)}
  | ctok {CTOK(Lexing.lexeme lexbuf)}
  | dtok {DTOK(Lexing.lexeme lexbuf)}
  | etok {ETOK(Lexing.lexeme lexbuf)}
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }