open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl


let n_frequency x l = 
  List.fold_left(fun count y -> if(x=y) then count+1 else count) 0 l
;;

let rec pair_list l = match l with
[] -> []
| h::t -> (h,n_frequency h l) :: pair_list (List.filter (fun x ->not(h=x)) t)
;;

  (* frequency : int -> 'a list -> ('a * int) list *)
(*Prende in input una lista di token e rest*)
let frequency n l = 
let classifica = List.sort (fun (_,count1) (_,count2) -> compare count2 count1) (pair_list l) in 
List.take n classifica
;;
