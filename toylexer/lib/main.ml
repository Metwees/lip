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

(*prende un token e la lista e restituisce quante volte appare il token all'interno di quest'ultima*)
let n_frequency x l = 
  List.fold_left(fun count y -> if(x=y) then count+1 else count) 0 l
;;

(*Prende la lista e per ogni token calcola quante volte appare, eliminando i token già selezionati*)
let rec pair_list l = match l with
[] -> []
| h::t -> (h,n_frequency h l) :: pair_list (List.filter (fun x ->not(h=x)) t)
;;

  (* frequency : int -> 'a list -> ('a * int) list *)
(*Prende in input il numero e una lista di token e restituisce *)
let frequency n l = 
(*classifica : ordina i token in base al loro numero e poi con List.take prende i primi n elementi della lista*)
let classifica = List.sort (fun (_,count1) (_,count2) -> compare count2 count1) (pair_list l) in 
List.take n classifica
;;
