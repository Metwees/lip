let rec lang1 (l : char list) = match l with
  [] -> false
| ['0']
| ['1'] -> true
| c::l1 -> if (c=='0' || c=='1') then lang1 l1 else false
;;

let rec lang2 (l : char list) = match l with
  []
| ['1'] 
| ['0'] -> true
| '1'::t -> lang2 t
| _ -> false;; 

let rec lang3_aux l = match l with
  ['0'] -> true
| '0'::l1
| '1'::l1 -> lang3_aux l1
| _ -> false
;;

let lang3 l = match l with
  '0'::l1 -> lang3_aux l1
| _ -> false
;;

let rec lang4_aux l = match l with 
[] -> false
| h::t -> if h=='1' then true else lang4_aux t;;

let rec lang4 l = match l with
[] 
| ['1']
| ['0'] -> false
| h::t -> if h=='1' then lang4 t else lang4_aux t;;

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
