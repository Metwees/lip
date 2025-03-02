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

let lang4_aux q e = match q with
  0 when e='0' -> 0
| 0 when e='1' -> 1
| 1 when e='0' -> 1
| 1 when e='1' -> 2
| 2 when e='0' -> 2
| _ -> -1
;;

let lang4 l = 
  if (List.fold_left lang4_aux 0 l = 2) then true else false
;;

let lang5_aux q e = match q with
  0 when e='0' -> 1
| 0 when e='1' -> 2
| 1 when e='0' -> 0
| 2 when e='1' -> 1
| _ -> -1
;;

let lang5 l = 
match List.fold_left lang5_aux 0 l with 
0 -> true 
| _ ->false;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
