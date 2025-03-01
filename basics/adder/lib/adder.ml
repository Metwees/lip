(* val addlist : int list -> int *)
(* addlist l adds the element of the list of integers l *)

(*let addlist l = 0 (* replace 0 with actual code *)*)
(*
let rec addlist (l: int list) = 
  begin match l with
    | [] -> 0
    | h::t -> h + (addlist t)
  end;;
*)
  let addlist l = 
    List.fold_left (fun sum x -> sum + x ) 0 l;;