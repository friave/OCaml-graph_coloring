let rec length l =
  match l with
  | [] -> 0
  | _ :: t -> 1 + length t;;

let rec append a b =
    match a with
    | [] -> b
    | h :: t -> h :: append t b;;

let rec replace lista n var counter =
  match lista with
  | [] -> raise (Failure "empty list replace")
  | h::t -> if counter = n then var::t 
  else 
    h::replace t n var (counter+1);;


let not x = 
  match x with
    true -> false
  | false -> true;;
    

let rec init_cw len l=
  if len-1 > 0 then 
    init_cw (len-1)(append l [0])
  else l;;


let rec get_n n list =
  match list with 
  | [] -> raise (Failure "empty list get_n")
  | h::t -> 
    if n = 0 then h else get_n (n-1) t;; 


let rec get_neig_c neig cw x =
  match neig with
  | [] -> x
  | h::t -> 
    get_neig_c t cw (append x [(get_n h cw)]);;


let rec get_c nc counter =
  if not (List.mem counter nc) then counter
  else
    get_c nc (counter+1);;


let color_node neig cw n =
  replace cw n (get_c(get_neig_c neig cw []) 1) 0;;


let rec coloring w cw counter=
  match w with
  | [] -> cw
  | h::t -> coloring t (color_node h cw counter) (counter+1) ;;


(* let w = [
[1;5];
[0;4];
[0;3;4];
[2;4;5];
[1;2;3];
[0;3]];; *)

let w = [
[1;2];
[0;3;4];
[0;3;4;8];
[1;2;4;5;6;7];
[1;2;3;5;7];
[3;4;6;7];
[3;5;7];
[4;5;6;3;8];
[2;7]
];;


let cw = init_cw (length w) [1];;
let cl = [1];;

let colored_graph = coloring w cw 0;;
