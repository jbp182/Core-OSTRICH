
open Syntax



(* tail-recursive append *)
let append l1 l2 =
    let rec append_aux acc l1 l2 =
        match l1, l2 with
        | [], [] -> List.rev acc
        | [], h :: t -> append_aux (h :: acc) [] t
        | h :: t, l -> append_aux (h :: acc) t l
    in
    append_aux [] l1 l2

(* tail-recursive map *)
let map f l = 
    let rec map_aux acc f l =
        match l with
        | [] -> List.rev acc
        | h::t -> map_aux ((f h)::acc) f t 
    in
    map_aux [] f l 




(* build select from list. tail-recursive *)
let mk_select_list l =
    let rec select_aux acc l =
        match l with 
        | hd::tl -> select_aux (Select(acc, hd)) tl
        | [] -> acc
    in 
    match l with
    | hd::tl -> select_aux hd tl
    | [] -> assert false


exception My_not_found of string

(* env get and put *)
let find x env = 
    try List.assoc x env with (* 'a -> ('a * 'b) list -> 'b *)
        Not_found -> raise(My_not_found(x))
let define x v env = (x,v)::env