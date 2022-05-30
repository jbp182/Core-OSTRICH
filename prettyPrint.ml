open Syntax
open Utils


(* PRINTS *)

let string_of_nodetype t =
    match t with
    | Empty -> "Empty"
    | Top -> "Top"
    | Screen -> "Screen"
    | Table -> "Table"
    | Column -> "Column"
    | Icon -> "Icon"
    | Expression -> "Expression"
    | Input -> "Input"
    | CheckBox -> "CheckBox"
    | Calendar -> "Calendar"
    | Container -> "Container"
    | List -> "List"
    | ListItem -> "ListItem"
    | Search -> "Search"
    | Chart -> "Chart"
    | Counter -> "Counter"
    | Pagination -> "Pagination"


let rec string_of_type t =
    match t with
    | NumT -> "Num"
    | StringT -> "String"
    (* | BoolC b -> "BoolC(" ^ (if b=None then "?" else string_of_bool (Option.get b) ) ^ ")" *)
    | BoolT -> "Bool"
    | LabelT label -> "Label("^label^")"
    | LabelAttrT (t,t') -> "{"^(string_of_type t)^":"^(string_of_type t')^"}"
    | NameT s -> "Name("^s^")"
    | RecordT l -> "{"^(String.concat ", " (map (fun (x,y) ->  x ^":"^ (string_of_type y)) l))^"}"
    | RecordAttrT t -> "Attr{"^string_of_type t^"}"
    | EntityT (t,t') -> "Entity("^string_of_type t^", "^string_of_type t'^")"
    | AttributeT (t,t') -> "Attribute("^string_of_type t^":"^string_of_type t'^")"
    | NodeT (l, pr) -> "Node( ["^(String.concat ", " (map (fun t -> string_of_nodetype t) l))^"], "^string_of_type pr^")"
    | BoxT t -> "Box("^string_of_type t^")"
    | ListT l -> "List("^(String.concat ", " (map (fun t -> string_of_type t) l))^")" (* Why is this a list? *)
    | TemplateT (t,t') -> "Template("^string_of_type t^" -> "^string_of_type t'^")"
    | ForAllNameT (s,t) -> "ForAllName("^s^" -> "^string_of_type t^")"
    | VarNT s  -> "VarNT("^s^")"
    | ForAllTypeT (s,t) -> "ForAllType("^s^" -> "^string_of_type t^")"
    | ForAllRowsT (s,t) -> "ForAllRows("^s^" -> "^string_of_type t^")"
    | VarT s  -> "VarT("^s^")"
    | VarR s  -> "VarR("^s^")"
    | ListAttrT n -> "ListAttrT(" ^ string_of_type n ^ ")"
    | Top -> "Top"

and string_of_alphas a =
    match a with
    | hd :: [] -> string_of_nodetype hd 
    | hd :: tl -> string_of_nodetype hd ^ " | " ^ string_of_alphas tl
    | [] -> ""

and string_of_recordt r =
    match r with
    | (s,t)::[] -> s^"="^string_of_type t
    | (s,t)::rem -> s^"="^string_of_type t^" ; "^string_of_recordt rem
    | _ -> ""

and string_of_listt l =
    match l with
    | x::[] -> string_of_type x 
    | x::r -> string_of_type x^" , "^string_of_listt r
    | [] -> "[]"

(*
let string_of_op op =
    match op with
    | Select -> "."
    | Concat -> " + "
*)

let string_tabs times =
    let rec loop acc times =
        if times = 0 then acc
        else loop (acc ^ "    ") (times - 1)
    in 
    loop "" times

let rec string_of_record_label r lvl = 
    match r with
    | (s,t)::[] -> s^"="^string_of_term_lvl t (lvl+1)
    | (s,t)::rem -> s^"="^string_of_term_lvl t (lvl+1)^" ; \n"^string_tabs lvl^string_of_record_label rem lvl
    | _ -> ""

and string_of_list l lvl =
    match l with
    | x::[] -> string_of_term_lvl x (lvl+1)
    | x::r -> string_of_term_lvl x (lvl+1)^" , \n" ^ string_tabs lvl ^string_of_list r lvl
    | [] -> ""

and string_of_record l lvl =
    match l with
    | (s,t)::[] -> s^"="^string_of_term_lvl t (lvl+1)
    | (s,t)::r -> s^"="^string_of_term_lvl t (lvl+1)^" ; \n"^string_tabs lvl ^string_of_record r lvl
    | _ -> ""

and string_of_term_lvl t lvl =
    match t with
    | Num n -> string_of_int n 
    | String s -> "'"^s^"'"
    | Bool b -> string_of_bool b

    | Var x -> "Var("^x^")"
    | Label l -> l

    | Entity(s,l,v) -> "Entity<"^s^", {"^string_of_term_lvl l lvl^"}, {"^string_of_term_lvl v lvl^"}>"
    | Attribute(n,l,t',v) -> "Attribute<"^n^", "^l^", "^string_of_type t'^", "^string_of_term_lvl v lvl^">"
(*
    | Op(o,e,e') -> string_of_term e^string_of_op o^string_of_term e'
*)
    | Let(x,e1,e2) -> "let "^x^"="^string_of_term_lvl e1 lvl^" in\n"^ string_tabs lvl ^string_of_term_lvl e2 (lvl+1)
    | Select(e,e') -> string_of_term_lvl e lvl^"."^string_of_term_lvl e' lvl

    | Node(a,p,n) -> "Node<"^string_of_nodetype a^", "^string_of_term_lvl p lvl^", \n " ^ string_tabs lvl ^string_of_term_lvl n (lvl+1)^">"
    | ForNode(x,t,e,e') -> "forNode("^x^":"^t^" in "^string_of_term_lvl e lvl^",\n"^ string_tabs lvl ^ string_of_term_lvl e' (lvl+1)^")"
    | IfNode(c,e,e') -> "ifNode("^string_of_term_lvl c lvl^",\n"^ string_tabs lvl ^string_of_term_lvl e (lvl+1)^",\n"^ string_tabs lvl ^string_of_term_lvl e' (lvl+1)^")"

    | NameOf e -> "(NameOf "^string_of_term_lvl e lvl^")"
    | LabelOf a -> "(LabelOf "^string_of_term_lvl a lvl^")"
    
    | Closure(e,env) -> "Closure(\n"^string_tabs lvl^string_of_term_lvl e (lvl+1)^")"
     (* " ,\n"^string_tabs (lvl-1)^"["^string_of_record env (lvl-1)^"\n"^string_tabs (lvl-1)^"])" *)
    | Box e -> "Box("^string_of_term_lvl e lvl^")"
    | LetBox(u,e1,e2) -> "letbox "^u^"*="^string_of_term_lvl e1 lvl^" in\n"^ string_tabs lvl ^string_of_term_lvl e2 (lvl+1)
    | VarRT u -> u^"*"

    | Record r -> "{"^string_of_record_label r (lvl-1)^"}"

    | IsOfType(a,t) -> string_of_term_lvl a lvl ^ " isOfType " ^ string_of_type t

    | AttributesOf e -> "AttributesOf " ^ string_of_term_lvl e lvl

    | List l -> "List[" ^ string_of_list l (lvl-1)^ "]"

    | Template(x,t,e) -> "Template<" ^ x ^ ", " ^ string_of_type t ^ ", \n" ^ string_tabs lvl ^ string_of_term_lvl e (lvl+1) ^ ">"

    | Instantiate(t,e) -> string_of_term_lvl t lvl ^ "(" ^ string_of_term_lvl e lvl ^ ")"

    | ForAllName(s,t) -> "ForAllName("^s^" in\n"^ string_tabs lvl ^string_of_term_lvl t (lvl+1)^")"
    | ForAllRows(s,t) -> "ForAllRows("^s^" in\n"^ string_tabs lvl ^string_of_term_lvl t (lvl+1)^")"
    | ForAllType(s,t) -> "ForAllType("^s^" in\n"^ string_tabs lvl ^string_of_term_lvl t (lvl+1)^")"

    | CallName(t, s) -> "CallName (" ^ string_of_type s ^ ") in\n"^ string_tabs lvl ^"(" ^ string_of_term_lvl t (lvl+1)^ ")"
    | CallRows(t,typ) -> "CallRows (" ^ string_of_type typ ^ ") in\n"^ string_tabs lvl ^"(" ^ string_of_term_lvl t (lvl+1)^ ")"
    | CallType(t,typ) -> "CallType (" ^ string_of_type typ ^ ") in\n"^ string_tabs lvl ^"(" ^ string_of_term_lvl t (lvl+1)^ ")"
    (* | CallRows(t, typ) -> "CallRows("^string_of_term t^" with "^string_of_type typ^")"
    | CallType(t, typ) -> "CallType("^string_of_term t^" with "^string_of_type typ^")" *)

    | IndexOf(l, i) -> "IndexOf("^string_of_term_lvl l lvl^", "^string_of_int i^")"

    | _ -> print_endline "Not yet implemented"; assert false

let string_of_term t =
    string_of_term_lvl t 1

let rec string_of_pair_list l lvl =
    match l with
    | x::[] -> string_of_pairtt_lvl x (lvl+1)
    | x::r -> string_of_pairtt_lvl x (lvl+1)^" , "^string_of_pair_list r lvl
    | [] -> ""

and string_of_record_pair r lvl =
    match r with
    | (l,e)::[] -> l^"="^string_of_pairtt_lvl e (lvl+1)
    | (l,e)::rem -> l^"="^string_of_pairtt_lvl e (lvl+1)^" ; "^string_of_record_pair rem lvl
    | _ -> ""

and string_of_aterm_lvl at lvl =
    match at with
    | ANum n -> string_of_int n 
    | AString s -> "'"^s^"'"
    | ABool b -> string_of_bool b

    | AVar x -> x
    | ALabel l -> l

    | AEntity(s,l,v) -> "Entity<"^s^", {"^string_of_pairtt_lvl l lvl^"}, {"^string_of_term_lvl v lvl^"}>"
    | AAttribute(n,l,t',v) -> "Attribute<"^n^", "^l^", "^string_of_type t'^", "^string_of_pairtt_lvl v lvl^">"

    | ALet(x,e1,e2) -> "let "^x^"="^string_of_pairtt_lvl e1 lvl^" in \n"^string_tabs lvl^string_of_pairtt_lvl e2 (lvl+1)
    | ASelect(e,e') -> string_of_pairtt_lvl e lvl^"."^string_of_pairtt_lvl e' lvl

    | ANode(a,p,n) -> "Node<"^string_of_nodetype a^", "^string_of_pairtt_lvl p lvl^", \n"^string_tabs lvl^string_of_pairtt_lvl n (lvl+1)^">"
    | AForNode(x,t,e,e') -> "forNode("^x^":"^t^" in "^string_of_pairtt_lvl e lvl^", \n"^string_tabs lvl^string_of_pair_list e' (lvl+1)^")"
    | AIfNode(c,e,e') -> "ifNode("^string_of_pairtt_lvl c lvl^", \n"^string_tabs lvl^string_of_pairtt_lvl e (lvl+1)^", \n"^string_tabs lvl^string_of_pairtt_lvl e' (lvl+1)^")"

    | ANameOf e -> "(NameOf "^string_of_pairtt_lvl e lvl^")"
    | ALabelOf a -> "(LabelOf "^string_of_pairtt_lvl a lvl^")"
    
    | AClosure(e,env) -> "Closure(\n"^string_tabs lvl^string_of_pairtt_lvl e (lvl+1)^" , \n"^string_tabs (lvl-1)^"["^string_of_record env (lvl)^"\n"^string_tabs (lvl-1)^"])"
    | ABox e -> "Box("^string_of_pairtt_lvl e lvl^")"
    | ALetBox(u,e1,e2) -> "letbox "^u^"*="^string_of_pairtt_lvl e1 lvl^" in \n"^string_tabs lvl^string_of_pairtt_lvl e2 (lvl+1)
    | AVarRT u -> u^"*"

    | ARecord r -> "{"^string_of_record_pair r (lvl-1)^"}"

    | AIsOfType(a,t) -> string_of_pairtt_lvl a lvl ^ " isOfType " ^ string_of_type t

    | AAttributesOf e -> "AttributesOf " ^ string_of_pairtt_lvl e lvl

    | AList l -> "List[" ^ string_of_pair_list l (lvl-1)^ "]"

    | ATemplate(x,t,e) -> "Template<" ^ x ^ ", " ^ string_of_type t ^ ", \n"^ string_tabs lvl ^ string_of_pairtt_lvl e (lvl+1)^ ">"

    | AInstantiate(t,e) -> string_of_pairtt_lvl t lvl ^ "(" ^ string_of_pairtt_lvl e lvl ^ ")"

    | AForAllName(s,t) -> "ForAllName("^s^" in \n" ^string_tabs lvl^string_of_pairtt_lvl t (lvl+1)^")"
    | AForAllRows(s,t) -> "ForAllRows("^s^" in \n" ^string_tabs lvl^string_of_pairtt_lvl t (lvl+1)^")"
    | AForAllType(s,t) -> "ForAllType("^s^" in \n" ^string_tabs lvl^string_of_pairtt_lvl t (lvl+1)^")"

    | ACallName(t, s) -> "CallName (" ^ string_of_type s ^ ") in \n"^string_tabs lvl^"(" ^ string_of_pairtt_lvl t (lvl+1) ^ ")"
    | ACallRows(t,typ) -> "CallRows (" ^ string_of_type typ ^ ") in \n"^string_tabs lvl^"(" ^ string_of_pairtt_lvl t (lvl+1) ^ ")"
    | ACallType(t,typ) -> "CallType (" ^ string_of_type typ ^ ") in \n"^string_tabs lvl^"(" ^ string_of_pairtt_lvl t (lvl+1) ^ ")"

    | AIndexOf(l, i) -> "IndexOf("^string_of_pairtt_lvl l lvl^", "^string_of_int i^")"

    | _ -> print_endline "Not yet implemented"; assert false


and string_of_pairtt_lvl (a,t) lvl =
    string_of_aterm_lvl a lvl^ " \n"^string_tabs (lvl-1)^"@<" ^ string_of_type t ^ ">@"


let string_of_pairtt (a,t) = 
    string_of_pairtt_lvl (a,t) 1

let print s =
    print_endline ("- : PRINT = \n" ^ s)
