open Syntax
open Utils


let build_entity_record r =
    Record( 
        ("list" , Record( 
            ("current" , Record r)::[]
        ))::[] 
    )


(* EVAL *)   

let rec eval e env =
    match e with
    | Num n -> Num n       
    | String s -> String s    
    | Bool b -> Bool b     

    | Var x -> find x env
    | Label l -> Label l

    | Entity(n,al,av) -> Entity(n, eval al env, av)

    | Attribute(n,l,t,p) -> Attribute(n, l, t, eval p env)

    | NameOf e1 -> 
        begin 
        match eval e1 env with
        | Entity(n, Record ar, Record v) -> Closure( Box( build_entity_record v ), env ) 
        | _ -> assert false
        end
    | LabelOf a ->
        begin
        match eval a env with
        | Attribute(n,l,t,p) -> Closure( Box( Label l ), env ) 
        | _ -> assert false
        end

    | Let(x,e1,e2) -> 
        let v1 = eval e1 env in
        let env' = define x v1 env in
        eval e2 env' 
    
    | Node(a,p,n) -> 
        let ps = eval p env in
        let ns = eval n env in
        begin 
        match ns with 
        | List l -> 
            Node(a, ps, ns)
        | _ -> assert false
        end

    | Box e1 -> 
        Closure(e1, env)
    | LetBox(u,e1,e2) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(Box(e3), env') ->
            let env'' = define u (Closure(e3, env')) env in 
            eval e2 env''
        | _ -> assert false
        end
    | VarRT v -> eval (find v env) env
    | Closure(e1, env') -> 
        eval e1 env'

    | Record r -> Record( map (fun (l, e) -> (l, eval e env)) r )

    | Select(e1, e2) ->
        let v1 = eval e1 env in
        let v2 = eval e2 env in 
        begin
        match v1, v2 with
        | Record r , Label l -> find l r 
        | Attribute(n, l', t, Record p) , Label l -> find l p 
        | Node(a,Record p,n) , Label l -> find l p 
        | _ -> assert false
        end

    | IsOfType(e,t) ->
        begin
        match eval e env with 
        | Attribute(n,l,t',p) -> Bool (t'=t)
        | _ -> assert false
        end

    | IfNode(c,t,f) ->
        begin
        match eval c env with
        | Bool b -> if b then 
                        eval t env
                    else 
                        eval f env
        | _ -> assert false
        end

    | AttributesOf e1 -> 
        begin
        match eval e1 env with
        | Entity(n, Record ar, Record vr) -> List (map (fun (l, a) -> a) ar) 
        | _ -> assert false
        end

    | PropsOf a ->          (* not tested yet *)
        begin
        match eval a env with
        | Attribute(n, l, t, p) -> p
        | _ -> assert false (* node???  *)
        end

    | ForNode(s, t, e1, e2) -> 
        let v1 = eval e1 env in
        begin
        match v1 with
        | Record r -> 
            let f = fun (l, e3) -> (
                let env' = define s e3 env in
                eval e2 env'
            ) in 
            List (map f r)      (* List ?? *)
        | List l ->
            let f = fun e3 -> (
                let env' = define s e3 env in
                eval e2 env'
            ) in
            List (map f l)
        | _ -> assert false
        end

    | List l -> List (map (fun x -> eval x env) l)  

    | Template(x,t,e1) -> Closure(e, env)
    | Instantiate(e1,e2) -> 
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(Template(x,t,e3), env') -> 
            let v2 = eval e2 env in
            let env'' = define x v2 env' in
            let v3 = eval e3 env'' in
            begin
            match v3 with
            | Node(a,p,n) -> Closure(Box( Node(a,p,n) ), env'')
            | Closure(Template(_,_,_), _) -> v3
            | _ -> assert false
            end
        | _ -> assert false
        end
    
    | ForAllName(n, e1) ->
        Closure(e, env)
    | CallName(e1, n) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(ForAllName(n', e2), env') ->
            eval e2 env'            (* ASSIM ??? *)
        | _ -> assert false
        end

    | ForAllType(t, e1) ->
        Closure(e, env)
    | CallType(e1, t) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(ForAllType(t', e2), env') ->
            eval e2 env'
        | _ -> assert false
        end

    | ForAllRows(x, e1) ->
        Closure(e, env)
    | CallRows(e1, e2) ->
        let v1 = eval e1 env in
        begin
        match v1 with
        | Closure(ForAllRows(x, e3), env') ->
            eval e3 env'
        | _ -> assert false
        end

    | IndexOf(l, i) ->
        begin
        match eval l env with
        | List l' -> List.nth l' i 
        | _ -> assert false
        end

    | _ -> print_endline "Not yet implemented"; assert false