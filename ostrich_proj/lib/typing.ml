open Syntax
open Utils
open PrettyPrint


type type_error =
    | TypeExpected of ty * ty
    | RecordExpected of ty
    | EntityExpected of ty
    | AttributeExpected of ty 
    | ListExpected of ty
    | BoxExpected of ty 
    | NamesDontMatch of ty * ty
    | BadSelectOp of ty * ty
    | NodeExpected of ty 
    | BoolExpected of ty 
    | TypesDontMatch of ty * ty
    | CollectionExpected of ty
    | BadInstantiation of ty 
    | TypeAbsExpected of ty
    | RowsAbsExpected of ty 
    | NameAbsExpected of ty 

exception Error of type_error

let error err = raise (Error err)


let rec unify t t' = (* Não precisa de ser unificação *)
    if t = t' then Some []
    else match t,t' with 
    | EntityT(n,a), EntityT(n',t') -> if n = n' then Some [] else None (* TODO Add rows *)
    | AttributeT(n,t), AttributeT(n',t') -> if n = n' then unify t t' else None
    | _, Top -> Some [] (* TODO Tirar quando houver foralltype *)
    | Top, _ -> Some []
    | _ -> None (* TODO *)



let (*rec*) apply unification t = t (* TODO: Just in case we need to apply substitutions *)




(* SUBST FUNCTIONS *)

let rec subst_name o n t =
    match t with
    | VarNT x -> 
        if x = o then 
            n 
        else t 
    | EntityT(e, ar) ->
        EntityT(subst_name o n e, ar)
    | AttributeT(e, t) ->
        AttributeT(subst_name o n e, t)
    | TemplateT(t1, t2) ->
        TemplateT(subst_name o n t1, subst_name o n t2)
    | ForAllNameT(s, t1) ->
        if s = o then
            subst_name o n t1
        else
            ForAllNameT(s, subst_name o n t1)
    | ForAllTypeT(t', t1) ->
        ForAllTypeT(t', subst_name o n t1)
    | ForAllRowsT(x, t1) ->
        ForAllRowsT(x, subst_name o n t1)
    | RecordT r -> 
        RecordT( map (fun (l,e) -> (l, subst_name o n e)) r )
    | RecordAttrT n' ->
        RecordAttrT(subst_name o n n')
    | LabelAttrT(n', t') ->
        LabelAttrT(subst_name o n n', t')
    | ListAttrT(n') -> ListAttrT(subst_name o n n')
    | _ -> t



let rec subst_type o n e =
    match e with
    | VarT t -> if t=o then n else e
    | LabelAttrT(l, t) -> LabelAttrT(l, subst_type o n t)
    | AttributeT(n', t) -> AttributeT(n', subst_type o n t)
    | BoxT b -> BoxT(subst_type o n b)
    | ListT l -> ListT( map (fun e' -> subst_type o n e') l)
    | TemplateT(t1, t2) -> TemplateT(subst_type o n t1, subst_type o n t2)
    | ForAllNameT(n', e1) -> ForAllNameT(n', subst_type o n e1)
    | ForAllTypeT(t, e1) -> 
        if t = o then
            subst_type o n e1
        else
            ForAllTypeT(t, subst_type o n e1)
    | ForAllRowsT(x, e1) -> ForAllRowsT(x, subst_type o n e1)
    | NodeT(a, t) -> NodeT(a, subst_type o n t)
    | RecordT r -> RecordT( map (fun (l,t) -> (l, subst_type o n t)) r )
    | _ -> e


let rec subst_rec o n e =
    match e with
    | VarR x -> if x = o then n else e 
    | ForAllRowsT(x, t1) ->
        if x = o then
            subst_rec o n t1
        else
            ForAllRowsT(x, subst_rec o n t1)
    | ForAllTypeT(t, e1) -> ForAllTypeT(t, subst_rec o n e1)
    | ForAllNameT(n', e1) -> ForAllNameT(n', subst_rec o n e1)
    | TemplateT(t1, t2) -> TemplateT(subst_rec o n t1, subst_rec o n t2)
    | ListT l -> ListT( map (fun e' -> subst_rec o n e') l )
    | RecordT r -> RecordT( map (fun (l,e') -> (l, subst_rec o n e')) r )
    | BoxT b -> BoxT(subst_rec o n b)
    | EntityT(n', r) -> EntityT(n', subst_rec o n r)
    | _ -> e





(* TYPECHECKER *)


(* ERRO *)
(* let list_to_set l =
    let rec loop acc l' =
        match l' with
        | [] -> List.rev acc
        | hd :: tl -> (
            try (List.find (fun e -> e = hd) tl) with
            | Not_found -> loop ( acc) tl
            | _ -> assert false
        )
    in 
    loop [] l *)



let type_entity_rec n =
    RecordT(
        ("list", RecordT(
            ("current", RecordAttrT n)::[]
        ))::[]
    )


let rec typecheck_t t env mod_env tenv nenv renv = 
    match t with
    | VarT st -> find st tenv
    | VarNT nt ->
        let _ = find nt nenv in t 
    | VarR rt -> find rt renv
    | AttributeT(n, t') ->
        AttributeT(typecheck_t n env mod_env tenv nenv renv, typecheck_t t' env mod_env tenv nenv renv)
    | EntityT(n, r) ->
        EntityT(typecheck_t n env mod_env tenv nenv renv, typecheck_t r env mod_env tenv nenv renv)
    | LabelAttrT(n, t') -> 
        LabelAttrT(typecheck_t n env mod_env tenv nenv renv, typecheck_t t' env mod_env tenv nenv renv)
    | BoxT dt -> 
        BoxT (typecheck_t dt env mod_env tenv nenv renv)
    | NodeT(al, pr) ->
        NodeT(al, typecheck_t pr env mod_env tenv nenv renv)
    | RecordT r ->
        RecordT( map (fun (l,e) -> (l, typecheck_t e env mod_env tenv nenv renv)) r )
    | _ -> t 


let rec typecheck_record r env mod_env tenv nenv renv =
    map (fun (l, e) -> (l, typecheck e env mod_env tenv nenv renv)) r


(* TYPES ANNOTATED *)

and typecheck e env mod_env tenv nenv renv =
    match e with
    | Num n -> (ANum(n) , NumT)
    | String s -> (AString(s), StringT)
    | Bool b -> (ABool(b), BoolT)
    | Var x -> (AVar(x), find x env)
    | Label l -> (ALabel(l), (LabelT l))

    | Entity(n, ar, vr) ->
        let (aar, tr) = typecheck ar env mod_env tenv nenv renv in
        begin
        match tr with
        | RecordT rt -> (AEntity(n, (aar, tr), vr), EntityT(NameT n, tr))
        | _ -> error (RecordExpected tr)
        end
    | Attribute(n,l,t,p) ->
        let (pa, pt) = typecheck p env mod_env tenv nenv renv in
        begin
        match pt with
        | RecordT r -> (AAttribute(n, l, t, (pa, pt)), AttributeT(NameT n, t))
        | _ -> error (RecordExpected pt)
        end

    | NameOf e1 -> 
        let (a1, t1) = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | EntityT(n, ar) -> (ANameOf(a1, t1), BoxT (type_entity_rec n))
        | _ -> error (EntityExpected t1)
        end
    | LabelOf a -> 
        let (aa,ta) = typecheck a env mod_env tenv nenv renv in
        begin
        match ta with
        | AttributeT(n,t) -> (ALabelOf(aa,ta), BoxT (LabelAttrT(n, t)))
        | _ -> error (AttributeExpected ta)
        end

    | Let(x,e1,e2) -> 
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        let env' = define x t1 env in
        let (a2,t2) = typecheck e2 env' mod_env tenv nenv renv in
        (ALet(x, (a1,t1), (a2,t2)), t2)

    | Node(a,p,n) -> 
        let (pa,pt) = typecheck p env mod_env tenv nenv renv in 
        let (na,nt) = typecheck n env mod_env tenv nenv renv in
        begin
        match nt with
        | ListT l -> (ANode(a, (pa,pt), (na,nt)), NodeT(a :: [], pt))
        | _ -> error (ListExpected nt)
        end

    | Box e1 -> 
        let (a1,t1) = typecheck e1 [] mod_env tenv nenv renv in
        (ABox(a1,t1), BoxT t1 )
    | LetBox(u,e1,e2) -> 
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | BoxT t1' -> 
            let mod_env' = define u t1' mod_env in
            let (a2,t2) = typecheck e2 env mod_env' tenv nenv renv in 
            (ALetBox(u, (a1,t1), (a2,t2)), t2 )
        | _ -> error (BoxExpected t1)
        end
    | VarRT u -> ((AVarRT u), find u mod_env)
    | Closure(e1, env') -> 
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        (AClosure((a1,t1), env'), t1)
    | Record r -> 
        let f (l,elem) = (
            let (a,t) = typecheck elem env mod_env tenv nenv renv in
            (l,t) 
        ) in 
        let rt = RecordT (map f r) in
        let f' (l,elem) = (
            let (a,t) = typecheck elem env mod_env tenv nenv renv in
            (l,(a,t)) 
        ) in 
        (ARecord(map f' r), rt)
    
    | Select(e1, e2) ->
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        let (a2,t2) = typecheck e2 env mod_env tenv nenv renv in
        begin
        match t1, t2 with
        | RecordT r , LabelT l -> (ASelect((a1,t1), (a2,t2)), find l r )
        | RecordAttrT n , LabelAttrT(n', t) ->
            if n = n' then
                (ASelect((a1,t1), (a2,t2)), t)
            else error (NamesDontMatch(n,n'))
        | AttributeT(n, t) , LabelT "DisplayName" -> (ASelect((a1,t1), (a2,t2)), StringT)
        | NodeT (al, RecordT pr) , LabelT l -> (ASelect((a1,t1), (a2,t2)), find l pr)
        | _ -> error (BadSelectOp(t1, t2))
        end
    
    | IsOfType(e', t) -> 
        let (ae,te) = typecheck e' env mod_env tenv nenv renv in
        (AIsOfType((ae,te), t), BoolT)
        (* begin 
        match te with
        | AttributeT(n, VarT t') -> (AIsOfType((ae,te), t), BoolC(None))
        | AttributeT(n, t') -> (AIsOfType((ae,te), t), BoolC(Some (t'=t)))
        | _ -> error (AttributeExpected te)
        (* | _ -> (AIsOfType((ae,te), t), BoolT) *)
        end *)
    
    | IfNode(c, t, f) -> 
        let (ac,tc) = typecheck c env mod_env tenv nenv renv in
        begin
        match tc with
        | BoolT ->
            let (at,tt) = typecheck t env mod_env tenv nenv renv in
            let (af,tf) = typecheck f env mod_env tenv nenv renv in
            begin
            match tt, tf with
            | NodeT (t1, RecordT pr1), NodeT (t2, RecordT pr2) -> (AIfNode((ac,tc), (at,tt), (af,tf)), NodeT (append t1 t2, RecordT (append pr1 pr2)))
            | BoxT (NodeT (t1, RecordT pr1)), NodeT (t2, RecordT pr2) -> (AIfNode((ac,tc), (at,tt), (af,tf)), BoxT (NodeT (append t1 t2, RecordT (append pr1 pr2))))
            | NodeT (t1, RecordT pr1), BoxT (NodeT (t2, RecordT pr2)) -> (AIfNode((ac,tc), (at,tt), (af,tf)), BoxT (NodeT (append t1 t2, RecordT (append pr1 pr2))))
            | BoxT (NodeT (t1, RecordT pr1)), BoxT (NodeT (t2, RecordT pr2)) -> (AIfNode((ac,tc), (at,tt), (af,tf)), BoxT (NodeT (append t1 t2, RecordT (append pr1 pr2))))
            | NodeT(t1, RecordT pr1), _ -> error (NodeExpected tf)
            | _, _ -> error (NodeExpected tt)
            end
        | _ -> error (BoolExpected tc)
        end 

    | AttributesOf e1 ->
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in 
        begin 
        match t1 with
        | EntityT(n, RecordT at) -> (AAttributesOf((a1,t1)), ListT (map (fun (l, t) -> t) at))
        | EntityT(n, VarR r) -> (AAttributesOf((a1,t1)), ListAttrT n)
        | _ -> error (EntityExpected t1)
        end 

    | ForNode(s, t, e1, e2) ->

        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | RecordT r ->              (* not tested *)
            let fa elem = (
                begin
                match elem with
                | (l, e') ->
                    let env' = define s e' env in
                    let t' = (
                        match e' with
                        | AttributeT(n, t') -> t'
                        | _ -> e'
                    ) in 
                    let tenv' = define t t' tenv in
                    let (a2,t2) = typecheck e2 env' mod_env tenv' nenv renv in
                    begin
                    match t2 with
                    | NodeT (a, pr) -> (a2,t2)
                    | BoxT (NodeT (a, pr)) -> (a2,t2)
                    | _ -> error (NodeExpected t2) 
                    end
                end
            ) in
            let lta = map fa r in
            let lt = map ( fun (a,t) -> t ) lta in
            (AForNode(s, t, (a1,t1), lta), ListT lt)
        | ListT l ->
            let fa elem = (
                let env' = define s elem env in
                let t' = (
                    match elem with
                    | AttributeT(n, t') -> t'
                    | _ -> elem
                ) in 
                let tenv' = define t t' tenv in
                let (a2,t2) = typecheck e2 env' mod_env tenv' nenv renv in
                let t2' = typecheck_t t2 env' mod_env tenv' nenv renv in
                begin
                match t2' with
                | NodeT (a, pr) -> (a2,t2')
                | BoxT (NodeT (a, pr)) -> (a2,t2')
                | _ -> error (NodeExpected t2')
                end
            ) in
            let lta = map fa l in
            let lt = map ( fun (a,t) -> t ) lta in
            (AForNode(s, t, (a1,t1), lta), ListT lt)
        | ListAttrT n ->
            let env' = define s (AttributeT(n, Top)) env in
            let tenv' = define t Top tenv in 
            let (a2,t2) = typecheck e2 env' mod_env tenv' env renv in
            begin
            match t2 with
            | BoxT (NodeT (a, pr)) -> (AForNode(s, t, (a1,t1), [(a2,t2)]), ListT [BoxT(NodeT(a, pr))])
            | NodeT (a, pr) -> (AForNode(s, t, (a1,t1), [(a2,t2)]), ListT [NodeT(a, pr)])
            | _ -> error (NodeExpected t2)
            end
        | _ -> error (CollectionExpected t1)
        end 
    
    | List l -> 
        let lta = map (fun x -> typecheck x env mod_env tenv nenv renv) l in
        let lt = map ( fun (a,t) -> t ) lta in
        (AList(lta), ListT lt)

    | Template(x, t, e1) ->
        let t' = typecheck_t t env mod_env tenv nenv renv in
        let env' = define x t env in
        let (a1,t1) = typecheck e1 env' mod_env tenv nenv renv in
        (ATemplate(x, t', (a1,t1)), TemplateT(t', t1))

    | Instantiate(e1, e2) ->
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        begin
        match t1 with
        | TemplateT(t3,t4) -> 
            let (a2,t2) = typecheck e2 env mod_env tenv nenv renv in 
            print_endline "Unifying ";
            print_endline (string_of_type t2);
            print_endline (string_of_type t3);
            let unification = unify t2 t3 in
            if unification <> None then (* Isto não pode ser um = tem que unificar os tipos e obter uma substituição *)
                let un = apply unification t4 in
                begin
                match un with
                | NodeT(a,p) -> (AInstantiate((a1,t1), (a2,t2)), BoxT un )
                | TemplateT(t5,t6) -> (AInstantiate((a1,t1), (a2,t2)), un)
                | _ -> assert false (* faz sentido ter isto aqui?? *)
                end
            else error (TypesDontMatch(t2, t3))
        | _ -> error (BadInstantiation t1)
        end

    | ForAllName(n, e1) ->
        let nenv' = define n (VarNT n) nenv in
        let (a1,t1) = typecheck e1 env mod_env tenv nenv' renv in
        (AForAllName(n, (a1,t1)), ForAllNameT(n, t1))

    | CallName(e1, n) ->
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        (* TODO typecheck do name, como no type *)
        begin
        match t1 with
        | ForAllNameT(n1, t2) -> (ACallName((a1,t1), n), subst_name n1 n t2)
        | _ -> error (NameAbsExpected t1)
        end

    | ForAllType(t, e1) ->
        let tenv' = define t (VarT t) tenv in
        let (a1,t1) = typecheck e1 env mod_env tenv' nenv renv in
        (AForAllType(t, (a1,t1)), ForAllTypeT(t, t1))

    | CallType(e1, t) ->
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        let t' = typecheck_t t env mod_env tenv nenv renv in
        begin
        match t1 with
        | ForAllTypeT(t'', t2) -> (ACallType((a1,t1), t'), subst_type t'' t' t2 )
        | _ -> error (TypeAbsExpected t1)
        end

    | ForAllRows(x, e1) ->
        let renv' = define x (VarR x) renv in
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv' in
        (AForAllRows(x, (a1,t1)), ForAllRowsT(x, t1))
        
    | CallRows(e1, r) ->
        let (a1,t1) = typecheck e1 env mod_env tenv nenv renv in
        let r' = typecheck_t r env mod_env tenv nenv renv in
        begin
        match t1 , r' with
        | ForAllRowsT(x, t2) , RecordT l -> (ACallRows((a1,t1), r'), subst_rec x r' t2 )
        | ForAllRowsT(x, t2) , VarR varr -> (ACallRows((a1,t1), r'), subst_rec x r' t2 )
        | _ -> error (RowsAbsExpected t1)
        end

    | IndexOf(l, i) ->
        let (al,tl) = typecheck l env mod_env tenv nenv renv in
        begin
        match tl with
        | ListT l' -> (AIndexOf((al,tl), i), List.nth l' i )
        | _ -> error (ListExpected tl)
        end

    | _ -> print_endline "Not yet implemented"; assert false
