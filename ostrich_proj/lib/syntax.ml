


(* TYPES *)

type node_ty = 
    | Empty
    | Top 
    | Screen 
    | Table 
    | Column
    | Icon 
    | Expression
    | Input
    | CheckBox
    | Calendar
    | Container
    | List
    | ListItem
    | Search
    | Chart
    | Counter
    | Pagination

type label = string


type ty = 
    | NumT
    | StringT
    (* | BoolC of bool option *)
    | BoolT
    | LabelT of label
    | LabelAttrT of ty * ty
    | NameT of string
    | RecordT of (label * ty) list
    | RecordAttrT of ty
    | ListAttrT of ty
    | EntityT of ty * ty
    | AttributeT of ty * ty
    | NodeT of node_ty list * ty
    | BoxT of ty
    | ListT of ty list
    | TemplateT of ty * ty
    | ForAllNameT of string * ty
    | VarNT of string 
    | ForAllTypeT of string * ty
    | VarT of string
    | ForAllRowsT of string * ty 
    | VarR of string
    | Top







(* TERMS *)

type 'a env = (string * 'a) list

type set_env = unit env

type prop = label * term


and term =
    | Num of int
    | String of string
    | Bool of bool

    | Var of string
    | Label of string
    
    | Entity of string * term * term
    | Attribute of string * label * ty * term

    | Select of term * term
    | Concat of term * term

    | NameOf of term
    | LabelOf of term
    | PropsOf of term
    | AttributesOf of term

    | IsOfType of term * ty
    | Let of string * term * term

    | Template of string * ty * term
    | Instantiate of term * term

    | Node of node_ty * term * term 
    | ForNode of string * string * term * term
    | IfNode of term * term * term

    | Box of term
    | LetBox of string * term * term
    | VarRT of string

    | Closure of term * term env

    | Record of (label * term) list

    | List of term list

    | ForAllName of string * term
    | CallName of term * ty

    | ForAllType of string * term
    | CallType of term * ty

    | ForAllRows of string * term
    | CallRows of term * ty

    | IndexOf of term * int

type pairtt = aterm * ty

and aterm =
    | ANum of int
    | AString of string
    | ABool of bool

    | AVar of string
    | ALabel of string
    
    | AEntity of string * pairtt * term
    | AAttribute of string * label * ty * pairtt

    | ASelect of pairtt * pairtt 
    | AConcat of pairtt * pairtt

    | ANameOf of pairtt
    | ALabelOf of pairtt
    | APropsOf of pairtt
    | AAttributesOf of pairtt

    | AIsOfType of pairtt * ty
    | ALet of string * pairtt * pairtt

    | ATemplate of string * ty * pairtt
    | AInstantiate of pairtt * pairtt

    | ANode of node_ty * pairtt * pairtt
    | AForNode of string * string * pairtt * pairtt list
    | AIfNode of pairtt * pairtt * pairtt

    | ABox of pairtt
    | ALetBox of string * pairtt * pairtt
    | AVarRT of string

    | AClosure of pairtt * term env

    | ARecord of (label * pairtt) list

    | AList of pairtt list

    | AForAllName of string * pairtt
    | ACallName of pairtt * ty

    | AForAllType of string * pairtt
    | ACallType of pairtt * ty

    | AForAllRows of string * pairtt
    | ACallRows of pairtt * ty

    | AIndexOf of pairtt * int
