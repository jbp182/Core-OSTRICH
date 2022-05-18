open Syntax
open Utils
open Eval 
open Typing 
open PrettyPrint



(* EXAMPLE 1 *)


let templ_def = 
        ForAllName("N",
            ForAllType("T",
                ForAllRows("R",
                    Template("e",
                                EntityT(VarNT "N", VarR "R"),
                        Template("a",
                                    AttributeT(VarNT "N", VarT "T"),
                            LetBox("name", 
                                NameOf (Var "e"), 
                                LetBox("label", 
                                    LabelOf (Var "a"),
                                    Node(Expression, 
                                        Record [("Value", 
                                            (Box 
                                            (mk_select_list [
                                                VarRT "name"; 
                                                Label "list";
                                                Label "current";
                                                VarRT "label"]))
                                            )
                                            ], 
                                    List [])
                                )
                            )
                        )
                    )
                )
            )  
        )
        

let templ_inst =

    Let("f" ,
        templ_def
        ,
        Let("e1", 
            Entity("Product", Record [("Description", Attribute("Product", "Description", StringT, Record []))], Record [("Description", String "HelloWorld")]),
            Let("e2",
                Entity("Store", Record [
                                        ("IsInStock", Attribute("Store", "IsInStock", BoolT, Record [])) ;
                                        ("Random", Attribute("Store", "Random", NumT, Record []))] ,
                                Record [
                                        ("IsInStock", Bool true) ;
                                        ("Random", Num 17)
                                ]) ,
                
                    
                ForNode("a", 
                "X", 
                AttributesOf (Var "e2"), 
                    Instantiate( 
                        Instantiate( 
                        CallRows( 
                            CallType( CallName(Var "f", NameT "Store") , VarT "X" ) 
                            ,
                            RecordT [
                                    ("IsInStock", AttributeT(NameT "Store", StringT)) ;
                                    ("Random", AttributeT(NameT "Store", NumT))
                                    ] 
                        ) 
                        , Var "e2" ) 
                    , Var "a" )
                )
                
            )

            
        )
        
    )


let rt = LetBox("n", 
            IndexOf(templ_inst, 0),
            VarRT "n")
let ct_rt = 
    LetBox("n",
            IndexOf(templ_inst, 1),
            VarRT "n")

let _ = print_endline "EVAL!!"
let _ = print(string_of_term (eval templ_def []))
let _ = print(string_of_term (eval templ_inst []))
let _ = print(string_of_term (eval rt []))
let _ = print(string_of_term (eval ct_rt []))

let _ = print_endline "TYPECHECK!!"
(* let _ = print(string_of_pairtt (typecheck templ_def [] [] [] [] []))
let _ = print(string_of_pairtt (typecheck templ_inst [] [] [] [] []))
let _ = print(string_of_pairtt (typecheck rt [] [] [] [] []))
let _ = print(string_of_pairtt (typecheck ct_rt [] [] [] [] [])) *)



(* EXAMPLE 2 ~ models *)

let templ = 
        ForAllName("N",
            ForAllRows("R",
                Template("e",
                    EntityT(VarNT "N", VarR "R"),
                    Node(Screen,
                            Record [("Name",
                                String "List")],
                            List [
                            LetBox("name",
                                NameOf (Var "e"),
                                Node(Table,
                                    Record [("Source", 
                                    Box(mk_select_list [
                                        VarRT "name";
                                        Label "list"
                                    ]))],
                                    ForNode("attr",
                                        "X",
                                        AttributesOf (Var "e"),
                                        Node(Column,
                                                Record [("Title",
                                                Select(Var "attr", Label "DisplayName"))],
                                                List [
                                                LetBox("label",
                                                    LabelOf(Var "attr"),
                                                    IfNode(IsOfType(Var "attr", BoolT),
                                                        Node(Icon, 
                                                            Record [("Visible",
                                                            Box(mk_select_list
                                                            [VarRT "name";
                                                            Label "list";
                                                            Label "current";
                                                            VarRT "label"]))],
                                                            List []),
                                                        Node(Expression,
                                                            Record [("Value",
                                                            Box(mk_select_list
                                                            [VarRT "name";
                                                            Label "list";
                                                            Label "current";
                                                            VarRT "label"]))],
                                                            List []))
                                                )
                                                ])
                                        )  
                                    )
                                )
                            ]
                    )
                )
            )  
        )



let models =
    Let("f" ,
        templ
        ,
        Let ("e",
        Entity("Product",
            Record[("Description", Attribute("Product", "Description", StringT, Record[("DisplayName", String "Description")]));
                    ("IsInStock", Attribute("Product", "IsInStock", BoolT, Record[("DisplayName", String "Is In Stock")]))],
            Record[("Description", String "LED Tv"); ("IsInStock", Bool true)]
            ),
        Instantiate(
            CallRows( 
                CallName(Var "f", NameT "Product"), 
                RecordT[("Description", AttributeT(NameT "Product", StringT));
                        ("IsInStock", AttributeT(NameT "Product", BoolT))]
            ),
            Var "e"
        )
        )
    )



let runt = LetBox("n",
                    models,
                    VarRT "n")

let _ = print(string_of_term (eval templ []))
let _ = print(string_of_term (eval models []))
let _ = print (string_of_term (eval runt []))


let _ = print(string_of_pairtt (typecheck templ [] [] [] [] []))
let _ = print(string_of_pairtt (typecheck models [] [] [] [] []))
let _ = print(string_of_pairtt (typecheck runt [] [] [] [] []))





(* EXAMPLE 3 ~ models 2022 *)

let templ = 
        ForAllName("N",
            ForAllRows("R",
                ForAllType("T",
                    Template("e",
                        EntityT(VarNT "N", VarR "R"),
                        Template("attr",
                            AttributeT(VarNT "N", VarT "T"),
                                Node(Column,
                                        Record [("Title",
                                        Select(Var "attr", Label "DisplayName"))],
                                        List [
                                        LetBox("name",
                                            NameOf (Var "e"),
                                            LetBox("label",
                                                LabelOf(Var "attr"),
                                                IfNode(IsOfType(Var "attr", BoolT),
                                                    Node(Icon, 
                                                        Record [("Visible",
                                                        Box(mk_select_list
                                                        [VarRT "name";
                                                        Label "list";
                                                        Label "current";
                                                        VarRT "label"]))],
                                                        List []),
                                                    Node(Expression,
                                                        Record [("Value",
                                                        Box(mk_select_list
                                                        [VarRT "name";
                                                        Label "list";
                                                        Label "current";
                                                        VarRT "label"]))],
                                                        List []))
                                            )
                                        )
                                ])
                        )
                    )
                )
            )
        )  


let _ = print(string_of_term (eval templ []))
let _ = print(string_of_pairtt (typecheck templ [] [] [] [] []))


let e = Instantiate( Instantiate(CallType( CallRows( CallName(Var "in", VarNT "N"), VarR "R" ), VarT "T"), Var "e"), Var "attr")
let e' = CallName(Var "in", VarNT "N")
let tin = ForAllNameT("N", TemplateT(EntityT(VarNT("N"), RecordT []), NodeT( [Column], RecordT [("Title",StringT)])))
let expt = (ACallName
  ((AVar "in",
    ForAllNameT ("N",
     TemplateT (EntityT (VarNT "N", RecordT []),
      NodeT ([Column], RecordT [("Title", StringT)])))),
  VarNT "N"),
 TemplateT (EntityT (VarNT "N", RecordT []),
  NodeT ([Column], RecordT [("Title", StringT)])))
let _ = assert( (typecheck e' [("in", tin)] [] [] [("N", VarNT "N")] []) = expt )


let e' = CallRows( CallName(Var "in", VarNT "N"), VarR "R" )
let tin = ForAllNameT("N", ForAllRowsT("R", TemplateT(EntityT(VarNT("N"), VarR "R"), NodeT( [Column], RecordT [("Title",StringT)]))))
let expt = (ACallRows
  ((ACallName
     ((AVar "in",
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          NodeT ([Column], RecordT [("Title", StringT)]))))),
     VarNT "N"),
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      NodeT ([Column], RecordT [("Title", StringT)])))),
  VarR "R"),
 TemplateT (EntityT (VarNT "N", VarR "R"),
  NodeT ([Column], RecordT [("Title", StringT)])))
let _ = assert( typecheck e' [("in", tin)] [] [] [("N", VarNT "N")] [("R", VarR "R")] = expt)





let e' = CallType(Var "in", Top)
let tin = ForAllTypeT("T", NodeT([Expression], RecordT [("random", VarT "T")]))
let expt = (ACallType
  ((AVar "in",
    ForAllTypeT ("T", NodeT ([Expression], RecordT [("random", VarT "T")]))),
  Top),
 NodeT ([Expression], RecordT [("random", Top)]))
let _ = assert( typecheck e' [("in", tin)] [] [] [] [] = expt)





let e' = CallType( CallRows( CallName(Var "in", VarNT "N"), VarR "R" ), VarT "S")
let tin = ForAllNameT("N", ForAllRowsT("R", ForAllTypeT("T", TemplateT(EntityT(VarNT("N"), VarR "R"), NodeT( [Column], RecordT [("Title",VarT "T")])))))
let expt = (ACallType
  ((ACallRows
     ((ACallName
        ((AVar "in",
          ForAllNameT ("N",
           ForAllRowsT ("R",
            ForAllTypeT ("T",
             TemplateT (EntityT (VarNT "N", VarR "R"),
              NodeT ([Column], RecordT [("Title", VarT "T")])))))),
        VarNT "N"),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          NodeT ([Column], RecordT [("Title", VarT "T")]))))),
     VarR "R"),
    ForAllTypeT ("T",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      NodeT ([Column], RecordT [("Title", VarT "T")])))),
  VarT "S"),
 TemplateT (EntityT (VarNT "N", VarR "R"),
  NodeT ([Column], RecordT [("Title", VarT "S")])))
let _ = assert( typecheck e' [("in", tin)] [] [("S", VarT "S")] [("N", VarNT "N")] [("R", VarR "R")] = expt )



let e' = Instantiate(CallType( CallRows( CallName(Var "in", VarNT "N"), VarR "R" ), VarT "S"), Var "e")
let tin = ForAllNameT("N", ForAllRowsT("R", ForAllTypeT("T", TemplateT(EntityT(VarNT("N"), VarR "R"), NodeT( [Column], RecordT [("Title",VarT "T")])))))
let te = EntityT(VarNT "N", VarR "R")
let expt = (AInstantiate
  ((ACallType
     ((ACallRows
        ((ACallName
           ((AVar "in",
             ForAllNameT ("N",
              ForAllRowsT ("R",
               ForAllTypeT ("T",
                TemplateT (EntityT (VarNT "N", VarR "R"),
                 NodeT ([Column], RecordT [("Title", VarT "T")])))))),
           VarNT "N"),
          ForAllRowsT ("R",
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             NodeT ([Column], RecordT [("Title", VarT "T")]))))),
        VarR "R"),
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         NodeT ([Column], RecordT [("Title", VarT "T")])))),
     VarT "S"),
    TemplateT (EntityT (VarNT "N", VarR "R"),
     NodeT ([Column], RecordT [("Title", VarT "S")]))),
  (AVar "e", EntityT (VarNT "N", VarR "R"))),
 BoxT (NodeT ([Column], RecordT [("Title", VarT "S")])))
let _ = assert( typecheck e' [("in", tin); ("e", te)] [] [("S", VarT "S")] [("N", VarNT "N")] [("R", VarR "R")] = expt)





let e' = Instantiate( Instantiate(CallType( CallRows( CallName(Var "in", VarNT "N"), VarR "R" ), VarT "S"), Var "e"), Var "attr")
let tin = ForAllNameT("N", ForAllRowsT("R", ForAllTypeT("T", TemplateT(EntityT(VarNT("N"), VarR "R"), TemplateT(AttributeT(VarNT "N", VarT "T"), NodeT( [Column], RecordT [("Title",VarT "T")]))))))
let tattr = AttributeT(VarNT "N", VarT "S")
let expt = (AInstantiate
  ((AInstantiate
     ((ACallType
        ((ACallRows
           ((ACallName
              ((AVar "in",
                ForAllNameT ("N",
                 ForAllRowsT ("R",
                  ForAllTypeT ("T",
                   TemplateT (EntityT (VarNT "N", VarR "R"),
                    TemplateT (AttributeT (VarNT "N", VarT "T"),
                     NodeT ([Column], RecordT [("Title", VarT "T")]))))))),
              VarNT "N"),
             ForAllRowsT ("R",
              ForAllTypeT ("T",
               TemplateT (EntityT (VarNT "N", VarR "R"),
                TemplateT (AttributeT (VarNT "N", VarT "T"),
                 NodeT ([Column], RecordT [("Title", VarT "T")])))))),
           VarR "R"),
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Column], RecordT [("Title", VarT "T")]))))),
        VarT "S"),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", Top),
         NodeT ([Column], RecordT [("Title", VarT "S")])))),
     (AVar "e", EntityT (VarNT "N", VarR "R"))),
    TemplateT (AttributeT (VarNT "N", Top),
     NodeT ([Column], RecordT [("Title", VarT "S")]))),
  (AVar "attr", AttributeT (VarNT "N", Top))),
 BoxT (NodeT ([Column], RecordT [("Title", VarT "S")])))
let _ = print( string_of_pairtt( typecheck e' [("in", tin); ("e", te); ("attr", tattr)] [] [("S", VarT "S")] [("N", VarNT "N")] [("R", VarR "R")] ))
(* let _ = assert( typecheck e' [("in", tin); ("e", te); ("attr", tattr)] [] [("T", Top)] [("N", VarNT "N")] [("R", VarR "R")] = expt ) *)



let outer = 
Let("in",
    templ,
    ForAllName("N",
                ForAllRows("R",
                    Template("e",
                        EntityT(VarNT "N", VarR "R"),
                        Node(Screen,
                                Record [("Name",
                                    String "List")],
                                List [
                                LetBox("name",
                                    NameOf (Var "e"),
                                    Node(Table,
                                        Record [("Source", 
                                        Box(mk_select_list [
                                            VarRT "name";
                                            Label "list"
                                        ]))],
                                        ForNode("attr",
                                            "X",
                                            AttributesOf (Var "e"),
                                            LetBox("appl",
                                                Instantiate( Instantiate(CallType( CallRows( CallName(Var "in", VarNT "N"), VarR "R" ), VarT "X"), Var "e"), Var "attr"),
                                                VarRT "appl"
                                            )
                                            )  
                                        )
                                    )
                                ]
                        )
                    )
                )  
            )
)

let expt = (ALet ("in",
  (AForAllName ("N",
    (AForAllRows ("R",
      (AForAllType ("T",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
            (ANode (Column,
              (ARecord
                [("Title",
                  (ASelect ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                    (ALabel "DisplayName", LabelT "DisplayName")),
                   StringT))],
               RecordT [("Title", StringT)]),
              (AList
                [(ALetBox ("name",
                   (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                    BoxT
                     (RecordT
                       [("list",
                         RecordT [("current", RecordAttrT (VarNT "N"))])])),
                   (ALetBox ("label",
                     (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                      BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                     (AIfNode
                       ((AIsOfType
                          ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                          BoolT),
                         BoolT),
                       (ANode (Icon,
                         (ARecord
                           [("Visible",
                             (ABox
                               (ASelect
                                 ((ASelect
                                    ((ASelect
                                       ((AVarRT "name",
                                         RecordT
                                          [("list",
                                            RecordT
                                             [("current",
                                               RecordAttrT (VarNT "N"))])]),
                                       (ALabel "list", LabelT "list")),
                                      RecordT
                                       [("current", RecordAttrT (VarNT "N"))]),
                                    (ALabel "current", LabelT "current")),
                                   RecordAttrT (VarNT "N")),
                                 (AVarRT "label",
                                  LabelAttrT (VarNT "N", VarT "T"))),
                                VarT "T"),
                              BoxT (VarT "T")))],
                          RecordT [("Visible", BoxT (VarT "T"))]),
                         (AList [], ListT [])),
                        NodeT ([Icon], RecordT [("Visible", BoxT (VarT "T"))])),
                       (ANode (Expression,
                         (ARecord
                           [("Value",
                             (ABox
                               (ASelect
                                 ((ASelect
                                    ((ASelect
                                       ((AVarRT "name",
                                         RecordT
                                          [("list",
                                            RecordT
                                             [("current",
                                               RecordAttrT (VarNT "N"))])]),
                                       (ALabel "list", LabelT "list")),
                                      RecordT
                                       [("current", RecordAttrT (VarNT "N"))]),
                                    (ALabel "current", LabelT "current")),
                                   RecordAttrT (VarNT "N")),
                                 (AVarRT "label",
                                  LabelAttrT (VarNT "N", VarT "T"))),
                                VarT "T"),
                              BoxT (VarT "T")))],
                          RecordT [("Value", BoxT (VarT "T"))]),
                         (AList [], ListT [])),
                        NodeT ([Expression],
                         RecordT [("Value", BoxT (VarT "T"))]))),
                      NodeT ([Icon; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))]))),
                    NodeT ([Icon; Expression],
                     RecordT
                      [("Visible", BoxT (VarT "T"));
                       ("Value", BoxT (VarT "T"))]))),
                  NodeT ([Icon; Expression],
                   RecordT
                    [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))],
               ListT
                [NodeT ([Icon; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])])),
             NodeT ([Column], RecordT [("Title", StringT)]))),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Column], RecordT [("Title", StringT)])))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Column], RecordT [("Title", StringT)]))))),
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Column], RecordT [("Title", StringT)])))))),
     ForAllRowsT ("R",
      ForAllTypeT ("T",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([Column], RecordT [("Title", StringT)]))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Column], RecordT [("Title", StringT)]))))))),
  (AForAllName ("N",
    (AForAllRows ("R",
      (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
        (ANode (Screen,
          (ARecord [("Name", (AString "List", StringT))],
           RecordT [("Name", StringT)]),
          (AList
            [(ALetBox ("name",
               (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                BoxT
                 (RecordT
                   [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
               (ANode (Table,
                 (ARecord
                   [("Source",
                     (ABox
                       (ASelect
                         ((AVarRT "name",
                           RecordT
                            [("list",
                              RecordT [("current", RecordAttrT (VarNT "N"))])]),
                         (ALabel "list", LabelT "list")),
                        RecordT [("current", RecordAttrT (VarNT "N"))]),
                      BoxT (RecordT [("current", RecordAttrT (VarNT "N"))])))],
                  RecordT
                   [("Source",
                     BoxT (RecordT [("current", RecordAttrT (VarNT "N"))]))]),
                 (AForNode ("attr", "X",
                   (AAttributesOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                    ListAttrT (VarNT "N")),
                   [(ALetBox ("appl",
                      (AInstantiate
                        ((AInstantiate
                           ((ACallType
                              ((ACallRows
                                 ((ACallName
                                    ((AVar "in",
                                      ForAllNameT ("N",
                                       ForAllRowsT ("R",
                                        ForAllTypeT ("T",
                                         TemplateT
                                          (EntityT (VarNT "N", VarR "R"),
                                          TemplateT
                                           (AttributeT (VarNT "N", VarT "T"),
                                           NodeT ([Column],
                                            RecordT [("Title", StringT)]))))))),
                                    VarNT "N"),
                                   ForAllRowsT ("R",
                                    ForAllTypeT ("T",
                                     TemplateT (EntityT (VarNT "N", VarR "R"),
                                      TemplateT
                                       (AttributeT (VarNT "N", VarT "T"),
                                       NodeT ([Column],
                                        RecordT [("Title", StringT)])))))),
                                 VarR "R"),
                                ForAllTypeT ("T",
                                 TemplateT (EntityT (VarNT "N", VarR "R"),
                                  TemplateT (AttributeT (VarNT "N", VarT "T"),
                                   NodeT ([Column],
                                    RecordT [("Title", StringT)]))))),
                              Top),
                             TemplateT (EntityT (VarNT "N", VarR "R"),
                              TemplateT (AttributeT (VarNT "N", Top),
                               NodeT ([Column], RecordT [("Title", StringT)])))),
                           (AVar "e", EntityT (VarNT "N", VarR "R"))),
                          TemplateT (AttributeT (VarNT "N", Top),
                           NodeT ([Column], RecordT [("Title", StringT)]))),
                        (AVar "attr", AttributeT (VarNT "N", Top))),
                       BoxT (NodeT ([Column], RecordT [("Title", StringT)]))),
                      (AVarRT "appl",
                       NodeT ([Column], RecordT [("Title", StringT)]))),
                     NodeT ([Column], RecordT [("Title", StringT)]))]),
                  ListT [NodeT ([Column], RecordT [("Title", StringT)])])),
                NodeT ([Table],
                 RecordT
                  [("Source",
                    BoxT (RecordT [("current", RecordAttrT (VarNT "N"))]))]))),
              NodeT ([Table],
               RecordT
                [("Source",
                  BoxT (RecordT [("current", RecordAttrT (VarNT "N"))]))]))],
           ListT
            [NodeT ([Table],
              RecordT
               [("Source",
                 BoxT (RecordT [("current", RecordAttrT (VarNT "N"))]))])])),
         NodeT ([Screen], RecordT [("Name", StringT)]))),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        NodeT ([Screen], RecordT [("Name", StringT)])))),
     ForAllRowsT ("R",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       NodeT ([Screen], RecordT [("Name", StringT)]))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      NodeT ([Screen], RecordT [("Name", StringT)])))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    NodeT ([Screen], RecordT [("Name", StringT)])))))
let _ = assert (typecheck outer [] [] [] [] [] = expt)



(* 2 entidades, com attr errado *)

let outer = 
Let("in",
    templ,
    ForAllName("N",
        ForAllRows("R",
            ForAllName("N2",
                ForAllRows("R2",
                    Template("e",
                        EntityT(VarNT "N", VarR "R"),
                        Template("fake",
                        EntityT(VarNT "N2", VarR "R2"),
                            Node(Screen,
                                    Record [("Name",
                                        String "List")],
                                    List [
                                    LetBox("name",
                                        NameOf (Var "e"),
                                        Node(Table,
                                            Record [("Source", 
                                            Box(mk_select_list [
                                                VarRT "name";
                                                Label "list"
                                            ]))],
                                            ForNode("attr",
                                                "X",
                                                AttributesOf (Var "fake"),
                                                LetBox("appl",
                                                    Instantiate( Instantiate(CallType( CallRows( CallName(Var "in", VarNT "N"), VarR "R" ), VarT "X"), Var "e"), Var "attr"),
                                                    VarRT "appl"
                                                )
                                                )  
                                            )
                                        )
                                    ]
                            )
                        )
                    )
                )  
            )
        )
    )
)


(* TODO dar bem qd da erro. try catch *)
(* let _ = typecheck outer [] [] [] [] [] *)


(* inner template. column *)
let templ = 
    ForAllName("N2",
        ForAllName("N",
            ForAllRows("R",
                ForAllType("T",
                    Template("e",
                        EntityT(VarNT "N", VarR "R"),
                        Template("attr",
                            AttributeT(VarNT "N2", VarT "T"),
                                Node(Column,
                                        Record [("Title",
                                        Select(Var "attr", Label "DisplayName"))],
                                        List [
                                        LetBox("name",
                                            NameOf (Var "e"),
                                            LetBox("label",
                                                LabelOf(Var "attr"),
                                                IfNode(IsOfType(Var "attr", BoolT),
                                                    Node(Icon, 
                                                        Record [("Visible",
                                                        Box(mk_select_list
                                                        [VarRT "name";
                                                        Label "list";
                                                        Label "current";
                                                        VarRT "label"]))],
                                                        List []),
                                                    Node(Expression,
                                                        Record [("Value",
                                                        Box(mk_select_list
                                                        [VarRT "name";
                                                        Label "list";
                                                        Label "current";
                                                        VarRT "label"]))],
                                                        List []))
                                            )
                                        )
                                ])
                        )
                    )
                )
            )
        )
    )


let _ = print(string_of_pairtt(typecheck templ [] [] [] [] []))
