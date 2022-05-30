open Syntax
open Utils
open Eval 
open Typing 
open PrettyPrint




(* auxiliary entities *)

let product_ent = Entity("Product", Record [
                                            ("Description", Attribute("Product", "Description", StringT, Record [("DisplayName", String "Description")]))], 
                                    Record [
                                            ("Description", String "HelloWorld")
                                    ])

let product_rec_t = RecordT [
                        ("Description", AttributeT(NameT "Product", StringT))
                    ]

let store_ent = Entity("Store", Record [
                                        ("IsInStock", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")])) ;
                                        ("Random", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")])) ;
                                        ("Location", Attribute("Store", "Location", StringT, Record [("DisplayName", String "Store Location")])) ] ,
                                Record [
                                        ("IsInStock", Bool true) ;
                                        ("Random", Num 17) ;
                                        ("Location", String "Portugal")
                                ])

let store_rec_t = RecordT [
                        ("IsInStock", AttributeT(NameT "Store", BoolT)) ;
                        ("Random", AttributeT(NameT "Store", NumT))
                    ]

let product_a = List [
    Attribute("Product", "Description", StringT, Record [("DisplayName", String "Description")])
]

let store_a1 = List [
    Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]) ;
    Attribute("Store", "Location", StringT, Record [("DisplayName", String "Store Location")])
]

let store_a2 = List [
    Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")])
]


(* NESTED TEMPLATES *)

(* Attribute *)
let attribute = 
ForAllName(
    "N",
ForAllRows(
    "R",
ForAllType(
    "T",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attr",
    AttributeT(VarNT "N", VarT "T"),
    
        LetBox("name", 
            NameOf (Var "e"), 
            LetBox("label", 
                LabelOf (Var "attr"),
                IfNode(
                    IsOfType(Var "attr", BoolT),
                    Node(CheckBox,
                        Record [("Visible", Box( mk_select_list [VarRT "name"; Label "list"; Label "current"; VarRT "label"] ))],
                        List []
                    ),
                    Node(Expression,
                        Record [("Value", Box( mk_select_list [VarRT "name"; Label "list"; Label "current"; VarRT "label"] ))],
                        List []
                    )
                )
            )
        )
)
)
)
)
)


let inst = 
Let("f",
    attribute,
    Let("e-prod",
        product_ent,
        Let("e-store",
            store_ent,
            ForNode(
                "a", 
                "aT", 
                AttributesOf (Var "e-store"), 
                    Instantiate( Instantiate( CallType( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , VarT "aT" ) , Var "e-store" ) , Var "a" )
                )
        )
    )
)

let rt0 = LetBox("inst", IndexOf(inst, 0), VarRT "inst")
let rt1 = LetBox("inst", IndexOf(inst, 1), VarRT "inst")


let expt = (AForAllName ("N",
  (AForAllRows ("R",
    (AForAllType ("T",
      (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
        (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
          (ALetBox ("name",
            (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
             BoxT
              (RecordT
                [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
            (ALetBox ("label",
              (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
               BoxT (LabelAttrT (VarNT "N", VarT "T"))),
              (AIfNode
                ((AIsOfType ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoolT),
                  BoolT),
                (ANode (CheckBox,
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
                                      [("current", RecordAttrT (VarNT "N"))])]),
                                (ALabel "list", LabelT "list")),
                               RecordT [("current", RecordAttrT (VarNT "N"))]),
                             (ALabel "current", LabelT "current")),
                            RecordAttrT (VarNT "N")),
                          (AVarRT "label", LabelAttrT (VarNT "N", VarT "T"))),
                         VarT "T"),
                       BoxT (VarT "T")))],
                   RecordT [("Visible", BoxT (VarT "T"))]),
                  (AList [], ListT [])),
                 NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                      [("current", RecordAttrT (VarNT "N"))])]),
                                (ALabel "list", LabelT "list")),
                               RecordT [("current", RecordAttrT (VarNT "N"))]),
                             (ALabel "current", LabelT "current")),
                            RecordAttrT (VarNT "N")),
                          (AVarRT "label", LabelAttrT (VarNT "N", VarT "T"))),
                         VarT "T"),
                       BoxT (VarT "T")))],
                   RecordT [("Value", BoxT (VarT "T"))]),
                  (AList [], ListT [])),
                 NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([CheckBox; Expression],
          RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([CheckBox; Expression],
         RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
   ForAllRowsT ("R",
    ForAllTypeT ("T",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (AttributeT (VarNT "N", VarT "T"),
       NodeT ([CheckBox; Expression],
        RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   ForAllTypeT ("T",
    TemplateT (EntityT (VarNT "N", VarR "R"),
     TemplateT (AttributeT (VarNT "N", VarT "T"),
      NodeT ([CheckBox; Expression],
       RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))))

let _ = assert( typecheck attribute [] [] [] [] [] = expt)


(* TODO , ASK - quando instantiate, vai buscar o tipo q ja estava. no tipo nao ve la dentro com instanciaçao feita. devia fazer typecheck com a instanciacao em ambiente? *)
let _ = print(string_of_term(eval inst []))

let _ = print(string_of_term(eval rt0 []))
let _ = print(string_of_term(eval rt1 []))





(* Labelled attribute *)
let labelled_attribute =
Let( "attr_templ", attribute,
ForAllName(
    "N",
ForAllRows(
    "R",
ForAllType(
    "T",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attr",
    AttributeT(VarNT "N", VarT "T"),

        Node(Container,
            Record [],
            List [
                Node(Expression,
                    Record [("Value", Select (Var "attr", Label "DisplayName"))],
                    List[]
                );
                LetBox(
                    "inner_templ",
                    Instantiate( Instantiate( CallType(CallRows(CallName(Var "attr_templ", VarNT "N"), VarR "R"), VarT "T"), Var "e"), Var "attr"),
                    Box(VarRT "inner_templ")
                )
            ]
        
        )
)
)
)
)
)
)


let inst = 
Let("f",
    labelled_attribute,
    Let("e-prod",
        product_ent,
        Let("e-store",
            store_ent,
            ForNode(
                "a", 
                "aT", 
                AttributesOf (Var "e-store"), 
                    Instantiate( Instantiate( CallType( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , VarT "aT" ) , Var "e-store" ) , Var "a" )
                )
        )
    )
)


let rt0 = LetBox("inst", IndexOf(inst, 0), VarRT "inst")
let rt1 = LetBox("inst", IndexOf(inst, 1), VarRT "inst")

let _ = print(string_of_pairtt(typecheck labelled_attribute [] [] [] [] []))

let expt = (ALet ("attr_templ",
  (AForAllName ("N",
    (AForAllRows ("R",
      (AForAllType ("T",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
            (ALetBox ("name",
              (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
               BoxT
                (RecordT
                  [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
              (ALetBox ("label",
                (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                 BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                (AIfNode
                  ((AIsOfType ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoolT),
                    BoolT),
                  (ANode (CheckBox,
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
                                        [("current", RecordAttrT (VarNT "N"))])]),
                                  (ALabel "list", LabelT "list")),
                                 RecordT [("current", RecordAttrT (VarNT "N"))]),
                               (ALabel "current", LabelT "current")),
                              RecordAttrT (VarNT "N")),
                            (AVarRT "label", LabelAttrT (VarNT "N", VarT "T"))),
                           VarT "T"),
                         BoxT (VarT "T")))],
                     RecordT [("Visible", BoxT (VarT "T"))]),
                    (AList [], ListT [])),
                   NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                        [("current", RecordAttrT (VarNT "N"))])]),
                                  (ALabel "list", LabelT "list")),
                                 RecordT [("current", RecordAttrT (VarNT "N"))]),
                               (ALabel "current", LabelT "current")),
                              RecordAttrT (VarNT "N")),
                            (AVarRT "label", LabelAttrT (VarNT "N", VarT "T"))),
                           VarT "T"),
                         BoxT (VarT "T")))],
                     RecordT [("Value", BoxT (VarT "T"))]),
                    (AList [], ListT [])),
                   NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
     ForAllRowsT ("R",
      ForAllTypeT ("T",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([CheckBox; Expression],
          RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([CheckBox; Expression],
         RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
  (AForAllName ("N",
    (AForAllRows ("R",
      (AForAllType ("T",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
            (ANode (Container, (ARecord [], RecordT []),
              (AList
                [(ANode (Expression,
                   (ARecord
                     [("Value",
                       (ASelect
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         (ALabel "DisplayName", LabelT "DisplayName")),
                        StringT))],
                    RecordT [("Value", StringT)]),
                   (AList [], ListT [])),
                  NodeT ([Expression], RecordT [("Value", StringT)]));
                 (ALetBox ("inner_templ",
                   (AInstantiate
                     ((AInstantiate
                        ((ACallType
                           ((ACallRows
                              ((ACallName
                                 ((AVar "attr_templ",
                                   ForAllNameT ("N",
                                    ForAllRowsT ("R",
                                     ForAllTypeT ("T",
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT
                                        (AttributeT (VarNT "N", VarT "T"),
                                        NodeT ([CheckBox; Expression],
                                         RecordT
                                          [("Visible", BoxT (VarT "T"));
                                           ("Value", BoxT (VarT "T"))]))))))),
                                 VarNT "N"),
                                ForAllRowsT ("R",
                                 ForAllTypeT ("T",
                                  TemplateT (EntityT (VarNT "N", VarR "R"),
                                   TemplateT (AttributeT (VarNT "N", VarT "T"),
                                    NodeT ([CheckBox; Expression],
                                     RecordT
                                      [("Visible", BoxT (VarT "T"));
                                       ("Value", BoxT (VarT "T"))])))))),
                              VarR "R"),
                             ForAllTypeT ("T",
                              TemplateT (EntityT (VarNT "N", VarR "R"),
                               TemplateT (AttributeT (VarNT "N", VarT "T"),
                                NodeT ([CheckBox; Expression],
                                 RecordT
                                  [("Visible", BoxT (VarT "T"));
                                   ("Value", BoxT (VarT "T"))]))))),
                           VarT "T"),
                          TemplateT (EntityT (VarNT "N", VarR "R"),
                           TemplateT (AttributeT (VarNT "N", VarT "T"),
                            NodeT ([CheckBox; Expression],
                             RecordT
                              [("Visible", BoxT (VarT "T"));
                               ("Value", BoxT (VarT "T"))])))),
                        (AVar "e", EntityT (VarNT "N", VarR "R"))),
                       TemplateT (AttributeT (VarNT "N", VarT "T"),
                        NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))]))),
                     (AVar "attr", AttributeT (VarNT "N", VarT "T"))),
                    BoxT
                     (NodeT ([CheckBox; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))]))),
                   (ABox
                     (AVarRT "inner_templ",
                      NodeT ([CheckBox; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))])),
                    BoxT
                     (NodeT ([CheckBox; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))])))),
                  BoxT
                   (NodeT ([CheckBox; Expression],
                     RecordT
                      [("Visible", BoxT (VarT "T"));
                       ("Value", BoxT (VarT "T"))])))],
               ListT
                [NodeT ([Expression], RecordT [("Value", StringT)]);
                 BoxT
                  (NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))])),
             NodeT ([Container], RecordT []))),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Container], RecordT [])))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Container], RecordT []))))),
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Container], RecordT [])))))),
     ForAllRowsT ("R",
      ForAllTypeT ("T",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([Container], RecordT []))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Container], RecordT [])))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   ForAllTypeT ("T",
    TemplateT (EntityT (VarNT "N", VarR "R"),
     TemplateT (AttributeT (VarNT "N", VarT "T"),
      NodeT ([Container], RecordT [])))))))

let _ = assert( typecheck labelled_attribute [] [] [] [] [] = expt )

let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt0 []))
let _ = print(string_of_term(eval rt1 []))




(* Filter *)

let filter =
Template(
    "attrsInFilter",
    ListAttrT(VarNT "N"),
        Node(Search, Record [("filterBy", Var "attrsInFilter")], List [])
)

let inst = 
Let("f",
    filter,
    Let("a-store",
        store_a1,
        Instantiate( Var "f", Var "a-store" )
    )
)

let rt = LetBox("inst", inst, VarRT "inst")

let expt = (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
  (ANode (Search,
    (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
     RecordT [("filterBy", ListAttrT (VarNT "N"))]),
    (AList [], ListT [])),
   NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
 TemplateT (ListAttrT (VarNT "N"),
  NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))])))
(* let _ = typecheck filter [] [] [] [] [] *)
let _ = assert( typecheck filter [] [] [] [] [] = expt )

let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt []))




(* Listing *)

let listing =
Let("filter_templ", filter,
Let("attr_templ", attribute,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "ent",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "showFilter",
    BoolT,
        Node(Container,
            Record [],
            List [
                IfNode(
                    Var "showFilter",
                    LetBox("inner",
                        Instantiate(Var "filter_templ", Var "attrs"),
                        Box(VarRT "inner")
                    ),
                    Node(Empty, Record [], List [])
                );
                Node(List,
                    Record [],
                    List [
                        Node(ListItem,
                            Record [],
                            List [
                                ForNode(
                                    "a",
                                    "aT",
                                    Var "attrs",
                                    LetBox("inner",
                                        Instantiate( Instantiate( CallType( CallRows( CallName(Var "attr_templ", VarNT "N"), VarR "R" ), VarT "aT" ) , Var "ent" ) , Var "a" ),
                                        Box(VarRT "inner")
                                    )
                                )
                            ]
                        )
                    ]
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


let inst = 
Let("f",
    listing,
    Let("e-prod",
        product_ent,
        Let("e-store",
            store_ent,
            Let("a-store",
                store_a1,
                Instantiate(Instantiate( Instantiate( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , Var "e-store" ) , Var "a-store" ) , Bool true )
            )
        )
    )
)


let rt = LetBox("inst", inst, VarRT "inst")

let expt = (ALet ("filter_templ",
  (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
    (ANode (Search,
      (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
       RecordT [("filterBy", ListAttrT (VarNT "N"))]),
      (AList [], ListT [])),
     NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
   TemplateT (ListAttrT (VarNT "N"),
    NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
  (ALet ("attr_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ALetBox ("name",
                (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                 BoxT
                  (RecordT
                    [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                (ALetBox ("label",
                  (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                  (AIfNode
                    ((AIsOfType
                       ((AVar "attr", AttributeT (VarNT "N", VarT "T")), BoolT),
                      BoolT),
                    (ANode (CheckBox,
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (ATemplate ("ent", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attrs", ListAttrT (VarNT "N"),
            (ATemplate ("showFilter", BoolT,
              (ANode (Container, (ARecord [], RecordT []),
                (AList
                  [(AIfNode ((AVar "showFilter", BoolT),
                     (ALetBox ("inner",
                       (AInstantiate
                         ((AVar "filter_templ",
                           TemplateT (ListAttrT (VarNT "N"),
                            NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                         (AVar "attrs", ListAttrT (VarNT "N"))),
                        BoxT
                         (NodeT ([Search],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                       (ABox
                         (AVarRT "inner",
                          NodeT ([Search],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                        BoxT
                         (NodeT ([Search],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                      BoxT
                       (NodeT ([Search],
                         RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                     (ANode (Empty, (ARecord [], RecordT []),
                       (AList [], ListT [])),
                      NodeT ([Empty], RecordT []))),
                    BoxT
                     (NodeT ([Search; Empty],
                       RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                   (ANode (Syntax.List, (ARecord [], RecordT []),
                     (AList
                       [(ANode (ListItem, (ARecord [], RecordT []),
                          (AList
                            [(AForNode ("a", "aT",
                               (AVar "attrs", ListAttrT (VarNT "N")),
                               [(ALetBox ("inner",
                                  (AInstantiate
                                    ((AInstantiate
                                       ((ACallType
                                          ((ACallRows
                                             ((ACallName
                                                ((AVar "attr_templ",
                                                  ForAllNameT ("N",
                                                   ForAllRowsT ("R",
                                                    ForAllTypeT ("T",
                                                     TemplateT
                                                      (EntityT (VarNT "N",
                                                        VarR "R"),
                                                      TemplateT
                                                       (AttributeT (VarNT "N",
                                                         VarT "T"),
                                                       NodeT
                                                        ([CheckBox; Expression],
                                                        RecordT
                                                         [("Visible",
                                                           BoxT (VarT "T"));
                                                          ("Value",
                                                           BoxT (VarT "T"))]))))))),
                                                VarNT "N"),
                                               ForAllRowsT ("R",
                                                ForAllTypeT ("T",
                                                 TemplateT
                                                  (EntityT (VarNT "N",
                                                    VarR "R"),
                                                  TemplateT
                                                   (AttributeT (VarNT "N",
                                                     VarT "T"),
                                                   NodeT
                                                    ([CheckBox; Expression],
                                                    RecordT
                                                     [("Visible",
                                                       BoxT (VarT "T"));
                                                      ("Value",
                                                       BoxT (VarT "T"))])))))),
                                             VarR "R"),
                                            ForAllTypeT ("T",
                                             TemplateT
                                              (EntityT (VarNT "N", VarR "R"),
                                              TemplateT
                                               (AttributeT (VarNT "N",
                                                 VarT "T"),
                                               NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT (VarT "T"));
                                                  ("Value", BoxT (VarT "T"))]))))),
                                          Top),
                                         TemplateT
                                          (EntityT (VarNT "N", VarR "R"),
                                          TemplateT
                                           (AttributeT (VarNT "N", Top),
                                           NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)])))),
                                       (AVar "ent",
                                        EntityT (VarNT "N", VarR "R"))),
                                      TemplateT (AttributeT (VarNT "N", Top),
                                       NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)]))),
                                    (AVar "a", AttributeT (VarNT "N", Top))),
                                   BoxT
                                    (NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)]))),
                                  (ABox
                                    (AVarRT "inner",
                                     NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)])),
                                   BoxT
                                    (NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)])))),
                                 BoxT
                                  (NodeT ([CheckBox; Expression],
                                    RecordT
                                     [("Visible", BoxT Top);
                                      ("Value", BoxT Top)])))]),
                              ListT
                               [BoxT
                                 (NodeT ([CheckBox; Expression],
                                   RecordT
                                    [("Visible", BoxT Top);
                                     ("Value", BoxT Top)]))])],
                           ListT
                            [ListT
                              [BoxT
                                (NodeT ([CheckBox; Expression],
                                  RecordT
                                   [("Visible", BoxT Top); ("Value", BoxT Top)]))]])),
                         NodeT ([ListItem], RecordT []))],
                      ListT [NodeT ([ListItem], RecordT [])])),
                    NodeT ([Syntax.List], RecordT []))],
                 ListT
                  [BoxT
                    (NodeT ([Search; Empty],
                      RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                   NodeT ([Syntax.List], RecordT [])])),
               NodeT ([Container], RecordT []))),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
       ForAllRowsT ("R",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (BoolT, NodeT ([Container], RecordT [])))))))

(* let _ = typecheck listing [] [] [] [] [] *)
let _ = assert( typecheck listing [] [] [] [] [] = expt )

let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt []))




let chart =
ForAllName(
    "N",
ForAllRows(
    "R",
ForAllType(
    "T",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "categoryAttr",
    AttributeT(VarNT "N", VarT "T"),
        Node(Chart,
            Record [("AttrGroup", Var "categoryAttr")],
            List []
        )
)
)
)
)
)

let inst = 
Let("f", chart,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-store", store_a1,
    ForNode(
        "a",
        "aT",
        store_a1,
        Instantiate(Instantiate( CallType( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , VarT "aT" ) , Var "e-store" ) , Var "a" )
    )
)
)
)
)


let rt0 = LetBox("inst", IndexOf( inst, 0), VarRT "inst")
let rt1 = LetBox("inst", IndexOf( inst, 1), VarRT "inst")


let expt = (AForAllName ("N",
  (AForAllRows ("R",
    (AForAllType ("T",
      (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
        (ATemplate ("categoryAttr", AttributeT (VarNT "N", VarT "T"),
          (ANode (Chart,
            (ARecord
              [("AttrGroup",
                (AVar "categoryAttr", AttributeT (VarNT "N", VarT "T")))],
             RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]),
            (AList [], ListT [])),
           NodeT ([Chart],
            RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Chart],
           RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([Chart],
          RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))),
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Chart],
         RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))))),
   ForAllRowsT ("R",
    ForAllTypeT ("T",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (AttributeT (VarNT "N", VarT "T"),
       NodeT ([Chart],
        RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   ForAllTypeT ("T",
    TemplateT (EntityT (VarNT "N", VarR "R"),
     TemplateT (AttributeT (VarNT "N", VarT "T"),
      NodeT ([Chart],
       RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))))))

let _ = assert(typecheck chart [] [] [] [] [] = expt)
let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt0 []))
let _ = print(string_of_term(eval rt1 []))


(* Pagination *)
let pagination =
Box(Node(Pagination, Record [], List []))


(* Table *)
let table =
Let("filter_templ", filter,
Let("attr_templ", attribute,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "showFilter",
    BoolT,
Template(
    "attrsInFilter",
    ListAttrT(VarNT "N"),
Template(
    "showPagination",
    BoolT,
Template(
    "allowBulk",
    BoolT,
        Node(Container,
            Record [],
            List [
                IfNode(
                    Var "showFilter",
                    LetBox("inner",
                        Instantiate(Var "filter_templ", Var "attrsInFilter"),
                        Box(VarRT "inner")
                    ),
                    Node(Empty, Record [], List [])
                );
                LetBox("name",
                    NameOf (Var "e"),
                    Node(Table,
                        Record [("Source", Box(Select(VarRT "name", Label "list")))],
                        List [
                            IfNode(Var "allowBulk",
                                Node(Column,
                                    Record [("Title", String "Select")],
                                    List [
                                        Node(CheckBox, Record [], List[])
                                    ]
                                ),
                                Node(Empty, Record [], List [])
                            );
                            ForNode(
                                "a",
                                "aT",
                                Var "attrs",
                                Node(Column,
                                    Record [("Title", Select(Var "a", Label "DisplayName"))],
                                    List [
                                        LetBox("inner",
                                            Instantiate(Instantiate(CallType(CallRows(CallName(Var "attr_templ", VarNT "N"), VarR "R"), VarT "aT"), Var "e"), Var "a"),
                                            Box(VarRT "inner")
                                        )
                                    ]
                                )
                            )
                        ]
                    )
                );
                IfNode(Var "showPagination",
                    pagination,
                    Node(Empty, Record [], List [])
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
)
)
)

let inst_alltrue = 
Let("f", table,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), VarR "R"), Var "e-store"), Var "a1-store"), Bool true), Var "a2-store"), Bool true), Bool true)
)
)
)
)
)

let inst_allfalse = 
Let("f", table,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), VarR "R"), Var "e-store"), Var "a1-store"), Bool false), Var "a2-store"), Bool false), Bool false)
)
)
)
)
)

let rt_t = LetBox("inst", inst_alltrue, VarRT "inst")
let rt_f = LetBox("inst", inst_allfalse, VarRT "inst")



(* let _ = print(string_of_pairtt(typecheck table [] [] [] [] [])) *)
let expt = (ALet ("filter_templ",
  (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
    (ANode (Search,
      (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
       RecordT [("filterBy", ListAttrT (VarNT "N"))]),
      (AList [], ListT [])),
     NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
   TemplateT (ListAttrT (VarNT "N"),
    NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
  (ALet ("attr_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ALetBox ("name",
                (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                 BoxT
                  (RecordT
                    [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                (ALetBox ("label",
                  (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                  (AIfNode
                    ((AIsOfType
                       ((AVar "attr", AttributeT (VarNT "N", VarT "T")), BoolT),
                      BoolT),
                    (ANode (CheckBox,
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attrs", ListAttrT (VarNT "N"),
            (ATemplate ("showFilter", BoolT,
              (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
                (ATemplate ("showPagination", BoolT,
                  (ATemplate ("allowBulk", BoolT,
                    (ANode (Container, (ARecord [], RecordT []),
                      (AList
                        [(AIfNode ((AVar "showFilter", BoolT),
                           (ALetBox ("inner",
                             (AInstantiate
                               ((AVar "filter_templ",
                                 TemplateT (ListAttrT (VarNT "N"),
                                  NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))]))),
                               (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (ABox
                               (AVarRT "inner",
                                NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                            BoxT
                             (NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (ANode (Empty, (ARecord [], RecordT []),
                             (AList [], ListT [])),
                            NodeT ([Empty], RecordT []))),
                          BoxT
                           (NodeT ([Search; Empty],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                         (ALetBox ("name",
                           (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                            BoxT
                             (RecordT
                               [("list",
                                 RecordT [("current", RecordAttrT (VarNT "N"))])])),
                           (ANode (Table,
                             (ARecord
                               [("Source",
                                 (ABox
                                   (ASelect
                                     ((AVarRT "name",
                                       RecordT
                                        [("list",
                                          RecordT
                                           [("current",
                                             RecordAttrT (VarNT "N"))])]),
                                     (ALabel "list", LabelT "list")),
                                    RecordT
                                     [("current", RecordAttrT (VarNT "N"))]),
                                  BoxT
                                   (RecordT
                                     [("current", RecordAttrT (VarNT "N"))])))],
                              RecordT
                               [("Source",
                                 BoxT
                                  (RecordT
                                    [("current", RecordAttrT (VarNT "N"))]))]),
                             (AList
                               [(AIfNode ((AVar "allowBulk", BoolT),
                                  (ANode (Column,
                                    (ARecord
                                      [("Title", (AString "Select", StringT))],
                                     RecordT [("Title", StringT)]),
                                    (AList
                                      [(ANode (CheckBox,
                                         (ARecord [], RecordT []),
                                         (AList [], ListT [])),
                                        NodeT ([CheckBox], RecordT []))],
                                     ListT [NodeT ([CheckBox], RecordT [])])),
                                   NodeT ([Column],
                                    RecordT [("Title", StringT)])),
                                  (ANode (Empty, (ARecord [], RecordT []),
                                    (AList [], ListT [])),
                                   NodeT ([Empty], RecordT []))),
                                 NodeT ([Column; Empty],
                                  RecordT [("Title", StringT)]));
                                (AForNode ("a", "aT",
                                  (AVar "attrs", ListAttrT (VarNT "N")),
                                  [(ANode (Column,
                                     (ARecord
                                       [("Title",
                                         (ASelect
                                           ((AVar "a",
                                             AttributeT (VarNT "N", Top)),
                                           (ALabel "DisplayName",
                                            LabelT "DisplayName")),
                                          StringT))],
                                      RecordT [("Title", StringT)]),
                                     (AList
                                       [(ALetBox ("inner",
                                          (AInstantiate
                                            ((AInstantiate
                                               ((ACallType
                                                  ((ACallRows
                                                     ((ACallName
                                                        ((AVar "attr_templ",
                                                          ForAllNameT ("N",
                                                           ForAllRowsT ("R",
                                                            ForAllTypeT ("T",
                                                             TemplateT
                                                              (EntityT
                                                                (VarNT "N",
                                                                VarR "R"),
                                                              TemplateT
                                                               (AttributeT
                                                                 (VarNT "N",
                                                                 VarT "T"),
                                                               NodeT
                                                                ([CheckBox;
                                                                  Expression],
                                                                RecordT
                                                                 [("Visible",
                                                                   BoxT
                                                                    (VarT "T"));
                                                                  ("Value",
                                                                   BoxT
                                                                    (VarT "T"))]))))))),
                                                        VarNT "N"),
                                                       ForAllRowsT ("R",
                                                        ForAllTypeT ("T",
                                                         TemplateT
                                                          (EntityT (VarNT "N",
                                                            VarR "R"),
                                                          TemplateT
                                                           (AttributeT
                                                             (VarNT "N",
                                                             VarT "T"),
                                                           NodeT
                                                            ([CheckBox;
                                                              Expression],
                                                            RecordT
                                                             [("Visible",
                                                               BoxT (VarT "T"));
                                                              ("Value",
                                                               BoxT (VarT "T"))])))))),
                                                     VarR "R"),
                                                    ForAllTypeT ("T",
                                                     TemplateT
                                                      (EntityT (VarNT "N",
                                                        VarR "R"),
                                                      TemplateT
                                                       (AttributeT (VarNT "N",
                                                         VarT "T"),
                                                       NodeT
                                                        ([CheckBox; Expression],
                                                        RecordT
                                                         [("Visible",
                                                           BoxT (VarT "T"));
                                                          ("Value",
                                                           BoxT (VarT "T"))]))))),
                                                  Top),
                                                 TemplateT
                                                  (EntityT (VarNT "N",
                                                    VarR "R"),
                                                  TemplateT
                                                   (AttributeT (VarNT "N", Top),
                                                   NodeT
                                                    ([CheckBox; Expression],
                                                    RecordT
                                                     [("Visible", BoxT Top);
                                                      ("Value", BoxT Top)])))),
                                               (AVar "e",
                                                EntityT (VarNT "N", VarR "R"))),
                                              TemplateT
                                               (AttributeT (VarNT "N", Top),
                                               NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)]))),
                                            (AVar "a",
                                             AttributeT (VarNT "N", Top))),
                                           BoxT
                                            (NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)]))),
                                          (ABox
                                            (AVarRT "inner",
                                             NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])),
                                           BoxT
                                            (NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))),
                                         BoxT
                                          (NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)])))],
                                      ListT
                                       [BoxT
                                         (NodeT ([CheckBox; Expression],
                                           RecordT
                                            [("Visible", BoxT Top);
                                             ("Value", BoxT Top)]))])),
                                    NodeT ([Column],
                                     RecordT [("Title", StringT)]))]),
                                 ListT
                                  [NodeT ([Column],
                                    RecordT [("Title", StringT)])])],
                              ListT
                               [NodeT ([Column; Empty],
                                 RecordT [("Title", StringT)]);
                                ListT
                                 [NodeT ([Column],
                                   RecordT [("Title", StringT)])]])),
                            NodeT ([Table],
                             RecordT
                              [("Source",
                                BoxT
                                 (RecordT
                                   [("current", RecordAttrT (VarNT "N"))]))]))),
                          NodeT ([Table],
                           RecordT
                            [("Source",
                              BoxT
                               (RecordT [("current", RecordAttrT (VarNT "N"))]))]));
                         (AIfNode ((AVar "showPagination", BoolT),
                           (ABox
                             (ANode (Pagination, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Pagination], RecordT [])),
                            BoxT (NodeT ([Pagination], RecordT []))),
                           (ANode (Empty, (ARecord [], RecordT []),
                             (AList [], ListT [])),
                            NodeT ([Empty], RecordT []))),
                          BoxT (NodeT ([Pagination; Empty], RecordT [])))],
                       ListT
                        [BoxT
                          (NodeT ([Search; Empty],
                            RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                         NodeT ([Table],
                          RecordT
                           [("Source",
                             BoxT
                              (RecordT [("current", RecordAttrT (VarNT "N"))]))]);
                         BoxT (NodeT ([Pagination; Empty], RecordT []))])),
                     NodeT ([Container], RecordT []))),
                   TemplateT (BoolT, NodeT ([Container], RecordT [])))),
                 TemplateT (BoolT,
                  TemplateT (BoolT, NodeT ([Container], RecordT []))))),
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT,
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
             TemplateT (BoolT,
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT,
                TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT,
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
       ForAllRowsT ("R",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT,
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT,
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (BoolT,
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))

let _ = assert(typecheck table [] [] [] [] [] = expt)

let _ = print(string_of_term(eval inst_alltrue []))
let _ = print(string_of_term(eval inst_allfalse []))

let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))



(* OS templates *)

(* Detail(entity + primAttrs + secAttrs) *)
let detail =
Let(
    "f", labelled_attribute,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "primAttrs",
    ListAttrT(VarNT "N"),
Template(
    "secAttrs",
    ListAttrT(VarNT "N"),
        Node(Container,
            Record [],
            List [
                Node(Column,
                    Record [],
                    List [
                        ForNode(
                            "a1",
                            "a1T",
                            Var "primAttrs",
                            LetBox("lab_attr",
                                Instantiate( Instantiate( CallType( CallRows( CallName(Var "f", VarNT "N") , VarR "R") , VarT "a1T" ) , Var "e" ) , Var "a1" ),
                                Box(VarRT "lab_attr")
                            )
                        )
                    ]
                );
                Node(Column,
                    Record [],
                    List [
                        ForNode(
                            "a2",
                            "a2T",
                            Var "secAttrs",
                            LetBox("lab_attr",
                                Instantiate( Instantiate( CallType( CallRows( CallName(Var "f", VarNT "N") , VarR "R") , VarT "a2T" ) , Var "e" ) , Var "a2" ),
                                Box(VarRT "lab_attr")
                            )
                        )
                    ]
                )
            ]
        )
)
)
)
)
)
)


let inst = 
Let("f", detail,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
    Instantiate( Instantiate( Instantiate( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , Var "e-store" ) , Var "a1-store" ) , Var "a2-store" )
)
)
)
)
)
)


let rt = LetBox("inst", inst, VarRT "inst")


let expt = (ALet ("f",
  (ALet ("attr_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ALetBox ("name",
                (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                 BoxT
                  (RecordT
                    [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                (ALetBox ("label",
                  (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                  (AIfNode
                    ((AIsOfType
                       ((AVar "attr", AttributeT (VarNT "N", VarT "T")), BoolT),
                      BoolT),
                    (ANode (CheckBox,
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ANode (Container, (ARecord [], RecordT []),
                (AList
                  [(ANode (Expression,
                     (ARecord
                       [("Value",
                         (ASelect
                           ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                           (ALabel "DisplayName", LabelT "DisplayName")),
                          StringT))],
                      RecordT [("Value", StringT)]),
                     (AList [], ListT [])),
                    NodeT ([Expression], RecordT [("Value", StringT)]));
                   (ALetBox ("inner_templ",
                     (AInstantiate
                       ((AInstantiate
                          ((ACallType
                             ((ACallRows
                                ((ACallName
                                   ((AVar "attr_templ",
                                     ForAllNameT ("N",
                                      ForAllRowsT ("R",
                                       ForAllTypeT ("T",
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT
                                          (AttributeT (VarNT "N", VarT "T"),
                                          NodeT ([CheckBox; Expression],
                                           RecordT
                                            [("Visible", BoxT (VarT "T"));
                                             ("Value", BoxT (VarT "T"))]))))))),
                                   VarNT "N"),
                                  ForAllRowsT ("R",
                                   ForAllTypeT ("T",
                                    TemplateT (EntityT (VarNT "N", VarR "R"),
                                     TemplateT
                                      (AttributeT (VarNT "N", VarT "T"),
                                      NodeT ([CheckBox; Expression],
                                       RecordT
                                        [("Visible", BoxT (VarT "T"));
                                         ("Value", BoxT (VarT "T"))])))))),
                                VarR "R"),
                               ForAllTypeT ("T",
                                TemplateT (EntityT (VarNT "N", VarR "R"),
                                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                                  NodeT ([CheckBox; Expression],
                                   RecordT
                                    [("Visible", BoxT (VarT "T"));
                                     ("Value", BoxT (VarT "T"))]))))),
                             VarT "T"),
                            TemplateT (EntityT (VarNT "N", VarR "R"),
                             TemplateT (AttributeT (VarNT "N", VarT "T"),
                              NodeT ([CheckBox; Expression],
                               RecordT
                                [("Visible", BoxT (VarT "T"));
                                 ("Value", BoxT (VarT "T"))])))),
                          (AVar "e", EntityT (VarNT "N", VarR "R"))),
                         TemplateT (AttributeT (VarNT "N", VarT "T"),
                          NodeT ([CheckBox; Expression],
                           RecordT
                            [("Visible", BoxT (VarT "T"));
                             ("Value", BoxT (VarT "T"))]))),
                       (AVar "attr", AttributeT (VarNT "N", VarT "T"))),
                      BoxT
                       (NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))]))),
                     (ABox
                       (AVarRT "inner_templ",
                        NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))])),
                      BoxT
                       (NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))])))),
                    BoxT
                     (NodeT ([CheckBox; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))])))],
                 ListT
                  [NodeT ([Expression], RecordT [("Value", StringT)]);
                   BoxT
                    (NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))])),
               NodeT ([Container], RecordT []))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Container], RecordT [])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Container], RecordT []))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Container], RecordT [])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Container], RecordT []))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Container], RecordT []))))))),
  (AForAllName ("N",
    (AForAllRows ("R",
      (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
        (ATemplate ("primAttrs", ListAttrT (VarNT "N"),
          (ATemplate ("secAttrs", ListAttrT (VarNT "N"),
            (ANode (Container, (ARecord [], RecordT []),
              (AList
                [(ANode (Column, (ARecord [], RecordT []),
                   (AList
                     [(AForNode ("a1", "a1T",
                        (AVar "primAttrs", ListAttrT (VarNT "N")),
                        [(ALetBox ("lab_attr",
                           (AInstantiate
                             ((AInstantiate
                                ((ACallType
                                   ((ACallRows
                                      ((ACallName
                                         ((AVar "f",
                                           ForAllNameT ("N",
                                            ForAllRowsT ("R",
                                             ForAllTypeT ("T",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (AttributeT (VarNT "N",
                                                  VarT "T"),
                                                NodeT ([Container], RecordT []))))))),
                                         VarNT "N"),
                                        ForAllRowsT ("R",
                                         ForAllTypeT ("T",
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT
                                            (AttributeT (VarNT "N", VarT "T"),
                                            NodeT ([Container], RecordT [])))))),
                                      VarR "R"),
                                     ForAllTypeT ("T",
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT
                                        (AttributeT (VarNT "N", VarT "T"),
                                        NodeT ([Container], RecordT []))))),
                                   Top),
                                  TemplateT (EntityT (VarNT "N", VarR "R"),
                                   TemplateT (AttributeT (VarNT "N", Top),
                                    NodeT ([Container], RecordT [])))),
                                (AVar "e", EntityT (VarNT "N", VarR "R"))),
                               TemplateT (AttributeT (VarNT "N", Top),
                                NodeT ([Container], RecordT []))),
                             (AVar "a1", AttributeT (VarNT "N", Top))),
                            BoxT (NodeT ([Container], RecordT []))),
                           (ABox
                             (AVarRT "lab_attr",
                              NodeT ([Container], RecordT [])),
                            BoxT (NodeT ([Container], RecordT [])))),
                          BoxT (NodeT ([Container], RecordT [])))]),
                       ListT [BoxT (NodeT ([Container], RecordT []))])],
                    ListT [ListT [BoxT (NodeT ([Container], RecordT []))]])),
                  NodeT ([Column], RecordT []));
                 (ANode (Column, (ARecord [], RecordT []),
                   (AList
                     [(AForNode ("a2", "a2T",
                        (AVar "secAttrs", ListAttrT (VarNT "N")),
                        [(ALetBox ("lab_attr",
                           (AInstantiate
                             ((AInstantiate
                                ((ACallType
                                   ((ACallRows
                                      ((ACallName
                                         ((AVar "f",
                                           ForAllNameT ("N",
                                            ForAllRowsT ("R",
                                             ForAllTypeT ("T",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (AttributeT (VarNT "N",
                                                  VarT "T"),
                                                NodeT ([Container], RecordT []))))))),
                                         VarNT "N"),
                                        ForAllRowsT ("R",
                                         ForAllTypeT ("T",
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT
                                            (AttributeT (VarNT "N", VarT "T"),
                                            NodeT ([Container], RecordT [])))))),
                                      VarR "R"),
                                     ForAllTypeT ("T",
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT
                                        (AttributeT (VarNT "N", VarT "T"),
                                        NodeT ([Container], RecordT []))))),
                                   Top),
                                  TemplateT (EntityT (VarNT "N", VarR "R"),
                                   TemplateT (AttributeT (VarNT "N", Top),
                                    NodeT ([Container], RecordT [])))),
                                (AVar "e", EntityT (VarNT "N", VarR "R"))),
                               TemplateT (AttributeT (VarNT "N", Top),
                                NodeT ([Container], RecordT []))),
                             (AVar "a2", AttributeT (VarNT "N", Top))),
                            BoxT (NodeT ([Container], RecordT []))),
                           (ABox
                             (AVarRT "lab_attr",
                              NodeT ([Container], RecordT [])),
                            BoxT (NodeT ([Container], RecordT [])))),
                          BoxT (NodeT ([Container], RecordT [])))]),
                       ListT [BoxT (NodeT ([Container], RecordT []))])],
                    ListT [ListT [BoxT (NodeT ([Container], RecordT []))]])),
                  NodeT ([Column], RecordT []))],
               ListT
                [NodeT ([Column], RecordT []); NodeT ([Column], RecordT [])])),
             NodeT ([Container], RecordT []))),
           TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT [])))),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT []))))),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT [])))))),
     ForAllRowsT ("R",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT []))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT [])))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT [])))))))

let _ = assert( typecheck detail [] [] [] [] [] = expt)
(* let _ = typecheck detail [] [] [] [] [] *)
(* let _ = print(string_of_pairtt(typecheck detail [] [] [] [] [])) *)

let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt []))


(* TODO - showFilter usa os primAttr. Nao dou a opcao de add outros attr opcionalmente, pq dps nao consigo instanciar esse template. 
Opcoes:
1- uso primAttr caso showfilter seja true
2- recebo attrsInFilter, independentemente do valor de showfilter
X (nao funciona 3- faco um ifnode logo na raiz do template)
 *)

(* List (entity + primAttrs + secAttrs) *)
let list =
Let(
    "la_templ", labelled_attribute,
Let(
    "a_templ", attribute,
Let(
    "f_templ", filter,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "primAttrs",
    ListAttrT(VarNT "N"),
Template(
    "secAttrs",
    ListAttrT(VarNT "N"),
Template(
    "showFilter",
    BoolT,
        Node(Container,
            Record [],
            List [
                IfNode(
                    Var "showFilter",
                    LetBox("inner",
                        Instantiate(Var "f_templ", Var "primAttrs"),
                        Box(VarRT "inner")
                    ),
                    Node(Empty, Record [], List [])
                );
                Node(List,
                    Record [],
                    List [
                        Node(ListItem,
                            Record [],
                            List [
                                Node(Container,
                                    Record [],
                                    List [
                                        ForNode(
                                            "a1",
                                            "a1T",
                                            Var "primAttrs",
                                            LetBox("inner",
                                                Instantiate( Instantiate( CallType( CallRows( CallName(Var "a_templ", VarNT "N") , VarR "R") , VarT "a1T" ) , Var "e" ) , Var "a1" ),
                                                Box(VarRT "inner")
                                            )
                                        )
                                    ]
                                );
                                Node(Container,
                                    Record [],
                                    List [
                                        ForNode(
                                            "a2",
                                            "a2T",
                                            Var "secAttrs",
                                            LetBox("inner",
                                                Instantiate( Instantiate( CallType( CallRows( CallName(Var "la_templ", VarNT "N") , VarR "R") , VarT "a2T" ) , Var "e" ) , Var "a2" ),
                                                Box(VarRT "inner")
                                            )
                                        )
                                    ]
                                )
                            ]
                        )
                        
                    ]
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
)
)


let inst_true = 
Let("f", list,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
    Instantiate(Instantiate( Instantiate( Instantiate( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , Var "e-store" ) , Var "a1-store" ) , Var "a2-store" ), Bool true)
)
)
)
)
)
)

let inst_false = 
Let("f", list,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
    Instantiate(Instantiate( Instantiate( Instantiate( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , Var "e-store" ) , Var "a1-store" ) , Var "a2-store" ), Bool false)
)
)
)
)
)
)


let rt_t = LetBox("inst", inst_true, VarRT "inst")
let rt_f = LetBox("inst", inst_false, VarRT "inst")


(* let _ = print(string_of_pairtt(typecheck list [] [] [] [] [])) *)
(* let _ = typecheck list [] [] [] [] [] *)
let expt = (ALet ("la_templ",
  (ALet ("attr_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ALetBox ("name",
                (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                 BoxT
                  (RecordT
                    [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                (ALetBox ("label",
                  (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                  (AIfNode
                    ((AIsOfType
                       ((AVar "attr", AttributeT (VarNT "N", VarT "T")), BoolT),
                      BoolT),
                    (ANode (CheckBox,
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ANode (Container, (ARecord [], RecordT []),
                (AList
                  [(ANode (Expression,
                     (ARecord
                       [("Value",
                         (ASelect
                           ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                           (ALabel "DisplayName", LabelT "DisplayName")),
                          StringT))],
                      RecordT [("Value", StringT)]),
                     (AList [], ListT [])),
                    NodeT ([Expression], RecordT [("Value", StringT)]));
                   (ALetBox ("inner_templ",
                     (AInstantiate
                       ((AInstantiate
                          ((ACallType
                             ((ACallRows
                                ((ACallName
                                   ((AVar "attr_templ",
                                     ForAllNameT ("N",
                                      ForAllRowsT ("R",
                                       ForAllTypeT ("T",
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT
                                          (AttributeT (VarNT "N", VarT "T"),
                                          NodeT ([CheckBox; Expression],
                                           RecordT
                                            [("Visible", BoxT (VarT "T"));
                                             ("Value", BoxT (VarT "T"))]))))))),
                                   VarNT "N"),
                                  ForAllRowsT ("R",
                                   ForAllTypeT ("T",
                                    TemplateT (EntityT (VarNT "N", VarR "R"),
                                     TemplateT
                                      (AttributeT (VarNT "N", VarT "T"),
                                      NodeT ([CheckBox; Expression],
                                       RecordT
                                        [("Visible", BoxT (VarT "T"));
                                         ("Value", BoxT (VarT "T"))])))))),
                                VarR "R"),
                               ForAllTypeT ("T",
                                TemplateT (EntityT (VarNT "N", VarR "R"),
                                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                                  NodeT ([CheckBox; Expression],
                                   RecordT
                                    [("Visible", BoxT (VarT "T"));
                                     ("Value", BoxT (VarT "T"))]))))),
                             VarT "T"),
                            TemplateT (EntityT (VarNT "N", VarR "R"),
                             TemplateT (AttributeT (VarNT "N", VarT "T"),
                              NodeT ([CheckBox; Expression],
                               RecordT
                                [("Visible", BoxT (VarT "T"));
                                 ("Value", BoxT (VarT "T"))])))),
                          (AVar "e", EntityT (VarNT "N", VarR "R"))),
                         TemplateT (AttributeT (VarNT "N", VarT "T"),
                          NodeT ([CheckBox; Expression],
                           RecordT
                            [("Visible", BoxT (VarT "T"));
                             ("Value", BoxT (VarT "T"))]))),
                       (AVar "attr", AttributeT (VarNT "N", VarT "T"))),
                      BoxT
                       (NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))]))),
                     (ABox
                       (AVarRT "inner_templ",
                        NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))])),
                      BoxT
                       (NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))])))),
                    BoxT
                     (NodeT ([CheckBox; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))])))],
                 ListT
                  [NodeT ([Expression], RecordT [("Value", StringT)]);
                   BoxT
                    (NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))])),
               NodeT ([Container], RecordT []))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Container], RecordT [])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Container], RecordT []))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Container], RecordT [])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Container], RecordT []))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Container], RecordT []))))))),
  (ALet ("a_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ALetBox ("name",
                (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                 BoxT
                  (RecordT
                    [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                (ALetBox ("label",
                  (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                  (AIfNode
                    ((AIsOfType
                       ((AVar "attr", AttributeT (VarNT "N", VarT "T")), BoolT),
                      BoolT),
                    (ANode (CheckBox,
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
    (ALet ("f_templ",
      (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
        (ANode (Search,
          (ARecord
            [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
           RecordT [("filterBy", ListAttrT (VarNT "N"))]),
          (AList [], ListT [])),
         NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
       TemplateT (ListAttrT (VarNT "N"),
        NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("primAttrs", ListAttrT (VarNT "N"),
              (ATemplate ("secAttrs", ListAttrT (VarNT "N"),
                (ATemplate ("showFilter", BoolT,
                  (ANode (Container, (ARecord [], RecordT []),
                    (AList
                      [(AIfNode ((AVar "showFilter", BoolT),
                         (ALetBox ("inner",
                           (AInstantiate
                             ((AVar "f_templ",
                               TemplateT (ListAttrT (VarNT "N"),
                                NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (AVar "primAttrs", ListAttrT (VarNT "N"))),
                            BoxT
                             (NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (ABox
                             (AVarRT "inner",
                              NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                            BoxT
                             (NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                         (ANode (Empty, (ARecord [], RecordT []),
                           (AList [], ListT [])),
                          NodeT ([Empty], RecordT []))),
                        BoxT
                         (NodeT ([Search; Empty],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                       (ANode (Syntax.List, (ARecord [], RecordT []),
                         (AList
                           [(ANode (ListItem, (ARecord [], RecordT []),
                              (AList
                                [(ANode (Container, (ARecord [], RecordT []),
                                   (AList
                                     [(AForNode ("a1", "a1T",
                                        (AVar "primAttrs",
                                         ListAttrT (VarNT "N")),
                                        [(ALetBox ("inner",
                                           (AInstantiate
                                             ((AInstantiate
                                                ((ACallType
                                                   ((ACallRows
                                                      ((ACallName
                                                         ((AVar "a_templ",
                                                           ForAllNameT ("N",
                                                            ForAllRowsT ("R",
                                                             ForAllTypeT ("T",
                                                              TemplateT
                                                               (EntityT
                                                                 (VarNT "N",
                                                                 VarR "R"),
                                                               TemplateT
                                                                (AttributeT
                                                                  (VarNT "N",
                                                                  VarT "T"),
                                                                NodeT
                                                                 ([CheckBox;
                                                                   Expression],
                                                                 RecordT
                                                                  [("Visible",
                                                                    BoxT
                                                                    (VarT "T"));
                                                                   ("Value",
                                                                    BoxT
                                                                    (VarT "T"))]))))))),
                                                         VarNT "N"),
                                                        ForAllRowsT ("R",
                                                         ForAllTypeT ("T",
                                                          TemplateT
                                                           (EntityT (
                                                             VarNT "N",
                                                             VarR "R"),
                                                           TemplateT
                                                            (AttributeT
                                                              (VarNT "N",
                                                              VarT "T"),
                                                            NodeT
                                                             ([CheckBox;
                                                               Expression],
                                                             RecordT
                                                              [("Visible",
                                                                BoxT (VarT "T"));
                                                               ("Value",
                                                                BoxT (VarT "T"))])))))),
                                                      VarR "R"),
                                                     ForAllTypeT ("T",
                                                      TemplateT
                                                       (EntityT (VarNT "N",
                                                         VarR "R"),
                                                       TemplateT
                                                        (AttributeT (
                                                          VarNT "N", 
                                                          VarT "T"),
                                                        NodeT
                                                         ([CheckBox;
                                                           Expression],
                                                         RecordT
                                                          [("Visible",
                                                            BoxT (VarT "T"));
                                                           ("Value",
                                                            BoxT (VarT "T"))]))))),
                                                   Top),
                                                  TemplateT
                                                   (EntityT (VarNT "N",
                                                     VarR "R"),
                                                   TemplateT
                                                    (AttributeT (VarNT "N",
                                                      Top),
                                                    NodeT
                                                     ([CheckBox; Expression],
                                                     RecordT
                                                      [("Visible", BoxT Top);
                                                       ("Value", BoxT Top)])))),
                                                (AVar "e",
                                                 EntityT (VarNT "N", VarR "R"))),
                                               TemplateT
                                                (AttributeT (VarNT "N", Top),
                                                NodeT ([CheckBox; Expression],
                                                 RecordT
                                                  [("Visible", BoxT Top);
                                                   ("Value", BoxT Top)]))),
                                             (AVar "a1",
                                              AttributeT (VarNT "N", Top))),
                                            BoxT
                                             (NodeT ([CheckBox; Expression],
                                               RecordT
                                                [("Visible", BoxT Top);
                                                 ("Value", BoxT Top)]))),
                                           (ABox
                                             (AVarRT "inner",
                                              NodeT ([CheckBox; Expression],
                                               RecordT
                                                [("Visible", BoxT Top);
                                                 ("Value", BoxT Top)])),
                                            BoxT
                                             (NodeT ([CheckBox; Expression],
                                               RecordT
                                                [("Visible", BoxT Top);
                                                 ("Value", BoxT Top)])))),
                                          BoxT
                                           (NodeT ([CheckBox; Expression],
                                             RecordT
                                              [("Visible", BoxT Top);
                                               ("Value", BoxT Top)])))]),
                                       ListT
                                        [BoxT
                                          (NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)]))])],
                                    ListT
                                     [ListT
                                       [BoxT
                                         (NodeT ([CheckBox; Expression],
                                           RecordT
                                            [("Visible", BoxT Top);
                                             ("Value", BoxT Top)]))]])),
                                  NodeT ([Container], RecordT []));
                                 (ANode (Container, (ARecord [], RecordT []),
                                   (AList
                                     [(AForNode ("a2", "a2T",
                                        (AVar "secAttrs",
                                         ListAttrT (VarNT "N")),
                                        [(ALetBox ("inner",
                                           (AInstantiate
                                             ((AInstantiate
                                                ((ACallType
                                                   ((ACallRows
                                                      ((ACallName
                                                         ((AVar "la_templ",
                                                           ForAllNameT ("N",
                                                            ForAllRowsT ("R",
                                                             ForAllTypeT ("T",
                                                              TemplateT
                                                               (EntityT
                                                                 (VarNT "N",
                                                                 VarR "R"),
                                                               TemplateT
                                                                (AttributeT
                                                                  (VarNT "N",
                                                                  VarT "T"),
                                                                NodeT
                                                                 ([Container],
                                                                 RecordT []))))))),
                                                         VarNT "N"),
                                                        ForAllRowsT ("R",
                                                         ForAllTypeT ("T",
                                                          TemplateT
                                                           (EntityT (
                                                             VarNT "N",
                                                             VarR "R"),
                                                           TemplateT
                                                            (AttributeT
                                                              (VarNT "N",
                                                              VarT "T"),
                                                            NodeT ([Container],
                                                             RecordT [])))))),
                                                      VarR "R"),
                                                     ForAllTypeT ("T",
                                                      TemplateT
                                                       (EntityT (VarNT "N",
                                                         VarR "R"),
                                                       TemplateT
                                                        (AttributeT (
                                                          VarNT "N", 
                                                          VarT "T"),
                                                        NodeT ([Container],
                                                         RecordT []))))),
                                                   Top),
                                                  TemplateT
                                                   (EntityT (VarNT "N",
                                                     VarR "R"),
                                                   TemplateT
                                                    (AttributeT (VarNT "N",
                                                      Top),
                                                    NodeT ([Container],
                                                     RecordT [])))),
                                                (AVar "e",
                                                 EntityT (VarNT "N", VarR "R"))),
                                               TemplateT
                                                (AttributeT (VarNT "N", Top),
                                                NodeT ([Container], RecordT []))),
                                             (AVar "a2",
                                              AttributeT (VarNT "N", Top))),
                                            BoxT
                                             (NodeT ([Container], RecordT []))),
                                           (ABox
                                             (AVarRT "inner",
                                              NodeT ([Container], RecordT [])),
                                            BoxT
                                             (NodeT ([Container], RecordT [])))),
                                          BoxT
                                           (NodeT ([Container], RecordT [])))]),
                                       ListT
                                        [BoxT (NodeT ([Container], RecordT []))])],
                                    ListT
                                     [ListT
                                       [BoxT (NodeT ([Container], RecordT []))]])),
                                  NodeT ([Container], RecordT []))],
                               ListT
                                [NodeT ([Container], RecordT []);
                                 NodeT ([Container], RecordT [])])),
                             NodeT ([ListItem], RecordT []))],
                          ListT [NodeT ([ListItem], RecordT [])])),
                        NodeT ([Syntax.List], RecordT []))],
                     ListT
                      [BoxT
                        (NodeT ([Search; Empty],
                          RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                       NodeT ([Syntax.List], RecordT [])])),
                   NodeT ([Container], RecordT []))),
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))),
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT, NodeT ([Container], RecordT []))))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (ListAttrT (VarNT "N"),
      TemplateT (BoolT, NodeT ([Container], RecordT []))))))))


let _ = assert (typecheck list [] [] [] [] [] = expt)

let _ = print(string_of_term(eval inst_true []))
let _ = print(string_of_term(eval inst_false []))
let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))




(* ListWithChart (entity + attrs + categoryAttr) *)
let list_with_chart =
Let("chart_templ", chart,
Let("listing_templ", listing,
ForAllName(
    "N",
ForAllRows(
    "R",
ForAllType(
    "T",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "categoryAttr",
    AttributeT(VarNT "N", VarT "T"),
Template(
    "showFilter",
    BoolT,
        Node(Container,
            Record [],
            List [
                Node(Column,
                    Record [],
                    List [
                        LetBox("inner",
                            Instantiate( Instantiate( CallType( CallRows( CallName(Var "chart_templ", VarNT "N") , VarR "R") , VarT "T" ) , Var "e" ) , Var "categoryAttr" ),
                            Box(VarRT "inner")
                        )
                    ]
                );
                Node(Column,
                    Record [],
                    List [
                        LetBox("inner",
                            Instantiate (Instantiate( Instantiate( CallRows( CallName(Var "listing_templ", VarNT "N") , VarR "R") , Var "e" ) , Var "attrs" ), Var "showFilter"),
                            Box(VarRT "inner")
                        )
                    ]
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
)
)


let inst_true = 
Let("f", list_with_chart,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("categoryAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
    Instantiate(Instantiate( Instantiate( Instantiate( CallType( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , NumT), Var "e-store" ) , Var "a1-store" ) , Var "categoryAttr" ), Bool true)
)
)
)
)
)
)
)

let inst_false = 
Let("f", list_with_chart,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("categoryAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
    Instantiate(Instantiate( Instantiate( Instantiate( CallType( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , NumT), Var "e-store" ) , Var "a1-store" ) , Var "categoryAttr" ), Bool false)
)
)
)
)
)
)
)

let rt_t = LetBox("inst", inst_true, VarRT "inst")
let rt_f = LetBox("inst", inst_false, VarRT "inst")



(* let _ = print(string_of_pairtt(typecheck list_with_chart [] [] [] [] [])) *)

let expt = (ALet ("chart_templ",
  (AForAllName ("N",
    (AForAllRows ("R",
      (AForAllType ("T",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("categoryAttr", AttributeT (VarNT "N", VarT "T"),
            (ANode (Chart,
              (ARecord
                [("AttrGroup",
                  (AVar "categoryAttr", AttributeT (VarNT "N", VarT "T")))],
               RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]),
              (AList [], ListT [])),
             NodeT ([Chart],
              RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Chart],
             RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Chart],
            RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))),
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Chart],
           RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))))),
     ForAllRowsT ("R",
      ForAllTypeT ("T",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([Chart],
          RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Chart],
         RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
  (ALet ("listing_templ",
    (ALet ("filter_templ",
      (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
        (ANode (Search,
          (ARecord
            [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
           RecordT [("filterBy", ListAttrT (VarNT "N"))]),
          (AList [], ListT [])),
         NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
       TemplateT (ListAttrT (VarNT "N"),
        NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
      (ALet ("attr_templ",
        (AForAllName ("N",
          (AForAllRows ("R",
            (AForAllType ("T",
              (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
                (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                  (ALetBox ("name",
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
                        (ANode (CheckBox,
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
                         NodeT ([CheckBox],
                          RecordT [("Visible", BoxT (VarT "T"))])),
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
                       NodeT ([CheckBox; Expression],
                        RecordT
                         [("Visible", BoxT (VarT "T"));
                          ("Value", BoxT (VarT "T"))]))),
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                  NodeT ([CheckBox; Expression],
                   RecordT
                    [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
               TemplateT (EntityT (VarNT "N", VarR "R"),
                TemplateT (AttributeT (VarNT "N", VarT "T"),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
             ForAllTypeT ("T",
              TemplateT (EntityT (VarNT "N", VarR "R"),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
           ForAllRowsT ("R",
            ForAllTypeT ("T",
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
         ForAllNameT ("N",
          ForAllRowsT ("R",
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
        (AForAllName ("N",
          (AForAllRows ("R",
            (ATemplate ("ent", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attrs", ListAttrT (VarNT "N"),
                (ATemplate ("showFilter", BoolT,
                  (ANode (Container, (ARecord [], RecordT []),
                    (AList
                      [(AIfNode ((AVar "showFilter", BoolT),
                         (ALetBox ("inner",
                           (AInstantiate
                             ((AVar "filter_templ",
                               TemplateT (ListAttrT (VarNT "N"),
                                NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (AVar "attrs", ListAttrT (VarNT "N"))),
                            BoxT
                             (NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (ABox
                             (AVarRT "inner",
                              NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                            BoxT
                             (NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                         (ANode (Empty, (ARecord [], RecordT []),
                           (AList [], ListT [])),
                          NodeT ([Empty], RecordT []))),
                        BoxT
                         (NodeT ([Search; Empty],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                       (ANode (Syntax.List, (ARecord [], RecordT []),
                         (AList
                           [(ANode (ListItem, (ARecord [], RecordT []),
                              (AList
                                [(AForNode ("a", "aT",
                                   (AVar "attrs", ListAttrT (VarNT "N")),
                                   [(ALetBox ("inner",
                                      (AInstantiate
                                        ((AInstantiate
                                           ((ACallType
                                              ((ACallRows
                                                 ((ACallName
                                                    ((AVar "attr_templ",
                                                      ForAllNameT ("N",
                                                       ForAllRowsT ("R",
                                                        ForAllTypeT ("T",
                                                         TemplateT
                                                          (EntityT (VarNT "N",
                                                            VarR "R"),
                                                          TemplateT
                                                           (AttributeT
                                                             (VarNT "N",
                                                             VarT "T"),
                                                           NodeT
                                                            ([CheckBox;
                                                              Expression],
                                                            RecordT
                                                             [("Visible",
                                                               BoxT (VarT "T"));
                                                              ("Value",
                                                               BoxT (VarT "T"))]))))))),
                                                    VarNT "N"),
                                                   ForAllRowsT ("R",
                                                    ForAllTypeT ("T",
                                                     TemplateT
                                                      (EntityT (VarNT "N",
                                                        VarR "R"),
                                                      TemplateT
                                                       (AttributeT (VarNT "N",
                                                         VarT "T"),
                                                       NodeT
                                                        ([CheckBox; Expression],
                                                        RecordT
                                                         [("Visible",
                                                           BoxT (VarT "T"));
                                                          ("Value",
                                                           BoxT (VarT "T"))])))))),
                                                 VarR "R"),
                                                ForAllTypeT ("T",
                                                 TemplateT
                                                  (EntityT (VarNT "N",
                                                    VarR "R"),
                                                  TemplateT
                                                   (AttributeT (VarNT "N",
                                                     VarT "T"),
                                                   NodeT
                                                    ([CheckBox; Expression],
                                                    RecordT
                                                     [("Visible",
                                                       BoxT (VarT "T"));
                                                      ("Value",
                                                       BoxT (VarT "T"))]))))),
                                              Top),
                                             TemplateT
                                              (EntityT (VarNT "N", VarR "R"),
                                              TemplateT
                                               (AttributeT (VarNT "N", Top),
                                               NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])))),
                                           (AVar "ent",
                                            EntityT (VarNT "N", VarR "R"))),
                                          TemplateT
                                           (AttributeT (VarNT "N", Top),
                                           NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)]))),
                                        (AVar "a", AttributeT (VarNT "N", Top))),
                                       BoxT
                                        (NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)]))),
                                      (ABox
                                        (AVarRT "inner",
                                         NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)])),
                                       BoxT
                                        (NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)])))),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])))]),
                                  ListT
                                   [BoxT
                                     (NodeT ([CheckBox; Expression],
                                       RecordT
                                        [("Visible", BoxT Top);
                                         ("Value", BoxT Top)]))])],
                               ListT
                                [ListT
                                  [BoxT
                                    (NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)]))]])),
                             NodeT ([ListItem], RecordT []))],
                          ListT [NodeT ([ListItem], RecordT [])])),
                        NodeT ([Syntax.List], RecordT []))],
                     ListT
                      [BoxT
                        (NodeT ([Search; Empty],
                          RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                       NodeT ([Syntax.List], RecordT [])])),
                   NodeT ([Container], RecordT []))),
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))),
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT, NodeT ([Container], RecordT []))))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
           ForAllRowsT ("R",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
         ForAllNameT ("N",
          ForAllRowsT ("R",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("categoryAttr", AttributeT (VarNT "N", VarT "T"),
                (ATemplate ("showFilter", BoolT,
                  (ANode (Container, (ARecord [], RecordT []),
                    (AList
                      [(ANode (Column, (ARecord [], RecordT []),
                         (AList
                           [(ALetBox ("inner",
                              (AInstantiate
                                ((AInstantiate
                                   ((ACallType
                                      ((ACallRows
                                         ((ACallName
                                            ((AVar "chart_templ",
                                              ForAllNameT ("N",
                                               ForAllRowsT ("R",
                                                ForAllTypeT ("T",
                                                 TemplateT
                                                  (EntityT (VarNT "N",
                                                    VarR "R"),
                                                  TemplateT
                                                   (AttributeT (VarNT "N",
                                                     VarT "T"),
                                                   NodeT ([Chart],
                                                    RecordT
                                                     [("AttrGroup",
                                                       AttributeT (VarNT "N",
                                                        VarT "T"))]))))))),
                                            VarNT "N"),
                                           ForAllRowsT ("R",
                                            ForAllTypeT ("T",
                                             TemplateT
                                              (EntityT (VarNT "N", VarR "R"),
                                              TemplateT
                                               (AttributeT (VarNT "N",
                                                 VarT "T"),
                                               NodeT ([Chart],
                                                RecordT
                                                 [("AttrGroup",
                                                   AttributeT (VarNT "N",
                                                    VarT "T"))])))))),
                                         VarR "R"),
                                        ForAllTypeT ("T",
                                         TemplateT
                                          (EntityT (VarNT "N", VarR "R"),
                                          TemplateT
                                           (AttributeT (VarNT "N", VarT "T"),
                                           NodeT ([Chart],
                                            RecordT
                                             [("AttrGroup",
                                               AttributeT (VarNT "N", VarT "T"))]))))),
                                      VarT "T"),
                                     TemplateT (EntityT (VarNT "N", VarR "R"),
                                      TemplateT
                                       (AttributeT (VarNT "N", VarT "T"),
                                       NodeT ([Chart],
                                        RecordT
                                         [("AttrGroup",
                                           AttributeT (VarNT "N", VarT "T"))])))),
                                   (AVar "e", EntityT (VarNT "N", VarR "R"))),
                                  TemplateT (AttributeT (VarNT "N", VarT "T"),
                                   NodeT ([Chart],
                                    RecordT
                                     [("AttrGroup",
                                       AttributeT (VarNT "N", VarT "T"))]))),
                                (AVar "categoryAttr",
                                 AttributeT (VarNT "N", VarT "T"))),
                               BoxT
                                (NodeT ([Chart],
                                  RecordT
                                   [("AttrGroup",
                                     AttributeT (VarNT "N", VarT "T"))]))),
                              (ABox
                                (AVarRT "inner",
                                 NodeT ([Chart],
                                  RecordT
                                   [("AttrGroup",
                                     AttributeT (VarNT "N", VarT "T"))])),
                               BoxT
                                (NodeT ([Chart],
                                  RecordT
                                   [("AttrGroup",
                                     AttributeT (VarNT "N", VarT "T"))])))),
                             BoxT
                              (NodeT ([Chart],
                                RecordT
                                 [("AttrGroup",
                                   AttributeT (VarNT "N", VarT "T"))])))],
                          ListT
                           [BoxT
                             (NodeT ([Chart],
                               RecordT
                                [("AttrGroup",
                                  AttributeT (VarNT "N", VarT "T"))]))])),
                        NodeT ([Column], RecordT []));
                       (ANode (Column, (ARecord [], RecordT []),
                         (AList
                           [(ALetBox ("inner",
                              (AInstantiate
                                ((AInstantiate
                                   ((AInstantiate
                                      ((ACallRows
                                         ((ACallName
                                            ((AVar "listing_templ",
                                              ForAllNameT ("N",
                                               ForAllRowsT ("R",
                                                TemplateT
                                                 (EntityT (VarNT "N", VarR "R"),
                                                 TemplateT
                                                  (ListAttrT (VarNT "N"),
                                                  TemplateT (BoolT,
                                                   NodeT ([Container],
                                                    RecordT []))))))),
                                            VarNT "N"),
                                           ForAllRowsT ("R",
                                            TemplateT
                                             (EntityT (VarNT "N", VarR "R"),
                                             TemplateT (ListAttrT (VarNT "N"),
                                              TemplateT (BoolT,
                                               NodeT ([Container], RecordT [])))))),
                                         VarR "R"),
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT (ListAttrT (VarNT "N"),
                                          TemplateT (BoolT,
                                           NodeT ([Container], RecordT []))))),
                                      (AVar "e", EntityT (VarNT "N", VarR "R"))),
                                     TemplateT (ListAttrT (VarNT "N"),
                                      TemplateT (BoolT,
                                       NodeT ([Container], RecordT [])))),
                                   (AVar "attrs", ListAttrT (VarNT "N"))),
                                  TemplateT (BoolT,
                                   NodeT ([Container], RecordT []))),
                                (AVar "showFilter", BoolT)),
                               BoxT (NodeT ([Container], RecordT []))),
                              (ABox
                                (AVarRT "inner",
                                 NodeT ([Container], RecordT [])),
                               BoxT (NodeT ([Container], RecordT [])))),
                             BoxT (NodeT ([Container], RecordT [])))],
                          ListT [BoxT (NodeT ([Container], RecordT []))])),
                        NodeT ([Column], RecordT []))],
                     ListT
                      [NodeT ([Column], RecordT []);
                       NodeT ([Column], RecordT [])])),
                   NodeT ([Container], RecordT []))),
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                TemplateT (BoolT, NodeT ([Container], RecordT []))))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   ForAllTypeT ("T",
    TemplateT (EntityT (VarNT "N", VarR "R"),
     TemplateT (ListAttrT (VarNT "N"),
      TemplateT (AttributeT (VarNT "N", VarT "T"),
       TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))

let _ = assert( typecheck list_with_chart [] [] [] [] [] = expt )


let _ = print(string_of_term(eval inst_true []))
let _ = print(string_of_term(eval inst_false []))
let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))





(* Dashboard (entity + attrs + statusAttr + categoryAttr) *)
let dashboard =
Let("listing_templ", listing,
Let("chart_templ", chart,
ForAllName(
    "N",
ForAllRows(
    "R",
ForAllType(
    "statusT",
ForAllType(
    "categT",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "statusAttr",
    AttributeT(VarNT "N", VarT "statusT"),
Template(
    "categoryAttr",
    AttributeT(VarNT "N", VarT "categT"),
        Node(Container,
            Record [],
            List [
                Node(Container,
                    Record [],
                    List [
                      Node(Counter,
                          Record [("Source", Var "statusAttr")],
                          List []
                      )
                    ]
                );
                Node(Container,
                    Record [],
                    List [
                        Node(Column,
                            Record [],
                            List [
                                LetBox("inner",
                                    Instantiate( Instantiate( Instantiate( CallRows( CallName(Var "listing_templ", VarNT "N") , VarR "R") , Var "e" ) , Var "attrs" ) , Bool false ),
                                    Box(VarRT "inner")
                                )
                            ]
                        );
                        Node(Column,
                            Record [],
                            List [
                                LetBox("inner",
                                    Instantiate( Instantiate( CallType( CallRows( CallName(Var "chart_templ", VarNT "N") , VarR "R") , VarT "categT" ) , Var "e" ) , Var "categoryAttr" ),
                                    Box(VarRT "inner")
                                )
                            ]
                        )
                    ]
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
)
)
)


let inst = 
Let("f", dashboard,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("statusAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
Let("categoryAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
    Instantiate(Instantiate( Instantiate(Instantiate( CallType( CallType( CallRows( CallName(Var "f", NameT "Store") , store_rec_t ) , BoolT), NumT ), Var "e-store") , Var "a1-store" ) , Var "statusAttr" ), Var "categoryAttr")
)
)
)
)
)
)
)

let rt = LetBox("inst", inst, VarRT "inst")





(* let _ = print(string_of_pairtt(typecheck dashboard [] [] [] [] [])) *)
(* let _ = typecheck dashboard [] [] [] [] [] *)

let expt = (ALet ("listing_templ",
  (ALet ("filter_templ",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (ALet ("attr_templ",
      (AForAllName ("N",
        (AForAllRows ("R",
          (AForAllType ("T",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                (ALetBox ("name",
                  (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                   BoxT
                    (RecordT
                      [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                  (ALetBox ("label",
                    (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                    (AIfNode
                      ((AIsOfType
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoolT),
                        BoolT),
                      (ANode (CheckBox,
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
                       NodeT ([CheckBox],
                        RecordT [("Visible", BoxT (VarT "T"))])),
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
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
         ForAllRowsT ("R",
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("ent", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(AIfNode ((AVar "showFilter", BoolT),
                       (ALetBox ("inner",
                         (AInstantiate
                           ((AVar "filter_templ",
                             TemplateT (ListAttrT (VarNT "N"),
                              NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (AVar "attrs", ListAttrT (VarNT "N"))),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                         (ABox
                           (AVarRT "inner",
                            NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                        BoxT
                         (NodeT ([Search],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                       (ANode (Empty, (ARecord [], RecordT []),
                         (AList [], ListT [])),
                        NodeT ([Empty], RecordT []))),
                      BoxT
                       (NodeT ([Search; Empty],
                         RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                     (ANode (Syntax.List, (ARecord [], RecordT []),
                       (AList
                         [(ANode (ListItem, (ARecord [], RecordT []),
                            (AList
                              [(AForNode ("a", "aT",
                                 (AVar "attrs", ListAttrT (VarNT "N")),
                                 [(ALetBox ("inner",
                                    (AInstantiate
                                      ((AInstantiate
                                         ((ACallType
                                            ((ACallRows
                                               ((ACallName
                                                  ((AVar "attr_templ",
                                                    ForAllNameT ("N",
                                                     ForAllRowsT ("R",
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))]))))))),
                                                  VarNT "N"),
                                                 ForAllRowsT ("R",
                                                  ForAllTypeT ("T",
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       VarT "T"),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible",
                                                         BoxT (VarT "T"));
                                                        ("Value",
                                                         BoxT (VarT "T"))])))))),
                                               VarR "R"),
                                              ForAllTypeT ("T",
                                               TemplateT
                                                (EntityT (VarNT "N", VarR "R"),
                                                TemplateT
                                                 (AttributeT (VarNT "N",
                                                   VarT "T"),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible",
                                                     BoxT (VarT "T"));
                                                    ("Value", BoxT (VarT "T"))]))))),
                                            Top),
                                           TemplateT
                                            (EntityT (VarNT "N", VarR "R"),
                                            TemplateT
                                             (AttributeT (VarNT "N", Top),
                                             NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))),
                                         (AVar "ent",
                                          EntityT (VarNT "N", VarR "R"))),
                                        TemplateT (AttributeT (VarNT "N", Top),
                                         NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)]))),
                                      (AVar "a", AttributeT (VarNT "N", Top))),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)]))),
                                    (ABox
                                      (AVarRT "inner",
                                       NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])))),
                                   BoxT
                                    (NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)])))]),
                                ListT
                                 [BoxT
                                   (NodeT ([CheckBox; Expression],
                                     RecordT
                                      [("Visible", BoxT Top);
                                       ("Value", BoxT Top)]))])],
                             ListT
                              [ListT
                                [BoxT
                                  (NodeT ([CheckBox; Expression],
                                    RecordT
                                     [("Visible", BoxT Top);
                                      ("Value", BoxT Top)]))]])),
                           NodeT ([ListItem], RecordT []))],
                        ListT [NodeT ([ListItem], RecordT [])])),
                      NodeT ([Syntax.List], RecordT []))],
                   ListT
                    [BoxT
                      (NodeT ([Search; Empty],
                        RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                     NodeT ([Syntax.List], RecordT [])])),
                 NodeT ([Container], RecordT []))),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
  (ALet ("chart_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("categoryAttr", AttributeT (VarNT "N", VarT "T"),
              (ANode (Chart,
                (ARecord
                  [("AttrGroup",
                    (AVar "categoryAttr", AttributeT (VarNT "N", VarT "T")))],
                 RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]),
                (AList [], ListT [])),
               NodeT ([Chart],
                RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Chart],
               RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Chart],
              RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Chart],
             RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Chart],
            RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Chart],
           RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("statusT",
          (AForAllType ("categT",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attrs", ListAttrT (VarNT "N"),
                (ATemplate ("statusAttr",
                  AttributeT (VarNT "N", VarT "statusT"),
                  (ATemplate ("categoryAttr",
                    AttributeT (VarNT "N", VarT "categT"),
                    (ANode (Container, (ARecord [], RecordT []),
                      (AList
                        [(ANode (Container, (ARecord [], RecordT []),
                           (AList
                             [(ANode (Counter,
                                (ARecord
                                  [("Source",
                                    (AVar "statusAttr",
                                     AttributeT (VarNT "N", VarT "statusT")))],
                                 RecordT
                                  [("Source",
                                    AttributeT (VarNT "N", VarT "statusT"))]),
                                (AList [], ListT [])),
                               NodeT ([Counter],
                                RecordT
                                 [("Source",
                                   AttributeT (VarNT "N", VarT "statusT"))]))],
                            ListT
                             [NodeT ([Counter],
                               RecordT
                                [("Source",
                                  AttributeT (VarNT "N", VarT "statusT"))])])),
                          NodeT ([Container], RecordT []));
                         (ANode (Container, (ARecord [], RecordT []),
                           (AList
                             [(ANode (Column, (ARecord [], RecordT []),
                                (AList
                                  [(ALetBox ("inner",
                                     (AInstantiate
                                       ((AInstantiate
                                          ((AInstantiate
                                             ((ACallRows
                                                ((ACallName
                                                   ((AVar "listing_templ",
                                                     ForAllNameT ("N",
                                                      ForAllRowsT ("R",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (ListAttrT (VarNT "N"),
                                                         TemplateT (BoolT,
                                                          NodeT ([Container],
                                                           RecordT []))))))),
                                                   VarNT "N"),
                                                  ForAllRowsT ("R",
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (ListAttrT (VarNT "N"),
                                                     TemplateT (BoolT,
                                                      NodeT ([Container],
                                                       RecordT [])))))),
                                                VarR "R"),
                                               TemplateT
                                                (EntityT (VarNT "N", VarR "R"),
                                                TemplateT
                                                 (ListAttrT (VarNT "N"),
                                                 TemplateT (BoolT,
                                                  NodeT ([Container],
                                                   RecordT []))))),
                                             (AVar "e",
                                              EntityT (VarNT "N", VarR "R"))),
                                            TemplateT (ListAttrT (VarNT "N"),
                                             TemplateT (BoolT,
                                              NodeT ([Container], RecordT [])))),
                                          (AVar "attrs", ListAttrT (VarNT "N"))),
                                         TemplateT (BoolT,
                                          NodeT ([Container], RecordT []))),
                                       (ABool false, BoolT)),
                                      BoxT (NodeT ([Container], RecordT []))),
                                     (ABox
                                       (AVarRT "inner",
                                        NodeT ([Container], RecordT [])),
                                      BoxT (NodeT ([Container], RecordT [])))),
                                    BoxT (NodeT ([Container], RecordT [])))],
                                 ListT [BoxT (NodeT ([Container], RecordT []))])),
                               NodeT ([Column], RecordT []));
                              (ANode (Column, (ARecord [], RecordT []),
                                (AList
                                  [(ALetBox ("inner",
                                     (AInstantiate
                                       ((AInstantiate
                                          ((ACallType
                                             ((ACallRows
                                                ((ACallName
                                                   ((AVar "chart_templ",
                                                     ForAllNameT ("N",
                                                      ForAllRowsT ("R",
                                                       ForAllTypeT ("T",
                                                        TemplateT
                                                         (EntityT (VarNT "N",
                                                           VarR "R"),
                                                         TemplateT
                                                          (AttributeT
                                                            (VarNT "N",
                                                            VarT "T"),
                                                          NodeT ([Chart],
                                                           RecordT
                                                            [("AttrGroup",
                                                              AttributeT
                                                               (VarNT "N",
                                                               VarT "T"))]))))))),
                                                   VarNT "N"),
                                                  ForAllRowsT ("R",
                                                   ForAllTypeT ("T",
                                                    TemplateT
                                                     (EntityT (VarNT "N",
                                                       VarR "R"),
                                                     TemplateT
                                                      (AttributeT (VarNT "N",
                                                        VarT "T"),
                                                      NodeT ([Chart],
                                                       RecordT
                                                        [("AttrGroup",
                                                          AttributeT
                                                           (VarNT "N",
                                                           VarT "T"))])))))),
                                                VarR "R"),
                                               ForAllTypeT ("T",
                                                TemplateT
                                                 (EntityT (VarNT "N", VarR "R"),
                                                 TemplateT
                                                  (AttributeT (VarNT "N",
                                                    VarT "T"),
                                                  NodeT ([Chart],
                                                   RecordT
                                                    [("AttrGroup",
                                                      AttributeT (VarNT "N",
                                                       VarT "T"))]))))),
                                             VarT "categT"),
                                            TemplateT
                                             (EntityT (VarNT "N", VarR "R"),
                                             TemplateT
                                              (AttributeT (VarNT "N",
                                                VarT "categT"),
                                              NodeT ([Chart],
                                               RecordT
                                                [("AttrGroup",
                                                  AttributeT (VarNT "N",
                                                   VarT "categT"))])))),
                                          (AVar "e",
                                           EntityT (VarNT "N", VarR "R"))),
                                         TemplateT
                                          (AttributeT (VarNT "N",
                                            VarT "categT"),
                                          NodeT ([Chart],
                                           RecordT
                                            [("AttrGroup",
                                              AttributeT (VarNT "N",
                                               VarT "categT"))]))),
                                       (AVar "categoryAttr",
                                        AttributeT (VarNT "N", VarT "categT"))),
                                      BoxT
                                       (NodeT ([Chart],
                                         RecordT
                                          [("AttrGroup",
                                            AttributeT (VarNT "N",
                                             VarT "categT"))]))),
                                     (ABox
                                       (AVarRT "inner",
                                        NodeT ([Chart],
                                         RecordT
                                          [("AttrGroup",
                                            AttributeT (VarNT "N",
                                             VarT "categT"))])),
                                      BoxT
                                       (NodeT ([Chart],
                                         RecordT
                                          [("AttrGroup",
                                            AttributeT (VarNT "N",
                                             VarT "categT"))])))),
                                    BoxT
                                     (NodeT ([Chart],
                                       RecordT
                                        [("AttrGroup",
                                          AttributeT (VarNT "N", VarT "categT"))])))],
                                 ListT
                                  [BoxT
                                    (NodeT ([Chart],
                                      RecordT
                                       [("AttrGroup",
                                         AttributeT (VarNT "N", VarT "categT"))]))])),
                               NodeT ([Column], RecordT []))],
                            ListT
                             [NodeT ([Column], RecordT []);
                              NodeT ([Column], RecordT [])])),
                          NodeT ([Container], RecordT []))],
                       ListT
                        [NodeT ([Container], RecordT []);
                         NodeT ([Container], RecordT [])])),
                     NodeT ([Container], RecordT []))),
                   TemplateT (AttributeT (VarNT "N", VarT "categT"),
                    NodeT ([Container], RecordT [])))),
                 TemplateT (AttributeT (VarNT "N", VarT "statusT"),
                  TemplateT (AttributeT (VarNT "N", VarT "categT"),
                   NodeT ([Container], RecordT []))))),
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (AttributeT (VarNT "N", VarT "statusT"),
                 TemplateT (AttributeT (VarNT "N", VarT "categT"),
                  NodeT ([Container], RecordT [])))))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (AttributeT (VarNT "N", VarT "statusT"),
                TemplateT (AttributeT (VarNT "N", VarT "categT"),
                 NodeT ([Container], RecordT []))))))),
           ForAllTypeT ("categT",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (AttributeT (VarNT "N", VarT "statusT"),
               TemplateT (AttributeT (VarNT "N", VarT "categT"),
                NodeT ([Container], RecordT [])))))))),
         ForAllTypeT ("statusT",
          ForAllTypeT ("categT",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (AttributeT (VarNT "N", VarT "statusT"),
              TemplateT (AttributeT (VarNT "N", VarT "categT"),
               NodeT ([Container], RecordT []))))))))),
       ForAllRowsT ("R",
        ForAllTypeT ("statusT",
         ForAllTypeT ("categT",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (AttributeT (VarNT "N", VarT "statusT"),
             TemplateT (AttributeT (VarNT "N", VarT "categT"),
              NodeT ([Container], RecordT [])))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("statusT",
        ForAllTypeT ("categT",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (AttributeT (VarNT "N", VarT "statusT"),
            TemplateT (AttributeT (VarNT "N", VarT "categT"),
             NodeT ([Container], RecordT []))))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("statusT",
      ForAllTypeT ("categT",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (AttributeT (VarNT "N", VarT "statusT"),
          TemplateT (AttributeT (VarNT "N", VarT "categT"),
           NodeT ([Container], RecordT []))))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   ForAllTypeT ("statusT",
    ForAllTypeT ("categT",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (AttributeT (VarNT "N", VarT "statusT"),
        TemplateT (AttributeT (VarNT "N", VarT "categT"),
         NodeT ([Container], RecordT []))))))))))


let _ = assert (typecheck dashboard [] [] [] [] [] = expt)

let _ = print(string_of_term(eval dashboard []))
let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt []))


(* TODO - que faco aqui? que falta? que faco com o rollingsumattr? *)
(* AccountDashboard (masterEntity + masterAttrs + detailEntity + detailAttrs + rollingSumAttr + categoryAttr) *)
let account_dashboard =
Let("la_templ", labelled_attribute,
Let("chart_templ", chart,
Let("listing_templ", listing,
ForAllName(
    "masterN",
ForAllName(
    "detailN",
ForAllRows(
    "masterR",
ForAllRows(
    "detailR",
ForAllType(
    "categT",
Template(
    "masterEnt",
    EntityT(VarNT "masterN", VarR "masterR"),
Template(
    "masterAttrs",
    ListAttrT(VarNT "masterN"),
Template(
    "detailEnt",
    EntityT(VarNT "detailN", VarR "detailR"),
Template(
    "detailAttrs",
    ListAttrT(VarNT "detailN"),
Template(
    "rollingSumAttr",
    AttributeT(VarNT "masterN", NumT),
Template(
    "categoryAttr",
    AttributeT(VarNT "detailN", VarT "categT"),
        Node(Container,
            Record [],
            List [
                Node(Container,
                    Record [],
                    List [
                        ForNode(
                            "ma",
                            "maT",
                            Var "masterAttrs",
                            Node(Column,
                                Record [],
                                List [
                                    LetBox("inner",
                                        Instantiate(Instantiate(CallType(CallRows(CallName(Var "la_templ", VarNT "masterN"), VarR "masterR"), VarT "maT"), Var "masterEnt"), Var "ma"),
                                        Box(VarRT "inner")
                                    )
                                ]
                            )
                        )
                    ]
                );
                Node(Container,
                    Record [],
                    List [
                        Node(Column,
                            Record [],
                            List [
                                LetBox("inner",
                                    Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "listing_templ", VarNT "detailN"), VarR "detailR"), Var "detailEnt"), Var "detailAttrs"), Bool false),
                                    Box(VarRT "inner")
                                )
                            ]                            
                        );
                        Node(Column,
                            Record [("Title", Select(Var "categoryAttr", Label "DisplayName"))],
                            List [
                                LetBox("inner",
                                    Instantiate(Instantiate(CallType(CallRows(CallName(Var "chart_templ", VarNT "detailN"), VarR "detailR"), VarT "categT"), Var "detailEnt"), Var "categoryAttr"),
                                    Box(VarRT "inner")
                                )
                            ]
                        )
                    ]
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
)
)
)
)
)
)
)


let inst = 
Let("f", account_dashboard,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("categoryAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
Let("rollingAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
     Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallType(CallRows(CallRows(CallName(CallName(Var "f", NameT "Store"), NameT "Product"), store_rec_t),product_rec_t), BoolT), store_ent), store_a1), product_ent), product_a), Var "rollingAttr"), Var "categoryAttr") 
)
)
)
)
)
)
)

let rt = LetBox("inst", inst, VarRT "inst")





(* let _ = print(string_of_pairtt(typecheck account_dashboard [] [] [] [] [])) *)

let expt = (ALet ("la_templ",
  (ALet ("attr_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ALetBox ("name",
                (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                 BoxT
                  (RecordT
                    [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                (ALetBox ("label",
                  (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                   BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                  (AIfNode
                    ((AIsOfType
                       ((AVar "attr", AttributeT (VarNT "N", VarT "T")), BoolT),
                      BoolT),
                    (ANode (CheckBox,
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([CheckBox], RecordT [("Visible", BoxT (VarT "T"))])),
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
                                          [("current", RecordAttrT (VarNT "N"))])]),
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
                     NodeT ([Expression], RecordT [("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([CheckBox; Expression],
            RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([CheckBox; Expression],
           RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
              (ANode (Container, (ARecord [], RecordT []),
                (AList
                  [(ANode (Expression,
                     (ARecord
                       [("Value",
                         (ASelect
                           ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                           (ALabel "DisplayName", LabelT "DisplayName")),
                          StringT))],
                      RecordT [("Value", StringT)]),
                     (AList [], ListT [])),
                    NodeT ([Expression], RecordT [("Value", StringT)]));
                   (ALetBox ("inner_templ",
                     (AInstantiate
                       ((AInstantiate
                          ((ACallType
                             ((ACallRows
                                ((ACallName
                                   ((AVar "attr_templ",
                                     ForAllNameT ("N",
                                      ForAllRowsT ("R",
                                       ForAllTypeT ("T",
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT
                                          (AttributeT (VarNT "N", VarT "T"),
                                          NodeT ([CheckBox; Expression],
                                           RecordT
                                            [("Visible", BoxT (VarT "T"));
                                             ("Value", BoxT (VarT "T"))]))))))),
                                   VarNT "N"),
                                  ForAllRowsT ("R",
                                   ForAllTypeT ("T",
                                    TemplateT (EntityT (VarNT "N", VarR "R"),
                                     TemplateT
                                      (AttributeT (VarNT "N", VarT "T"),
                                      NodeT ([CheckBox; Expression],
                                       RecordT
                                        [("Visible", BoxT (VarT "T"));
                                         ("Value", BoxT (VarT "T"))])))))),
                                VarR "R"),
                               ForAllTypeT ("T",
                                TemplateT (EntityT (VarNT "N", VarR "R"),
                                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                                  NodeT ([CheckBox; Expression],
                                   RecordT
                                    [("Visible", BoxT (VarT "T"));
                                     ("Value", BoxT (VarT "T"))]))))),
                             VarT "T"),
                            TemplateT (EntityT (VarNT "N", VarR "R"),
                             TemplateT (AttributeT (VarNT "N", VarT "T"),
                              NodeT ([CheckBox; Expression],
                               RecordT
                                [("Visible", BoxT (VarT "T"));
                                 ("Value", BoxT (VarT "T"))])))),
                          (AVar "e", EntityT (VarNT "N", VarR "R"))),
                         TemplateT (AttributeT (VarNT "N", VarT "T"),
                          NodeT ([CheckBox; Expression],
                           RecordT
                            [("Visible", BoxT (VarT "T"));
                             ("Value", BoxT (VarT "T"))]))),
                       (AVar "attr", AttributeT (VarNT "N", VarT "T"))),
                      BoxT
                       (NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))]))),
                     (ABox
                       (AVarRT "inner_templ",
                        NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))])),
                      BoxT
                       (NodeT ([CheckBox; Expression],
                         RecordT
                          [("Visible", BoxT (VarT "T"));
                           ("Value", BoxT (VarT "T"))])))),
                    BoxT
                     (NodeT ([CheckBox; Expression],
                       RecordT
                        [("Visible", BoxT (VarT "T"));
                         ("Value", BoxT (VarT "T"))])))],
                 ListT
                  [NodeT ([Expression], RecordT [("Value", StringT)]);
                   BoxT
                    (NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))])),
               NodeT ([Container], RecordT []))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Container], RecordT [])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Container], RecordT []))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Container], RecordT [])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Container], RecordT []))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (AttributeT (VarNT "N", VarT "T"),
        NodeT ([Container], RecordT []))))))),
  (ALet ("chart_templ",
    (AForAllName ("N",
      (AForAllRows ("R",
        (AForAllType ("T",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("categoryAttr", AttributeT (VarNT "N", VarT "T"),
              (ANode (Chart,
                (ARecord
                  [("AttrGroup",
                    (AVar "categoryAttr", AttributeT (VarNT "N", VarT "T")))],
                 RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]),
                (AList [], ListT [])),
               NodeT ([Chart],
                RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Chart],
               RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Chart],
              RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))),
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Chart],
             RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))])))))),
       ForAllRowsT ("R",
        ForAllTypeT ("T",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Chart],
            RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Chart],
           RecordT [("AttrGroup", AttributeT (VarNT "N", VarT "T"))]))))))),
    (ALet ("listing_templ",
      (ALet ("filter_templ",
        (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
          (ANode (Search,
            (ARecord
              [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
             RecordT [("filterBy", ListAttrT (VarNT "N"))]),
            (AList [], ListT [])),
           NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
         TemplateT (ListAttrT (VarNT "N"),
          NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
        (ALet ("attr_templ",
          (AForAllName ("N",
            (AForAllRows ("R",
              (AForAllType ("T",
                (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
                  (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                    (ALetBox ("name",
                      (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                       BoxT
                        (RecordT
                          [("list",
                            RecordT [("current", RecordAttrT (VarNT "N"))])])),
                      (ALetBox ("label",
                        (ALabelOf
                          (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                        (AIfNode
                          ((AIsOfType
                             ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                             BoolT),
                            BoolT),
                          (ANode (CheckBox,
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
                           NodeT ([CheckBox],
                            RecordT [("Visible", BoxT (VarT "T"))])),
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
                         NodeT ([CheckBox; Expression],
                          RecordT
                           [("Visible", BoxT (VarT "T"));
                            ("Value", BoxT (VarT "T"))]))),
                       NodeT ([CheckBox; Expression],
                        RecordT
                         [("Visible", BoxT (VarT "T"));
                          ("Value", BoxT (VarT "T"))]))),
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   TemplateT (AttributeT (VarNT "N", VarT "T"),
                    NodeT ([CheckBox; Expression],
                     RecordT
                      [("Visible", BoxT (VarT "T"));
                       ("Value", BoxT (VarT "T"))])))),
                 TemplateT (EntityT (VarNT "N", VarR "R"),
                  TemplateT (AttributeT (VarNT "N", VarT "T"),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
               ForAllTypeT ("T",
                TemplateT (EntityT (VarNT "N", VarR "R"),
                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                  NodeT ([CheckBox; Expression],
                   RecordT
                    [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
             ForAllRowsT ("R",
              ForAllTypeT ("T",
               TemplateT (EntityT (VarNT "N", VarR "R"),
                TemplateT (AttributeT (VarNT "N", VarT "T"),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
           ForAllNameT ("N",
            ForAllRowsT ("R",
             ForAllTypeT ("T",
              TemplateT (EntityT (VarNT "N", VarR "R"),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
          (AForAllName ("N",
            (AForAllRows ("R",
              (ATemplate ("ent", EntityT (VarNT "N", VarR "R"),
                (ATemplate ("attrs", ListAttrT (VarNT "N"),
                  (ATemplate ("showFilter", BoolT,
                    (ANode (Container, (ARecord [], RecordT []),
                      (AList
                        [(AIfNode ((AVar "showFilter", BoolT),
                           (ALetBox ("inner",
                             (AInstantiate
                               ((AVar "filter_templ",
                                 TemplateT (ListAttrT (VarNT "N"),
                                  NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))]))),
                               (AVar "attrs", ListAttrT (VarNT "N"))),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (ABox
                               (AVarRT "inner",
                                NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                            BoxT
                             (NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (ANode (Empty, (ARecord [], RecordT []),
                             (AList [], ListT [])),
                            NodeT ([Empty], RecordT []))),
                          BoxT
                           (NodeT ([Search; Empty],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                         (ANode (Syntax.List, (ARecord [], RecordT []),
                           (AList
                             [(ANode (ListItem, (ARecord [], RecordT []),
                                (AList
                                  [(AForNode ("a", "aT",
                                     (AVar "attrs", ListAttrT (VarNT "N")),
                                     [(ALetBox ("inner",
                                        (AInstantiate
                                          ((AInstantiate
                                             ((ACallType
                                                ((ACallRows
                                                   ((ACallName
                                                      ((AVar "attr_templ",
                                                        ForAllNameT ("N",
                                                         ForAllRowsT ("R",
                                                          ForAllTypeT ("T",
                                                           TemplateT
                                                            (EntityT
                                                              (VarNT "N",
                                                              VarR "R"),
                                                            TemplateT
                                                             (AttributeT
                                                               (VarNT "N",
                                                               VarT "T"),
                                                             NodeT
                                                              ([CheckBox;
                                                                Expression],
                                                              RecordT
                                                               [("Visible",
                                                                 BoxT
                                                                  (VarT "T"));
                                                                ("Value",
                                                                 BoxT
                                                                  (VarT "T"))]))))))),
                                                      VarNT "N"),
                                                     ForAllRowsT ("R",
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))])))))),
                                                   VarR "R"),
                                                  ForAllTypeT ("T",
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       VarT "T"),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible",
                                                         BoxT (VarT "T"));
                                                        ("Value",
                                                         BoxT (VarT "T"))]))))),
                                                Top),
                                               TemplateT
                                                (EntityT (VarNT "N", VarR "R"),
                                                TemplateT
                                                 (AttributeT (VarNT "N", Top),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible", BoxT Top);
                                                    ("Value", BoxT Top)])))),
                                             (AVar "ent",
                                              EntityT (VarNT "N", VarR "R"))),
                                            TemplateT
                                             (AttributeT (VarNT "N", Top),
                                             NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)]))),
                                          (AVar "a",
                                           AttributeT (VarNT "N", Top))),
                                         BoxT
                                          (NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)]))),
                                        (ABox
                                          (AVarRT "inner",
                                           NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)])),
                                         BoxT
                                          (NodeT ([CheckBox; Expression],
                                            RecordT
                                             [("Visible", BoxT Top);
                                              ("Value", BoxT Top)])))),
                                       BoxT
                                        (NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)])))]),
                                    ListT
                                     [BoxT
                                       (NodeT ([CheckBox; Expression],
                                         RecordT
                                          [("Visible", BoxT Top);
                                           ("Value", BoxT Top)]))])],
                                 ListT
                                  [ListT
                                    [BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)]))]])),
                               NodeT ([ListItem], RecordT []))],
                            ListT [NodeT ([ListItem], RecordT [])])),
                          NodeT ([Syntax.List], RecordT []))],
                       ListT
                        [BoxT
                          (NodeT ([Search; Empty],
                            RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                         NodeT ([Syntax.List], RecordT [])])),
                     NodeT ([Container], RecordT []))),
                   TemplateT (BoolT, NodeT ([Container], RecordT [])))),
                 TemplateT (ListAttrT (VarNT "N"),
                  TemplateT (BoolT, NodeT ([Container], RecordT []))))),
               TemplateT (EntityT (VarNT "N", VarR "R"),
                TemplateT (ListAttrT (VarNT "N"),
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
             ForAllRowsT ("R",
              TemplateT (EntityT (VarNT "N", VarR "R"),
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
           ForAllNameT ("N",
            ForAllRowsT ("R",
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
         ForAllNameT ("N",
          ForAllRowsT ("R",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
      (AForAllName ("masterN",
        (AForAllName ("detailN",
          (AForAllRows ("masterR",
            (AForAllRows ("detailR",
              (AForAllType ("categT",
                (ATemplate ("masterEnt",
                  EntityT (VarNT "masterN", VarR "masterR"),
                  (ATemplate ("masterAttrs", ListAttrT (VarNT "masterN"),
                    (ATemplate ("detailEnt",
                      EntityT (VarNT "detailN", VarR "detailR"),
                      (ATemplate ("detailAttrs", ListAttrT (VarNT "detailN"),
                        (ATemplate ("rollingSumAttr",
                          AttributeT (VarNT "masterN", NumT),
                          (ATemplate ("categoryAttr",
                            AttributeT (VarNT "detailN", VarT "categT"),
                            (ANode (Container, (ARecord [], RecordT []),
                              (AList
                                [(ANode (Container, (ARecord [], RecordT []),
                                   (AList
                                     [(AForNode ("ma", "maT",
                                        (AVar "masterAttrs",
                                         ListAttrT (VarNT "masterN")),
                                        [(ANode (Column,
                                           (ARecord [], RecordT []),
                                           (AList
                                             [(ALetBox ("inner",
                                                (AInstantiate
                                                  ((AInstantiate
                                                     ((ACallType
                                                        ((ACallRows
                                                           ((ACallName
                                                              ((AVar "la_templ",
                                                                ForAllNameT
                                                                 ("N",
                                                                 ForAllRowsT
                                                                  ("R",
                                                                  ForAllTypeT
                                                                   ("T",
                                                                   TemplateT
                                                                    (
                                                                    EntityT
                                                                    (VarNT "N",
                                                                    VarR "R"),
                                                                    TemplateT
                                                                    (AttributeT
                                                                    (VarNT "N",
                                                                    VarT "T"),
                                                                    NodeT
                                                                    ([Container],
                                                                    RecordT []))))))),
                                                              VarNT "masterN"),
                                                             ForAllRowsT ("R",
                                                              ForAllTypeT ("T",
                                                               TemplateT
                                                                (EntityT
                                                                  (VarNT
                                                                    "masterN",
                                                                  VarR "R"),
                                                                TemplateT
                                                                 (AttributeT
                                                                   (VarNT
                                                                    "masterN",
                                                                   VarT "T"),
                                                                 NodeT
                                                                  ([Container],
                                                                  RecordT [])))))),
                                                           VarR "masterR"),
                                                          ForAllTypeT ("T",
                                                           TemplateT
                                                            (EntityT
                                                              (VarNT "masterN",
                                                              VarR "masterR"),
                                                            TemplateT
                                                             (AttributeT
                                                               (VarNT "masterN",
                                                               VarT "T"),
                                                             NodeT
                                                              ([Container],
                                                              RecordT []))))),
                                                        Top),
                                                       TemplateT
                                                        (EntityT
                                                          (VarNT "masterN",
                                                          VarR "masterR"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "masterN",
                                                           Top),
                                                         NodeT ([Container],
                                                          RecordT [])))),
                                                     (AVar "masterEnt",
                                                      EntityT (VarNT "masterN",
                                                       VarR "masterR"))),
                                                    TemplateT
                                                     (AttributeT
                                                       (VarNT "masterN", Top),
                                                     NodeT ([Container],
                                                      RecordT []))),
                                                  (AVar "ma",
                                                   AttributeT (VarNT "masterN",
                                                    Top))),
                                                 BoxT
                                                  (NodeT ([Container],
                                                    RecordT []))),
                                                (ABox
                                                  (AVarRT "inner",
                                                   NodeT ([Container],
                                                    RecordT [])),
                                                 BoxT
                                                  (NodeT ([Container],
                                                    RecordT [])))),
                                               BoxT
                                                (NodeT ([Container],
                                                  RecordT [])))],
                                            ListT
                                             [BoxT
                                               (NodeT ([Container], RecordT []))])),
                                          NodeT ([Column], RecordT []))]),
                                       ListT [NodeT ([Column], RecordT [])])],
                                    ListT
                                     [ListT [NodeT ([Column], RecordT [])]])),
                                  NodeT ([Container], RecordT []));
                                 (ANode (Container, (ARecord [], RecordT []),
                                   (AList
                                     [(ANode (Column, (ARecord [], RecordT []),
                                        (AList
                                          [(ALetBox ("inner",
                                             (AInstantiate
                                               ((AInstantiate
                                                  ((AInstantiate
                                                     ((ACallRows
                                                        ((ACallName
                                                           ((AVar
                                                              "listing_templ",
                                                             ForAllNameT ("N",
                                                              ForAllRowsT ("R",
                                                               TemplateT
                                                                (EntityT
                                                                  (VarNT "N",
                                                                  VarR "R"),
                                                                TemplateT
                                                                 (ListAttrT
                                                                   (VarNT "N"),
                                                                 TemplateT
                                                                  (BoolT,
                                                                  NodeT
                                                                   ([Container],
                                                                   RecordT []))))))),
                                                           VarNT "detailN"),
                                                          ForAllRowsT ("R",
                                                           TemplateT
                                                            (EntityT
                                                              (VarNT "detailN",
                                                              VarR "R"),
                                                            TemplateT
                                                             (ListAttrT
                                                               (VarNT "detailN"),
                                                             TemplateT (BoolT,
                                                              NodeT
                                                               ([Container],
                                                               RecordT [])))))),
                                                        VarR "detailR"),
                                                       TemplateT
                                                        (EntityT
                                                          (VarNT "detailN",
                                                          VarR "detailR"),
                                                        TemplateT
                                                         (ListAttrT
                                                           (VarNT "detailN"),
                                                         TemplateT (BoolT,
                                                          NodeT ([Container],
                                                           RecordT []))))),
                                                     (AVar "detailEnt",
                                                      EntityT (VarNT "detailN",
                                                       VarR "detailR"))),
                                                    TemplateT
                                                     (ListAttrT
                                                       (VarNT "detailN"),
                                                     TemplateT (BoolT,
                                                      NodeT ([Container],
                                                       RecordT [])))),
                                                  (AVar "detailAttrs",
                                                   ListAttrT (VarNT "detailN"))),
                                                 TemplateT (BoolT,
                                                  NodeT ([Container],
                                                   RecordT []))),
                                               (ABool false, BoolT)),
                                              BoxT
                                               (NodeT ([Container], RecordT []))),
                                             (ABox
                                               (AVarRT "inner",
                                                NodeT ([Container], RecordT [])),
                                              BoxT
                                               (NodeT ([Container], RecordT [])))),
                                            BoxT
                                             (NodeT ([Container], RecordT [])))],
                                         ListT
                                          [BoxT
                                            (NodeT ([Container], RecordT []))])),
                                       NodeT ([Column], RecordT []));
                                      (ANode (Column,
                                        (ARecord
                                          [("Title",
                                            (ASelect
                                              ((AVar "categoryAttr",
                                                AttributeT (VarNT "detailN",
                                                 VarT "categT")),
                                              (ALabel "DisplayName",
                                               LabelT "DisplayName")),
                                             StringT))],
                                         RecordT [("Title", StringT)]),
                                        (AList
                                          [(ALetBox ("inner",
                                             (AInstantiate
                                               ((AInstantiate
                                                  ((ACallType
                                                     ((ACallRows
                                                        ((ACallName
                                                           ((AVar "chart_templ",
                                                             ForAllNameT ("N",
                                                              ForAllRowsT ("R",
                                                               ForAllTypeT
                                                                ("T",
                                                                TemplateT
                                                                 (EntityT
                                                                   (VarNT "N",
                                                                   VarR "R"),
                                                                 TemplateT
                                                                  (AttributeT
                                                                    (
                                                                    VarNT "N",
                                                                    VarT "T"),
                                                                  NodeT
                                                                   ([Chart],
                                                                   RecordT
                                                                    [("AttrGroup",
                                                                    AttributeT
                                                                    (VarNT "N",
                                                                    VarT "T"))]))))))),
                                                           VarNT "detailN"),
                                                          ForAllRowsT ("R",
                                                           ForAllTypeT ("T",
                                                            TemplateT
                                                             (EntityT
                                                               (VarNT "detailN",
                                                               VarR "R"),
                                                             TemplateT
                                                              (AttributeT
                                                                (VarNT
                                                                  "detailN",
                                                                VarT "T"),
                                                              NodeT (
                                                               [Chart],
                                                               RecordT
                                                                [("AttrGroup",
                                                                  AttributeT
                                                                   (VarNT "N",
                                                                   VarT "T"))])))))),
                                                        VarR "detailR"),
                                                       ForAllTypeT ("T",
                                                        TemplateT
                                                         (EntityT
                                                           (VarNT "detailN",
                                                           VarR "detailR"),
                                                         TemplateT
                                                          (AttributeT
                                                            (VarNT "detailN",
                                                            VarT "T"),
                                                          NodeT ([Chart],
                                                           RecordT
                                                            [("AttrGroup",
                                                              AttributeT
                                                               (VarNT "N",
                                                               VarT "T"))]))))),
                                                     VarT "categT"),
                                                    TemplateT
                                                     (EntityT (VarNT "detailN",
                                                       VarR "detailR"),
                                                     TemplateT
                                                      (AttributeT
                                                        (VarNT "detailN",
                                                        VarT "categT"),
                                                      NodeT ([Chart],
                                                       RecordT
                                                        [("AttrGroup",
                                                          AttributeT
                                                           (VarNT "N",
                                                           VarT "categT"))])))),
                                                  (AVar "detailEnt",
                                                   EntityT (VarNT "detailN",
                                                    VarR "detailR"))),
                                                 TemplateT
                                                  (AttributeT (VarNT "detailN",
                                                    VarT "categT"),
                                                  NodeT ([Chart],
                                                   RecordT
                                                    [("AttrGroup",
                                                      AttributeT (VarNT "N",
                                                       VarT "categT"))]))),
                                               (AVar "categoryAttr",
                                                AttributeT (VarNT "detailN",
                                                 VarT "categT"))),
                                              BoxT
                                               (NodeT ([Chart],
                                                 RecordT
                                                  [("AttrGroup",
                                                    AttributeT (VarNT "N",
                                                     VarT "categT"))]))),
                                             (ABox
                                               (AVarRT "inner",
                                                NodeT ([Chart],
                                                 RecordT
                                                  [("AttrGroup",
                                                    AttributeT (VarNT "N",
                                                     VarT "categT"))])),
                                              BoxT
                                               (NodeT ([Chart],
                                                 RecordT
                                                  [("AttrGroup",
                                                    AttributeT (VarNT "N",
                                                     VarT "categT"))])))),
                                            BoxT
                                             (NodeT ([Chart],
                                               RecordT
                                                [("AttrGroup",
                                                  AttributeT (VarNT "N",
                                                   VarT "categT"))])))],
                                         ListT
                                          [BoxT
                                            (NodeT ([Chart],
                                              RecordT
                                               [("AttrGroup",
                                                 AttributeT (VarNT "N",
                                                  VarT "categT"))]))])),
                                       NodeT ([Column],
                                        RecordT [("Title", StringT)]))],
                                    ListT
                                     [NodeT ([Column], RecordT []);
                                      NodeT ([Column],
                                       RecordT [("Title", StringT)])])),
                                  NodeT ([Container], RecordT []))],
                               ListT
                                [NodeT ([Container], RecordT []);
                                 NodeT ([Container], RecordT [])])),
                             NodeT ([Container], RecordT []))),
                           TemplateT
                            (AttributeT (VarNT "detailN", VarT "categT"),
                            NodeT ([Container], RecordT [])))),
                         TemplateT (AttributeT (VarNT "masterN", NumT),
                          TemplateT
                           (AttributeT (VarNT "detailN", VarT "categT"),
                           NodeT ([Container], RecordT []))))),
                       TemplateT (ListAttrT (VarNT "detailN"),
                        TemplateT (AttributeT (VarNT "masterN", NumT),
                         TemplateT
                          (AttributeT (VarNT "detailN", VarT "categT"),
                          NodeT ([Container], RecordT [])))))),
                     TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                      TemplateT (ListAttrT (VarNT "detailN"),
                       TemplateT (AttributeT (VarNT "masterN", NumT),
                        TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                         NodeT ([Container], RecordT []))))))),
                   TemplateT (ListAttrT (VarNT "masterN"),
                    TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                     TemplateT (ListAttrT (VarNT "detailN"),
                      TemplateT (AttributeT (VarNT "masterN", NumT),
                       TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                        NodeT ([Container], RecordT [])))))))),
                 TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
                  TemplateT (ListAttrT (VarNT "masterN"),
                   TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                    TemplateT (ListAttrT (VarNT "detailN"),
                     TemplateT (AttributeT (VarNT "masterN", NumT),
                      TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                       NodeT ([Container], RecordT []))))))))),
               ForAllTypeT ("categT",
                TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
                 TemplateT (ListAttrT (VarNT "masterN"),
                  TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                   TemplateT (ListAttrT (VarNT "detailN"),
                    TemplateT (AttributeT (VarNT "masterN", NumT),
                     TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                      NodeT ([Container], RecordT [])))))))))),
             ForAllRowsT ("detailR",
              ForAllTypeT ("categT",
               TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
                TemplateT (ListAttrT (VarNT "masterN"),
                 TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                  TemplateT (ListAttrT (VarNT "detailN"),
                   TemplateT (AttributeT (VarNT "masterN", NumT),
                    TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                     NodeT ([Container], RecordT []))))))))))),
           ForAllRowsT ("masterR",
            ForAllRowsT ("detailR",
             ForAllTypeT ("categT",
              TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
               TemplateT (ListAttrT (VarNT "masterN"),
                TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                 TemplateT (ListAttrT (VarNT "detailN"),
                  TemplateT (AttributeT (VarNT "masterN", NumT),
                   TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                    NodeT ([Container], RecordT [])))))))))))),
         ForAllNameT ("detailN",
          ForAllRowsT ("masterR",
           ForAllRowsT ("detailR",
            ForAllTypeT ("categT",
             TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
              TemplateT (ListAttrT (VarNT "masterN"),
               TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
                TemplateT (ListAttrT (VarNT "detailN"),
                 TemplateT (AttributeT (VarNT "masterN", NumT),
                  TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                   NodeT ([Container], RecordT []))))))))))))),
       ForAllNameT ("masterN",
        ForAllNameT ("detailN",
         ForAllRowsT ("masterR",
          ForAllRowsT ("detailR",
           ForAllTypeT ("categT",
            TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
             TemplateT (ListAttrT (VarNT "masterN"),
              TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
               TemplateT (ListAttrT (VarNT "detailN"),
                TemplateT (AttributeT (VarNT "masterN", NumT),
                 TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                  NodeT ([Container], RecordT [])))))))))))))),
     ForAllNameT ("masterN",
      ForAllNameT ("detailN",
       ForAllRowsT ("masterR",
        ForAllRowsT ("detailR",
         ForAllTypeT ("categT",
          TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
           TemplateT (ListAttrT (VarNT "masterN"),
            TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
             TemplateT (ListAttrT (VarNT "detailN"),
              TemplateT (AttributeT (VarNT "masterN", NumT),
               TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
                NodeT ([Container], RecordT [])))))))))))))),
   ForAllNameT ("masterN",
    ForAllNameT ("detailN",
     ForAllRowsT ("masterR",
      ForAllRowsT ("detailR",
       ForAllTypeT ("categT",
        TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
         TemplateT (ListAttrT (VarNT "masterN"),
          TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
           TemplateT (ListAttrT (VarNT "detailN"),
            TemplateT (AttributeT (VarNT "masterN", NumT),
             TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
              NodeT ([Container], RecordT [])))))))))))))),
 ForAllNameT ("masterN",
  ForAllNameT ("detailN",
   ForAllRowsT ("masterR",
    ForAllRowsT ("detailR",
     ForAllTypeT ("categT",
      TemplateT (EntityT (VarNT "masterN", VarR "masterR"),
       TemplateT (ListAttrT (VarNT "masterN"),
        TemplateT (EntityT (VarNT "detailN", VarR "detailR"),
         TemplateT (ListAttrT (VarNT "detailN"),
          TemplateT (AttributeT (VarNT "masterN", NumT),
           TemplateT (AttributeT (VarNT "detailN", VarT "categT"),
            NodeT ([Container], RecordT [])))))))))))))

let _ = assert(typecheck account_dashboard [] [] [] [] [] = expt)


let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt []))



(* TODO - attrsinfilter, mantenho como parametro sempre? ou uso uma das outras listas? *)

(* MasterDetail (entity + masterAttrs + detailAttrs + showFilter + attrsInFilter) *)
let master_detail =
Let("listing", listing,
Let("detail", detail,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "entity",
    EntityT(VarNT "N", VarR "R"),
Template(
    "masterAttrs",
    ListAttrT(VarNT "N"),
Template(
    "detailAttrs",
    ListAttrT(VarNT "N"),
Template(
    "showFilter",
    BoolT,
(* Template(
    "attrsInFilter",
    ListAttrT(VarNT "N"), *)
        Node(Container,
            Record [],
            List [
                Node(Column,
                    Record [],
                    List [
                        LetBox("inner",
                            Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "listing", VarNT "N"), VarR "R"), Var "entity"), Var "masterAttrs"), Var "showFilter"),
                            Box (VarRT "inner")
                        )
                    ]
                );
                Node(Column,
                    Record [],
                    List [
                        LetBox("inner",
                            Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "detail", VarNT "N"), VarR "R"), Var "entity"), Var "masterAttrs"), Var "detailAttrs"),
                            Box (VarRT "inner")
                        )
                    ]
                )
            ]
        )
(* ) *)
)
)
)
)
)
)
)
)


let inst_true = 
Let("f", master_detail,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("categoryAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
Let("rollingAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
     Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Var "a2-store"), Bool true)
)
)
)
)
)
)
)
)

let inst_false = 
Let("f", master_detail,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("categoryAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
Let("rollingAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
     Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Var "a2-store"), Bool false)
)
)
)
)
)
)
)
)

let rt_t = LetBox("inst", inst_true, VarRT "inst")
let rt_f = LetBox("inst", inst_false, VarRT "inst")



(* let _ = print(string_of_pairtt(typecheck master_detail [] [] [] [] [])) *)
let expt = (ALet ("listing",
  (ALet ("filter_templ",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (ALet ("attr_templ",
      (AForAllName ("N",
        (AForAllRows ("R",
          (AForAllType ("T",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                (ALetBox ("name",
                  (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                   BoxT
                    (RecordT
                      [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                  (ALetBox ("label",
                    (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                    (AIfNode
                      ((AIsOfType
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoolT),
                        BoolT),
                      (ANode (CheckBox,
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
                       NodeT ([CheckBox],
                        RecordT [("Visible", BoxT (VarT "T"))])),
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
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
         ForAllRowsT ("R",
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("ent", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(AIfNode ((AVar "showFilter", BoolT),
                       (ALetBox ("inner",
                         (AInstantiate
                           ((AVar "filter_templ",
                             TemplateT (ListAttrT (VarNT "N"),
                              NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (AVar "attrs", ListAttrT (VarNT "N"))),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                         (ABox
                           (AVarRT "inner",
                            NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                        BoxT
                         (NodeT ([Search],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                       (ANode (Empty, (ARecord [], RecordT []),
                         (AList [], ListT [])),
                        NodeT ([Empty], RecordT []))),
                      BoxT
                       (NodeT ([Search; Empty],
                         RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                     (ANode (Syntax.List, (ARecord [], RecordT []),
                       (AList
                         [(ANode (ListItem, (ARecord [], RecordT []),
                            (AList
                              [(AForNode ("a", "aT",
                                 (AVar "attrs", ListAttrT (VarNT "N")),
                                 [(ALetBox ("inner",
                                    (AInstantiate
                                      ((AInstantiate
                                         ((ACallType
                                            ((ACallRows
                                               ((ACallName
                                                  ((AVar "attr_templ",
                                                    ForAllNameT ("N",
                                                     ForAllRowsT ("R",
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))]))))))),
                                                  VarNT "N"),
                                                 ForAllRowsT ("R",
                                                  ForAllTypeT ("T",
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       VarT "T"),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible",
                                                         BoxT (VarT "T"));
                                                        ("Value",
                                                         BoxT (VarT "T"))])))))),
                                               VarR "R"),
                                              ForAllTypeT ("T",
                                               TemplateT
                                                (EntityT (VarNT "N", VarR "R"),
                                                TemplateT
                                                 (AttributeT (VarNT "N",
                                                   VarT "T"),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible",
                                                     BoxT (VarT "T"));
                                                    ("Value", BoxT (VarT "T"))]))))),
                                            Top),
                                           TemplateT
                                            (EntityT (VarNT "N", VarR "R"),
                                            TemplateT
                                             (AttributeT (VarNT "N", Top),
                                             NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))),
                                         (AVar "ent",
                                          EntityT (VarNT "N", VarR "R"))),
                                        TemplateT (AttributeT (VarNT "N", Top),
                                         NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)]))),
                                      (AVar "a", AttributeT (VarNT "N", Top))),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)]))),
                                    (ABox
                                      (AVarRT "inner",
                                       NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])))),
                                   BoxT
                                    (NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)])))]),
                                ListT
                                 [BoxT
                                   (NodeT ([CheckBox; Expression],
                                     RecordT
                                      [("Visible", BoxT Top);
                                       ("Value", BoxT Top)]))])],
                             ListT
                              [ListT
                                [BoxT
                                  (NodeT ([CheckBox; Expression],
                                    RecordT
                                     [("Visible", BoxT Top);
                                      ("Value", BoxT Top)]))]])),
                           NodeT ([ListItem], RecordT []))],
                        ListT [NodeT ([ListItem], RecordT [])])),
                      NodeT ([Syntax.List], RecordT []))],
                   ListT
                    [BoxT
                      (NodeT ([Search; Empty],
                        RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                     NodeT ([Syntax.List], RecordT [])])),
                 NodeT ([Container], RecordT []))),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
  (ALet ("detail",
    (ALet ("f",
      (ALet ("attr_templ",
        (AForAllName ("N",
          (AForAllRows ("R",
            (AForAllType ("T",
              (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
                (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                  (ALetBox ("name",
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
                        (ANode (CheckBox,
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
                         NodeT ([CheckBox],
                          RecordT [("Visible", BoxT (VarT "T"))])),
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
                       NodeT ([CheckBox; Expression],
                        RecordT
                         [("Visible", BoxT (VarT "T"));
                          ("Value", BoxT (VarT "T"))]))),
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                  NodeT ([CheckBox; Expression],
                   RecordT
                    [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
               TemplateT (EntityT (VarNT "N", VarR "R"),
                TemplateT (AttributeT (VarNT "N", VarT "T"),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
             ForAllTypeT ("T",
              TemplateT (EntityT (VarNT "N", VarR "R"),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
           ForAllRowsT ("R",
            ForAllTypeT ("T",
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
         ForAllNameT ("N",
          ForAllRowsT ("R",
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
        (AForAllName ("N",
          (AForAllRows ("R",
            (AForAllType ("T",
              (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
                (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                  (ANode (Container, (ARecord [], RecordT []),
                    (AList
                      [(ANode (Expression,
                         (ARecord
                           [("Value",
                             (ASelect
                               ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                               (ALabel "DisplayName", LabelT "DisplayName")),
                              StringT))],
                          RecordT [("Value", StringT)]),
                         (AList [], ListT [])),
                        NodeT ([Expression], RecordT [("Value", StringT)]));
                       (ALetBox ("inner_templ",
                         (AInstantiate
                           ((AInstantiate
                              ((ACallType
                                 ((ACallRows
                                    ((ACallName
                                       ((AVar "attr_templ",
                                         ForAllNameT ("N",
                                          ForAllRowsT ("R",
                                           ForAllTypeT ("T",
                                            TemplateT
                                             (EntityT (VarNT "N", VarR "R"),
                                             TemplateT
                                              (AttributeT (VarNT "N", VarT "T"),
                                              NodeT ([CheckBox; Expression],
                                               RecordT
                                                [("Visible", BoxT (VarT "T"));
                                                 ("Value", BoxT (VarT "T"))]))))))),
                                       VarNT "N"),
                                      ForAllRowsT ("R",
                                       ForAllTypeT ("T",
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT
                                          (AttributeT (VarNT "N", VarT "T"),
                                          NodeT ([CheckBox; Expression],
                                           RecordT
                                            [("Visible", BoxT (VarT "T"));
                                             ("Value", BoxT (VarT "T"))])))))),
                                    VarR "R"),
                                   ForAllTypeT ("T",
                                    TemplateT (EntityT (VarNT "N", VarR "R"),
                                     TemplateT
                                      (AttributeT (VarNT "N", VarT "T"),
                                      NodeT ([CheckBox; Expression],
                                       RecordT
                                        [("Visible", BoxT (VarT "T"));
                                         ("Value", BoxT (VarT "T"))]))))),
                                 VarT "T"),
                                TemplateT (EntityT (VarNT "N", VarR "R"),
                                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                                  NodeT ([CheckBox; Expression],
                                   RecordT
                                    [("Visible", BoxT (VarT "T"));
                                     ("Value", BoxT (VarT "T"))])))),
                              (AVar "e", EntityT (VarNT "N", VarR "R"))),
                             TemplateT (AttributeT (VarNT "N", VarT "T"),
                              NodeT ([CheckBox; Expression],
                               RecordT
                                [("Visible", BoxT (VarT "T"));
                                 ("Value", BoxT (VarT "T"))]))),
                           (AVar "attr", AttributeT (VarNT "N", VarT "T"))),
                          BoxT
                           (NodeT ([CheckBox; Expression],
                             RecordT
                              [("Visible", BoxT (VarT "T"));
                               ("Value", BoxT (VarT "T"))]))),
                         (ABox
                           (AVarRT "inner_templ",
                            NodeT ([CheckBox; Expression],
                             RecordT
                              [("Visible", BoxT (VarT "T"));
                               ("Value", BoxT (VarT "T"))])),
                          BoxT
                           (NodeT ([CheckBox; Expression],
                             RecordT
                              [("Visible", BoxT (VarT "T"));
                               ("Value", BoxT (VarT "T"))])))),
                        BoxT
                         (NodeT ([CheckBox; Expression],
                           RecordT
                            [("Visible", BoxT (VarT "T"));
                             ("Value", BoxT (VarT "T"))])))],
                     ListT
                      [NodeT ([Expression], RecordT [("Value", StringT)]);
                       BoxT
                        (NodeT ([CheckBox; Expression],
                          RecordT
                           [("Visible", BoxT (VarT "T"));
                            ("Value", BoxT (VarT "T"))]))])),
                   NodeT ([Container], RecordT []))),
                 TemplateT (AttributeT (VarNT "N", VarT "T"),
                  NodeT ([Container], RecordT [])))),
               TemplateT (EntityT (VarNT "N", VarR "R"),
                TemplateT (AttributeT (VarNT "N", VarT "T"),
                 NodeT ([Container], RecordT []))))),
             ForAllTypeT ("T",
              TemplateT (EntityT (VarNT "N", VarR "R"),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([Container], RecordT [])))))),
           ForAllRowsT ("R",
            ForAllTypeT ("T",
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([Container], RecordT []))))))),
         ForAllNameT ("N",
          ForAllRowsT ("R",
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Container], RecordT [])))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Container], RecordT []))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("primAttrs", ListAttrT (VarNT "N"),
              (ATemplate ("secAttrs", ListAttrT (VarNT "N"),
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(ANode (Column, (ARecord [], RecordT []),
                       (AList
                         [(AForNode ("a1", "a1T",
                            (AVar "primAttrs", ListAttrT (VarNT "N")),
                            [(ALetBox ("lab_attr",
                               (AInstantiate
                                 ((AInstantiate
                                    ((ACallType
                                       ((ACallRows
                                          ((ACallName
                                             ((AVar "f",
                                               ForAllNameT ("N",
                                                ForAllRowsT ("R",
                                                 ForAllTypeT ("T",
                                                  TemplateT
                                                   (EntityT (VarNT "N",
                                                     VarR "R"),
                                                   TemplateT
                                                    (AttributeT (VarNT "N",
                                                      VarT "T"),
                                                    NodeT ([Container],
                                                     RecordT []))))))),
                                             VarNT "N"),
                                            ForAllRowsT ("R",
                                             ForAllTypeT ("T",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (AttributeT (VarNT "N",
                                                  VarT "T"),
                                                NodeT ([Container], RecordT [])))))),
                                          VarR "R"),
                                         ForAllTypeT ("T",
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT
                                            (AttributeT (VarNT "N", VarT "T"),
                                            NodeT ([Container], RecordT []))))),
                                       Top),
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT (AttributeT (VarNT "N", Top),
                                        NodeT ([Container], RecordT [])))),
                                    (AVar "e", EntityT (VarNT "N", VarR "R"))),
                                   TemplateT (AttributeT (VarNT "N", Top),
                                    NodeT ([Container], RecordT []))),
                                 (AVar "a1", AttributeT (VarNT "N", Top))),
                                BoxT (NodeT ([Container], RecordT []))),
                               (ABox
                                 (AVarRT "lab_attr",
                                  NodeT ([Container], RecordT [])),
                                BoxT (NodeT ([Container], RecordT [])))),
                              BoxT (NodeT ([Container], RecordT [])))]),
                           ListT [BoxT (NodeT ([Container], RecordT []))])],
                        ListT [ListT [BoxT (NodeT ([Container], RecordT []))]])),
                      NodeT ([Column], RecordT []));
                     (ANode (Column, (ARecord [], RecordT []),
                       (AList
                         [(AForNode ("a2", "a2T",
                            (AVar "secAttrs", ListAttrT (VarNT "N")),
                            [(ALetBox ("lab_attr",
                               (AInstantiate
                                 ((AInstantiate
                                    ((ACallType
                                       ((ACallRows
                                          ((ACallName
                                             ((AVar "f",
                                               ForAllNameT ("N",
                                                ForAllRowsT ("R",
                                                 ForAllTypeT ("T",
                                                  TemplateT
                                                   (EntityT (VarNT "N",
                                                     VarR "R"),
                                                   TemplateT
                                                    (AttributeT (VarNT "N",
                                                      VarT "T"),
                                                    NodeT ([Container],
                                                     RecordT []))))))),
                                             VarNT "N"),
                                            ForAllRowsT ("R",
                                             ForAllTypeT ("T",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (AttributeT (VarNT "N",
                                                  VarT "T"),
                                                NodeT ([Container], RecordT [])))))),
                                          VarR "R"),
                                         ForAllTypeT ("T",
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT
                                            (AttributeT (VarNT "N", VarT "T"),
                                            NodeT ([Container], RecordT []))))),
                                       Top),
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT (AttributeT (VarNT "N", Top),
                                        NodeT ([Container], RecordT [])))),
                                    (AVar "e", EntityT (VarNT "N", VarR "R"))),
                                   TemplateT (AttributeT (VarNT "N", Top),
                                    NodeT ([Container], RecordT []))),
                                 (AVar "a2", AttributeT (VarNT "N", Top))),
                                BoxT (NodeT ([Container], RecordT []))),
                               (ABox
                                 (AVarRT "lab_attr",
                                  NodeT ([Container], RecordT [])),
                                BoxT (NodeT ([Container], RecordT [])))),
                              BoxT (NodeT ([Container], RecordT [])))]),
                           ListT [BoxT (NodeT ([Container], RecordT []))])],
                        ListT [ListT [BoxT (NodeT ([Container], RecordT []))]])),
                      NodeT ([Column], RecordT []))],
                   ListT
                    [NodeT ([Column], RecordT []);
                     NodeT ([Column], RecordT [])])),
                 NodeT ([Container], RecordT []))),
               TemplateT (ListAttrT (VarNT "N"),
                NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (ListAttrT (VarNT "N"),
               NodeT ([Container], RecordT []))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT [])))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT []))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT [])))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (ListAttrT (VarNT "N"), NodeT ([Container], RecordT []))))))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (ATemplate ("entity", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("masterAttrs", ListAttrT (VarNT "N"),
            (ATemplate ("detailAttrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(ANode (Column, (ARecord [], RecordT []),
                       (AList
                         [(ALetBox ("inner",
                            (AInstantiate
                              ((AInstantiate
                                 ((AInstantiate
                                    ((ACallRows
                                       ((ACallName
                                          ((AVar "listing",
                                            ForAllNameT ("N",
                                             ForAllRowsT ("R",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (ListAttrT (VarNT "N"),
                                                TemplateT (BoolT,
                                                 NodeT ([Container],
                                                  RecordT []))))))),
                                          VarNT "N"),
                                         ForAllRowsT ("R",
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT (ListAttrT (VarNT "N"),
                                            TemplateT (BoolT,
                                             NodeT ([Container], RecordT [])))))),
                                       VarR "R"),
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT (ListAttrT (VarNT "N"),
                                        TemplateT (BoolT,
                                         NodeT ([Container], RecordT []))))),
                                    (AVar "entity",
                                     EntityT (VarNT "N", VarR "R"))),
                                   TemplateT (ListAttrT (VarNT "N"),
                                    TemplateT (BoolT,
                                     NodeT ([Container], RecordT [])))),
                                 (AVar "masterAttrs", ListAttrT (VarNT "N"))),
                                TemplateT (BoolT,
                                 NodeT ([Container], RecordT []))),
                              (AVar "showFilter", BoolT)),
                             BoxT (NodeT ([Container], RecordT []))),
                            (ABox
                              (AVarRT "inner", NodeT ([Container], RecordT [])),
                             BoxT (NodeT ([Container], RecordT [])))),
                           BoxT (NodeT ([Container], RecordT [])))],
                        ListT [BoxT (NodeT ([Container], RecordT []))])),
                      NodeT ([Column], RecordT []));
                     (ANode (Column, (ARecord [], RecordT []),
                       (AList
                         [(ALetBox ("inner",
                            (AInstantiate
                              ((AInstantiate
                                 ((AInstantiate
                                    ((ACallRows
                                       ((ACallName
                                          ((AVar "detail",
                                            ForAllNameT ("N",
                                             ForAllRowsT ("R",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (ListAttrT (VarNT "N"),
                                                TemplateT
                                                 (ListAttrT (VarNT "N"),
                                                 NodeT ([Container],
                                                  RecordT []))))))),
                                          VarNT "N"),
                                         ForAllRowsT ("R",
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT (ListAttrT (VarNT "N"),
                                            TemplateT (ListAttrT (VarNT "N"),
                                             NodeT ([Container], RecordT [])))))),
                                       VarR "R"),
                                      TemplateT (EntityT (VarNT "N", VarR "R"),
                                       TemplateT (ListAttrT (VarNT "N"),
                                        TemplateT (ListAttrT (VarNT "N"),
                                         NodeT ([Container], RecordT []))))),
                                    (AVar "entity",
                                     EntityT (VarNT "N", VarR "R"))),
                                   TemplateT (ListAttrT (VarNT "N"),
                                    TemplateT (ListAttrT (VarNT "N"),
                                     NodeT ([Container], RecordT [])))),
                                 (AVar "masterAttrs", ListAttrT (VarNT "N"))),
                                TemplateT (ListAttrT (VarNT "N"),
                                 NodeT ([Container], RecordT []))),
                              (AVar "detailAttrs", ListAttrT (VarNT "N"))),
                             BoxT (NodeT ([Container], RecordT []))),
                            (ABox
                              (AVarRT "inner", NodeT ([Container], RecordT [])),
                             BoxT (NodeT ([Container], RecordT [])))),
                           BoxT (NodeT ([Container], RecordT [])))],
                        ListT [BoxT (NodeT ([Container], RecordT []))])),
                      NodeT ([Column], RecordT []))],
                   ListT
                    [NodeT ([Column], RecordT []);
                     NodeT ([Column], RecordT [])])),
                 NodeT ([Container], RecordT []))),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
       ForAllRowsT ("R",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (ListAttrT (VarNT "N"),
      TemplateT (BoolT, NodeT ([Container], RecordT []))))))))

let _ = assert(typecheck master_detail [] [] [] [] [] = expt)

let _ = print(string_of_term(eval inst_true []))
let _ = print(string_of_term(eval inst_false []))

let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))





(* FourColumnGallery *)
let four_col =
Let("listing", listing,
Let("filter", filter,
ForAllName(
    "N",
ForAllRows(
    "R",
(* ForAllName(
    "imgN",
ForAllRows(
    "imgR", *)
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
(* Template(
    "imgEnt",
    EntityT(VarNT "imgN", VarR "imgR"), *)
Template(
    "showFilter",
    BoolT,
Template(
    "attrsInFilter",
    ListAttrT(VarNT "N"),
Template(
    "showPagination",
    BoolT,
        Node(Container,
            Record [],
            List [
                Node(Column,
                    Record [],
                    List [
                        LetBox("inner",
                            Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "listing", VarNT "N"), VarR "R"), Var "e"), Var "attrs"), Bool false),
                            Box(VarRT "inner")
                        );
                        IfNode(
                            Var "showPagination",
                            pagination,
                            Node(Empty, Record [], List [])
                        )
                    ]
                );
                Node(Column,
                    Record [],
                    List [
                        IfNode(
                            Var "showFilter",
                            LetBox("inner",
                                Instantiate(Var "filter", Var "attrsInFilter"),
                                Box(VarRT "inner")
                            ),
                            Node(Empty, Record [], List [])
                        )
                    ]
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
)
)
(* )
)
)
) *)

let inst_true_true = 
Let("f", four_col,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("categoryAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
Let("rollingAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
     Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Bool true), Var "a2-store"), Bool true)
)
)
)
)
)
)
)
)

let inst_false_false = 
Let("f", four_col,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("categoryAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
Let("rollingAttr", Attribute("Store", "Random", NumT, Record [("DisplayName", String "Random Title")]),
     Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Bool false), Var "a2-store"), Bool false)
)
)
)
)
)
)
)
)


let rt_t = LetBox("inst", inst_true_true, VarRT "inst")
let rt_f = LetBox("inst", inst_false_false, VarRT "inst")




(* let _ = typecheck four_col [] [] [] [] [] *)
let expt = (ALet ("listing",
  (ALet ("filter_templ",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (ALet ("attr_templ",
      (AForAllName ("N",
        (AForAllRows ("R",
          (AForAllType ("T",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                (ALetBox ("name",
                  (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                   BoxT
                    (RecordT
                      [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                  (ALetBox ("label",
                    (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                    (AIfNode
                      ((AIsOfType
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoolT),
                        BoolT),
                      (ANode (CheckBox,
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
                       NodeT ([CheckBox],
                        RecordT [("Visible", BoxT (VarT "T"))])),
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
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
         ForAllRowsT ("R",
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("ent", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(AIfNode ((AVar "showFilter", BoolT),
                       (ALetBox ("inner",
                         (AInstantiate
                           ((AVar "filter_templ",
                             TemplateT (ListAttrT (VarNT "N"),
                              NodeT ([Search],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                           (AVar "attrs", ListAttrT (VarNT "N"))),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                         (ABox
                           (AVarRT "inner",
                            NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])),
                          BoxT
                           (NodeT ([Search],
                             RecordT [("filterBy", ListAttrT (VarNT "N"))])))),
                        BoxT
                         (NodeT ([Search],
                           RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                       (ANode (Empty, (ARecord [], RecordT []),
                         (AList [], ListT [])),
                        NodeT ([Empty], RecordT []))),
                      BoxT
                       (NodeT ([Search; Empty],
                         RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                     (ANode (Syntax.List, (ARecord [], RecordT []),
                       (AList
                         [(ANode (ListItem, (ARecord [], RecordT []),
                            (AList
                              [(AForNode ("a", "aT",
                                 (AVar "attrs", ListAttrT (VarNT "N")),
                                 [(ALetBox ("inner",
                                    (AInstantiate
                                      ((AInstantiate
                                         ((ACallType
                                            ((ACallRows
                                               ((ACallName
                                                  ((AVar "attr_templ",
                                                    ForAllNameT ("N",
                                                     ForAllRowsT ("R",
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))]))))))),
                                                  VarNT "N"),
                                                 ForAllRowsT ("R",
                                                  ForAllTypeT ("T",
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       VarT "T"),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible",
                                                         BoxT (VarT "T"));
                                                        ("Value",
                                                         BoxT (VarT "T"))])))))),
                                               VarR "R"),
                                              ForAllTypeT ("T",
                                               TemplateT
                                                (EntityT (VarNT "N", VarR "R"),
                                                TemplateT
                                                 (AttributeT (VarNT "N",
                                                   VarT "T"),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible",
                                                     BoxT (VarT "T"));
                                                    ("Value", BoxT (VarT "T"))]))))),
                                            Top),
                                           TemplateT
                                            (EntityT (VarNT "N", VarR "R"),
                                            TemplateT
                                             (AttributeT (VarNT "N", Top),
                                             NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))),
                                         (AVar "ent",
                                          EntityT (VarNT "N", VarR "R"))),
                                        TemplateT (AttributeT (VarNT "N", Top),
                                         NodeT ([CheckBox; Expression],
                                          RecordT
                                           [("Visible", BoxT Top);
                                            ("Value", BoxT Top)]))),
                                      (AVar "a", AttributeT (VarNT "N", Top))),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)]))),
                                    (ABox
                                      (AVarRT "inner",
                                       NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])),
                                     BoxT
                                      (NodeT ([CheckBox; Expression],
                                        RecordT
                                         [("Visible", BoxT Top);
                                          ("Value", BoxT Top)])))),
                                   BoxT
                                    (NodeT ([CheckBox; Expression],
                                      RecordT
                                       [("Visible", BoxT Top);
                                        ("Value", BoxT Top)])))]),
                                ListT
                                 [BoxT
                                   (NodeT ([CheckBox; Expression],
                                     RecordT
                                      [("Visible", BoxT Top);
                                       ("Value", BoxT Top)]))])],
                             ListT
                              [ListT
                                [BoxT
                                  (NodeT ([CheckBox; Expression],
                                    RecordT
                                     [("Visible", BoxT Top);
                                      ("Value", BoxT Top)]))]])),
                           NodeT ([ListItem], RecordT []))],
                        ListT [NodeT ([ListItem], RecordT [])])),
                      NodeT ([Syntax.List], RecordT []))],
                   ListT
                    [BoxT
                      (NodeT ([Search; Empty],
                        RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                     NodeT ([Syntax.List], RecordT [])])),
                 NodeT ([Container], RecordT []))),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
  (ALet ("filter",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (AForAllName ("N",
      (AForAllRows ("R",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attrs", ListAttrT (VarNT "N"),
            (ATemplate ("showFilter", BoolT,
              (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
                (ATemplate ("showPagination", BoolT,
                  (ANode (Container, (ARecord [], RecordT []),
                    (AList
                      [(ANode (Column, (ARecord [], RecordT []),
                         (AList
                           [(ALetBox ("inner",
                              (AInstantiate
                                ((AInstantiate
                                   ((AInstantiate
                                      ((ACallRows
                                         ((ACallName
                                            ((AVar "listing",
                                              ForAllNameT ("N",
                                               ForAllRowsT ("R",
                                                TemplateT
                                                 (EntityT (VarNT "N", VarR "R"),
                                                 TemplateT
                                                  (ListAttrT (VarNT "N"),
                                                  TemplateT (BoolT,
                                                   NodeT ([Container],
                                                    RecordT []))))))),
                                            VarNT "N"),
                                           ForAllRowsT ("R",
                                            TemplateT
                                             (EntityT (VarNT "N", VarR "R"),
                                             TemplateT (ListAttrT (VarNT "N"),
                                              TemplateT (BoolT,
                                               NodeT ([Container], RecordT [])))))),
                                         VarR "R"),
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT (ListAttrT (VarNT "N"),
                                          TemplateT (BoolT,
                                           NodeT ([Container], RecordT []))))),
                                      (AVar "e", EntityT (VarNT "N", VarR "R"))),
                                     TemplateT (ListAttrT (VarNT "N"),
                                      TemplateT (BoolT,
                                       NodeT ([Container], RecordT [])))),
                                   (AVar "attrs", ListAttrT (VarNT "N"))),
                                  TemplateT (BoolT,
                                   NodeT ([Container], RecordT []))),
                                (ABool false, BoolT)),
                               BoxT (NodeT ([Container], RecordT []))),
                              (ABox
                                (AVarRT "inner",
                                 NodeT ([Container], RecordT [])),
                               BoxT (NodeT ([Container], RecordT [])))),
                             BoxT (NodeT ([Container], RecordT [])));
                            (AIfNode ((AVar "showPagination", BoolT),
                              (ABox
                                (ANode (Pagination, (ARecord [], RecordT []),
                                  (AList [], ListT [])),
                                 NodeT ([Pagination], RecordT [])),
                               BoxT (NodeT ([Pagination], RecordT []))),
                              (ANode (Empty, (ARecord [], RecordT []),
                                (AList [], ListT [])),
                               NodeT ([Empty], RecordT []))),
                             BoxT (NodeT ([Pagination; Empty], RecordT [])))],
                          ListT
                           [BoxT (NodeT ([Container], RecordT []));
                            BoxT (NodeT ([Pagination; Empty], RecordT []))])),
                        NodeT ([Column], RecordT []));
                       (ANode (Column, (ARecord [], RecordT []),
                         (AList
                           [(AIfNode ((AVar "showFilter", BoolT),
                              (ALetBox ("inner",
                                (AInstantiate
                                  ((AVar "filter",
                                    TemplateT (ListAttrT (VarNT "N"),
                                     NodeT ([Search],
                                      RecordT
                                       [("filterBy", ListAttrT (VarNT "N"))]))),
                                  (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                                 BoxT
                                  (NodeT ([Search],
                                    RecordT
                                     [("filterBy", ListAttrT (VarNT "N"))]))),
                                (ABox
                                  (AVarRT "inner",
                                   NodeT ([Search],
                                    RecordT
                                     [("filterBy", ListAttrT (VarNT "N"))])),
                                 BoxT
                                  (NodeT ([Search],
                                    RecordT
                                     [("filterBy", ListAttrT (VarNT "N"))])))),
                               BoxT
                                (NodeT ([Search],
                                  RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                              (ANode (Empty, (ARecord [], RecordT []),
                                (AList [], ListT [])),
                               NodeT ([Empty], RecordT []))),
                             BoxT
                              (NodeT ([Search; Empty],
                                RecordT [("filterBy", ListAttrT (VarNT "N"))])))],
                          ListT
                           [BoxT
                             (NodeT ([Search; Empty],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))]))])),
                        NodeT ([Column], RecordT []))],
                     ListT
                      [NodeT ([Column], RecordT []);
                       NodeT ([Column], RecordT [])])),
                   NodeT ([Container], RecordT []))),
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))),
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT, NodeT ([Container], RecordT []))))),
             TemplateT (BoolT,
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT,
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
       ForAllRowsT ("R",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT,
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (BoolT,
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))


let _ = assert(typecheck four_col [] [] [] [] [] = expt)
(* let _ = print(string_of_pairtt(typecheck four_col [] [] [] [] [])) *)

let _ = print(string_of_term(eval inst_true_true []))
let _ = print(string_of_term(eval inst_false_false []))
let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))




(* AdminDashboard (entity + attrs + statusAttr) *)
let admin_dashboard =
Let("table_templ", table,
ForAllName(
    "N",
ForAllRows(
    "R",
ForAllType(
    "T",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "statusAttr",
    AttributeT(VarNT "N", VarT "T"),
        Node(Container,
            Record [],
            List [
                Node(Counter,
                    Record [("Source", Var "statusAttr")],
                    List []
                );
                LetBox("inner",
                    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "table_templ", VarNT "N"), VarR "R"), Var "e"), Var "attrs"), Bool false), Var "attrs"), Bool false), Bool false),
                    Box(VarRT "inner")
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


let inst = 
Let("f", admin_dashboard,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("statusAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
     Instantiate(Instantiate(Instantiate(CallType(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), BoolT), Var "e-store"), Var "a1-store"), Var "statusAttr")
)
)
)
)
)
)
)

let rt = LetBox("inst", inst, VarRT "inst")



(* let _ = print(string_of_pairtt(typecheck admin_dashboard [] [] [] [] [])) *)
let expt = (ALet ("table_templ",
  (ALet ("filter_templ",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (ALet ("attr_templ",
      (AForAllName ("N",
        (AForAllRows ("R",
          (AForAllType ("T",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                (ALetBox ("name",
                  (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                   BoxT
                    (RecordT
                      [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                  (ALetBox ("label",
                    (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                    (AIfNode
                      ((AIsOfType
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoolT),
                        BoolT),
                      (ANode (CheckBox,
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
                       NodeT ([CheckBox],
                        RecordT [("Visible", BoxT (VarT "T"))])),
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
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
         ForAllRowsT ("R",
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
                  (ATemplate ("showPagination", BoolT,
                    (ATemplate ("allowBulk", BoolT,
                      (ANode (Container, (ARecord [], RecordT []),
                        (AList
                          [(AIfNode ((AVar "showFilter", BoolT),
                             (ALetBox ("inner",
                               (AInstantiate
                                 ((AVar "filter_templ",
                                   TemplateT (ListAttrT (VarNT "N"),
                                    NodeT ([Search],
                                     RecordT
                                      [("filterBy", ListAttrT (VarNT "N"))]))),
                                 (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                                BoxT
                                 (NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))]))),
                               (ABox
                                 (AVarRT "inner",
                                  NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))])),
                                BoxT
                                 (NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))])))),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (ANode (Empty, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Empty], RecordT []))),
                            BoxT
                             (NodeT ([Search; Empty],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                           (ALetBox ("name",
                             (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                              BoxT
                               (RecordT
                                 [("list",
                                   RecordT
                                    [("current", RecordAttrT (VarNT "N"))])])),
                             (ANode (Table,
                               (ARecord
                                 [("Source",
                                   (ABox
                                     (ASelect
                                       ((AVarRT "name",
                                         RecordT
                                          [("list",
                                            RecordT
                                             [("current",
                                               RecordAttrT (VarNT "N"))])]),
                                       (ALabel "list", LabelT "list")),
                                      RecordT
                                       [("current", RecordAttrT (VarNT "N"))]),
                                    BoxT
                                     (RecordT
                                       [("current", RecordAttrT (VarNT "N"))])))],
                                RecordT
                                 [("Source",
                                   BoxT
                                    (RecordT
                                      [("current", RecordAttrT (VarNT "N"))]))]),
                               (AList
                                 [(AIfNode ((AVar "allowBulk", BoolT),
                                    (ANode (Column,
                                      (ARecord
                                        [("Title", (AString "Select", StringT))],
                                       RecordT [("Title", StringT)]),
                                      (AList
                                        [(ANode (CheckBox,
                                           (ARecord [], RecordT []),
                                           (AList [], ListT [])),
                                          NodeT ([CheckBox], RecordT []))],
                                       ListT [NodeT ([CheckBox], RecordT [])])),
                                     NodeT ([Column],
                                      RecordT [("Title", StringT)])),
                                    (ANode (Empty, (ARecord [], RecordT []),
                                      (AList [], ListT [])),
                                     NodeT ([Empty], RecordT []))),
                                   NodeT ([Column; Empty],
                                    RecordT [("Title", StringT)]));
                                  (AForNode ("a", "aT",
                                    (AVar "attrs", ListAttrT (VarNT "N")),
                                    [(ANode (Column,
                                       (ARecord
                                         [("Title",
                                           (ASelect
                                             ((AVar "a",
                                               AttributeT (VarNT "N", Top)),
                                             (ALabel "DisplayName",
                                              LabelT "DisplayName")),
                                            StringT))],
                                        RecordT [("Title", StringT)]),
                                       (AList
                                         [(ALetBox ("inner",
                                            (AInstantiate
                                              ((AInstantiate
                                                 ((ACallType
                                                    ((ACallRows
                                                       ((ACallName
                                                          ((AVar "attr_templ",
                                                            ForAllNameT ("N",
                                                             ForAllRowsT ("R",
                                                              ForAllTypeT ("T",
                                                               TemplateT
                                                                (EntityT
                                                                  (VarNT "N",
                                                                  VarR "R"),
                                                                TemplateT
                                                                 (AttributeT
                                                                   (VarNT "N",
                                                                   VarT "T"),
                                                                 NodeT
                                                                  ([CheckBox;
                                                                    Expression],
                                                                  RecordT
                                                                   [("Visible",
                                                                    BoxT
                                                                    (VarT "T"));
                                                                    ("Value",
                                                                    BoxT
                                                                    (VarT "T"))]))))))),
                                                          VarNT "N"),
                                                         ForAllRowsT ("R",
                                                          ForAllTypeT ("T",
                                                           TemplateT
                                                            (EntityT
                                                              (VarNT "N",
                                                              VarR "R"),
                                                            TemplateT
                                                             (AttributeT
                                                               (VarNT "N",
                                                               VarT "T"),
                                                             NodeT
                                                              ([CheckBox;
                                                                Expression],
                                                              RecordT
                                                               [("Visible",
                                                                 BoxT
                                                                  (VarT "T"));
                                                                ("Value",
                                                                 BoxT
                                                                  (VarT "T"))])))))),
                                                       VarR "R"),
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))]))))),
                                                    Top),
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       Top),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible", BoxT Top);
                                                        ("Value", BoxT Top)])))),
                                                 (AVar "e",
                                                  EntityT (VarNT "N", VarR "R"))),
                                                TemplateT
                                                 (AttributeT (VarNT "N", Top),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible", BoxT Top);
                                                    ("Value", BoxT Top)]))),
                                              (AVar "a",
                                               AttributeT (VarNT "N", Top))),
                                             BoxT
                                              (NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)]))),
                                            (ABox
                                              (AVarRT "inner",
                                               NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])),
                                             BoxT
                                              (NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])))),
                                           BoxT
                                            (NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))],
                                        ListT
                                         [BoxT
                                           (NodeT ([CheckBox; Expression],
                                             RecordT
                                              [("Visible", BoxT Top);
                                               ("Value", BoxT Top)]))])),
                                      NodeT ([Column],
                                       RecordT [("Title", StringT)]))]),
                                   ListT
                                    [NodeT ([Column],
                                      RecordT [("Title", StringT)])])],
                                ListT
                                 [NodeT ([Column; Empty],
                                   RecordT [("Title", StringT)]);
                                  ListT
                                   [NodeT ([Column],
                                     RecordT [("Title", StringT)])]])),
                              NodeT ([Table],
                               RecordT
                                [("Source",
                                  BoxT
                                   (RecordT
                                     [("current", RecordAttrT (VarNT "N"))]))]))),
                            NodeT ([Table],
                             RecordT
                              [("Source",
                                BoxT
                                 (RecordT
                                   [("current", RecordAttrT (VarNT "N"))]))]));
                           (AIfNode ((AVar "showPagination", BoolT),
                             (ABox
                               (ANode (Pagination, (ARecord [], RecordT []),
                                 (AList [], ListT [])),
                                NodeT ([Pagination], RecordT [])),
                              BoxT (NodeT ([Pagination], RecordT []))),
                             (ANode (Empty, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Empty], RecordT []))),
                            BoxT (NodeT ([Pagination; Empty], RecordT [])))],
                         ListT
                          [BoxT
                            (NodeT ([Search; Empty],
                              RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                           NodeT ([Table],
                            RecordT
                             [("Source",
                               BoxT
                                (RecordT [("current", RecordAttrT (VarNT "N"))]))]);
                           BoxT (NodeT ([Pagination; Empty], RecordT []))])),
                       NodeT ([Container], RecordT []))),
                     TemplateT (BoolT, NodeT ([Container], RecordT [])))),
                   TemplateT (BoolT,
                    TemplateT (BoolT, NodeT ([Container], RecordT []))))),
                 TemplateT (ListAttrT (VarNT "N"),
                  TemplateT (BoolT,
                   TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
               TemplateT (BoolT,
                TemplateT (ListAttrT (VarNT "N"),
                 TemplateT (BoolT,
                  TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT,
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT,
                TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT,
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
  (AForAllName ("N",
    (AForAllRows ("R",
      (AForAllType ("T",
        (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
          (ATemplate ("attrs", ListAttrT (VarNT "N"),
            (ATemplate ("statusAttr", AttributeT (VarNT "N", VarT "T"),
              (ANode (Container, (ARecord [], RecordT []),
                (AList
                  [(ANode (Counter,
                     (ARecord
                       [("Source",
                         (AVar "statusAttr", AttributeT (VarNT "N", VarT "T")))],
                      RecordT [("Source", AttributeT (VarNT "N", VarT "T"))]),
                     (AList [], ListT [])),
                    NodeT ([Counter],
                     RecordT [("Source", AttributeT (VarNT "N", VarT "T"))]));
                   (ALetBox ("inner",
                     (AInstantiate
                       ((AInstantiate
                          ((AInstantiate
                             ((AInstantiate
                                ((AInstantiate
                                   ((AInstantiate
                                      ((ACallRows
                                         ((ACallName
                                            ((AVar "table_templ",
                                              ForAllNameT ("N",
                                               ForAllRowsT ("R",
                                                TemplateT
                                                 (EntityT (VarNT "N", VarR "R"),
                                                 TemplateT
                                                  (ListAttrT (VarNT "N"),
                                                  TemplateT (BoolT,
                                                   TemplateT
                                                    (ListAttrT (VarNT "N"),
                                                    TemplateT (BoolT,
                                                     TemplateT (BoolT,
                                                      NodeT ([Container],
                                                       RecordT [])))))))))),
                                            VarNT "N"),
                                           ForAllRowsT ("R",
                                            TemplateT
                                             (EntityT (VarNT "N", VarR "R"),
                                             TemplateT (ListAttrT (VarNT "N"),
                                              TemplateT (BoolT,
                                               TemplateT
                                                (ListAttrT (VarNT "N"),
                                                TemplateT (BoolT,
                                                 TemplateT (BoolT,
                                                  NodeT ([Container],
                                                   RecordT []))))))))),
                                         VarR "R"),
                                        TemplateT
                                         (EntityT (VarNT "N", VarR "R"),
                                         TemplateT (ListAttrT (VarNT "N"),
                                          TemplateT (BoolT,
                                           TemplateT (ListAttrT (VarNT "N"),
                                            TemplateT (BoolT,
                                             TemplateT (BoolT,
                                              NodeT ([Container], RecordT [])))))))),
                                      (AVar "e", EntityT (VarNT "N", VarR "R"))),
                                     TemplateT (ListAttrT (VarNT "N"),
                                      TemplateT (BoolT,
                                       TemplateT (ListAttrT (VarNT "N"),
                                        TemplateT (BoolT,
                                         TemplateT (BoolT,
                                          NodeT ([Container], RecordT []))))))),
                                   (AVar "attrs", ListAttrT (VarNT "N"))),
                                  TemplateT (BoolT,
                                   TemplateT (ListAttrT (VarNT "N"),
                                    TemplateT (BoolT,
                                     TemplateT (BoolT,
                                      NodeT ([Container], RecordT [])))))),
                                (ABool false, BoolT)),
                               TemplateT (ListAttrT (VarNT "N"),
                                TemplateT (BoolT,
                                 TemplateT (BoolT,
                                  NodeT ([Container], RecordT []))))),
                             (AVar "attrs", ListAttrT (VarNT "N"))),
                            TemplateT (BoolT,
                             TemplateT (BoolT, NodeT ([Container], RecordT [])))),
                          (ABool false, BoolT)),
                         TemplateT (BoolT, NodeT ([Container], RecordT []))),
                       (ABool false, BoolT)),
                      BoxT (NodeT ([Container], RecordT []))),
                     (ABox (AVarRT "inner", NodeT ([Container], RecordT [])),
                      BoxT (NodeT ([Container], RecordT [])))),
                    BoxT (NodeT ([Container], RecordT [])))],
                 ListT
                  [NodeT ([Counter],
                    RecordT [("Source", AttributeT (VarNT "N", VarT "T"))]);
                   BoxT (NodeT ([Container], RecordT []))])),
               NodeT ([Container], RecordT []))),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([Container], RecordT [])))),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([Container], RecordT []))))),
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([Container], RecordT [])))))),
       ForAllTypeT ("T",
        TemplateT (EntityT (VarNT "N", VarR "R"),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (AttributeT (VarNT "N", VarT "T"),
           NodeT ([Container], RecordT []))))))),
     ForAllRowsT ("R",
      ForAllTypeT ("T",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (AttributeT (VarNT "N", VarT "T"),
          NodeT ([Container], RecordT [])))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     ForAllTypeT ("T",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (AttributeT (VarNT "N", VarT "T"),
         NodeT ([Container], RecordT []))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   ForAllTypeT ("T",
    TemplateT (EntityT (VarNT "N", VarR "R"),
     TemplateT (ListAttrT (VarNT "N"),
      TemplateT (AttributeT (VarNT "N", VarT "T"),
       NodeT ([Container], RecordT []))))))))


let _ = assert(typecheck admin_dashboard [] [] [] [] [] = expt)

let _ = print(string_of_term(eval inst []))
let _ = print(string_of_term(eval rt []))





(* List With Filters (entity + attrs + showFilters + attrsInFilter + showPagination) *)
let list_with_filters =
Let("table_templ", table,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "showFilter",
    BoolT,
Template(
    "attrsInFilter",
    ListAttrT(VarNT "N"),
Template(
    "showPagination",
    BoolT,
        Node(Container,
            Record [],
            List [
                LetBox("inner",
                    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "table_templ", VarNT "N"), VarR "R"), Var "e"), Var "attrs"), Var "showFilter"), Var "attrsInFilter"), Var "showPagination"), Bool false),
                    Box(VarRT "inner")
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
)

let inst_alltrue = 
Let("f", list_with_filters,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("statusAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Bool true), Var "a2-store"), Bool true)
)
)
)
)
)
)
)

let inst_allfalse = 
Let("f", list_with_filters,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("statusAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Bool false), Var "a2-store"), Bool false)
)
)
)
)
)
)
)

let rt_t = LetBox("inst", inst_alltrue, VarRT "inst")
let rt_f = LetBox("inst", inst_allfalse, VarRT "inst")





(* let _ = print(string_of_pairtt(typecheck list_with_filters [] [] [] [] [])) *)
let expt = (ALet ("table_templ",
  (ALet ("filter_templ",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (ALet ("attr_templ",
      (AForAllName ("N",
        (AForAllRows ("R",
          (AForAllType ("T",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                (ALetBox ("name",
                  (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                   BoxT
                    (RecordT
                      [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                  (ALetBox ("label",
                    (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                    (AIfNode
                      ((AIsOfType
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoolT),
                        BoolT),
                      (ANode (CheckBox,
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
                       NodeT ([CheckBox],
                        RecordT [("Visible", BoxT (VarT "T"))])),
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
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
         ForAllRowsT ("R",
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
                  (ATemplate ("showPagination", BoolT,
                    (ATemplate ("allowBulk", BoolT,
                      (ANode (Container, (ARecord [], RecordT []),
                        (AList
                          [(AIfNode ((AVar "showFilter", BoolT),
                             (ALetBox ("inner",
                               (AInstantiate
                                 ((AVar "filter_templ",
                                   TemplateT (ListAttrT (VarNT "N"),
                                    NodeT ([Search],
                                     RecordT
                                      [("filterBy", ListAttrT (VarNT "N"))]))),
                                 (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                                BoxT
                                 (NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))]))),
                               (ABox
                                 (AVarRT "inner",
                                  NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))])),
                                BoxT
                                 (NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))])))),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (ANode (Empty, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Empty], RecordT []))),
                            BoxT
                             (NodeT ([Search; Empty],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                           (ALetBox ("name",
                             (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                              BoxT
                               (RecordT
                                 [("list",
                                   RecordT
                                    [("current", RecordAttrT (VarNT "N"))])])),
                             (ANode (Table,
                               (ARecord
                                 [("Source",
                                   (ABox
                                     (ASelect
                                       ((AVarRT "name",
                                         RecordT
                                          [("list",
                                            RecordT
                                             [("current",
                                               RecordAttrT (VarNT "N"))])]),
                                       (ALabel "list", LabelT "list")),
                                      RecordT
                                       [("current", RecordAttrT (VarNT "N"))]),
                                    BoxT
                                     (RecordT
                                       [("current", RecordAttrT (VarNT "N"))])))],
                                RecordT
                                 [("Source",
                                   BoxT
                                    (RecordT
                                      [("current", RecordAttrT (VarNT "N"))]))]),
                               (AList
                                 [(AIfNode ((AVar "allowBulk", BoolT),
                                    (ANode (Column,
                                      (ARecord
                                        [("Title", (AString "Select", StringT))],
                                       RecordT [("Title", StringT)]),
                                      (AList
                                        [(ANode (CheckBox,
                                           (ARecord [], RecordT []),
                                           (AList [], ListT [])),
                                          NodeT ([CheckBox], RecordT []))],
                                       ListT [NodeT ([CheckBox], RecordT [])])),
                                     NodeT ([Column],
                                      RecordT [("Title", StringT)])),
                                    (ANode (Empty, (ARecord [], RecordT []),
                                      (AList [], ListT [])),
                                     NodeT ([Empty], RecordT []))),
                                   NodeT ([Column; Empty],
                                    RecordT [("Title", StringT)]));
                                  (AForNode ("a", "aT",
                                    (AVar "attrs", ListAttrT (VarNT "N")),
                                    [(ANode (Column,
                                       (ARecord
                                         [("Title",
                                           (ASelect
                                             ((AVar "a",
                                               AttributeT (VarNT "N", Top)),
                                             (ALabel "DisplayName",
                                              LabelT "DisplayName")),
                                            StringT))],
                                        RecordT [("Title", StringT)]),
                                       (AList
                                         [(ALetBox ("inner",
                                            (AInstantiate
                                              ((AInstantiate
                                                 ((ACallType
                                                    ((ACallRows
                                                       ((ACallName
                                                          ((AVar "attr_templ",
                                                            ForAllNameT ("N",
                                                             ForAllRowsT ("R",
                                                              ForAllTypeT ("T",
                                                               TemplateT
                                                                (EntityT
                                                                  (VarNT "N",
                                                                  VarR "R"),
                                                                TemplateT
                                                                 (AttributeT
                                                                   (VarNT "N",
                                                                   VarT "T"),
                                                                 NodeT
                                                                  ([CheckBox;
                                                                    Expression],
                                                                  RecordT
                                                                   [("Visible",
                                                                    BoxT
                                                                    (VarT "T"));
                                                                    ("Value",
                                                                    BoxT
                                                                    (VarT "T"))]))))))),
                                                          VarNT "N"),
                                                         ForAllRowsT ("R",
                                                          ForAllTypeT ("T",
                                                           TemplateT
                                                            (EntityT
                                                              (VarNT "N",
                                                              VarR "R"),
                                                            TemplateT
                                                             (AttributeT
                                                               (VarNT "N",
                                                               VarT "T"),
                                                             NodeT
                                                              ([CheckBox;
                                                                Expression],
                                                              RecordT
                                                               [("Visible",
                                                                 BoxT
                                                                  (VarT "T"));
                                                                ("Value",
                                                                 BoxT
                                                                  (VarT "T"))])))))),
                                                       VarR "R"),
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))]))))),
                                                    Top),
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       Top),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible", BoxT Top);
                                                        ("Value", BoxT Top)])))),
                                                 (AVar "e",
                                                  EntityT (VarNT "N", VarR "R"))),
                                                TemplateT
                                                 (AttributeT (VarNT "N", Top),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible", BoxT Top);
                                                    ("Value", BoxT Top)]))),
                                              (AVar "a",
                                               AttributeT (VarNT "N", Top))),
                                             BoxT
                                              (NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)]))),
                                            (ABox
                                              (AVarRT "inner",
                                               NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])),
                                             BoxT
                                              (NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])))),
                                           BoxT
                                            (NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))],
                                        ListT
                                         [BoxT
                                           (NodeT ([CheckBox; Expression],
                                             RecordT
                                              [("Visible", BoxT Top);
                                               ("Value", BoxT Top)]))])),
                                      NodeT ([Column],
                                       RecordT [("Title", StringT)]))]),
                                   ListT
                                    [NodeT ([Column],
                                      RecordT [("Title", StringT)])])],
                                ListT
                                 [NodeT ([Column; Empty],
                                   RecordT [("Title", StringT)]);
                                  ListT
                                   [NodeT ([Column],
                                     RecordT [("Title", StringT)])]])),
                              NodeT ([Table],
                               RecordT
                                [("Source",
                                  BoxT
                                   (RecordT
                                     [("current", RecordAttrT (VarNT "N"))]))]))),
                            NodeT ([Table],
                             RecordT
                              [("Source",
                                BoxT
                                 (RecordT
                                   [("current", RecordAttrT (VarNT "N"))]))]));
                           (AIfNode ((AVar "showPagination", BoolT),
                             (ABox
                               (ANode (Pagination, (ARecord [], RecordT []),
                                 (AList [], ListT [])),
                                NodeT ([Pagination], RecordT [])),
                              BoxT (NodeT ([Pagination], RecordT []))),
                             (ANode (Empty, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Empty], RecordT []))),
                            BoxT (NodeT ([Pagination; Empty], RecordT [])))],
                         ListT
                          [BoxT
                            (NodeT ([Search; Empty],
                              RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                           NodeT ([Table],
                            RecordT
                             [("Source",
                               BoxT
                                (RecordT [("current", RecordAttrT (VarNT "N"))]))]);
                           BoxT (NodeT ([Pagination; Empty], RecordT []))])),
                       NodeT ([Container], RecordT []))),
                     TemplateT (BoolT, NodeT ([Container], RecordT [])))),
                   TemplateT (BoolT,
                    TemplateT (BoolT, NodeT ([Container], RecordT []))))),
                 TemplateT (ListAttrT (VarNT "N"),
                  TemplateT (BoolT,
                   TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
               TemplateT (BoolT,
                TemplateT (ListAttrT (VarNT "N"),
                 TemplateT (BoolT,
                  TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT,
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT,
                TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT,
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
  (AForAllName ("N",
    (AForAllRows ("R",
      (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
        (ATemplate ("attrs", ListAttrT (VarNT "N"),
          (ATemplate ("showFilter", BoolT,
            (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
              (ATemplate ("showPagination", BoolT,
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(ALetBox ("inner",
                       (AInstantiate
                         ((AInstantiate
                            ((AInstantiate
                               ((AInstantiate
                                  ((AInstantiate
                                     ((AInstantiate
                                        ((ACallRows
                                           ((ACallName
                                              ((AVar "table_templ",
                                                ForAllNameT ("N",
                                                 ForAllRowsT ("R",
                                                  TemplateT
                                                   (EntityT (VarNT "N",
                                                     VarR "R"),
                                                   TemplateT
                                                    (ListAttrT (VarNT "N"),
                                                    TemplateT (BoolT,
                                                     TemplateT
                                                      (ListAttrT (VarNT "N"),
                                                      TemplateT (BoolT,
                                                       TemplateT (BoolT,
                                                        NodeT ([Container],
                                                         RecordT [])))))))))),
                                              VarNT "N"),
                                             ForAllRowsT ("R",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (ListAttrT (VarNT "N"),
                                                TemplateT (BoolT,
                                                 TemplateT
                                                  (ListAttrT (VarNT "N"),
                                                  TemplateT (BoolT,
                                                   TemplateT (BoolT,
                                                    NodeT ([Container],
                                                     RecordT []))))))))),
                                           VarR "R"),
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT (ListAttrT (VarNT "N"),
                                            TemplateT (BoolT,
                                             TemplateT (ListAttrT (VarNT "N"),
                                              TemplateT (BoolT,
                                               TemplateT (BoolT,
                                                NodeT ([Container], RecordT [])))))))),
                                        (AVar "e",
                                         EntityT (VarNT "N", VarR "R"))),
                                       TemplateT (ListAttrT (VarNT "N"),
                                        TemplateT (BoolT,
                                         TemplateT (ListAttrT (VarNT "N"),
                                          TemplateT (BoolT,
                                           TemplateT (BoolT,
                                            NodeT ([Container], RecordT []))))))),
                                     (AVar "attrs", ListAttrT (VarNT "N"))),
                                    TemplateT (BoolT,
                                     TemplateT (ListAttrT (VarNT "N"),
                                      TemplateT (BoolT,
                                       TemplateT (BoolT,
                                        NodeT ([Container], RecordT [])))))),
                                  (AVar "showFilter", BoolT)),
                                 TemplateT (ListAttrT (VarNT "N"),
                                  TemplateT (BoolT,
                                   TemplateT (BoolT,
                                    NodeT ([Container], RecordT []))))),
                               (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                              TemplateT (BoolT,
                               TemplateT (BoolT,
                                NodeT ([Container], RecordT [])))),
                            (AVar "showPagination", BoolT)),
                           TemplateT (BoolT, NodeT ([Container], RecordT []))),
                         (ABool false, BoolT)),
                        BoxT (NodeT ([Container], RecordT []))),
                       (ABox (AVarRT "inner", NodeT ([Container], RecordT [])),
                        BoxT (NodeT ([Container], RecordT [])))),
                      BoxT (NodeT ([Container], RecordT [])))],
                   ListT [BoxT (NodeT ([Container], RecordT []))])),
                 NodeT ([Container], RecordT []))),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT,
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllRowsT ("R",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (BoolT,
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (BoolT,
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))

       
let _ = assert(typecheck list_with_filters [] [] [] [] [] = expt)

let _ = print(string_of_term(eval inst_alltrue []))
let _ = print(string_of_term(eval inst_allfalse []))
let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))




(* Bulk Actions With Filters (entity + attrs + showFilter + attrsInFilter + showPagination) *)
let bulk_actions = 
Let("table_templ", table,
ForAllName(
    "N",
ForAllRows(
    "R",
Template(
    "e",
    EntityT(VarNT "N", VarR "R"),
Template(
    "attrs",
    ListAttrT(VarNT "N"),
Template(
    "showFilter",
    BoolT,
Template(
    "attrsInFilter",
    ListAttrT(VarNT "N"),
Template(
    "showPagination",
    BoolT,
        Node(Container,
            Record [],
            List [
                LetBox("inner",
                    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "table_templ", VarNT "N"), VarR "R"), Var "e"), Var "attrs"), Var "showFilter"), Var "attrsInFilter"), Var "showPagination"), Bool true),
                    Box(VarRT "inner")
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
)


let inst_alltrue = 
Let("f", bulk_actions,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("statusAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Bool true), Var "a2-store"), Bool true)
)
)
)
)
)
)
)

let inst_allfalse = 
Let("f", bulk_actions,
Let("e-prod", product_ent,
Let("e-store", store_ent,
Let("a-prod", product_a,
Let("a1-store", store_a1,
Let("a2-store", store_a2,
Let("statusAttr", Attribute("Store", "IsInStock", BoolT, Record [("DisplayName", String "Is In Stock")]),
    Instantiate(Instantiate(Instantiate(Instantiate(Instantiate(CallRows(CallName(Var "f", NameT "Store"), store_rec_t), Var "e-store"), Var "a1-store"), Bool false), Var "a2-store"), Bool false)
)
)
)
)
)
)
)

let rt_t = LetBox("inst", inst_alltrue, VarRT "inst")
let rt_f = LetBox("inst", inst_allfalse, VarRT "inst")





(* let _ = print(string_of_pairtt(typecheck bulk_actions [] [] [] [] [])) *)

let expt = (ALet ("table_templ",
  (ALet ("filter_templ",
    (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
      (ANode (Search,
        (ARecord [("filterBy", (AVar "attrsInFilter", ListAttrT (VarNT "N")))],
         RecordT [("filterBy", ListAttrT (VarNT "N"))]),
        (AList [], ListT [])),
       NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
     TemplateT (ListAttrT (VarNT "N"),
      NodeT ([Search], RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
    (ALet ("attr_templ",
      (AForAllName ("N",
        (AForAllRows ("R",
          (AForAllType ("T",
            (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
              (ATemplate ("attr", AttributeT (VarNT "N", VarT "T"),
                (ALetBox ("name",
                  (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                   BoxT
                    (RecordT
                      [("list", RecordT [("current", RecordAttrT (VarNT "N"))])])),
                  (ALetBox ("label",
                    (ALabelOf (AVar "attr", AttributeT (VarNT "N", VarT "T")),
                     BoxT (LabelAttrT (VarNT "N", VarT "T"))),
                    (AIfNode
                      ((AIsOfType
                         ((AVar "attr", AttributeT (VarNT "N", VarT "T")),
                         BoolT),
                        BoolT),
                      (ANode (CheckBox,
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
                       NodeT ([CheckBox],
                        RecordT [("Visible", BoxT (VarT "T"))])),
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
                     NodeT ([CheckBox; Expression],
                      RecordT
                       [("Visible", BoxT (VarT "T"));
                        ("Value", BoxT (VarT "T"))]))),
                   NodeT ([CheckBox; Expression],
                    RecordT
                     [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
                 NodeT ([CheckBox; Expression],
                  RecordT
                   [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))),
               TemplateT (AttributeT (VarNT "N", VarT "T"),
                NodeT ([CheckBox; Expression],
                 RecordT
                  [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))),
             TemplateT (EntityT (VarNT "N", VarR "R"),
              TemplateT (AttributeT (VarNT "N", VarT "T"),
               NodeT ([CheckBox; Expression],
                RecordT
                 [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))),
           ForAllTypeT ("T",
            TemplateT (EntityT (VarNT "N", VarR "R"),
             TemplateT (AttributeT (VarNT "N", VarT "T"),
              NodeT ([CheckBox; Expression],
               RecordT
                [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))])))))),
         ForAllRowsT ("R",
          ForAllTypeT ("T",
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (AttributeT (VarNT "N", VarT "T"),
             NodeT ([CheckBox; Expression],
              RecordT
               [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         ForAllTypeT ("T",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (AttributeT (VarNT "N", VarT "T"),
            NodeT ([CheckBox; Expression],
             RecordT [("Visible", BoxT (VarT "T")); ("Value", BoxT (VarT "T"))]))))))),
      (AForAllName ("N",
        (AForAllRows ("R",
          (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
            (ATemplate ("attrs", ListAttrT (VarNT "N"),
              (ATemplate ("showFilter", BoolT,
                (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
                  (ATemplate ("showPagination", BoolT,
                    (ATemplate ("allowBulk", BoolT,
                      (ANode (Container, (ARecord [], RecordT []),
                        (AList
                          [(AIfNode ((AVar "showFilter", BoolT),
                             (ALetBox ("inner",
                               (AInstantiate
                                 ((AVar "filter_templ",
                                   TemplateT (ListAttrT (VarNT "N"),
                                    NodeT ([Search],
                                     RecordT
                                      [("filterBy", ListAttrT (VarNT "N"))]))),
                                 (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                                BoxT
                                 (NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))]))),
                               (ABox
                                 (AVarRT "inner",
                                  NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))])),
                                BoxT
                                 (NodeT ([Search],
                                   RecordT
                                    [("filterBy", ListAttrT (VarNT "N"))])))),
                              BoxT
                               (NodeT ([Search],
                                 RecordT [("filterBy", ListAttrT (VarNT "N"))]))),
                             (ANode (Empty, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Empty], RecordT []))),
                            BoxT
                             (NodeT ([Search; Empty],
                               RecordT [("filterBy", ListAttrT (VarNT "N"))])));
                           (ALetBox ("name",
                             (ANameOf (AVar "e", EntityT (VarNT "N", VarR "R")),
                              BoxT
                               (RecordT
                                 [("list",
                                   RecordT
                                    [("current", RecordAttrT (VarNT "N"))])])),
                             (ANode (Table,
                               (ARecord
                                 [("Source",
                                   (ABox
                                     (ASelect
                                       ((AVarRT "name",
                                         RecordT
                                          [("list",
                                            RecordT
                                             [("current",
                                               RecordAttrT (VarNT "N"))])]),
                                       (ALabel "list", LabelT "list")),
                                      RecordT
                                       [("current", RecordAttrT (VarNT "N"))]),
                                    BoxT
                                     (RecordT
                                       [("current", RecordAttrT (VarNT "N"))])))],
                                RecordT
                                 [("Source",
                                   BoxT
                                    (RecordT
                                      [("current", RecordAttrT (VarNT "N"))]))]),
                               (AList
                                 [(AIfNode ((AVar "allowBulk", BoolT),
                                    (ANode (Column,
                                      (ARecord
                                        [("Title", (AString "Select", StringT))],
                                       RecordT [("Title", StringT)]),
                                      (AList
                                        [(ANode (CheckBox,
                                           (ARecord [], RecordT []),
                                           (AList [], ListT [])),
                                          NodeT ([CheckBox], RecordT []))],
                                       ListT [NodeT ([CheckBox], RecordT [])])),
                                     NodeT ([Column],
                                      RecordT [("Title", StringT)])),
                                    (ANode (Empty, (ARecord [], RecordT []),
                                      (AList [], ListT [])),
                                     NodeT ([Empty], RecordT []))),
                                   NodeT ([Column; Empty],
                                    RecordT [("Title", StringT)]));
                                  (AForNode ("a", "aT",
                                    (AVar "attrs", ListAttrT (VarNT "N")),
                                    [(ANode (Column,
                                       (ARecord
                                         [("Title",
                                           (ASelect
                                             ((AVar "a",
                                               AttributeT (VarNT "N", Top)),
                                             (ALabel "DisplayName",
                                              LabelT "DisplayName")),
                                            StringT))],
                                        RecordT [("Title", StringT)]),
                                       (AList
                                         [(ALetBox ("inner",
                                            (AInstantiate
                                              ((AInstantiate
                                                 ((ACallType
                                                    ((ACallRows
                                                       ((ACallName
                                                          ((AVar "attr_templ",
                                                            ForAllNameT ("N",
                                                             ForAllRowsT ("R",
                                                              ForAllTypeT ("T",
                                                               TemplateT
                                                                (EntityT
                                                                  (VarNT "N",
                                                                  VarR "R"),
                                                                TemplateT
                                                                 (AttributeT
                                                                   (VarNT "N",
                                                                   VarT "T"),
                                                                 NodeT
                                                                  ([CheckBox;
                                                                    Expression],
                                                                  RecordT
                                                                   [("Visible",
                                                                    BoxT
                                                                    (VarT "T"));
                                                                    ("Value",
                                                                    BoxT
                                                                    (VarT "T"))]))))))),
                                                          VarNT "N"),
                                                         ForAllRowsT ("R",
                                                          ForAllTypeT ("T",
                                                           TemplateT
                                                            (EntityT
                                                              (VarNT "N",
                                                              VarR "R"),
                                                            TemplateT
                                                             (AttributeT
                                                               (VarNT "N",
                                                               VarT "T"),
                                                             NodeT
                                                              ([CheckBox;
                                                                Expression],
                                                              RecordT
                                                               [("Visible",
                                                                 BoxT
                                                                  (VarT "T"));
                                                                ("Value",
                                                                 BoxT
                                                                  (VarT "T"))])))))),
                                                       VarR "R"),
                                                      ForAllTypeT ("T",
                                                       TemplateT
                                                        (EntityT (VarNT "N",
                                                          VarR "R"),
                                                        TemplateT
                                                         (AttributeT
                                                           (VarNT "N",
                                                           VarT "T"),
                                                         NodeT
                                                          ([CheckBox;
                                                            Expression],
                                                          RecordT
                                                           [("Visible",
                                                             BoxT (VarT "T"));
                                                            ("Value",
                                                             BoxT (VarT "T"))]))))),
                                                    Top),
                                                   TemplateT
                                                    (EntityT (VarNT "N",
                                                      VarR "R"),
                                                    TemplateT
                                                     (AttributeT (VarNT "N",
                                                       Top),
                                                     NodeT
                                                      ([CheckBox; Expression],
                                                      RecordT
                                                       [("Visible", BoxT Top);
                                                        ("Value", BoxT Top)])))),
                                                 (AVar "e",
                                                  EntityT (VarNT "N", VarR "R"))),
                                                TemplateT
                                                 (AttributeT (VarNT "N", Top),
                                                 NodeT ([CheckBox; Expression],
                                                  RecordT
                                                   [("Visible", BoxT Top);
                                                    ("Value", BoxT Top)]))),
                                              (AVar "a",
                                               AttributeT (VarNT "N", Top))),
                                             BoxT
                                              (NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)]))),
                                            (ABox
                                              (AVarRT "inner",
                                               NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])),
                                             BoxT
                                              (NodeT ([CheckBox; Expression],
                                                RecordT
                                                 [("Visible", BoxT Top);
                                                  ("Value", BoxT Top)])))),
                                           BoxT
                                            (NodeT ([CheckBox; Expression],
                                              RecordT
                                               [("Visible", BoxT Top);
                                                ("Value", BoxT Top)])))],
                                        ListT
                                         [BoxT
                                           (NodeT ([CheckBox; Expression],
                                             RecordT
                                              [("Visible", BoxT Top);
                                               ("Value", BoxT Top)]))])),
                                      NodeT ([Column],
                                       RecordT [("Title", StringT)]))]),
                                   ListT
                                    [NodeT ([Column],
                                      RecordT [("Title", StringT)])])],
                                ListT
                                 [NodeT ([Column; Empty],
                                   RecordT [("Title", StringT)]);
                                  ListT
                                   [NodeT ([Column],
                                     RecordT [("Title", StringT)])]])),
                              NodeT ([Table],
                               RecordT
                                [("Source",
                                  BoxT
                                   (RecordT
                                     [("current", RecordAttrT (VarNT "N"))]))]))),
                            NodeT ([Table],
                             RecordT
                              [("Source",
                                BoxT
                                 (RecordT
                                   [("current", RecordAttrT (VarNT "N"))]))]));
                           (AIfNode ((AVar "showPagination", BoolT),
                             (ABox
                               (ANode (Pagination, (ARecord [], RecordT []),
                                 (AList [], ListT [])),
                                NodeT ([Pagination], RecordT [])),
                              BoxT (NodeT ([Pagination], RecordT []))),
                             (ANode (Empty, (ARecord [], RecordT []),
                               (AList [], ListT [])),
                              NodeT ([Empty], RecordT []))),
                            BoxT (NodeT ([Pagination; Empty], RecordT [])))],
                         ListT
                          [BoxT
                            (NodeT ([Search; Empty],
                              RecordT [("filterBy", ListAttrT (VarNT "N"))]));
                           NodeT ([Table],
                            RecordT
                             [("Source",
                               BoxT
                                (RecordT [("current", RecordAttrT (VarNT "N"))]))]);
                           BoxT (NodeT ([Pagination; Empty], RecordT []))])),
                       NodeT ([Container], RecordT []))),
                     TemplateT (BoolT, NodeT ([Container], RecordT [])))),
                   TemplateT (BoolT,
                    TemplateT (BoolT, NodeT ([Container], RecordT []))))),
                 TemplateT (ListAttrT (VarNT "N"),
                  TemplateT (BoolT,
                   TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
               TemplateT (BoolT,
                TemplateT (ListAttrT (VarNT "N"),
                 TemplateT (BoolT,
                  TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (ListAttrT (VarNT "N"),
                TemplateT (BoolT,
                 TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
           TemplateT (EntityT (VarNT "N", VarR "R"),
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (ListAttrT (VarNT "N"),
               TemplateT (BoolT,
                TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
         ForAllRowsT ("R",
          TemplateT (EntityT (VarNT "N", VarR "R"),
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT,
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT,
               TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
       ForAllNameT ("N",
        ForAllRowsT ("R",
         TemplateT (EntityT (VarNT "N", VarR "R"),
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT,
              TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
     ForAllNameT ("N",
      ForAllRowsT ("R",
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT,
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
  (AForAllName ("N",
    (AForAllRows ("R",
      (ATemplate ("e", EntityT (VarNT "N", VarR "R"),
        (ATemplate ("attrs", ListAttrT (VarNT "N"),
          (ATemplate ("showFilter", BoolT,
            (ATemplate ("attrsInFilter", ListAttrT (VarNT "N"),
              (ATemplate ("showPagination", BoolT,
                (ANode (Container, (ARecord [], RecordT []),
                  (AList
                    [(ALetBox ("inner",
                       (AInstantiate
                         ((AInstantiate
                            ((AInstantiate
                               ((AInstantiate
                                  ((AInstantiate
                                     ((AInstantiate
                                        ((ACallRows
                                           ((ACallName
                                              ((AVar "table_templ",
                                                ForAllNameT ("N",
                                                 ForAllRowsT ("R",
                                                  TemplateT
                                                   (EntityT (VarNT "N",
                                                     VarR "R"),
                                                   TemplateT
                                                    (ListAttrT (VarNT "N"),
                                                    TemplateT (BoolT,
                                                     TemplateT
                                                      (ListAttrT (VarNT "N"),
                                                      TemplateT (BoolT,
                                                       TemplateT (BoolT,
                                                        NodeT ([Container],
                                                         RecordT [])))))))))),
                                              VarNT "N"),
                                             ForAllRowsT ("R",
                                              TemplateT
                                               (EntityT (VarNT "N", VarR "R"),
                                               TemplateT
                                                (ListAttrT (VarNT "N"),
                                                TemplateT (BoolT,
                                                 TemplateT
                                                  (ListAttrT (VarNT "N"),
                                                  TemplateT (BoolT,
                                                   TemplateT (BoolT,
                                                    NodeT ([Container],
                                                     RecordT []))))))))),
                                           VarR "R"),
                                          TemplateT
                                           (EntityT (VarNT "N", VarR "R"),
                                           TemplateT (ListAttrT (VarNT "N"),
                                            TemplateT (BoolT,
                                             TemplateT (ListAttrT (VarNT "N"),
                                              TemplateT (BoolT,
                                               TemplateT (BoolT,
                                                NodeT ([Container], RecordT [])))))))),
                                        (AVar "e",
                                         EntityT (VarNT "N", VarR "R"))),
                                       TemplateT (ListAttrT (VarNT "N"),
                                        TemplateT (BoolT,
                                         TemplateT (ListAttrT (VarNT "N"),
                                          TemplateT (BoolT,
                                           TemplateT (BoolT,
                                            NodeT ([Container], RecordT []))))))),
                                     (AVar "attrs", ListAttrT (VarNT "N"))),
                                    TemplateT (BoolT,
                                     TemplateT (ListAttrT (VarNT "N"),
                                      TemplateT (BoolT,
                                       TemplateT (BoolT,
                                        NodeT ([Container], RecordT [])))))),
                                  (AVar "showFilter", BoolT)),
                                 TemplateT (ListAttrT (VarNT "N"),
                                  TemplateT (BoolT,
                                   TemplateT (BoolT,
                                    NodeT ([Container], RecordT []))))),
                               (AVar "attrsInFilter", ListAttrT (VarNT "N"))),
                              TemplateT (BoolT,
                               TemplateT (BoolT,
                                NodeT ([Container], RecordT [])))),
                            (AVar "showPagination", BoolT)),
                           TemplateT (BoolT, NodeT ([Container], RecordT []))),
                         (ABool true, BoolT)),
                        BoxT (NodeT ([Container], RecordT []))),
                       (ABox (AVarRT "inner", NodeT ([Container], RecordT [])),
                        BoxT (NodeT ([Container], RecordT [])))),
                      BoxT (NodeT ([Container], RecordT [])))],
                   ListT [BoxT (NodeT ([Container], RecordT []))])),
                 NodeT ([Container], RecordT []))),
               TemplateT (BoolT, NodeT ([Container], RecordT [])))),
             TemplateT (ListAttrT (VarNT "N"),
              TemplateT (BoolT, NodeT ([Container], RecordT []))))),
           TemplateT (BoolT,
            TemplateT (ListAttrT (VarNT "N"),
             TemplateT (BoolT, NodeT ([Container], RecordT [])))))),
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT,
           TemplateT (ListAttrT (VarNT "N"),
            TemplateT (BoolT, NodeT ([Container], RecordT []))))))),
       TemplateT (EntityT (VarNT "N", VarR "R"),
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT,
          TemplateT (ListAttrT (VarNT "N"),
           TemplateT (BoolT, NodeT ([Container], RecordT [])))))))),
     ForAllRowsT ("R",
      TemplateT (EntityT (VarNT "N", VarR "R"),
       TemplateT (ListAttrT (VarNT "N"),
        TemplateT (BoolT,
         TemplateT (ListAttrT (VarNT "N"),
          TemplateT (BoolT, NodeT ([Container], RecordT []))))))))),
   ForAllNameT ("N",
    ForAllRowsT ("R",
     TemplateT (EntityT (VarNT "N", VarR "R"),
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT,
        TemplateT (ListAttrT (VarNT "N"),
         TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))),
 ForAllNameT ("N",
  ForAllRowsT ("R",
   TemplateT (EntityT (VarNT "N", VarR "R"),
    TemplateT (ListAttrT (VarNT "N"),
     TemplateT (BoolT,
      TemplateT (ListAttrT (VarNT "N"),
       TemplateT (BoolT, NodeT ([Container], RecordT [])))))))))


let _ = assert(typecheck bulk_actions [] [] [] [] [] = expt)

let _ = print(string_of_term(eval inst_alltrue []))
let _ = print(string_of_term(eval inst_allfalse []))

let _ = print(string_of_term(eval rt_t []))
let _ = print(string_of_term(eval rt_f []))