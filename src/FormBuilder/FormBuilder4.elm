module Example.FormBuilder3 exposing
  (..)

type DataTypes
  = Field
  | Bool

type alias Data type_ =
  { id : String
  , default : Maybe type_
  }

type Facts
  = Fact (List Facts)

section =
  default
  
id : String -> { fact | id : Maybe String } -> fact
id id fact =
  { fact | id = Just id }

list : List Facts -> { fact | children : List Facts } -> fact
list children fact =
  { fact | children = fact.children ++ children }

type alias Model =
  { id : Maybe String

  }

default =
  { id = Nothing
  }

infosec =
  Field "infosec"

infoSection : String -> String -> String -> String -> DataTypes
infoSection key title descLabel riskLabel =
  Section None
    [ id key
    , control <| BulletList NumericBullets (Just title)
    ]
    [ Section Bool
        [ id "selected"
        , yesNo
        ]
    ]
    


    -- |> data ( Field key )
    -- |> display BulletList NumericBullets
    -- |> children



        -- [ Bool "selected"
        --     |> default False
        --     |> display YesNo

        -- , Text "description"
        --     |> display TextInput descLabel
        --     |> placeholder "None"
        
        -- , Text "risk"
        --     |> display TextInput descLabel
        --     |> 
        -- ]

    -- |> id "infosec"
    -- |> list
    -- |> (
    --   section
    --     |> text "version"
    --     |> default "v1.0.0"
    --     |> textLabel "Version"

    -- ]
    -- |> bulletList NumericBullets
    -- |> withTitle 

  

-- toDataTree :
--   ( meta -> ( Sections branch leaf meta ) -> Maybe data ) ->
--   meta ->
--   Tree ( Sections branch leaf meta ) ->
--   Tree ( DataNode branch leaf data meta )
-- toDataTree dataTypeMap meta tree =
--   MultiwayTree.foldr
--     (\ node_ ( children_ ) ->
--       case node_ of
--         Leaf id _ _ ->
--           ( children_ ++ [ ] )
--     )
--     ( Tree ( [] )
--     tree


-- pathIndex :
--   Tree ( DataNode branch leaf data meta ) ->
--   Dict String ( Zipper ( DataNode branch leaf data meta ) )
-- pathIndex tree =
--   let
--     applyZipper dict ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--       children
--         |> List.indexedMap
--             (\ index _ ->
--                 goToChild index zipper
--                   |> Maybe.map
--                       ( applyZipper dict )
--             )
--         |> keepJusts
--         |> Dict.
--         -- |> Dict.insert node.path zipper

--   in
--     applyZipper Dict.empty ( tree, [] )


-- Leaf _ _ _ ->

--   Dict.insert node.path zipper dict

-- Branch _ _ _ _ ->

--   children
--     |> List.indexedMap
--         (\ index _ ->
--             goToChild index zipper
--               |> Maybe.map
--                 ( applyZipper
--                     dict
--                     -- ( Dict.insert node.path zipper dict )
--                 )
--         )
--     |> keepJusts
--     |> dict
    
  -- MultiwayTree.foldl
  --   (\ node dict ->
  --       Dict.insert node.path tree dict
  --   )
  --   Dict.empty
  --   tree


-- type alias FormZipperCtx =
--   { data : List ( Id, Maybe ( DataTypes meta ) )
--   , view : List ( Id, Maybe ( Html Command ) )
--   }

-- type alias FormZipperResult =
--   { ctx : FormZipperCtx
--   }

-- toFormZipper : meta -> FormZipperCtx -> Zipper ( Sections meta ) -> FormZipperResult
-- toFormZipper meta ctx ( ( subtree, crumbs ) as zipper ) =
--   case subtree of
--     Tree node children ->
--       case node of
--         Section _ _ _ _ ->
--           children
--             |> List.foldr
--                 (\ child ( index, acc ) ->
--                     ( index + 1
--                     , List.append acc
--                         [ goToChild index zipper  
--                             |> Maybe.map
--                                 ( toFormZipper meta
--                                     { data = 
--                                     }
--                                 )
--                         ]
--                     )
--                 )
--         _ ->
--           let
--             data = toData node meta
--           in

-- toForm : Sections meta -> meta -> Form meta
-- toForm node meta =
--   let
--     tree = toTree node

--     applyZipper :
--       ( List ( Tree ( DataNode meta ) ) ) ->
--       ( List ( Tree ( ViewNode meta ) ) ) ->
--       Zipper ( Sections meta ) ->
--       ( Maybe ( Tree ( DataNode meta ) )
--       , List ( Tree ( DataNode meta ) )
--       , Maybe ( Tree ( ViewNode meta ) )
--       , List ( Tree ( ViewNode meta ) )
--       )
--     applyZipper dataChildren viewChildren ( ( subtree, crumbs ) as zipper ) =
--       case subtree of
--         Tree node children ->
--           case Debug.log "Form Zipper" node of
--             Section id _ _ _ ->
--               let
--                 ( _, children_ ) =
--                   children
--                   |> List.foldr
--                       (\ child  ( index, acc ) ->
--                           ( index + 1
--                           , List.append acc
--                               [ goToChild index zipper
--                                   |> Maybe.map (applyZipper [] [])
--                               ]
--                           )
--                       ) ( 0, [] )
--               in

--               case id of
--                 -- 
--                 Nothing -> ( Nothing, dataChildren, Nothing, viewChildren )
--                 Just id_ -> -- ( Nothing, dataChildren, Nothing, viewChildren )
--                   let

--                     -- children_ =
--                     --   children
--                     --     |> List.indexedMap
--                     --         (\ index _ ->
--                     --           goToChild index zipper
--                     --           |> Maybe.map (applyZipper [] [])
--                     --         )
--                     --     |> keepJusts
                  
                    
                    
--                     -- u = Debug.log "index" index
--                     -- v = Debug.log "children" children_
                    
--                     data = toData node meta
--                     data_ =
--                       case Debug.log "Section" data of
--                         Nothing -> Tree ( DataNode zipper id_ Nothing ) dataChildren
--                         Just ( id, data ) -> Tree ( DataNode zipper id_ Nothing ) dataChildren

--                     -- t = Debug.log "Data Children" ( List.length dataChildren )

--                   in
--                     ( Nothing, dataChildren, Nothing, viewChildren )

--                   -- let
--                   --   data = Tree ( DataNode )
--                   --   data =
--                   --     children
--                   --       |> List.indexedMap
--                   --           (\ index _ ->
--                   --             goToChild index zipper
--                   --             |> Maybe.map (applyZipper [] [])
--                   --           )
--                   --       |> keepJusts
--                   -- in
--                   --   ( Just data, [], Nothing, [] )

--             _ ->
--               let
--                 data_ = toData node meta

--                 view_ = toView node meta data

--               ( Nothing
--               , dataChildren ++
--                 case toData node meta of
--                   Nothing -> []
--                   Just ( id, data ) ->
--                     let
--                       t = Debug.log "Field" data
--                     in
--                       [ Tree ( DataNode zipper id data ) [] ]
--               , Nothing
--               , viewChildren
--               )

--     ( data, dataChildren, view, viewChildren ) = applyZipper [] [] ( tree, [] )

--     data_ = Maybe.withDefault ( Tree ( DataNode ( tree, [] ) "" Nothing ) [] ) data

--     view_ = Maybe.withDefault ( Tree ( ViewNode ( tree, [] ) ( div [] [] ) Nothing ) [] ) view
--   in
--     Form tree data_ view_

-- applyZipper ( tree, [] )

-- case subtree of
--   Tree datum children ->
--     ( toData datum meta
--     )

-- toData : Zipper ( Sections meta ) -> meta -> Maybe ( String, Maybe ( DataTypes meta ) )


-- , FileUploadDefaults(..)
-- , OptionDefaults(..)

-- , IsField
-- , field
-- , noField
-- , Placeholder
-- , placeholder
-- , noPlaceholder
-- , Hidden
-- , hidden
-- , visible

-- , DataNodes(..)

-- , TriStateControls(..)
-- , DataTypes(..)
-- , FieldTypes(..)

-- , DataSection
-- , withField
-- , withoutField
-- , getId


-- , asDataSection
-- , getDataChildren
-- , asDataTree
-- , Node
-- , dataTree
-- , controlTree
-- , questionnaire



-- import Dict exposing (..)


-- toData : Sections meta -> meta -> Maybe ( Id, Maybe ( DataTypes meta ) )
-- toData node meta =
--   case node of
--     Section id _ _ _ ->
--       Maybe.map (\ id_ -> (id_, Nothing) ) id

--     Bool id default _ _ ->
--       Just ( id, Just ( BoolData <| DataValue default default meta ) )

--     FileUpload id default _ _ ->
--       Just ( id, Just ( StringListData <| DataValue default default meta ) )
    
--     Options id default _ _ _ ->
--       let
--         default_ = Maybe.map Set.fromList default
--       in
--         Just ( id, Just ( OptionsData <| DataValue default_ default_ meta ) )

--     Text id default _ _ ->
--       Just ( id, Just ( TextData <| DataValue default default meta ) )

-- toView : Sections meta -> meta -> Maybe ( Id, Maybe ( Html Command ) )
-- toView node meta =
--   case node of
--     Section id container mods _ ->
--       let
--         t = Debug.log "Section" container
--       in
--         div [] [ text "Section: " ++ (Maybe.withDefault "-" id) ]

--     Bool id _ control mods ->
--       let
--         t = Debug.log "Bool" control
--       in
--         div [] [ text "Bool: " ++ id ]

--     FileUpload id _ control mods ->
--       let
--         t = Debug.log "FileUpload" control
--       in
--         div [] [ text "FileUpload: " ++ id ]

--     Options id _ options control mods ->
--       let
--         t = Debug.log "Options" control
--       in
--         div [] [ text "Options: " ++ id ]

--     Text id _ control mods ->
--       let
--         t = Debug.log "Text" control
--       in
--         div [] [ text "Text: " ++ id ]


      --  [ DataNode zipper ( toData node meta )
    


-- toFormZipper : meta -> Zipper (Sections branch leaf meta) ->  DataNode branch leaf meta
-- toFormZipper meta ( ( Tree datum children, crumbs ) as zipper ) =
--   case datum of
--     Branch id _ _ _ ->
--       let
--         ( index, children_) =
--           children
--             |> List.foldr
--               (\ child ( index, acc ) ->
--                 ( index + 1
--                 , List.append acc
--                     [ goToChild index zipper
--                         |> Maybe.map ( toFormZipper meta )
--                     ]
--                 )
--               ) ( 0, [] )

--         t = Debug.log "Children" children_

--       in
--         DataNode zipper (Maybe.withDefault "" id) Nothing

--     Leaf id _ _->
--       DataNode zipper (Maybe.withDefault "" id) ( toDataType datum meta )

-- ( ( Sections branch leaf meta ) -> meta -> Maybe ( DataTypes meta ) ) ->


-- type DataFacts meta
--   = Field ContainerControls
--   | Bool ( Maybe Bool ) BoolControls
--   | FileUpload ( Maybe ( List String ) ) FileUploadControls
--   | Options ( Maybe ( List String ) ) ( List (String, String) ) OptionControls
--   | Text ( Maybe String ) TextControls


  -- List.map toTree (

  -- )

-- toTree : Sections meta -> Tree ( Sections meta )
-- toTree node =
--   List.map toTree (
--     case node of
--       Section _ _ _ children -> children
--       _ -> []
--   ) |> Tree node



-- type Sections meta
--   = Section ( Maybe Id ) Containers ( Mods meta ) ( List (Sections meta) )
--   | Bool Id ( Maybe Bool ) BoolControls (Mods meta)
--   | FileUpload Id ( Maybe ( List String ) ) FileUploadControls ( Mods meta )
--   | Options Id ( Maybe ( List String ) ) ( List (String, String) ) OptionControls ( Mods meta )
--   | Text Id ( Maybe String ) TextControls ( Mods meta )


-- withField : Sections meta -> Maybe (Sections meta)
-- withField node =
--   case node of
--     Section id _ _ _ ->
--       Maybe.map (\ _ -> node ) id
--     _ -> Just node


-- withoutField : Sections meta -> Maybe (Sections meta)
-- withoutField node =
--   case withField node of
--     Nothing -> Just node
--     _ -> Nothing


-- getId : Sections meta -> Maybe String
-- getId node =
--   case node of
--     Section id _ _ _ -> id
--     Bool id _ _ _ -> Just  id
--     FileUpload id _ _ _ -> Just id
--     Options id _ _ _ _ -> Just id
--     Text id _ _ _ -> Just id


-- mustGetId : Sections meta -> String
-- mustGetId node =
--   Maybe.withDefault "" <| getId node


-- type alias FormNode meta =
--   { section : Zipper ( Sections meta )
--   , view : Maybe ( Zipper ( ViewNode meta ) )
--   , data : Maybe ( Zipper ( DataNode meta ) )
--   }


-- type TriStateControls
--   = YesNoMaybe


-- type BoolValidations
--   = Bool_IsTrue
--   | Bool_IsFalse


-- type OptionsValidations
--   = Options_NoneSelected


-- type TextValidations
--   = Text_Required


-- type FieldTypes
--   = NoField
--   | Field Id


-- type alias SectionMeta type_ meta =
--   { id : String
--   , default : type_
--   , value : Maybe type_
--   , meta : meta
--   }



-- type alias IsField = Bool


-- field : Bool
-- field = True


-- noField : Bool
-- noField = False


-- type alias Placeholder = Maybe String


-- placeholder : String -> Maybe String
-- placeholder value = Just value


-- noPlaceholder : Maybe String
-- noPlaceholder = Nothing


-- type alias Hidden = Bool


-- hidden : Bool
-- hidden = True


-- visible : Bool
-- visible = False

-- type alias SectionMeta type_ meta =
--   { id : String
--   , default : type_
--   , value : Maybe type_
--   , meta : meta
--   }


-- let
--   data =
--     case toData node meta of
--       Nothing -> dataChildren
--       Just ( id, value ) ->
--         case node of
--           Section _ _ _ _ ->
--             DataNode zipper id value
          
--           _ ->
--             ( dataChildren ++ node )
    -- case node of
    --   Section fieldType _ _ _ ->
    --     case fieldType of
    --       NoField -> ctx
    --       Field id -> 
      
    -- case toData zipper meta of
    --   Nothing -> ( dataTree, dataChildren ) -- Skip the data node
    --   Just dataNode ->
    --     case data.data
-- in
--   ( data
--   )

-- Section fieldType containers mods _ ->
--   ( case fieldType of
--       NoField -> Nothing
--       Field id -> Just ( id, Nothing )
--   , case containers of
--       NoContainer -> Nothing
--       BulletList 
--   )

-- Bool id default control mods ->
--   case control of
--     YesNo -> 

  -- in
  --   Form tree view data


-- type alias DataNode =
--   { def : Zipper ( Sections meta )
--   }

-- dataZipper : Zipper ( Sections meta ) -> 

-- type alias FilteredMap model = Selector model -> Predicate model -> Tree ( Sections model ) -> Modifiers model

-- filteredMap : Selector dim -> Predicate dim -> Tree ( Sections Dim ) -> dim -> dim

-- filteredMap : Selector dim -> Predicate dim -> Tree ( Sections dim ) -> ( dim -> dim ) -> Modifiers dim
-- filteredMap selector predicate tree map =
--   MetaMod (\ model ->
--     tree
--     |> selector
--     |> sectionMap
--   )

-- visible : Selector meta -> Predicate meta -> Tree (Sections meta) -> { model | visible : Bool } -> { model | visible : Bool }


-- toTree : Sections meta -> Tree (Sections meta)
-- toTree node =
--   case node of


-- asDataSection : Sections -> Maybe DataSection
-- asDataSection node =
--   case node of
--     Section _ _ _ -> Nothing
--     Section_Bool id value _ _ -> Just <| DataSection id <| Data_Bool value
--     Section_FileUpload id value _ _ -> DataSection id <| Data_StringList value
--     Section_Options id value _ _ _ -> DataSection id <| Data_Options <| Set.fromList value
--     Section_Text id value _ _ -> DataSection id <| Data_String value
--     -- Section_TriState id value _ _ -> DataSection id <| Data_TriState value


-- getDataChildren : Sections -> List Sections
-- getDataChildren node =
--   case node of
--     Section _ _ children ->
--       ( children
--           |> List.filterMap withField
--       ) ++
--       ( children
--           |> List.filterMap withoutField
--           |> List.concatMap getDataChildren
--       )

--     _ -> []


-- asDataTree : Sections -> Tree DataSection
-- asDataTree node =
--   Tree
--     ( asDataSection node
--     )
--     ( node
--         |> getDataChildren
--         |> List.map asDataTree
--     )
    

-- type alias Node =
--   { id : Maybe Id
--   , data : Zipper DataSection
--   -- , data : DataTypes
--   -- , children : List Sections
--   }

-- buildForm : Sections -> String
-- buildForm node =




-- buildForm : Sections -> Tree Sections
-- buildForm node =
--   let
--     dataTree = asDataTree node

--     zipper 
--   in
    
--   case node of
--     Section fieldType container children ->
--       case fieldType of
--         NoField ->
--           Node
--             Nothing
--             Data_None
--             []
        
--         Field id ->
--           Node
--             (Just id)
--             Data_None
--             children

--     Section_Bool id value control meta ->
--       Node
--         (Just id)

    
--     Section_FileUpload id value control meta ->
--       Node
--         (Just id)

--     Section_Options id value options control meta ->
--       Node
--         (Just id)

--     Section_Text id value control meta ->
--       Node
--         (Just id)

--     Section_TriState id value control meta ->
--       Node
--         (Just id)


  

-- asControlTree : Sections -> Html Command
-- asControlTree node =
--   Section _ container _ ->
--     case container of
--       NoContainer ->
--         div [] 

--       BulletList bulletType title ->

--       Grid ->

--       Header title ->
--         (\ children ->
--             UI.formControl
--               { id = def.id
--               , header = Just
--                 [ Html.text title
--                 ]
--               , section = Just children
--               , aside = Nothing
--               , footer = Nothing
--               }
--         )

--       LabeledSection title ->

--       List title ->
--   Tree
--     ( asControlNode node
--     )
--     ( node
--         |> getControlChildren
--         |> List.map asControlTree
--     )



-- asControlNode command : Sections -> ( List Sections -> Html command )
-- asControlNode node =
--   (\ children ->
--   case node of
--     Section _ container _ ->
--       case container of
--         NoContainer ->
--           div []

--         BulletList bulletType title ->

--         Grid ->

--         Header title ->
--           (\ children ->
--               UI.formControl
--                 { id = def.id
--                 , header = Just
--                   [ Html.text title
--                   ]
--                 , section = Just children
--                 , aside = Nothing
--                 , footer = Nothing
--                 }
--           )

--         LabeledSection title ->

--         List title ->



-- type DataTypes
--   = Property Id (List DataTypes)
--   | Bool Id Bool (List BoolValidations)
--   | Options Id (List String) (List (String, String)) (List OptionsValidations)
--   | Text Id String (List TextValidations)


-- type Nodes dimensions
--   = Node (List dimensions) (List (Nodes dimensions))

-- dataTree : Nodes dimensions -> Tree (String, Maybe DataTypes)
-- dataTree node =

-- controlTree : Nodes { control : ControlDefinitions } -> Tree ControlDefinitions
-- controlTree node =
--   case node of
--     Node dimensions children ->
--       children
--         |> List.map controlTree
--         |> Tree dimensions.control

