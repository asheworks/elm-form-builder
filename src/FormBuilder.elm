module FormBuilder exposing
  ( keepJusts

  , Id
  , DataValue

  , DataModifiers(..)

  , MetaModifiers(..)
  , MetaMods

  , SectionZipper
  , Predicate
  , Selector
  , Sections(..)

  , Form
  , ZipperState
  , zipperState
  , sectionZipper
  , applyZipper
  
  , toId
  , section
  , field
  , applyLeafMods
  , byId
  , default
  , toTree
  , toForm
  )


import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)


--


keepJusts : List (Maybe a) -> List a
keepJusts list = 
  case list of 
    [] ->
      []
    mx :: xs ->
      case mx of
        Nothing ->
          keepJusts xs
          
        Just x ->
          x :: keepJusts xs


type alias Id = String


type alias DataValue meta type_ =
  { default : Maybe type_
  , value : Maybe type_
  , meta : meta
  }


type DataModifiers branch leaf meta type_
  = BranchMod ( branch -> branch )
  | LeafMod ( leaf -> leaf )
  | ValueMod ( DataValue meta type_ -> DataValue meta type_ )


applyLeafMods
  : ( DataValue meta type_ -> leaf )
  -> DataValue meta type_
  -> List ( DataModifiers branch leaf meta type_ )
  -> leaf
applyLeafMods leafMap value mods =
  List.foldr
    (\ mod value_ ->
        case mod of
          LeafMod map ->
            map value_
          _ -> value_
    )
    ( List.foldr
        (\ mod value_ ->
            case mod of
              ValueMod map ->
                map value_
              _ -> value_
        )
        value
        mods
        |> leafMap
    ) mods


type MetaModifiers branch leaf meta
  = MetaMod ( meta -> meta )
  | SectionMod ( SectionZipper branch leaf meta -> SectionZipper branch leaf meta )


type alias MetaMods  branch leaf meta = List ( Tree ( Sections branch leaf meta ) -> MetaModifiers branch leaf meta )


type alias SectionZipper  branch leaf meta = Zipper ( Sections branch leaf  meta )


type alias Predicate  branch leaf meta = Zipper ( Sections branch leaf  meta ) -> Bool


type alias Selector  branch leaf meta = Tree ( Sections branch leaf  meta ) -> SectionZipper branch leaf meta


type Sections branch leaf meta
  = Branch ( Maybe Id ) branch ( MetaMods branch leaf meta ) ( List ( Sections branch leaf meta ) )
  | Leaf ( Maybe Id ) leaf ( MetaMods branch leaf meta )


toId : String -> Maybe Id
toId value =
  if value == "" then
    Nothing
  else
    Just value


section
  : Id
  -> MetaMods branch leaf meta
  -> branch
  -> List ( Sections branch leaf meta )
  -> Sections branch leaf meta
section id mods container children =
  Branch ( toId id ) container mods children


field : Id -> MetaMods branch leaf meta -> leaf -> Sections branch leaf meta
field id mods data =
  Leaf ( toId id ) data mods


type alias DataDim dim type_ =
  { dim | default : Maybe type_
        , value : Maybe type_
  }


default : type_ -> DataModifiers branch leaf meta type_
default value =
  ValueMod (\ model ->
    { model | default = Just value }
  )
  

toTree : Sections branch leaf meta -> Tree ( Sections branch leaf meta )
toTree node =
  case node of
    Branch _ _ _ children ->
      Tree node <| List.map toTree children

    Leaf _ _ _ ->
      Tree node []


appendPath : String -> Maybe ( String ) -> String
appendPath path id =
  case id of
    Nothing -> path
    Just id_ ->
      if String.length path == 0 then
        id_
      else
        path ++ "." ++ id_


type alias DataNode branch leaf data meta =
  { section : Zipper ( Sections branch leaf meta )
  , path : String
  , id : String
  , data : Maybe ( data )
  }


-- toDataNode
--   : Zipper ( Sections branch leaf meta )
--   -> String
--   -> Maybe String
--   -> Maybe data
--   -> Forest (DataNode branch leaf data meta)
--   -> Tree (DataNode branch leaf data meta)
-- toDataNode zipper path id dataType =
--   let
--     id_ = Maybe.withDefault "" id

--     path_ = appendPath path id
--   in
--     Tree ( DataNode zipper path_ id_ dataType )


-- toDataTree
--   : ( meta -> ( Sections branch leaf meta ) -> Maybe data )
--   -> meta
--   -> Tree ( Sections branch leaf meta )
--   -> Tree ( DataNode branch leaf data meta )
-- toDataTree dataTypeMap meta tree =
--   let
--     applyZipper path ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--       let
--         dataType = dataTypeMap meta node
--       in
--         case node of

--           Leaf id _ _ ->
--             toDataNode zipper path id dataType []

--           Branch id _ _ _ ->
--               children
--                 |> List.indexedMap
--                     (\ index _ ->
--                         goToChild index zipper
--                           |> Maybe.map ( applyZipper ( appendPath path id ) )
--                     )
--                 |> keepJusts
--                 |> toDataNode zipper path id dataType
--   in
--     applyZipper "" ( tree, [] )


type alias Form branch leaf data meta =
  { def : Tree ( Sections branch leaf meta )
  , data : Tree ( DataNode branch leaf data meta )
  }


-- type alias ControlNode branch leaf data meta =
--   { section : Zipper ( Sections branch leaf meta )
--   , path : String
--   , id : String
--   , 
--   }

-- toControlNode
--   : Zipper ( Sections branch leaf meta )
--   -> String
--   -> Maybe String
--   -> Maybe data
--   -> Forest (DataNode branch leaf data meta)
--   -> Tree (DataNode branch leaf data meta)
-- toControlNode zipper path id control =



-- type alias RenderMeta meta =
--   { dataTypeMap : ( meta -> ( Sections branch leaf meta ) -> Maybe data )
--   , meta : meta
--   , depth : Int
--   , index : Int
--   , path : String
--   }


-- render
--   : ( meta -> ( Sections branch leaf meta ) -> Maybe data )
--   -> meta
--   -> Tree ( Sections branch leaf meta )
--   -> Tree ( DataNode branch leaf data meta )
-- render dataTypeMap meta tree =
--   renderZipper
--     { dataTypeMap = dataTypeMap
--     , meta = meta
--     , depth = 0
--     , index = 0
--     , path = ""
--     }
--     ( tree, [] )


type alias ZipperState =
  { depth : Int
  , index : Int
  , path : String
  }


zipperState : ZipperState
zipperState =
  ZipperState 0 0 ""


toDataNode
  : ( meta -> ( Sections branch leaf meta ) -> Maybe data )
  -> meta
  -> ZipperState
  -> Maybe String
  -> Zipper ( Sections branch leaf meta )
  -> Forest ( DataNode branch leaf data meta ) -- aka List ( DataNode branch leaf data meta )
  -> Tree ( DataNode branch leaf data meta )
toDataNode dataTypeMap meta state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  let
    id_ = Maybe.withDefault "" id

    path_ = appendPath state.path id

    dataType = dataTypeMap meta node
  in
    Tree ( DataNode zipper path_ id_ dataType )


applyZipper
  : ( ZipperState -> Maybe String -> Zipper ( Sections branch leaf meta ) -> List type_ -> type_ )
  -> Tree ( Sections branch leaf meta )
  -> type_
applyZipper mapper tree =
  sectionZipper zipperState mapper ( tree, [] )


sectionZipper
  : ZipperState
  -> ( ZipperState -> Maybe String -> Zipper ( Sections branch leaf meta ) -> List ( type_ ) -> type_ )
  -> Zipper ( Sections branch leaf meta )
  -> type_
sectionZipper state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  case node of
    Leaf id _ _ ->
      mapper state id zipper []
      
    Branch id _ _ _ ->
      children
        |> List.indexedMap
            (\ index _ ->
                goToChild index zipper
                  |> Maybe.map
                      ( sectionZipper
                          { state
                              | depth = state.depth + 1
                              , index = index
                              , path = appendPath state.path id
                          }
                          mapper
                      )
            )
        |> keepJusts
        |> mapper state id zipper


toForm
  : ( meta
  -> ( Sections branch leaf meta ) -> Maybe data )
  -> Sections branch leaf meta
  -> meta
  -> Form branch leaf data meta
toForm dataTypeMap node meta =
  let
    tree : Tree (Sections branch leaf meta)
    tree = toTree node

    dataTree : Tree ( DataNode branch leaf data meta )
    dataTree = applyZipper ( toDataNode dataTypeMap meta ) tree

    -- dataTree =
    --   sectionZipper
    --     zipperState
    --     ( toDataNode dataTypeMap meta )
    --     ( tree, [] )
    -- dataTree = toDataTree dataTypeMap meta tree
  in
    Form tree dataTree


byId
  : String
  -> Tree (Sections branch leaf meta)
  -> Zipper (Sections branch leaf meta)
byId key tree =
  ( tree, [] )
