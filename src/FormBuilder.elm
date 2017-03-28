module FormBuilder exposing (..)

-- module FormBuilder exposing
--   ( Id
--   , DataValue

--   , BranchModifiers
--   , LeafModifiers

--   , SectionZipper
--   , Predicate
  
--   , Sections(..)

--   , DataNode

--   , Form
--   , ZipperState
--   , zipperState
--   , sectionZipper
--   , applySectionZipper
--   , dataZipper
--   , applyDataZipper

--   , section
--   , field
--   -- , byId
--   , toTree
--   , appendPath
--   , toForm


--   , getDataNodeByPathSegments
--   , getDataNodeByPath
--   , mapDataNodeByPath
--   )

-- , Selector
-- , toId

-- toId : String -> Maybe Id
-- toId value =
--   if value == "" then
--     Nothing
--   else
--     Just value

-- byId
--   : String
--   -> Tree ( Sections branch branchModel leaf leafModel model meta )
--   -> Zipper ( Sections branch branchModel leaf leafModel model meta )
-- byId key tree =
--   ( tree, [] )


import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)

--

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = flip Maybe.andThen


type alias Id = String


type alias DataValue model meta = ( model, meta )
  -- { model : model
  -- , meta : meta
  -- }


type alias Node branch branchModel leaf leafModel model meta =
  ( Sections branch branchModel leaf leafModel model meta, DataValue model meta )


-- type alias Node branch branchModel leaf leafModel model meta =
--   { data : DataValue model meta
--   , section : Sections branch branchModel leaf leafModel model meta
--   }


type alias Modifiers model meta = List ( ( DataValue model meta -> DataValue model meta ) )


-- type alias BranchModifiers model meta = List ( ( DataValue model meta -> DataValue model meta ) )

-- type alias LeafModifiers model meta = List ( ( DataValue model meta -> DataValue model meta ) )


-- type alias SectionZipper branch branchModel leaf leafModel meta =
--   Zipper ( Sections branch branchModel leaf leafModel meta )


-- type alias Predicate branch branchModel leaf leafModel meta =
--   Zipper ( Sections branch branchModel leaf leafModel meta ) -> Bool


type Sections branch branchModel leaf leafModel model meta
  = Branch Id ( Modifiers branch meta ) branch ( List ( Sections branch branchModel leaf leafModel model meta ) )
  | Leaf Id ( Modifiers leaf meta ) leaf


type alias DataValueMap branch branchModel leaf leafModel model meta =
  ( meta -> ( Sections branch branchModel leaf leafModel model meta ) -> model )

-- section
--   : Id
--   -> branch
--   -> List ( Sections branch branchModel leaf leafModel model meta )
--   -> List ( Modifier branch meta )
--   -> Sections branch branchModel leaf leafModel model meta
-- section id mods container children =
--   Branch id container mods children


-- -- -> LeafModifiers leaf meta

-- field
--   : Id
--   -> List ( Modifier branch meta )
--   -> leaf
--   -> Sections branch branchModel leaf leafModel model meta
-- field id mods data =
--   Leaf id data mods


-- toTree
--   : Sections branch branchModel leaf leafModel model meta
--   -> Tree ( Sections branch branchModel leaf leafModel model meta )
-- toTree section =
--   case section of
--     Branch _ _ _ children ->
--       Tree section <| List.map toTree children

--     Leaf _ _ _ ->
--       Tree section []

-- toDataNode
--   : ( meta
--       -> ( Sections branch branchModel leaf leafModel model meta )
--       -> model
--       )
--   -> meta
--   -> ZipperState
--   -> String
--   -> Zipper ( Sections branch branchModel leaf leafModel model meta )
--   -> Forest ( DataNode branch branchModel leaf leafModel model meta ) -- aka List ( DataNode branch leaf meta data )
--   -> Tree ( DataNode branch branchModel leaf leafModel model meta )
-- toDataNode dataValueMap meta state id zipper =
--   let
--     path = appendPath state.path id

--     dataValue = dataValueMap meta ( MultiwayTreeZipper.datum zipper )
--   in
--     Tree ( DataNode zipper path id dataValue )


toNode
  : DataValueMap branch branchModel leaf leafModel model meta
  -> meta
  -> Sections branch branchModel leaf leafModel model meta
  -> Node branch branchModel leaf leafModel model meta
  -- -> Node branch branchModel leaf leafModel model meta
toNode dataValueMap meta section =
  ( section, ( dataValueMap meta section, meta ) )
  -- , { model = dataValueMap meta section
  --   , meta = meta
  --   }
  -- )


toTree
  : DataValueMap branch branchModel leaf leafModel model meta
  -> meta
  -> Sections branch branchModel leaf leafModel model meta
  -> Tree ( Node branch branchModel leaf leafModel model meta )
toTree dataValueMap meta section =
  case section of
    Branch _ _ _ children ->
      Tree ( toNode dataValueMap meta section ) <| List.map ( toTree dataValueMap meta ) children

    Leaf _ _ _ ->
      Tree ( toNode dataValueMap meta section ) []


appendPath : String -> String -> String
appendPath path id =
  if String.length path == 0 then
    id
  else
    path ++ "." ++ id


-- type alias DataNode branch branchModel leaf leafModel model meta =
--   { section : Zipper ( Sections branch branchModel leaf leafModel model meta )
--   , path : String
--   , id : String
--   , model : model
--   }

type alias Form branch branchModel leaf leafModel model meta = Tree ( Node branch branchModel leaf leafModel model meta )
-- type alias Form branch branchModel leaf leafModel model meta =
--   { sections : Zipper ( Sections branch branchModel leaf leafModel model meta )
--   , data : Zipper ( DataNode branch branchModel leaf leafModel model meta )
--   }


type alias ZipperState =
  { depth : Int
  , index : Int
  , path : String
  }


type alias Mapper branch branchModel leaf leafModel model meta type_ =
  ( ZipperState -> String -> Zipper ( Node branch branchModel leaf leafModel model meta ) -> List type_ -> type_ )


-- type alias DataNodeMapper branch branchModel leaf leafModel model meta type_ =
--   ( ZipperState -> String -> Zipper ( DataNode branch branchModel leaf leafModel model meta ) -> List type_ -> type_ )


zipperState : ZipperState
zipperState =
  ZipperState 0 0 ""


-- toDataNode
--   : ( meta
--       -> ( Sections branch branchModel leaf leafModel model meta )
--       -> model
--       )
--   -> meta
--   -> ZipperState
--   -> String
--   -> Zipper ( Sections branch branchModel leaf leafModel model meta )
--   -> Forest ( DataNode branch branchModel leaf leafModel model meta ) -- aka List ( DataNode branch leaf meta data )
--   -> Tree ( DataNode branch branchModel leaf leafModel model meta )
-- toDataNode dataValueMap meta state id zipper =
--   let
--     path = appendPath state.path id

--     dataValue = dataValueMap meta ( MultiwayTreeZipper.datum zipper )
--   in
--     Tree ( DataNode zipper path id dataValue )


sectionZipper
  : ZipperState
  -> Mapper branch branchModel leaf leafModel model meta type_
  -> Zipper ( Node branch branchModel leaf leafModel model meta )
  -> type_
sectionZipper state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  case node of
    ( Leaf id _ _, _ ) ->
      mapper state id zipper []
      
    ( Branch id _ _ _, _ ) ->
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
        |> List.filterMap identity
        |> mapper state id zipper


applySectionZipper
  : Mapper branch branchModel leaf leafModel model meta type_
  -> Zipper ( Node branch branchModel leaf leafModel model meta )
  -> type_
applySectionZipper mapper zipper =
  sectionZipper zipperState mapper zipper


-- dataZipper
--   : ZipperState
--   -> Mapper branch branchModel leaf leafModel model meta type_
--   -> Zipper ( Node branch branchModel leaf leafModel model meta )
--   -> type_
-- dataZipper state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--   mapper state node.id zipper

--     ( children
--         |> List.indexedMap
--             (\ index _ ->
--                 goToChild index zipper
--                   |> Maybe.map
--                       ( dataZipper
--                           { state
--                               | depth = state.depth + 1
--                               , index = index
--                               , path = appendPath state.path node.id
--                           }
--                           mapper
--                       )
--             )
--         |> List.filterMap identity
--     )


-- applyDataZipper
--   : Mapper branch branchModel leaf leafModel model meta type_
--   -> Tree ( Node branch branchModel leaf leafModel model meta )
--   -> type_
-- applyDataZipper mapper tree =
--   dataZipper zipperState mapper ( tree, [] )


toSectionByPathIndex
  : ZipperState
  -> String
  -> Zipper ( Node branch branchModel leaf leafModel model meta )
  -> List ( List ( String, ( Zipper ( Node branch branchModel leaf leafModel model meta ) ) ) )
  -> List ( String, ( Zipper ( Node branch branchModel leaf leafModel model meta ) ) )
toSectionByPathIndex state id zipper =
  let
    path_ = appendPath state.path id
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, zipper )
    )


toDataByPathIndex
  : ZipperState
  -> String
  -> Zipper ( Node branch branchModel leaf leafModel model meta )
  -> List ( List ( String, ( Zipper ( Node branch branchModel leaf leafModel model meta ) ) ) )
  -> List ( String, Zipper ( Node branch branchModel leaf leafModel model meta ) )
toDataByPathIndex state id zipper =
  let
    path_ = appendPath state.path id
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, zipper )
    )


-- getDataNodeByPathSegments
--   : List String
--   -> Zipper ( Node branch branchModel leaf leafModel model meta )
--   -> Maybe ( Zipper ( Node branch branchModel leaf leafModel model meta ) )
-- getDataNodeByPathSegments segments zipper =
--   segments
--     |> List.head
--     |> Maybe.andThen
--         (\ id ->
--             let
--               data = MultiwayTreeZipper.datum zipper
--             in
--               if data.id == id then
--                 case List.tail segments of
--                   Nothing ->
--                     Just zipper

--                   Just segments_ ->
--                     if List.length segments_ == 0 then
--                       Just zipper
--                     else
--                       Just zipper
--                         &> goToNext
--                         &> getDataNodeByPathSegments segments_
--               else
--                 Just zipper
--                   &> goRight
--                   &> getDataNodeByPathSegments segments
--         )
    

-- getDataNodeByPath
--   : Form branch branchModel leaf leafModel model meta
--   -> String
--   -> Maybe model
-- getDataNodeByPath form path =
--   ( getDataNodeByPathSegments
--       ( String.split "." path )
--       form.data
--   )
--     |> Maybe.map MultiwayTreeZipper.datum
--     |> Maybe.map .model


-- mapDataNodeByPath
--   : Form branch branchModel leaf leafModel model meta
--   -> String
--   -> ( Node branch branchModel leaf leafModel model meta
--       -> Node branch branchModel leaf leafModel model meta
--       )
--   -> Form branch branchModel leaf leafModel model meta
-- mapDataNodeByPath form path mapper =
--   ( getDataNodeByPathSegments
--       ( String.split "." path )
--       form.data
--   )
--     &> MultiwayTreeZipper.updateDatum mapper
--     &> goToRoot
--     |> Maybe.map
--         (\ zipper ->
--             form.sections zipper
--             -- Node <| form.sections zipper
--             -- Form form.sections zipper
--         )
--     |> Maybe.withDefault form


-- toForm
--   : ( meta
--       -> ( Sections branch branchModel leaf leafModel model meta )
--       -> model
--       )
--   -> Sections branch branchModel leaf leafModel model meta
--   -> meta
--   -> Form branch branchModel leaf leafModel model meta
-- toForm dataTypeMap node meta =
--   let
--     sections : Zipper ( Sections branch branchModel leaf leafModel model meta )
--     sections = ( toTree node, [] )

--     data : Zipper ( DataNode branch branchModel leaf leafModel model meta )
--     data = ( applySectionZipper ( toDataNode dataTypeMap meta ) sections, [] )

--   in
--     Form sections data
