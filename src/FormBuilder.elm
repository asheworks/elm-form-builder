module FormBuilder exposing (..)


import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)

--

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = flip Maybe.andThen


type alias Id = String


type alias DataValue model meta = ( model, meta )


type alias Node branch branchModel leaf leafModel model meta =
  ( Sections branch branchModel leaf leafModel model meta, DataValue model meta )


type alias Modifiers model meta = List ( ( DataValue model meta -> DataValue model meta ) )


type Sections branch branchModel leaf leafModel model meta
  = Branch Id ( Modifiers branch meta ) branch ( List ( Sections branch branchModel leaf leafModel model meta ) )
  | Leaf Id ( Modifiers leaf meta ) leaf


type alias DataValueMap branch branchModel leaf leafModel model meta =
  ( meta -> ( Sections branch branchModel leaf leafModel model meta ) -> model )


toNode
  : DataValueMap branch branchModel leaf leafModel model meta
  -> meta
  -> Sections branch branchModel leaf leafModel model meta
  -> Node branch branchModel leaf leafModel model meta
toNode dataValueMap meta section =
  ( section, ( dataValueMap meta section, meta ) )


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


type alias Form branch branchModel leaf leafModel model meta = Tree ( Node branch branchModel leaf leafModel model meta )


type alias Mapper context branch branchModel leaf leafModel model meta type_ =
  ( context -> String -> Zipper ( Node branch branchModel leaf leafModel model meta ) -> List type_ -> type_ )


type alias StateMap context branch branchModel leaf leafModel model meta =
  ( Int -> context -> Zipper ( Node branch branchModel leaf leafModel model meta ) -> context )


sectionZipper
  : List ( StateMap context branch branchModel leaf leafModel model meta )
  -> context
  -> Mapper context branch branchModel leaf leafModel model meta type_
  -> Zipper ( Node branch branchModel leaf leafModel model meta )
  -> type_
sectionZipper stateMaps state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  case node of
    ( Leaf id _ _, _ ) ->
      mapper state id zipper []
      
    ( Branch id _ _ _, _ ) ->
      children
        |> List.indexedMap
            (\ index _ ->
                goToChild index zipper
                  |> Maybe.map
                      ( sectionZipper stateMaps ( List.foldr (\ map state_ -> map index state_ zipper ) state stateMaps ) mapper
                          -- { state
                          --     | depth = state.depth + 1
                          --     , index = index
                          --     , path = appendPath state.path id
                          -- }
                          -- mapper
                      )
            )
        |> List.filterMap identity
        |> mapper state id zipper


applySectionZipper
  : List ( StateMap context branch branchModel leaf leafModel model meta )
  -> context
  -> Mapper context branch branchModel leaf leafModel model meta type_
  -> Zipper ( Node branch branchModel leaf leafModel model meta )
  -> type_
applySectionZipper stateMaps state mapper zipper =
  sectionZipper stateMaps state mapper zipper


-- zipperState : ZipperState
-- zipperState =
--   ZipperState 0 0 ""


-- sectionZipper
--   : ZipperState
--   -> Mapper branch branchModel leaf leafModel model meta type_
--   -> Zipper ( Node branch branchModel leaf leafModel model meta )
--   -> type_
-- sectionZipper state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--   case node of
--     ( Leaf id _ _, _ ) ->
--       mapper state id zipper []
      
--     ( Branch id _ _ _, _ ) ->
--       children
--         |> List.indexedMap
--             (\ index _ ->
--                 goToChild index zipper
--                   |> Maybe.map
--                       ( sectionZipper
--                           { state
--                               | depth = state.depth + 1
--                               , index = index
--                               , path = appendPath state.path id
--                           }
--                           mapper
--                       )
--             )
--         |> List.filterMap identity
--         |> mapper state id zipper


-- applySectionZipper
--   : Mapper branch branchModel leaf leafModel model meta type_
--   -> Zipper ( Node branch branchModel leaf leafModel model meta )
--   -> type_
-- applySectionZipper mapper zipper =
--   sectionZipper zipperState mapper zipper


type alias DepthState context = { context | depth : Int }

depthStateMap : StateMap ( DepthState context ) branch branchModel leaf leafModel model meta 
depthStateMap index state ( ( ( ( Tree node _ ) as tree ), crumbs ) as zipper ) =
  case node of
    ( Leaf _ _ _, _ ) -> state
    ( Branch _ _ _ _, _ ) ->
      { state
          | depth = state.depth + 1
      }

type alias IndexState context = { context | index : Int }

indexStateMap : StateMap ( IndexState context ) branch branchModel leaf leafModel model meta 
indexStateMap index state ( ( ( ( Tree node _ ) as tree ), crumbs ) as zipper ) =
  case node of
    ( Leaf _ _ _, _ ) -> state
    ( Branch _ _ _ _, _ ) ->
      { state
          | index = state.index
      }

type alias PathState context = { context | path : String }

pathStateMap : StateMap ( PathState context ) branch branchModel leaf leafModel model meta 
pathStateMap index state ( ( ( ( Tree node _ ) as tree ), crumbs ) as zipper ) =
  case node of
    ( Leaf _ _ _, _ ) -> state
    ( Branch id _ _ _, _ ) ->
      { state
          | path = appendPath state.path id
      }



type alias DepthIndexPathState context =
  { depth : Int
  , index : Int
  , path : String
  , context : context
  }

depthIndexPathState : context -> DepthIndexPathState context
depthIndexPathState context =
  DepthIndexPathState 0 0 "" context


toSectionByPathIndex
  : DepthIndexPathState String
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
  : DepthIndexPathState String
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


-- encodeZipper
--   : ZipperState
--   -> Mapper branch branchModel leaf leafModel model meta type_
--   -> Zipper ( Node branch branchModel leaf leafModel model meta )
--   -> type_
-- encodeZipper state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--   case node of
--     ( Leaf id _ _, _ ) ->
--       mapper state id zipper []
      
--     ( Branch id _ _ _, _ ) ->
--       children
--         |> List.indexedMap
--             (\ index _ ->
--                 goToChild index zipper
--                   |> Maybe.map
--                       ( sectionZipper
--                           { state
--                               | depth = state.depth + 1
--                               , index = index
--                               , path = appendPath state.path id
--                           }
--                           mapper
--                       )
--             )
--         |> List.filterMap identity
--         |> mapper state id zipper


-- applyEncodeZipper
--   : Mapper branch branchModel leaf leafModel model meta type_
--   -> Zipper ( Node branch branchModel leaf leafModel model meta )
--   -> type_
-- applyEncodeZipper mapper zipper =
--   sectionZipper zipperState mapper zipper



-- type alias SectionZipper branch branchModel leaf leafModel meta =
--   Zipper ( Sections branch branchModel leaf leafModel meta )


-- type alias Predicate branch branchModel leaf leafModel meta =
--   Zipper ( Sections branch branchModel leaf leafModel meta ) -> Bool


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

