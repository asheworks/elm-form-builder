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

  , DataNode

  , Form
  , ZipperState
  , zipperState
  , sectionZipper
  , applySectionZipper
  , dataZipper
  , applyDataZipper

  , toId
  , section
  , field
  , applyLeafMods
  , byId
  , default
  , toTree
  , appendPath
  , toForm

  )


import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)

import Dict exposing (..)

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
  , data : data
  }


type alias Form branch leaf data meta =
  { def : Tree ( Sections branch leaf meta )
  , data : Tree ( DataNode branch leaf data meta )
  , indexes :
      { section : Dict String ( Sections branch leaf meta )
      , sectionZipper : Dict String ( Zipper ( Sections branch leaf meta ) )
      , data : Dict String ( DataNode branch leaf data meta )
      , dataZipper : Dict String ( Zipper ( DataNode branch leaf data meta ) )
      }
  }


type alias ZipperState =
  { depth : Int
  , index : Int
  , path : String
  }


zipperState : ZipperState
zipperState =
  ZipperState 0 0 ""


toDataNode
  : ( meta -> ( Sections branch leaf meta ) -> data )
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


sectionZipper
  : ZipperState
  -> ( ZipperState -> Maybe String -> Zipper ( Sections branch leaf meta ) -> List type_ -> type_ )
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


applySectionZipper
  : ( ZipperState -> Maybe String -> Zipper ( Sections branch leaf meta ) -> List type_ -> type_ )
  -> Tree ( Sections branch leaf meta )
  -> type_
applySectionZipper mapper tree =
  sectionZipper zipperState mapper ( tree, [] )


dataZipper
  : ZipperState
  -> ( ZipperState -> String -> Zipper ( DataNode branch leaf data meta ) -> List type_ -> type_ )
  -> Zipper ( DataNode branch leaf data meta )
  -> type_
dataZipper state mapper ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  mapper state node.id zipper

    ( children
        |> List.indexedMap
            (\ index _ ->
                goToChild index zipper
                  |> Maybe.map
                      ( dataZipper
                          { state
                              | depth = state.depth + 1
                              , index = index
                              , path = appendPath state.path (Just node.id)
                          }
                          mapper
                      )
            )
        |> keepJusts
        -- |> mapper state node.id zipper
    )
  -- if List.length children == 0 then
  --   mapper state id zipper []
  -- else
  --   chil

applyDataZipper
  : ( ZipperState -> String -> Zipper ( DataNode branch leaf data meta ) -> List ( type_ ) -> type_ )
  -> Tree ( DataNode branch leaf data meta )
  -> type_
applyDataZipper mapper tree =
  dataZipper zipperState mapper ( tree, [] )


toSectionZipperIndex
  : ZipperState
  -> Maybe String
  -> Zipper ( Sections branch leaf meta )
  -> List ( List ( String, ( Zipper ( Sections branch leaf meta ) ) ) )
  -> List ( String, ( Zipper ( Sections branch leaf meta ) ) )
toSectionZipperIndex state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  let
    id_ = Maybe.withDefault "" id

    path_ = appendPath state.path id
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, zipper )
    )

toSectionIndex
  : ZipperState
  -> Maybe String
  -> Zipper ( Sections branch leaf meta )
  -> List ( List ( String, Sections branch leaf meta ) )
  -> List ( String, Sections branch leaf meta )
toSectionIndex state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  let
    id_ = Maybe.withDefault "" id

    path_ = appendPath state.path id
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, node )
    )


toDataZipperIndex
  : ZipperState
  -> String
  -> Zipper ( DataNode branch leaf data meta )
  -> List ( List ( String, ( Zipper ( DataNode branch leaf data meta ) ) ) )
  -> List ( String, Zipper ( DataNode branch leaf data meta ) )
toDataZipperIndex state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  let
    path_ = appendPath state.path (Just id)
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, zipper )
    )

toDataIndex
  : ZipperState
  -> String
  -> Zipper ( DataNode branch leaf data meta )
  -> List ( List ( String, ( DataNode branch leaf data meta ) ) )
  -> List ( String, DataNode branch leaf data meta )
toDataIndex state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  let
    path_ = appendPath state.path (Just id)
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, node )
    )


toForm
  : ( meta
  -> ( Sections branch leaf meta ) -> data )
  -> Sections branch leaf meta
  -> meta
  -> Form branch leaf data meta
toForm dataTypeMap node meta =
  let
    tree : Tree ( Sections branch leaf meta )
    tree = toTree node

    dataTree : Tree ( DataNode branch leaf data meta )
    dataTree = applySectionZipper ( toDataNode dataTypeMap meta ) tree

    indexes =
      { section = Dict.fromList <| applySectionZipper toSectionIndex tree
      , sectionZipper = Dict.fromList <| applySectionZipper toSectionZipperIndex tree
      , data = Dict.fromList <| applyDataZipper toDataIndex dataTree
      , dataZipper = Dict.fromList <| applyDataZipper toDataZipperIndex dataTree
      }

    -- dataIndex =
    --   MultiwayTree.foldr
    --     (\ node dict ->
    --         Dict.insert node.path node.section
    --     ) Dict.empty dataTree

  in
    Form tree dataTree indexes -- ( flatten dataTree )


byId
  : String
  -> Tree ( Sections branch leaf meta )
  -> Zipper ( Sections branch leaf meta )
byId key tree =
  ( tree, [] )
