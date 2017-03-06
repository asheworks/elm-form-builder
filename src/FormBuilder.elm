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


  , getDataNodeByPathSegments
  , getDataNodeByPath
  , mapDataNodeByPath
  -- , getDataByPath
  -- , getDataZipperByPath
  )


import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)

--

(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) = flip Maybe.andThen

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
  { sections : Zipper ( Sections branch leaf meta )
  , data : Zipper ( DataNode branch leaf data meta )
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
  -> Zipper ( Sections branch leaf meta )
  -> type_
applySectionZipper mapper zipper =
  sectionZipper zipperState mapper zipper


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
    )


applyDataZipper
  : ( ZipperState -> String -> Zipper ( DataNode branch leaf data meta ) -> List ( type_ ) -> type_ )
  -> Tree ( DataNode branch leaf data meta )
  -> type_
applyDataZipper mapper tree =
  dataZipper zipperState mapper ( tree, [] )


toSectionByPathIndex
  : ZipperState
  -> Maybe String
  -> Zipper ( Sections branch leaf meta )
  -> List ( List ( String, ( Zipper ( Sections branch leaf meta ) ) ) )
  -> List ( String, ( Zipper ( Sections branch leaf meta ) ) )
toSectionByPathIndex state id zipper =
  let
    id_ = Maybe.withDefault "" id

    path_ = appendPath state.path id
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, zipper )
    )


toDataByPathIndex
  : ZipperState
  -> String
  -> Zipper ( DataNode branch leaf data meta )
  -> List ( List ( String, ( Zipper ( DataNode branch leaf data meta ) ) ) )
  -> List ( String, Zipper ( DataNode branch leaf data meta ) )
toDataByPathIndex state id zipper =
  let
    path_ = appendPath state.path (Just id)
  in
    (\ children ->
      ( List.concat children )
      |> (::) ( path_, zipper )
    )


getDataNodeByPathSegments
  : List String
  -> Zipper ( DataNode branch leaf data meta )
  -> Maybe ( Zipper ( DataNode branch leaf data meta ) )
getDataNodeByPathSegments segments zipper =
  segments
    |> List.head
    |> Maybe.andThen
        (\ id ->
            let
              data = MultiwayTreeZipper.datum zipper
            in
              if data.id == id then
                case List.tail segments of
                  Nothing ->
                    Just zipper

                  Just segments_ ->
                    if List.length segments_ == 0 then
                      Just zipper
                    else
                      Just zipper
                        &> goToNext
                        &> getDataNodeByPathSegments segments_
              else
                Just zipper
                  &> goRight
                  &> getDataNodeByPathSegments segments
        )
    

getDataNodeByPath
  : Form branch leaf data meta
  -> String
  -> Maybe data
getDataNodeByPath form path =
  ( getDataNodeByPathSegments
      ( String.split "." path )
      form.data
  )
    |> Maybe.map MultiwayTreeZipper.datum
    |> Maybe.map .data


mapDataNodeByPath
  : Form branch leaf data meta
  -> String
  -> ( DataNode branch leaf data meta -> DataNode branch leaf data meta )
  -> Form branch leaf data meta
mapDataNodeByPath form path mapper =
  ( getDataNodeByPathSegments
      ( String.split "." path )
      form.data
  )
    &> MultiwayTreeZipper.updateDatum mapper
    &> goToRoot
    |> Maybe.map
        (\ zipper ->
            Form form.sections zipper
        )
    |> Maybe.withDefault form
  -- let
  --   form_ =
  --     ( getDataNodeByPathSegments
  --         ( String.split "." path )
  --         form.data
  --     )
  --       &> MultiwayTreeZipper.updateDatum mapper
  --       &> goToRoot
  --       |> Maybe.map
  --           (\ zipper ->
  --               Form form.sections zipper
  --           )
  --       |> Maybe.withDefault form
  --     -- |> Maybe.map
  --     --     (\ zipper ->
  --     --         let
  --     --           MultiwayTreeZipper.updateDatum mapper zipper
                
  --     --     )
  --     -- |> Maybe.withDefault form

  --   -- t =
  --   --   zipper
  --   --   |> Maybe.map
  --       -- |> MultiwayTreeZipper.updateDatum mapper
  --       -- |> MultiwayTreeZipper.goToRoot
  --       -- |> Maybe.map
  --       --     (\ zipper ->
  --       --         Form form.sections zipper
  --       --     )
  -- in
  --   form--Maybe.withDefault form maybeForm


toForm
  : ( meta
  -> ( Sections branch leaf meta ) -> data )
  -> Sections branch leaf meta
  -> meta
  -> Form branch leaf data meta
toForm dataTypeMap node meta =
  let
    sections : Zipper ( Sections branch leaf meta )
    sections = ( toTree node, [] )

    data : Zipper ( DataNode branch leaf data meta )
    data = ( applySectionZipper ( toDataNode dataTypeMap meta ) sections, [] )

  in
    Form sections data
  

byId
  : String
  -> Tree ( Sections branch leaf meta )
  -> Zipper ( Sections branch leaf meta )
byId key tree =
  ( tree, [] )




-- if id == ( MultiwayTreeZipper.datum zipper ).id then
--   Maybe.andThen
--     (\ tail ->
--         getDataNodeByPathSegments tail ( zipper goToChild )
--     )
--     ( List.tail segments )
-- else
--   Nothing

-- &> Maybe.andThen
--     (\ id ->
--         if id == ( MultiwayTreeZipper.datum zipper ).id then
--           Maybe.andThen
--             (\ tail ->
--                 getDataNodeByPathSegments tail ( zipper goToChild )
--             )
--             ( List.tail segments )
--         else
--           Nothing
--     )

-- let
--   id = List.head
--   data = MultiwayTreeZipper.datum zipper

--   if data.id
-- in

-- case ( Just zipper &> goRight ) of
--   Just sibling ->
--     let
--       sib = MultiwayTreeZipper.datum sibling
--       s = Debug.log ( sib.path ++ " -- " ++ sib.id ) segments
--       t = Debug.log "Sib" sib.data
--     in
--       zipper
--   Nothing -> -- End of tier

--     let
--       node = MultiwayTreeZipper.datum zipper
--       s = Debug.log ( node.path ++ " -- " ++ node.id ) segments
--       t = Debug.log "Sib" node.data
--     in
--       zipper



    

-- type alias Form branch leaf data meta =
--   { def : Tree ( Sections branch leaf meta )
--   , data : Tree ( DataNode branch leaf data meta )
--   , indexes :
--       { section : Dict String ( Sections branch leaf meta )
--       , sectionZipper : Dict String ( Zipper ( Sections branch leaf meta ) )
--       , data : Dict String ( DataNode branch leaf data meta )
--       , dataZipper : Dict String ( Zipper ( DataNode branch leaf data meta ) )
--       }
--   }


-- toSectionIndex
--   : ZipperState
--   -> Maybe String
--   -> Zipper ( Sections branch leaf meta )
--   -> List ( List ( String, Sections branch leaf meta ) )
--   -> List ( String, Sections branch leaf meta )
-- toSectionIndex state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--   let
--     id_ = Maybe.withDefault "" id

--     path_ = appendPath state.path id
--   in
--     (\ children ->
--       ( List.concat children )
--       |> (::) ( path_, node )
--     )


-- toDataIndex
--   : ZipperState
--   -> String
--   -> Zipper ( DataNode branch leaf data meta )
--   -> List ( List ( String, ( DataNode branch leaf data meta ) ) )
--   -> List ( String, DataNode branch leaf data meta )
-- toDataIndex state id ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--   let
--     path_ = appendPath state.path (Just id)
--   in
--     (\ children ->
--       ( List.concat children )
--       |> (::) ( path_, node )
--     )


-- getDataByPath
--   : Form branch leaf data meta
--   -> String
--   -> Result String data
-- getDataByPath form path =
--   case Dict.get path form.indexes.data of
--     Nothing -> Err <| "Missing data at path [ " ++ path ++ " ]"
--     Just dataNode -> Ok dataNode.data

-- -> Zipper ( DataNode branch leaf data meta )


  -- Just zipper
  --   &> goToChild 0

-- getDataByPath
--   : Form branch leaf data meta
--   -> String
--   -> data
-- getDataByPath form path =
--   String.split path "."
--     |> 

--   let
--     ( tree, crumbs ) = form.data
--   in


  -- case form.data of
    
  -- form.data

-- getDataZipperByPath
--   : Form branch leaf data meta
--   -> String
--   -> Result String ( Zipper ( DataNode branch leaf data meta ) )
-- getDataZipperByPath form path =
--   case Dict.get path form.indexes.dataZipper of
--     Nothing -> Err <| "Missing data zipper at path  [ " ++ path ++ " ]"
--     Just zipper -> Ok zipper



-- toForm
--   : ( meta
--   -> ( Sections branch leaf meta ) -> data )
--   -> Sections branch leaf meta
--   -> meta
--   -> Form branch leaf data meta
-- toForm dataTypeMap node meta =
--   let
--     tree : Tree ( Sections branch leaf meta )
--     tree = toTree node

--     dataTree : Tree ( DataNode branch leaf data meta )
--     dataTree = applySectionZipper ( toDataNode dataTypeMap meta ) tree

--     indexes =
--       { section = Dict.fromList <| applySectionZipper toSectionIndex tree
--       , sectionZipper = Dict.fromList <| applySectionZipper toSectionZipperIndex tree
--       , data = Dict.fromList <| applyDataZipper toDataIndex dataTree
--       , dataZipper = Dict.fromList <| applyDataZipper toDataZipperIndex dataTree
--       }

--   in
--     Form tree dataTree indexes


    -- sectionsIndex : Dict String ( Zipper ( Sections branch leaf meta ) )
    -- sectionsIndex = Dict.fromList <| applySectionZipper toSectionIndex sections

    -- dataIndex : Dict String ( Zipper ( Section))
    -- dataIndex = Dict.fromList <| applyDataZipper toDataZipperIndex data

  --sectionsIndex dataIndex

  --   indexes =
  --     { section = Dict.fromList <| applySectionZipper toSectionIndex tree
  --     , sectionZipper = Dict.fromList <| applySectionZipper toSectionZipperIndex tree
  --     , data = Dict.fromList <| applyDataZipper toDataIndex dataTree
  --     , dataZipper = Dict.fromList <| applyDataZipper toDataZipperIndex dataTree
  --     }

  -- in
  --   Form tree dataTree indexes


-- update
--   : Form branch leaf data meta
--   -> Zipper ( DataNode branch leaf data meta )
--   -> Form branch leaf data meta
-- update form zipper =
--   Form form.def 
--   zipper
--     |> goToRoot

