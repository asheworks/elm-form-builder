module FormBuilder exposing
  ( keepJusts

  , Id
--   , Title

--   , BoolControls(..)

--   , FileUploadControls(..)

--   , OptionControls(..)

--   , TextInputModel
--   , placeholder
--   , TextControls(..)

--   , BulletTypes(..)
--   , ContainerFacts(..)
  , DataValue
--   , DataTypes(..)
--   , DataFacts(..)

  , DataModifiers(..)

  , MetaModifiers(..)
  , MetaMods

  , SectionZipper
  , Predicate
  , Selector
  , Sections(..)

  , Form
  
  , toId
  , section
  , field

  , applyLeafMods

  , byId
--   , boolIs
--   , visible
  , default

  , toTree

--   , toDataType
  , toForm

  )

-- import Dict exposing (..)
-- import Html exposing (..)
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
-- import Set exposing (..)

--


-- type Command
--   = BoolData_Update String Bool
--   -- | OptionsData_Update String ( Int, String )
--   -- | StringListData_Update 
--   -- | TextData_Update String String
  

-- type Event
--   = BoolData_Updated String Bool
--   -- | InputField_Updated String String
--   -- | RadioField_Updated String (Int, String)

-- type Effect
--   = None


-- commandMap : Form branch leaf data meta -> model -> Command -> Event
-- commandMap form model command =
--   case command of
    
--     BoolData_Update id value ->
--       BoolData_Updated id value

--     -- InputField_Update id value ->
--     --   InputField_Updated id value
    
--     -- RadioField_Update id value ->
--     --   RadioField_Updated id value



-- eventMap : Form branch leaf data meta -> model -> Event -> ( model, Maybe Effect )
-- eventMap form model event =
--   let
--     -- tree = toTree root
--     model_ = case event of

--     BoolData_Updated id value ->
--       model
--   in
--     ( model, Nothing )

      -- updateTreeById tree id model
      --   (\ leaf ->
      --     case leaf of
      --       BoolField def -> def.set model value
      --       _ -> model
      --   )

  --   InputField_Updated id value ->
  --     updateTreeById tree id model
  --       (\ leaf ->
  --         case leaf of
  --           InputField def -> def.set model value
  --           _ -> model
  --       )

  --   RadioField_Updated id (index, value) ->
  --     updateTreeById tree id model
  --       (\ leaf ->
  --         case leaf of
  --           RadioField def -> --def.set model value
  --             if Set.member value (def.get model) then
  --               def.set model <| Set.remove value (def.get model)
  --             else
  --               def.set model <| Set.insert value (def.get model)
  --           _ -> model
  --       )
  -- in
  --   ( model_, Nothing )


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


-- type alias Title = String


-- type BoolControls
--   = YesNo
--   | YesNoMaybe


-- type FileUploadControls
--   = MutiUpload


-- type OptionControls
--   = Checkbox Title
--   | Radio Title


-- type alias TextInputModel =
--   { placeholder : Maybe String
--   }


-- defaultTextInputModel : TextInputModel
-- defaultTextInputModel =
--   { placeholder = Nothing
--   }


-- placeholder : String -> TextInputModel -> TextInputModel
-- placeholder value model =
--   { model | placeholder = Just value }


-- type TextControls
--   = TextInput Title ( List ( TextInputModel -> TextInputModel ) )
--   | TextLabel Title


--


-- type BulletTypes
--   = AlphaBullets
--   | NumericBullets


-- type ContainerFacts
--   = BulletList BulletTypes Title
--   | Grid
--   | Header Title
--   | LabeledSection Title
--   | List Title


type alias DataValue meta type_ =
  { default : Maybe type_
  , value : Maybe type_
  , meta : meta
  }


-- type DataTypes meta
--   = BoolData ( DataValue meta Bool )
--   | ListStringData ( DataValue meta ( List String )  )
--   | OptionData ( DataValue meta ( Set String ) )
--   | TextData ( DataValue meta String )


-- type DataFacts meta
--   = Bool ( DataMods meta Bool ) BoolControls
--   | FileUpload ( DataMods meta ( List String ) ) FileUploadControls
--   | Option ( DataMods meta ( Set String ) ) ( List ( String, String ) ) OptionControls
--   | Text ( DataMods meta String ) TextControls


type DataModifiers branch leaf meta type_
  = BranchMod ( branch -> branch )
  | LeafMod ( leaf -> leaf )
  | ValueMod ( DataValue meta type_ -> DataValue meta type_ )
--   | TypeMod ( DataTypes meta -> DataTypes meta )

applyLeafMods : ( DataValue meta type_ -> leaf ) -> DataValue meta type_ -> List ( DataModifiers branch leaf meta type_ ) -> leaf
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

-- applyMods : ( DataValue meta type_ -> DataTypes meta ) -> DataValue meta type_ -> List ( DataModifiers meta type_ ) -> DataTypes meta
-- applyMods typeMap value mods =
--   List.foldr
--     (\ mod value_ ->
--         case mod of
--           TypeMod map ->
--             map value_
--           _ -> value_
--     )
--     ( List.foldr
--         (\ mod value_ ->
--             case mod of
--               DataMod map ->
--                 map value_
--               _ -> value_
--         )
--         value
--         mods
--         |> typeMap
--     ) mods

type MetaModifiers branch leaf meta
  = MetaMod ( meta -> meta )
  | SectionMod ( SectionZipper branch leaf meta -> SectionZipper branch leaf meta )


-- type alias DataMods branch leaf meta type_ = List ( DataModifiers branch leaf meta type_ )

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


section : Id -> MetaMods branch leaf meta -> branch -> List ( Sections branch leaf meta ) -> Sections branch leaf meta
section id mods container children =
  Branch ( toId id ) container mods children


field : Id -> MetaMods branch leaf meta -> leaf -> Sections branch leaf meta
field id mods data =
  Leaf ( toId id ) data mods


byId : String -> Tree (Sections branch leaf meta) -> Zipper (Sections branch leaf meta)
byId key tree =
  ( tree, [] )


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


type alias DataNode branch leaf data meta =
  { section : Zipper ( Sections branch leaf meta )
  , path : String
  , id : String
  , data : Maybe ( data )
  -- , form : Zipper ( FormNode meta )
  -- , view : Maybe ( Zipper ( ViewNode meta ) )
  -- , data : Maybe ( DataTypes meta )
  }


-- type alias ViewNode branch leaf meta =
--   { section : Zipper ( Sections branch leaf meta )
--   , path : String
--   , id : String
--   , view : Html Command
--   -- , form : Zipper ( FormNode meta )
--   -- , data : Maybe ( Zipper ( DataNode branch leaf meta ) )
--   }


type alias Form branch leaf data meta =
  { def : Tree ( Sections branch leaf meta )
  -- , form : Tree ( FormNode meta )
  , data : Tree ( DataNode branch leaf data meta )
  -- , view : Tree ( ViewNode branch leaf meta )
  -- , view : Html Command
  
  }

-- applyMods : ( DataValue meta type_ -> DataTypes meta ) -> DataValue meta type_ -> List ( DataModifiers meta type_ ) -> DataTypes meta
-- applyMods typeMap value mods =
--   List.foldr
--     (\ mod value_ ->
--         case mod of
--           TypeMod map ->
--             map value_
--           _ -> value_
--     )
--     ( List.foldr
--         (\ mod value_ ->
--             case mod of
--               DataMod map ->
--                 map value_
--               _ -> value_
--         )
--         value
--         mods
--         |> typeMap
--     ) mods


appendPath : String -> String -> String
appendPath path id =
  if String.length path == 0 then
    id
  else
    path ++ "." ++ id


toDataTree :
  ( meta -> ( Sections branch leaf meta ) -> Maybe data ) ->
  meta ->
  Tree ( Sections branch leaf meta ) ->
  Tree ( DataNode branch leaf data meta )
toDataTree dataTypeMap meta tree =
  let
    applyZipper path ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
      let
        dataType = dataTypeMap meta node
      in
        case node of

          Leaf id _ _ ->
            let
              id_ = Maybe.withDefault "" id

              path_ = appendPath path id_
            
              data_ = Tree ( DataNode zipper path_ id_ dataType ) []
            in
              data_

          Branch id _ _ _ ->
            let
              id_ = Maybe.withDefault "" id

              path_ = appendPath path id_

              data_ = Tree ( DataNode zipper path_ id_ dataType )
            in
              children
                |> List.indexedMap
                    (\ index _ ->
                        goToChild index zipper
                          |> Maybe.map ( applyZipper path_ )
                    )
                |> keepJusts
                |> data_
  in
    applyZipper "" ( tree, [] )


toForm :
  ( meta -> ( Sections branch leaf meta ) -> Maybe data ) ->
  Sections branch leaf meta ->
  meta ->
  Form branch leaf data meta
toForm dataTypeMap node meta =
  let
    tree : Tree (Sections branch leaf meta)
    tree = toTree node

    dataTree : Tree ( DataNode branch leaf data meta )
    dataTree = toDataTree dataTypeMap meta tree

    -- view = toView meta tree
  in
    Form tree dataTree-- view
