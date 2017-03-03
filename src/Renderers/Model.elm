module Renderers.Model exposing
  ( Title
  , BoolControls(..)

  , FileUploadControls(..)

  , OptionControls(..)

  , TextInputModel
  , placeholder
  , TextControls(..)

  , BulletTypes(..)
  , ContainerFacts(..)

  , DataTypes(..)
  , DataFacts(..)

  , boolIs
  , visible

  , toDataType
  )


import FormBuilder exposing (..)
-- import Renderers.Model exposing (..)
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)

type alias Title = String


type BoolControls
  = YesNo
  | YesNoMaybe


type FileUploadControls
  = MutiUpload


type OptionControls
  = Checkbox Title
  | Radio Title


type alias TextInputModel =
  { placeholder : Maybe String
  }


defaultTextInputModel : TextInputModel
defaultTextInputModel =
  { placeholder = Nothing
  }


placeholder : String -> TextInputModel -> TextInputModel
placeholder value model =
  { model | placeholder = Just value }


type TextControls
  = TextInput Title ( List ( TextInputModel -> TextInputModel ) )
  | TextLabel Title

type BulletTypes
  = AlphaBullets
  | NumericBullets


type ContainerFacts
  = BulletList BulletTypes Title
  | Grid
  | Header Title
  | LabeledSection Title
  | OrderedList Title


type DataTypes meta
  = BoolData ( DataValue meta Bool )
  | ListStringData ( DataValue meta ( List String )  )
  | OptionData ( DataValue meta ( Set String ) )
  | TextData ( DataValue meta String )


-- type DataMods branch meta type_ = List ( DataModifiers branch (DataTypes meta) meta type_ )

type DataFacts branch meta
  = Bool
      ( List ( DataModifiers branch (DataTypes meta) meta Bool ) )
      BoolControls
  | FileUpload
      ( List ( DataModifiers branch (DataTypes meta) meta ( List String ) ) )
      -- ( DataModifiers branch (DataTypes meta) meta ( List String ) )
      FileUploadControls
  | Option
      ( List ( DataModifiers branch (DataTypes meta) meta ( Set String ) ) )
      -- ( DataModifiers branch (DataTypes meta) meta ( Set String ) )
      ( List ( String, String ) )
      OptionControls
  | Text
      ( List ( DataModifiers branch (DataTypes meta) meta String ) )
      -- ( DataModifiers branch (DataTypes meta) meta String )
      TextControls


boolIs : Bool -> Zipper (Sections branch leaf meta) -> Bool
boolIs expected zipper =
  False



type alias VisibleDim dim =
  { dim | visible : Bool }


visible : Selector branch leaf ( VisibleDim meta ) -> Predicate branch leaf ( VisibleDim meta ) -> Tree ( Sections branch leaf ( VisibleDim meta ) ) -> MetaModifiers branch leaf ( VisibleDim meta )
visible selector predicate tree =
  MetaMod (\ model ->
    { model | visible =
        tree
        |> selector
        |> predicate
    }
  )


toDataType : meta -> ( Sections branch ( DataFacts branch meta) meta ) -> Maybe ( DataTypes meta )
toDataType meta node =
  case Debug.log "toDataType" node of
    Branch _ _ _ _ ->
      Nothing
    Leaf _ leaf _ ->
      Just <|
        case leaf of
          Bool mods control ->
            applyLeafMods BoolData ( DataValue Nothing Nothing meta ) mods
          FileUpload mods control ->
            applyLeafMods ListStringData ( DataValue Nothing Nothing meta ) mods
          Option mods options control ->
            applyLeafMods OptionData ( DataValue Nothing Nothing meta ) mods
          Text mods control ->
            applyLeafMods TextData ( DataValue Nothing Nothing meta ) mods