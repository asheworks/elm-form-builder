module Renderers.Model exposing
  ( Command(..)
  , Event(..)
  , Effect(..)

  , Model
  , defaultModel

  , Title
  , BoolControls(..)

  , FileUploadControls(..)

  , OptionControls(..)

  , TextInputModel
  , defaultTextInputModel
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


import CQRS exposing (State)

import FormBuilder exposing (..)
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)


type Command
  = BoolData_Update String Bool
  | CheckboxData_Update String String --( Int, String )
  -- | OptionsData_Update String ( Int, String )
  -- | StringListData_Update 
  | TextData_Update String String
  

type Event
  = BoolData_Updated String Bool
  | CheckboxData_Updated String String--( Int,  String )
  | TextData_Updated String String
  -- | InputField_Updated String String
  -- | RadioField_Updated String (Int, String)


type Effect
  = None


type alias Model branch leaf data meta =
  { form : State ( Form branch leaf data meta )
  }


defaultModel
  : (meta -> Sections branch leaf meta -> data)
  -> Sections branch leaf meta
  -> meta
  -> { form : State ( Form branch leaf data meta ) }
defaultModel mapper def meta =
  { form = State <| toForm mapper def meta
  }


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


type ContainerFacts meta

  = BulletList
      ( List ( BranchModifiers (DataTypes meta) meta Bool ) )
      BulletTypes
      Title

  | Grid
      ( List ( BranchModifiers (DataTypes meta) meta Bool ) )

  | Header
      ( List ( BranchModifiers (DataTypes meta) meta Bool ) )
      Title

  | LabeledSection
      ( List ( BranchModifiers (DataTypes meta) meta Bool ) )
      Title

  | OrderedList
      ( List ( BranchModifiers (DataTypes meta) meta Bool ) )
      Title


type DataTypes meta
  = Meta meta
  | BoolData ( DataValue meta Bool )
  | ListStringData ( DataValue meta ( List String )  )
  | OptionData ( DataValue meta ( Set String ) )
  | TextData ( DataValue meta String )


type DataFacts meta

  = Bool
      ( List ( LeafModifiers (DataTypes meta) meta Bool ) )
      BoolControls

  | FileUpload
      ( List ( LeafModifiers (DataTypes meta) meta ( List String ) ) )
      FileUploadControls

  | Option
      ( List ( LeafModifiers (DataTypes meta) meta ( Set String ) ) )
      ( List ( String, String ) )
      OptionControls

  | Text
      ( List ( LeafModifiers (DataTypes meta) meta String ) )
      TextControls


boolIs : Bool -> Zipper (Sections branch leaf meta) -> Bool
boolIs expected ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
  let
    t = Debug.log "Renderers Model boolIs"
  in
    expected
  -- case node of
  --   Branch 
  -- False



type alias VisibleDim dim =
  { dim | visible : Bool }


visible
  : Selector branch leaf ( VisibleDim meta )
  -> Predicate branch leaf ( VisibleDim meta )
  -> Tree ( Sections branch leaf ( VisibleDim meta ) )
  -> MetaModifiers branch leaf ( VisibleDim meta )
visible selector predicate tree =
  MetaMod (\ model ->
    { model | visible =
        tree
        |> selector
        |> predicate
    }
  )


toDataType
  : meta
  -> ( Sections branch ( DataFacts meta) meta )
  -> DataTypes meta
toDataType meta node =
  case node of
    Branch _ _ _ _ ->
      Meta meta

    Leaf _ leaf _ ->
      case leaf of
        Bool mods control ->
          applyLeafMods BoolData ( DataValue Nothing Nothing meta ) mods
        FileUpload mods control ->
          applyLeafMods ListStringData ( DataValue Nothing Nothing meta ) mods
        Option mods options control ->
          applyLeafMods OptionData ( DataValue Nothing Nothing meta ) mods
        Text mods control ->
          applyLeafMods TextData ( DataValue Nothing Nothing meta ) mods
