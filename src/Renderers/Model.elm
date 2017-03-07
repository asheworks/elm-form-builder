module Renderers.Model exposing
  ( Command(..)
  , Event(..)
  , Effect(..)

  , Model
  , Meta
  -- , defaultModel

  , Title
  , title

  -- , BoolControls(..)

  -- , FileUploadControls(..)

  -- , OptionControls(..)

  -- , TextInputModel
  -- , defaultTextInputModel
  
  , default
  , placeholder
  , visible
  -- , TextControls(..)

  , BulletTypes(..)

  , Models(..)

  , BranchModels(..)
  , BranchFacts(..)

  -- , DataTypes(..)
  , LeafModels(..)
  , LeafFacts(..)

  -- , placeholder
  -- , boolIs
  -- , visible

  , RendererSections
  , RendererZipper
  , RendererDataNode
  , RendererForm
  -- , SectionTypes
  , toDataValue
  )


-- boolIs : Bool -> Zipper (Sections branch leaf meta) -> Bool
-- boolIs expected ( ( ( ( Tree node children ) as tree ), crumbs ) as zipper ) =
--   let
--     t = Debug.log "Renderers Model boolIs"
--   in
--     expected
--   -- case node of
--   --   Branch 
--   -- False


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


type alias Meta =
  { visible : Bool
  }

type alias Model meta =
  RendererForm meta


type alias Title = String


title : Title -> { model | title : Title } -> { model | title : Title }
title value model =
  { model | title = value }


type BulletTypes
  = AlphaBullets
  | NumericBullets


type alias BulletListModel =
  { title : Title
  , bulletType : BulletTypes
  }


type alias GridModel =
  { title : Title
  }


type alias HeaderModel =
  { title : Title
  }


type alias LabeledSectionModel =
  { title : Title
  }


type alias OrderedListModel =
  { title : Title
  }

type alias CheckboxModel =
  { title : String
  , options : List ( String, String )
  , values : Set String
  }


type alias MultiUploadModel =
  { title : String
  , values : Set String
  }


type alias RadioModel =
  { title : String
  , options : List ( String, String )
  , value : String
  }


type alias TextInputModel =
  { title : String
  , placeholder : String
  , default : Maybe String
  , value : String
  }


type alias TextLabelModel =
  { title : String
  , default : Maybe String
  , value : String
  }


type alias YesNoModel =
  { title : String
  , value : Bool
  }


type alias YesNoMaybeModel =
  { title : String
  , value : Maybe Bool
  }


type alias DefaultDim target type_ =
  { target
      | default : Maybe type_
      , value : type_
  }


default : type_ -> DataValue ( DefaultDim model type_ ) meta -> DataValue ( DefaultDim model type_ ) meta
default value data =
  let
    model = data.model
  in
    DataValue
      { model
          | default = Just value
          , value = value
      }
      data.meta

  
type alias PlaceholderDim target =
  { target
      | placeholder : String
  }


placeholder : String -> DataValue ( PlaceholderDim model ) meta -> DataValue ( PlaceholderDim model ) meta
placeholder value data =
  let
    model = data.model
  in
    DataValue
      { model
          | placeholder = value
      }
      data.meta


type alias VisibleDim target =
  { target
      | visible : Bool
  }


visible : Bool -> DataValue model ( VisibleDim meta ) -> DataValue model ( VisibleDim meta )
visible value data =
  let
    meta = data.meta

    t = Debug.log "** VISIBLE" value
  in
    DataValue
      data.model
      { meta
          | visible = value
      }


type BranchModels meta
  = BulletListControl ( DataValue BulletListModel meta )
  | GridControl ( DataValue GridModel meta )
  | HeaderControl ( DataValue HeaderModel meta )
  | LabeledSectionControl ( DataValue LabeledSectionModel meta )
  | OrderedListControl ( DataValue OrderedListModel meta )


type LeafModels meta
  = CheckboxControl ( DataValue CheckboxModel meta )
  | MultiUploadControl ( DataValue MultiUploadModel meta )
  | RadioControl ( DataValue RadioModel meta )
  | TextInputControl ( DataValue TextInputModel meta )
  | TextLabelControl ( DataValue TextLabelModel meta )
  | YesNoControl ( DataValue YesNoModel meta )
  | YesNoMaybeControl ( DataValue YesNoMaybeModel meta )


type Models meta
  = BranchModel ( BranchModels meta )
  | LeafModel ( LeafModels meta )


type BranchFacts meta
  = BulletList Title BulletTypes ( BranchModifiers BulletListModel meta )
  | Grid Title ( BranchModifiers GridModel meta )
  | Header Title ( BranchModifiers HeaderModel meta )
  | LabeledSection Title ( BranchModifiers LabeledSectionModel meta )
  | OrderedList Title ( BranchModifiers OrderedListModel meta )


type LeafFacts meta
  = Checkbox Title ( List ( String, String ) ) ( LeafModifiers CheckboxModel meta )
  | MultiUpload Title ( LeafModifiers MultiUploadModel meta )
  | Radio Title ( List ( String, String ) ) ( LeafModifiers RadioModel meta )
  | TextInput Title ( LeafModifiers TextInputModel meta )
  | TextLabel Title ( LeafModifiers TextLabelModel meta )
  | YesNo Title ( LeafModifiers YesNoModel meta )
  | YesNoMaybe Title ( LeafModifiers YesNoMaybeModel meta )


type alias RendererSections meta =
  Sections ( BranchFacts meta ) BranchModels ( LeafFacts meta ) LeafModels ( Models meta ) meta


type alias RendererZipper meta =
  Zipper ( RendererSections meta )


type alias RendererDataNode meta =
  DataNode ( BranchFacts meta ) BranchModels ( LeafFacts meta ) LeafModels ( Models meta ) meta


type alias RendererForm meta =
  Form ( BranchFacts meta ) BranchModels ( LeafFacts meta ) LeafModels ( Models meta ) meta


applyMods
  : model
  -> meta
  -> List ( DataValue model meta -> DataValue model meta )
  -> DataValue model meta
applyMods model meta mods =
  mods
    |> List.foldl
        (\ mod model_ ->
            mod model_
            -- let
            --   value = mod model_

            --   t = Debug.log "Apply Mod" value
            -- in
            --   value
        )
        ( DataValue model meta )


toDataValue
  : meta
  -> RendererSections meta
  -> Models meta
toDataValue meta node =
  case node of
    Branch _ branch _ _ ->
      BranchModel <|
        case branch of

          BulletList title bulletType mods ->
            BulletListControl <| applyMods ( BulletListModel title bulletType ) meta mods

          Grid title mods ->
            GridControl <| applyMods ( GridModel title ) meta mods

          Header title mods ->
            HeaderControl <| applyMods ( HeaderModel title ) meta mods

          LabeledSection title mods ->
            LabeledSectionControl <| applyMods ( LabeledSectionModel title ) meta mods

          OrderedList title mods ->
            OrderedListControl <| applyMods ( OrderedListModel title ) meta mods

    Leaf _ leaf _ ->
      LeafModel <|
        case leaf of

          Checkbox title options mods ->
            CheckboxControl <| applyMods ( CheckboxModel title options Set.empty ) meta mods

          MultiUpload title mods ->
            MultiUploadControl <| applyMods ( MultiUploadModel title Set.empty ) meta mods

          Radio title options mods ->
            RadioControl <| applyMods ( RadioModel title options "" ) meta mods

          TextInput title mods ->
            TextInputControl <| applyMods ( TextInputModel title "" Nothing "" ) meta mods

          TextLabel title mods ->
            TextLabelControl <| applyMods ( TextLabelModel title Nothing "" ) meta mods

          YesNo title mods ->
            YesNoControl <| applyMods ( YesNoModel title False ) meta mods

          YesNoMaybe title mods ->
            YesNoMaybeControl <| applyMods ( YesNoMaybeModel title Nothing ) meta mods
