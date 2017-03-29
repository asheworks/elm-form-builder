module Renderers.Model exposing (..)

import CQRS exposing (State)

import FormBuilder exposing (..)
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)


type Command
  = Checkbox_Update RendererZipper String
  | TextInput_Update RendererZipper String
  | YesNo_Update RendererZipper Bool
  | YesNoMaybe_Update RendererZipper Bool
  -- | OptionsData_Update String ( Int, String )
  -- | StringListData_Update 
  

type Event
  = Checkbox_Updated RendererZipper String
  | TextInput_Updated RendererZipper String
  | YesNo_Updated RendererZipper Bool
  | YesNoMaybe_Updated RendererZipper Bool
  -- | InputField_Updated String String
  -- | RadioField_Updated String (Int, String)


type Effect
  = None


type alias Meta =
  { visible : Bool
  }


type alias RendererForm =
  Form BranchFacts BranchModels LeafFacts LeafModels Models Meta


type alias RendererSections =
  Sections BranchFacts BranchModels LeafFacts LeafModels Models Meta


type alias RendererNode =
  Node BranchFacts BranchModels LeafFacts LeafModels Models Meta


type alias RendererZipper =
  Zipper RendererNode


type alias RendererDataNode =
  Node BranchFacts BranchModels LeafFacts LeafModels Models Meta


type alias RendererDataValue model =
  DataValue model Meta


type alias BranchMapper =
  ( BranchModels Meta -> BranchModels Meta )


type alias LeafMapper =
  ( LeafModels Meta -> LeafModels Meta )


type alias Model =
  { form : Maybe RendererForm
  }


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


type alias CheckboxModel =
  { title : String
  , options : List ( String, String )
  , values : Maybe ( Set String )
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


type alias MultiUploadModel =
  { title : String
  , values : Set String
  }

type alias OrderedListModel =
  { title : Title
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


type alias ValueDim target type_ =
  { target
      | value : type_
  }


value : type_ -> DataValue ( ValueDim model type_ ) meta -> DataValue ( ValueDim model type_ ) meta
value value ( model, meta ) =
  ( { model
      | value = value
    }
  , meta
  )


type alias DefaultDim target type_ =
  { target
      | default : Maybe type_
      -- , value : type_
  }


default : type_ -> DataValue ( DefaultDim model type_ ) meta -> DataValue ( DefaultDim model type_ ) meta
default value ( model, meta ) =
  ( { model
      | default = Just value
      -- , value = value
    }
  , meta
  )

  
type alias PlaceholderDim target =
  { target
      | placeholder : String
  }


placeholder : String -> DataValue ( PlaceholderDim model ) meta -> DataValue ( PlaceholderDim model ) meta
placeholder value ( model, meta ) =
  ( { model
      | placeholder = value
    }
  , meta
  )


type alias VisibleDim target =
  { target
      | visible : Bool
  }


visible : Bool -> DataValue model ( VisibleDim meta ) -> DataValue model ( VisibleDim meta )
visible value ( model, meta ) =
  ( model
  , { meta
      | visible = value
    }
  )


type BranchModels
  = BulletListControl ( DataValue BulletListModel Meta )
  | GridControl ( DataValue GridModel Meta )
  | HeaderControl ( DataValue HeaderModel Meta )
  | LabeledSectionControl ( DataValue LabeledSectionModel Meta )
  | OrderedListControl ( DataValue OrderedListModel Meta )


type LeafModels
  = CheckboxControl ( DataValue CheckboxModel Meta )
  | MultiUploadControl ( DataValue MultiUploadModel Meta )
  | RadioControl ( DataValue RadioModel Meta )
  | TextInputControl ( DataValue TextInputModel Meta )
  | TextLabelControl ( DataValue TextLabelModel Meta )
  | YesNoControl ( DataValue YesNoModel Meta )
  | YesNoMaybeControl ( DataValue YesNoMaybeModel Meta )


type Models
  = BranchModel BranchModels
  | LeafModel LeafModels


type BranchFacts
  = BulletList Title BulletTypes ( Modifiers BulletListModel Meta )
  | Grid Title ( Modifiers GridModel Meta )
  | Header Title ( Modifiers HeaderModel Meta )
  | LabeledSection Title ( Modifiers LabeledSectionModel Meta )
  | OrderedList Title ( Modifiers OrderedListModel Meta )


type LeafFacts
  = Checkbox Title ( List ( String, String ) ) ( Modifiers CheckboxModel Meta )
  | MultiUpload Title ( Modifiers MultiUploadModel Meta )
  | Radio Title ( List ( String, String ) ) ( Modifiers RadioModel Meta )
  | TextInput Title ( Modifiers TextInputModel Meta )
  | TextLabel Title ( Modifiers TextLabelModel Meta )
  | YesNo Title ( Modifiers YesNoModel Meta )
  | YesNoMaybe Title ( Modifiers YesNoMaybeModel Meta )



applyMods
  : model
  -> Meta
  -> List ( RendererDataValue model -> RendererDataValue model )
  -> RendererDataValue model
applyMods model meta mods =
  mods
    |> List.foldl
        (\ mod model_ ->
            mod model_
        )
        ( model, meta )


toDataValue
  : Meta
  -> RendererSections
  -> Models
toDataValue meta node =
  case node of

    Branch _ _ branch _ ->
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

    Leaf _ _ leaf ->
      LeafModel <|
        case leaf of

          Checkbox title options mods ->
            CheckboxControl <| applyMods ( CheckboxModel title options Nothing) meta mods

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
