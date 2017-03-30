module Renderers.DataDecoder exposing (..)

import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)

-- import Json.Decode as Decode
import Json.Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline exposing (required, optional, decode, hardcoded)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode

import FormBuilder exposing (..)
import Renderers.Model exposing (..)

decodeForm
  : Encode.Value
  -> Model
  -> Model
decodeForm data model =
  let
    zipForm =
      applySectionZipper
        [ decoderContextMap
        ]
        { data = Just data
        }
        decodeNode

    do : ( RendererForm -> Tree RendererNode )
    do = (\ form -> zipForm ( form, [] ) )

    form =
      model.form
        |> Maybe.map do
  in
    { model | form = form }


type alias DecoderContext =
  { data : Maybe Encode.Value
  }


type alias OpaqueProperty =
  { value : Decode.Value
  }


decodeOpaqueProperty : String -> Decoder OpaqueProperty
decodeOpaqueProperty id =
  decode OpaqueProperty
    |> required id Decode.value


decodeOpaquePropertyValue : String -> Maybe Encode.Value -> Maybe Decode.Value
decodeOpaquePropertyValue id data =
  data
    |> Maybe.andThen
      (\ data ->
            Decode.decodeValue ( decodeOpaqueProperty id ) data
              |> Result.toMaybe
              |> Maybe.map
                  (\ prop ->
                      -- let
                      --   t = Debug.log "decoded" prop.value
                      -- in
                        prop.value
                  )
      )


decoderContextMap : StateMap DecoderContext branch branchModel leaf leafModel model meta 
decoderContextMap index state ( ( ( ( Tree node _ ) as tree ), crumbs ) as zipper ) =
  case node of
    ( Branch id _ _ _, _ ) ->
      let
        data = decodeOpaquePropertyValue id state.data
      in
        { state | data = data }

    ( Leaf id _ _, _ ) ->
      let
        data = decodeOpaquePropertyValue id state.data

        -- t = Debug.log "Decoded" data
      in
        { state | data = data }


updateNodeDatum : ( Models -> value -> Models ) -> RendererZipper -> value -> Maybe RendererZipper
updateNodeDatum updater zipper value =
  updateDatum
    (\ ( node, ( control, nodeMeta ) ) ->
          -- ( node, ( updateNodeModel control value, nodeMeta ) )
          ( node, ( updater control value, nodeMeta ) )
    ) zipper


updateNode : String -> DecoderContext -> Decoder value -> ( Models -> value -> Models ) -> RendererZipper -> Tree RendererNode
updateNode id state decoder updater zipper =
  decodeOpaquePropertyValue id state.data
    |> Result.fromMaybe "No Value"
    |> Result.andThen ( Decode.decodeValue decoder )
    |> Result.toMaybe
    |> Maybe.andThen ( updateNodeDatum updater zipper )
    |> Maybe.map MultiwayTreeZipper.datum
    |> Maybe.map (\ node -> Tree node [] )
    |> Maybe.withDefault ( Tree ( MultiwayTreeZipper.datum zipper ) [] )


decodeNode
  : DecoderContext
  -> String
  -> RendererZipper
  -> List ( Tree RendererNode )
  -> Tree RendererNode
decodeNode state id zipper children =
  case MultiwayTreeZipper.datum zipper of

    ( _, ( BranchModel branch, _ ) ) ->
      case branch of

        BulletListControl ( model, _ ) -> Tree ( MultiwayTreeZipper.datum zipper ) children

        GridControl ( model, _ ) -> Tree ( MultiwayTreeZipper.datum zipper ) children

        HeaderControl ( model, _ ) -> Tree ( MultiwayTreeZipper.datum zipper ) children

        LabeledSectionControl ( model, _ ) -> Tree ( MultiwayTreeZipper.datum zipper ) children

        OrderedListControl ( model, _ ) -> Tree ( MultiwayTreeZipper.datum zipper ) children

    ( _, ( LeafModel leaf as ctrlModel, _ ) ) ->

      case leaf of

        CheckboxControl ( model, _ ) ->
          updateNode id state ( Decode.list Decode.string ) updateCheckboxModel zipper
          -- Tree ( MultiwayTreeZipper.datum zipper ) []

        MultiUploadControl ( model, _ ) ->
          updateNode id state ( Decode.list Decode.string ) updateMultiUploadModel zipper
          -- Tree ( MultiwayTreeZipper.datum zipper ) []

        RadioControl ( model, _ ) ->
          updateNode id state Decode.string updateRadioModel zipper
          -- Tree ( MultiwayTreeZipper.datum zipper ) []

        TextInputControl ( model, _ ) ->

          updateNode id state Decode.string updateTextInputModel zipper

        TextLabelControl ( model, _ ) ->
        
          updateNode id state Decode.string updateTextLabelModel zipper
          -- Tree ( MultiwayTreeZipper.datum zipper ) []

        YesNoControl ( model, _ ) ->

          updateNode id state Decode.bool updateYesNoModel zipper
          -- Tree ( MultiwayTreeZipper.datum zipper ) []

        YesNoMaybeControl ( model, _ ) ->
          updateNode id state ( Decode.nullable Decode.bool ) updateYesNoMaybeModel zipper
          --Tree ( MultiwayTreeZipper.datum zipper ) []


updateCheckboxModel : Models -> List String -> Models
updateCheckboxModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        CheckboxControl ( model, meta ) ->
          LeafModel <| CheckboxControl ( { model | values = Just <| Set.fromList value }, meta )

        _ -> nodeModel


updateMultiUploadModel : Models -> List String -> Models
updateMultiUploadModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        CheckboxControl ( model, meta ) ->
          LeafModel <| CheckboxControl ( { model | values = Just <| Set.fromList value }, meta )

        _ -> nodeModel


updateRadioModel : Models -> String -> Models
updateRadioModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        RadioControl ( model, meta ) ->
          LeafModel <| RadioControl ( { model | value = value }, meta )

        _ -> nodeModel


updateTextInputModel : Models -> String -> Models
updateTextInputModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        TextInputControl ( model, meta ) ->
          LeafModel <| TextInputControl ( { model | value = value }, meta )

        _ -> nodeModel
      

updateTextLabelModel : Models -> String -> Models
updateTextLabelModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        TextLabelControl ( model, meta ) ->
          LeafModel <| TextLabelControl ( { model | value = value }, meta )

        _ -> nodeModel


updateYesNoModel : Models -> Bool -> Models
updateYesNoModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        YesNoControl ( model, meta ) ->
          LeafModel <| YesNoControl ( { model | value = value }, meta )

        _ -> nodeModel


updateYesNoMaybeModel : Models -> Maybe Bool -> Models
updateYesNoMaybeModel nodeModel value =
  case nodeModel of
    BranchModel branch -> nodeModel
    LeafModel leaf ->
      case leaf of
        YesNoMaybeControl ( model, meta ) ->
          LeafModel <| YesNoMaybeControl ( { model | value = value }, meta )

        _ -> nodeModel

              -- decodeOpaquePropertyValue id state.data
              --   |> Result.fromMaybe "No Value"
              --   |> Result.andThen ( Decode.decodeValue Decode.string )
              --   |> Result.toMaybe
              --   |> Maybe.andThen ( updateTextInput zipper )
              --   |> Maybe.map MultiwayTreeZipper.datum
              --   |> Maybe.map (\ node -> Tree node [] )
              --   |> Maybe.withDefault ( Tree ( MultiwayTreeZipper.datum zipper ) [] )
