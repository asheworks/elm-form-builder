module Renderers.DataEncoder exposing (..)

import MultiwayTreeZipper exposing (..)
import Set exposing (..)

import Json.Encode as Encode

import FormBuilder exposing (..)
import Renderers.Model exposing (..)


encodeForm
  : Model
  -> Encode.Value
encodeForm model =
  let
    zipForm = applySectionZipper [] "" encodeNode

    do : ( RendererForm -> Maybe ( String, Encode.Value ) )
    do = (\ form -> zipForm ( form, [] ) )

    data =
      model.form
        |> Maybe.andThen do
        |> Maybe.withDefault ( "infosec", Encode.object [] )

  in
    Encode.object [ data ]


encodeBranch : String -> String -> List ( String, Encode.Value ) -> model -> ( model -> Maybe Encode.Value ) -> Maybe ( String, Encode.Value)
encodeBranch state id children model encodeModel =
    if List.length children > 0 then
      Just ( id, Encode.object children )
    else
      Nothing


encodeControl : String -> String -> model -> ( model -> Maybe Encode.Value ) -> Maybe ( String, Encode.Value )
encodeControl state id model encodeModel =
  encodeModel model
    |> Maybe.map
        (\ value ->
            ( id
            , value
            )
        )

encodeNode
  : String
  -> String
  -> RendererZipper
  -> List ( Maybe ( String, Encode.Value ) )
  -> Maybe ( String, Encode.Value )
encodeNode state id zipper children =
  case MultiwayTreeZipper.datum zipper of

    ( _, ( BranchModel branch, _ ) ) ->

      let
        do = encodeBranch state id ( List.filterMap identity children )
      in
        case branch of

          BulletListControl ( model, _ ) ->
            do model encodeEmpty

          GridControl ( model, _ ) ->
            do model encodeEmpty

          HeaderControl ( model, _ ) ->
            do model encodeEmpty

          LabeledSectionControl ( model, _ ) ->
            do model encodeEmpty

          OrderedListControl ( model, _ ) ->
            do model encodeEmpty

    ( _, ( LeafModel leaf as ctrlModel, _ ) ) ->

      let
        do = encodeControl state id
      in
        case leaf of

          CheckboxControl ( model, _ ) ->
            do model encodeCheckboxControl

          MultiUploadControl ( model, _ ) ->
            do model encodeMultiUploadControl

          RadioControl ( model, _ ) ->
            do model encodeRadioControl

          TextInputControl ( model, _ ) ->
            do model encodeTextInputControl

          TextLabelControl ( model, _ ) ->
            do model encodeTextLabelControl

          YesNoControl ( model, _ ) ->
            do model encodeYesNoControl

          YesNoMaybeControl ( model, _ ) ->
            do model encodeYesNoMaybeControl


encodeEmpty : model -> Maybe Encode.Value
encodeEmpty model =
  Nothing


encodeCheckboxControl : CheckboxModel -> Maybe Encode.Value
encodeCheckboxControl model =
  model.values
    |> Maybe.map Set.toList
    |> Maybe.andThen
        (\ items ->
            if List.length items == 0 then
              Nothing
            else
              Just <| Encode.list ( List.map Encode.string items )
        )


encodeMultiUploadControl : MultiUploadModel -> Maybe Encode.Value
encodeMultiUploadControl model =
  if List.length ( Set.toList model.values ) == 0 then
    Nothing
  else
    Just <| Encode.list ( List.map Encode.string ( Set.toList model.values ) )


encodeRadioControl : RadioModel -> Maybe Encode.Value
encodeRadioControl model =
  Just <| Encode.string model.value


encodeTextInputControl : TextInputModel -> Maybe Encode.Value
encodeTextInputControl model =
  if model.value == "" then
    Nothing
  else
    Just <| Encode.string model.value


encodeTextLabelControl : TextLabelModel -> Maybe Encode.Value
encodeTextLabelControl model =
  if model.value == "" then
    Nothing
  else
    Just <| Encode.string model.value


encodeYesNoControl : YesNoModel -> Maybe Encode.Value
encodeYesNoControl model =
  if not model.value then
    Nothing
  else
    Just <| Encode.bool model.value


encodeYesNoMaybeControl : YesNoMaybeModel -> Maybe Encode.Value
encodeYesNoMaybeControl model =
  Maybe.map Encode.bool model.value


-- Just <| Encode.object []

  -- if List.length ( Set.toList model.values ) == 0 then
  --   Nothing
  -- else
  --   Just <| Encode.list ( List.map Encode.string ( Maybe.withDefault [] <| Maybe.map Set.toList model.values ) )


  -- Just <| Maybe.withDefault Encode.null ( Maybe.map Encode.bool model.value )



    -- ( id, value ) =
    --   model.form |> Maybe.map zipper  ( zipForm mapper )
    --     -- (\ form ->
    --     --     applySectionZipper mapper ( form, [] )
    --     -- )
    --   |> Maybe.withDefault ( "infosec", Encode.null )

-- encodeMeta meta =
--   Encode.object
--     [ ( "visible", Encode.bool meta.visible )
--     ]

-- encodeBranch : ZipperState -> String -> List ( String, Encode.Value ) -> model -> Meta -> String -> ( model -> Maybe Encode.Value ) -> Maybe ( String, Encode.Value)
-- encodeBranch state id children model meta type_ encodeModel =
  -- let
  --   children_ =
  --     children
  --       -- |> List.map
  --       --     (\ ( id, value ) ->
  --       --         value
  --       --           |> Maybe.map (\ item -> ( id, item ) )
  --       --     )
  --       |> List.filterMap identity
  -- in


  --   |> List.filter
  --       (\ ( _, value ) ->
  --           value
  --             |> Maybe.map (\ _ -> True )
  --             |> Maybe.withDefault False
  --       )
  --   |> List.map (\ ( id, value ) -> ( id, Maybe.with))
  -- ( id
  -- , Encode.object children
  -- )


-- encodeControl : ZipperState -> String -> model -> Meta -> String -> ( model -> Maybe Encode.Value ) -> Maybe ( String, Encode.Value )
-- encodeControl state id model meta type_ encodeModel =
--   encodeModel model
--     |> Maybe.map
--         (\ value ->
--             ( id
--             , value
--             )
--         )



-- import Char exposing (..)
-- import Css exposing (..)
-- import Dict exposing (..)
-- import Html exposing (..)
-- import Html.Attributes as Attr
-- import MultiwayTree exposing (..)

-- import Json.Decode exposing (Decoder, list, string)
-- import Json.Decode.Pipeline exposing (required, decode, hardcoded)
-- import Json.Decode as Decode exposing (decodeValue)

-- import UI as UI
-- import UI.Input
-- import UI.FieldLabel
-- import UI.YesNo as UI


-- encodeCheckboxControl : CheckboxModel -> Encode.Value
-- encodeCheckboxControl model =
--   Encode.object
--     [ ( "selected", Encode.list ( List.map Encode.string ( Maybe.withDefault [] <| Maybe.map Set.toList model.values ) ) )
--     ]


-- encodeMultiUploadControl : MultiUploadModel -> Encode.Value
-- encodeMultiUploadControl model =
--   Encode.object
--     [ ( "files", Encode.list ( List.map Encode.string ( Set.toList model.values ) ) )
--     ]


-- encodeRadioControl : RadioModel -> Encode.Value
-- encodeRadioControl model =
--   Encode.object
--     [ ( "selected", Encode.string model.value )
--     ]


-- encodeTextInputControl : TextInputModel -> Encode.Value
-- encodeTextInputControl model =
--   Encode.object
--     [ ( "value", Encode.string model.value )
--     ]


-- encodeTextLabelControl : TextLabelModel -> Encode.Value
-- encodeTextLabelControl model =
--   Encode.object
--     [ ( "value", Encode.string model.value )
--     ]


-- encodeYesNoControl : YesNoModel -> Encode.Value
-- encodeYesNoControl model =
--   Encode.object
--     [ ( "value", Encode.bool model.value)
--     ]


-- encodeYesNoMaybeControl : YesNoMaybeModel -> Encode.Value
-- encodeYesNoMaybeControl model =
--   Encode.object
--     [ ( "value", Maybe.withDefault Encode.null ( Maybe.map Encode.bool model.value ))
--     ]


-- encodeBulletType : BulletTypes -> Encode.Value
-- encodeBulletType model =
--   case model of
--     AlphaBullets -> "
-- encodeBulletListControl : BulletListModel -> Encode.Value
-- encodeBulletListControl model =
--   Encode.object
--     [ ( "title", Encode.string model.title )
--     , ( "bulletType", Encode.string model.bulletType )
--     ]

  -- , children
  --   |> List.map
  --       (\ ( id, data ) ->
  --           ( id)
  --       )
  -- )
  -- Encode.object
  --   [ ( id, Encode.list children )
  --   ]
  -- Encode.object
  --   [ ( "type", Encode.string type_ )
  --   , ( "data", encodeModel model )
  --   , ( "meta", encodeMeta meta )
  --   , ( "children", Encode.list children )
  --   ]

  -- , Encode.object
  --     [ ( "type", Encode.string type_ )
  --     , ( "data", encodeModel model )
  --     -- , ( "meta", encodeMeta meta )
  --     ]
  -- )
  
  -- Encode.object
  --   [ ( id, Encode.object
  --       [ ( "type", Encode.string type_ )
  --       , ( "data", encodeModel model )
  --       -- , ( "meta", encodeMeta meta )
  --       ]
  --     )
  --   ]
