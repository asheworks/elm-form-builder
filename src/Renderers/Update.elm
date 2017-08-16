module Renderers.Update
    exposing
        ( commandMap
        , eventMap
        )

import MultiwayTreeZipper exposing (..)
import Renderers.Model exposing (..)
import FormBuilder exposing (..)
import Set exposing (..)


commandMap : model -> Command -> Event
commandMap model command =
    case command of
        Checkbox_Update zipper value ->
            Checkbox_Updated zipper value

        TextInput_Update zipper value ->
            TextInput_Updated zipper value

        YesNo_Update zipper value ->
            YesNo_Updated zipper value

        YesNoMaybe_Update zipper value ->
            YesNoMaybe_Updated zipper value

        KeyDown_Update zipper keyCode ->
            KeyDown_Updated zipper keyCode



-- RadioField_Update id value ->
--   RadioField_Updated id value


eventMap :
    Model
    -> Event
    -> ( Model, Maybe Effect )
eventMap model event =
    let
        ( ( node, crumbs ) as zipper, map ) =
            case event of
                Checkbox_Updated zipper value ->
                    ( zipper, (setCheckbox value) )

                TextInput_Updated zipper value ->
                    ( zipper, (setTextInput value) )

                YesNo_Updated zipper value ->
                    ( zipper, (setYesNo value) )

                YesNoMaybe_Updated zipper value ->
                    ( zipper, (setYesNoMaybe value) )

                KeyDown_Updated zipper keyCode ->
                    ( zipper, identity )

        zipper_ =
            updateDatum
                (\(( node, ( model, meta ) ) as zipper) ->
                    let
                        model_ =
                            case model of
                                BranchModel m ->
                                    model

                                LeafModel m ->
                                    LeafModel (map m)
                    in
                        ( node, ( model_, meta ) )
                )
                zipper

        tree =
            zipper_
                |> Maybe.andThen goToRoot
                |> Maybe.map (\( tree, _ ) -> tree)
    in
        ( { model | form = tree }, Nothing )


setCheckbox :
    String
    -> LeafModels
    -> LeafModels
setCheckbox value leafModel =
    case leafModel of
        CheckboxControl ( model, meta ) ->
            CheckboxControl
                ( { model
                    | values =
                        model.values
                            |> Maybe.map
                                (\set ->
                                    if Set.member value set then
                                        Set.remove value set
                                    else
                                        Set.insert value set
                                )
                            |> Maybe.withDefault (Set.fromList [ value ])
                            |> Just
                  }
                , meta
                )

        _ ->
            leafModel


setTextInput :
    String
    -> LeafModels
    -> LeafModels
setTextInput value leafModel =
    case leafModel of
        TextInputControl ( model, meta ) ->
            TextInputControl
                ( { model | value = value }
                , meta
                )

        _ ->
            leafModel


setYesNo :
    Bool
    -> LeafModels
    -> LeafModels
setYesNo value leafModel =
    case leafModel of
        YesNoControl ( model, meta ) ->
            YesNoControl
                ( { model | value = value }
                , meta
                )

        _ ->
            leafModel


setYesNoMaybe :
    Bool
    -> LeafModels
    -> LeafModels
setYesNoMaybe value leafModel =
    case leafModel of
        YesNoMaybeControl ( model, meta ) ->
            YesNoMaybeControl
                ( { model | value = Just value }
                , meta
                )

        _ ->
            leafModel
