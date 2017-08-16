module FormBuilderExample.View exposing (..)

import Html exposing (..)
import Renderers.DataEncoder exposing (..)
import Json.Encode as Encode


--
-- import Renderers.UIRenderer exposing (..)

import Renderers.CSVRenderer exposing (..)


--

import FormBuilderExample.Model exposing (Command(..), Model)
import InfoSecForm.VV1_0_0_Conditionals as Conditionals


--


view : Model -> Html Command
view model =
    let
        data_ =
            encodeForm model.formBuilder.state

        string_ =
            Encode.encode 0 data_

        view_ =
            render
                model.formBuilder.state
                Conditionals.mapper
                |> Maybe.map (Html.map FormBuilder_Command)
                |> Maybe.withDefault (div [] [])
    in
        view_



-- t = Debug.log "DATA" string_
-- Html.map FormBuilder_Command ( render model.formBuilder.state )
