module FormBuilderExample.View exposing (..)

import Html exposing (..)

import FormBuilderExample.Model exposing (Command(..), Model)

import Renderers.UIRenderer exposing (..)
import Renderers.DataEncoder exposing (..)

import Json.Encode as Encode
--


view : Model -> Html Command
view model =
  let
    data_ = encode model.formBuilder.state

    string_ = Encode.encode 0 data_

    t = Debug.log "DATA" string_

    view_ =
      render model.formBuilder.state
        |> Maybe.map ( Html.map FormBuilder_Command )
        |> Maybe.withDefault ( div [][] )
  -- Html.map FormBuilder_Command ( render model.formBuilder.state )
  in
    view_
