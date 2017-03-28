module FormBuilderExample.View exposing (..)

import Html exposing (..)

import FormBuilderExample.Model exposing (Command(..), Model)

import Renderers.UIRenderer exposing (..)

--


view : Model -> Html Command
view model =
  render model.formBuilder.state
    |> Maybe.map ( Html.map FormBuilder_Command )
    |> Maybe.withDefault ( div [][] )
  -- Html.map FormBuilder_Command ( render model.formBuilder.state )
