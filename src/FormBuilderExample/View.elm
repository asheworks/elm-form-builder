module FormBuilderExample.View exposing (..)

import Html exposing (..)

import FormBuilderExample.Model exposing (Command(..), Model)

import Renderers.UIRenderer exposing (..)

--

-- import FormBuilderExample.InfoSec as FormBuilder

--

view : Model -> Html Command
view model =
  Html.map FormBuilder_Command ( render model.formBuilder.state )
