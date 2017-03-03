module FormBuilderExample.View exposing (..)

import Html exposing (..)

--

import FormBuilderExample.InfoSec as FormBuilder

--
import FormBuilderExample.Style exposing (..)
import FormBuilderExample.Model exposing (Command(..), Model)

--

import FormBuilder as FormBuilder

import Renderers.Model exposing (..)
import Renderers.UIRenderer exposing (..)

--

{ id, class, classList } =
    cssNamespace

--

view : Model -> Html Command
view model =
  let
    meta =
      { visible = False
      }

    form =
      FormBuilder.toForm
      toDataType
      FormBuilder.infosec
      meta
  in
    Html.map FormBuilder_Command ( render form )


-- import Example.FormBuilder2 as FB2

-- import Example.FormBuilder as FormBuilder
-- import Example.Questionnaire exposing (..)
-- import UI as UI
-- import UI.Input
-- import UI.FieldLabel
-- -- import UI.LabeledInput as UI


-- view : Model -> Html Command
-- view model =
--     Html.map FormBuilder_Command <| FormBuilder.buildForm questionnaire sampleData


-- Html.map FormBuilder_Command form.view

-- formBuilder : Model -> Html Command
-- formBuilder model =
--     Html.map FormBuilder_Command
--         <| FormBuilder.buildForm
--             questionnaire
--             model.questionnaire.state

-- formBuilder2 : Model -> Html Command
-- formBuilder2 model =
--     Html.map 