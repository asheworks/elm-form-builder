module FormBuilderExample.Update exposing (..)

import CQRS exposing (eventBinder)

--
-- import FormBuilderExample.InfoSec exposing (..)
import FormBuilderExample.Model exposing (..)

--

-- import FormBuilder exposing (..)
-- -- import FormBuilder.Model exposing (..)
-- import FormBuilder.Update exposing (..)
-- -- import Example.FormBuilder as FormBuilder
-- -- import Example.Questionnaire as Questionnaire
-- --
-- import FormBuilder as FormBuilder

-- import Renderers.Model exposing (..)
-- import Renderers.UIRenderer exposing (..)

decode : Context -> Model
decode =
  mapContext


encode : Model -> Context
encode _ =
  Nothing


init : Model -> ( Model, Maybe effect )
init model =
  ( model, Nothing )
  -- let
  --   meta =
  --     { visible = False
  --     }

  --   form =
  --     FormBuilder.toForm
  --     toDataType
  --     infosec
  --     meta

  --   model_ =
  --     { model | form = form
  --     }

  -- in
  --   ( model_, Nothing )


commandMap : Model -> Command -> Event
commandMap model command =
  case Debug.log "Example - CommandMap" command of
    FormBuilder_Command command_ -> Temp
      -- FormBuilder_Event <| commandMap model.form.state command_

-- FormBuilder_Event <| FormBuilder.commandMap Questionnaire.questionnaire model.questionnaire.state command_


eventMap : Model -> Event -> ( Model, Maybe Effect )
eventMap model event =
  case Debug.log "Example - EventMap" event of
    _ -> (model, Nothing)
-- FormBuilder_Event event_ ->
--     eventBinder
--         (FormBuilder.eventMap Questionnaire.questionnaire)
--         model.questionnaire
--         (\ state -> { model | questionnaire = state } )
--         FormBuilder_Effect
--         event_


