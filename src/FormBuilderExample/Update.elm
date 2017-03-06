module FormBuilderExample.Update exposing (..)

import CQRS exposing (eventBinder)

import FormBuilderExample.Model exposing (..)
import Renderers.Update as FormBuilder

--

decode : Context -> Model
decode =
  mapContext


encode : Model -> Context
encode _ =
  Nothing


init : Model -> ( Model, Maybe effect )
init model =
  ( model, Nothing )


commandMap : Model -> Command -> Event
commandMap model command =
  case Debug.log "Example - CommandMap" command of
    FormBuilder_Command command_ ->
      FormBuilder_Event <| FormBuilder.commandMap model.formBuilder command_


eventMap : Model -> Event -> ( Model, Maybe Effect )
eventMap model event =
  case Debug.log "Example - EventMap" event of
    FormBuilder_Event event_ ->
      eventBinder
        FormBuilder.eventMap
        model.formBuilder
        (\ state -> { model | formBuilder = state } )
        FormBuilder_Effect
        event_
