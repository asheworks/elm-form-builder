module FormBuilderExample.Update exposing (..)

import CQRS exposing (eventBinder, State)

import FormBuilderExample.Model exposing (..)
import Renderers.Update as FormBuilder

import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)

import Renderers.DataDecoder as DataDecoder
--

decode : Context -> Model
decode =
  mapContext


encode : Model -> Context
encode _ =
  Nothing


init : Model -> ( Model, Maybe effect )
init model =
  let
    data = Decode.decodeString Decode.value "{\"infosec\":{\"details\":{\"companyName\":\"a\",\"contactName\":\"a\"},\"services\":{\"offered\":{\"selected\":[\"other\"],\"description\":\"asdf\"}}}}"
    data_ = Result.withDefault ( Encode.string "fail" ) data
    t = Debug.log "Decode Data" data_

    model_ = DataDecoder.decodeForm data_ model.formBuilder.state
      -- case DataDecoder.decode data model.formBuilder.state of
      --   Nothing -> model.formBuilder.state
      --   Just result -> result
    -- {"infosec":{"details":{"companyName":"a","contactName":"a"},"services":{"offered":{"selected":["other"],"description":"asdf"}}}}
    -- model_ = model
    -- u = Debug.log "Decoded Data" ( Maybe.map (\ ( node, _ ) -> node ) model_.form )
  in
    ( { model | formBuilder = State model_ }, Nothing )


commandMap : Model -> Command -> Event
commandMap model command =
  -- case Debug.log "Example - CommandMap" command of
  case command of
    FormBuilder_Command command_ ->
      FormBuilder_Event <| FormBuilder.commandMap model.formBuilder command_


eventMap : Model -> Event -> ( Model, Maybe Effect )
eventMap model event =
  -- case Debug.log "Example - EventMap" event of
  case event of
    FormBuilder_Event event_ ->
      eventBinder
        FormBuilder.eventMap
        model.formBuilder
        (\ state -> { model | formBuilder = state } )
        FormBuilder_Effect
        event_
