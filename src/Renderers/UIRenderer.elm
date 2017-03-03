module Renderers.UIRenderer exposing
  ( render
  )

import FormBuilder exposing (..)
import FormBuilder.Model exposing (..)
import Renderers.Model exposing (..)

import Html exposing (..)
import MultiwayTree exposing (..)


render :
  Form ContainerFacts (DataFacts branch meta) (DataTypes meta) meta ->
  Html Command
render form =
  foldl
    (\ node children ->
      case node of
        Branch _ _ _ _ ->
          [ div [] ( children ++ [ renderNode node ] ) ]
        Leaf _ _ _ ->
          children ++ [ renderNode node ]
    )
    []
    form.def
    |> div []

renderNode : Sections a (DataFacts branch meta) meta1 -> Html Command
renderNode node =
      case node of
        Branch id container mods_ _ ->
          let
            t = Debug.log "Section" container
          in
            div [] [ text <| "Section: " ++ (Maybe.withDefault "-" id) ]

        Leaf id leaf mods_ ->
          let
            id_ = Maybe.withDefault "" id
          in
            case leaf of

              Bool mods control ->
                let
                  t = Debug.log "Bool" control
                in
                  div [] [ text <| "Bool: " ++ id_ ]

              FileUpload mods control ->
                let
                  t = Debug.log "FileUpload" control
                in
                  div [] [ text <| "FileUpload: " ++ id_ ]

              Option mods values control ->
                let
                  t = Debug.log "Options" control
                in
                  div [] [ text <| "Options: " ++ id_ ]

              Text mods control ->
                let
                  t = Debug.log "Text" control
                in
                  div [] [ text <| "Text: " ++ id_ ]