module FormBuilderExample.Stub exposing (main, stub)

import CQRS exposing (..)
import Html exposing (div)
import Html.Attributes as Attr
import Css exposing (..)

--

import FormBuilderExample.Model exposing (..)
import FormBuilderExample.Update as Example
import FormBuilderExample.View as Example

--

styles : List Mixin -> Html.Attribute msg
styles =
    asPairs >> Attr.style


main : Program Context Model Command
main =
  program stub

--

stub : Definition Context Model Command Event Effect
stub =
  { decode = Example.decode
  , encode = Example.encode
  , init = init
  , view = (\model -> Example.view model |> view)
  , commandMap = Example.commandMap
  , eventMap = Example.eventMap
  , eventHandler = eventHandler
  , subscriptions = subscriptions
  }

view : Html.Html command -> Html.Html command
view child =
    child
    -- div
    --     [ styles
    --         [ backgroundColor (hex "#EEE")
    --         , property "width" "calc(100vw - 150px)"
    --         , property "height" "calc(100vh - 150px)"
    --         , minWidth (px 800)
    --         , minHeight (px 600)
    --         , marginLeft (px 25)
    --         , marginTop (px 25)
    --         , padding (px 50)
    --         , overflow scroll
    --         ]
    --     ]
    --     [ child
    --     ]


eventHandler : ( Model, Effect ) -> Cmd msg
eventHandler ( model, effect ) =
    case Debug.log "Example eventHandler" effect of
        _ -> Cmd.none
        
subscriptions : Model -> Sub Command
subscriptions model =
    Sub.none

init : Model -> ( Model, Maybe Effect )
init model =
    Example.init model
