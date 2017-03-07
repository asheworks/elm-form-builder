module FormBuilderExample.Model
    exposing
        ( Command(..)
        , Event(..)
        , Effect(..)
        , Context
        , ContextValues
        , Model
        , mapContext
        , mapValues
        , defaultModel
        )

--

import CQRS exposing (State)

import FormBuilder exposing (..)

import Renderers.Model as Renderers

-- import FormBuilderExample.InfoSec exposing (..)
import FormBuilderExample.Sample exposing (..)

--

type alias ContextValues =
  {}


type alias Context =
  Maybe ContextValues


-- type alias Meta =
--   { visible : Bool
--   }


type Command
  = FormBuilder_Command Renderers.Command


type Event
  = FormBuilder_Event Renderers.Event


type Effect
  = FormBuilder_Effect Renderers.Effect
    

type alias Model =
  { formBuilder : State ( Renderers.Model Renderers.Meta )
  }


mapContext : Context -> Model
mapContext context =
  Maybe.withDefault
    {}
    context
    |> mapValues


mapValues : ContextValues -> Model
mapValues values =
  defaultModel


defaultModel : Model
defaultModel =
  let
    mapper :
      ( Renderers.Meta
      -> Renderers.RendererSections Renderers.Meta
      -> Renderers.Models Renderers.Meta
      -- -> DataValue Renderers.Models Meta
      )
    mapper = Renderers.toDataValue

    node : Renderers.RendererSections Renderers.Meta
    node = sample

    meta : Renderers.Meta
    meta =
      { visible = True
      }


    form : Renderers.Model Renderers.Meta
    form = toForm mapper node meta
  in
    { formBuilder = State <| form
    }
