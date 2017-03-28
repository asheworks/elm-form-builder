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

-- formDef = infosec

import FormBuilderExample.Sample exposing (..)

formDef = sample

--

type alias ContextValues =
  {}


type alias Context =
  Maybe ContextValues


type Command
  = FormBuilder_Command Renderers.Command


type Event
  = FormBuilder_Event Renderers.Event


type Effect
  = FormBuilder_Effect Renderers.Effect
    

type alias Model =
  { formBuilder : State ( Renderers.Model )
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
      -> Renderers.RendererSections
      -> Renderers.Models
      )
    mapper = Renderers.toDataValue

    node : Renderers.RendererSections
    node = formDef

    meta : Renderers.Meta
    meta =
      { visible = True
      }


    model : Renderers.Model
    model = { form = Just <| toTree mapper meta node }
  in
    { formBuilder = State <| model
    }
