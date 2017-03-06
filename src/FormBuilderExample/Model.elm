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

import FormBuilderExample.InfoSec exposing (..)

import FormBuilder exposing (..)
import FormBuilder.Model as FormBuilder

import Renderers.Model exposing (..)

-- import CQRS exposing (State)

--

-- import FormBuilder as FormBuilder

-- import Example.Questionnaire as Questionnaire
--

type alias ContextValues =
    {}


type alias Context =
    Maybe ContextValues


type alias Meta =
    { visible : Bool
    }

type alias Model =
    { form : Form ContainerFacts ( DataFacts ContainerFacts Meta ) ( DataTypes Meta ) Meta
         --questionnaire : State Questionnaire.SampleData
    }


type Command
    = FormBuilder_Command FormBuilder.Command

type Event
    = FormBuilder_Event FormBuilder.Event
    | Temp


type Effect
    = FormBuilder_Effect FormBuilder.Effect
    

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
    meta =
      { visible = False
      }

    form =
      FormBuilder.toForm
      toDataType
      infosec
      meta
  in
    { form = form
    }
    -- { --questionnaire = State Questionnaire.sampleData
    -- }
