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
-- import FormBuilder.Model as FormBuilder

import Renderers.Model as FormBuilder
--exposing (..)

import FormBuilderExample.InfoSec exposing (..)

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
  { formBuilder : State (
      FormBuilder.Model
        ( FormBuilder.ContainerFacts Meta )
        ( FormBuilder.DataFacts Meta )
        ( FormBuilder.DataTypes Meta )
        Meta
    )
  }

    -- { form : State ( Form ContainerFacts ( DataFacts ContainerFacts Meta ) ( DataTypes Meta ) Meta )
    --      --questionnaire : State Questionnaire.SampleData
    -- }


type Command
  = FormBuilder_Command FormBuilder.Command


type Event
  = FormBuilder_Event FormBuilder.Event


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
  { formBuilder = State <| FormBuilder.defaultModel
      FormBuilder.toDataType
      infosec
      { visible = False
      }
  }
  -- let
  --   meta =
  --     { visible = False
  --     }

  --   form =
  --     FormBuilder.toForm
  --     toDataType
  --     infosec
  --     meta
  -- in
  --   { form = State form
  --   }
    -- { --questionnaire = State Questionnaire.sampleData
    -- }
