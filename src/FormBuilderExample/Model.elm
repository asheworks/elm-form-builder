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

import FormBuilder.Model as FormBuilder

-- import CQRS exposing (State)

--

-- import FormBuilder as FormBuilder

-- import Example.Questionnaire as Questionnaire
--

type alias ContextValues =
    {}


type alias Context =
    Maybe ContextValues


type alias Model =
    { --questionnaire : State Questionnaire.SampleData
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
    { --questionnaire = State Questionnaire.sampleData
    }
