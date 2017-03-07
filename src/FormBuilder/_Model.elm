module FormBuilder.Model exposing
  ( Command(..)
  , Event(..)
  , Effect(..)

  , Model
  , defaultModel
  )

import CQRS exposing (State)

import FormBuilder exposing (..)

type Command
  = BoolData_Update String Bool
  | CheckboxData_Update ( Int, String )
  -- | OptionsData_Update String ( Int, String )
  -- | StringListData_Update 
  | TextData_Update String String
  

type Event
  = BoolData_Updated String Bool
  | CheckboxData_Updated ( Int,  String )
  | TextData_Updated String String
  -- | InputField_Updated String String
  -- | RadioField_Updated String (Int, String)

type Effect
  = None

type alias Model branch leaf data meta =
  { form : State ( Form branch leaf data meta )
  }


defaultModel
  : (meta -> Sections branch leaf meta -> data)
  -> Sections branch leaf meta
  -> meta
  -> { form : State ( Form branch leaf data meta ) }
defaultModel mapper def meta =
  { form = State <| toForm mapper def meta
  }