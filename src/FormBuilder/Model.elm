module FormBuilder.Model exposing
  ( Command(..)
  , Event(..)
  , Effect(..)
  )

type Command
  = BoolData_Update String Bool
  -- | OptionsData_Update String ( Int, String )
  -- | StringListData_Update 
  -- | TextData_Update String String
  

type Event
  = BoolData_Updated String Bool
  -- | InputField_Updated String String
  -- | RadioField_Updated String (Int, String)

type Effect
  = None
