module Renderers.Update exposing
  ( commandMap
  , eventMap
  )


-- import CQRS exposing (State)

-- import FormBuilder exposing (..)
import Renderers.Model exposing (..)

import Set exposing (..)

commandMap : model -> Command -> Event
commandMap model command =
  case command of
    
    Checkbox_Update path value ->
      Checkbox_Updated path value

    TextInput_Update path value ->
      TextInput_Updated path value
    
    YesNo_Update path value ->
      YesNo_Updated path value

    YesNoMaybe_Update path value ->
      YesNoMaybe_Updated path value

    -- RadioField_Update id value ->
    --   RadioField_Updated id value





eventMap
  : Model meta
  -> Event
  ->  ( Model meta
      , Maybe Effect
      )
eventMap model event =
  case event of

    Checkbox_Updated path value ->
      ( updateNode model path <| setLeaf <| setCheckbox value
      , Nothing
      )

    TextInput_Updated path value ->
      ( updateNode model path <| setLeaf <| setTextInput value
      , Nothing
      )

    YesNo_Updated path value ->
      ( updateNode model path <| setLeaf <| setYesNo value
      , Nothing
      )

    YesNoMaybe_Updated path value ->
      ( updateNode model path <| setLeaf <| setYesNoMaybe value
      , Nothing
      )


setCheckbox
  : String
  -> LeafModels meta
  -> LeafModels meta
setCheckbox value leafModel =
  case leafModel of
    CheckboxControl dataValue ->
      let
        model_ = dataValue.model
      in
        CheckboxControl
          { dataValue | model =
              { model_ | values =
                  model_.values
                    |> Maybe.map
                        (\ set ->
                            if Set.member value set then
                              Set.remove value set
                            else
                              Set.insert value set
                        )
                    |> Maybe.withDefault ( Set.fromList [ value ] )
                    |> Just
              }
          }
    _ -> leafModel


setTextInput
  : String
  -> LeafModels meta
  -> LeafModels meta
setTextInput value leafModel =
  case leafModel of
    TextInputControl dataValue ->
      let
        model_ = dataValue.model
      in
        TextInputControl
          { dataValue | model =
              { model_ | value = value
              }
          }
    _ -> leafModel


setYesNo
  : Bool
  -> LeafModels meta
  -> LeafModels meta
setYesNo value leafModel =
  case leafModel of
    YesNoControl dataValue ->
      let
        model_ = dataValue.model
      in
        YesNoControl
          { dataValue | model =
              { model_ | value = value
              }
          }
    _ -> leafModel



setYesNoMaybe
  : Bool
  -> LeafModels meta
  -> LeafModels meta
setYesNoMaybe value leafModel =
  case leafModel of
    YesNoMaybeControl dataValue ->
      let
        model_ = dataValue.model
      in
        YesNoMaybeControl
          { dataValue | model =
              { model_ | value = Just value
              }
          }
    _ -> leafModel
