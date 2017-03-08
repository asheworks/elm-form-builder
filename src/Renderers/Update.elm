module Renderers.Update exposing
  ( commandMap
  , eventMap
  )


import CQRS exposing (State)

import FormBuilder exposing (..)
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

    -- CheckboxData_Update (index, value) ->
    --   CheckboxData_Updated (index, value)

    -- InputField_Update id value ->
    --   InputField_Updated id value
    
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
      ( updateNode model path <| setLeaf <| setTextInput value--( Just value )
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


-- setYesNoMaybe
--   : Bool
--   -> Mapper meta
-- setYesNoMaybe value =
--   let
--     t = Debug.log "setBoolData" value
--   in
--   (\ node -> --node
--     let
--       model = node.model
--     in
      
--       { node | model =
--         case node.model of
--           LeafModel leafModel ->
--             let
--               v = Debug.log "found leaf model" leafModel
--             in
--             case leafModel of
--               YesNoMaybeControl dataValue ->
--                 let
--                   model_ = dataValue.model

--                   u = Debug.log "found model" model_
--                 in
--                   LeafModel <| YesNoMaybeControl

--                     { dataValue | model =
--                         { model_ | value = Just value
--                         }
--                     }
--               _ -> node.model
--           _ -> node.model
--       }
--       -- { node | data =
--       --     case node.data of
--       --       BoolData dataValue ->
--       --         BoolData
--       --           { dataValue | value = value }
              
--       --       _ -> node.data
--       -- }
--   )


-- eventMap
--   : Model ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta
--   -> Event
--   ->  ( Model ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta
--       , Maybe Effect
--       )
-- eventMap model event =
--   case event of

--     BoolData_Updated path value ->
--       ( updateNode model path <| setBoolData ( Just value )
--       , Nothing
--       )

--     CheckboxData_Updated path value ->
--       ( updateNode model path <| setOptionData value
--       , Nothing
--       )

--     TextData_Updated path value ->
--       ( updateNode model path <| setTextData ( Just value )
--       , Nothing
--       )


-- updateNode
--   : Model ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta
--   -> String
--   -> ( DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta -> DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta )
--   -> Model ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta
-- updateNode model path mapper =
--   { model | form = State ( mapDataNodeByPath model.form.state path mapper )
--   }


-- setBoolData
--   : Maybe Bool
--   -> ( DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta -> DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta )
-- setBoolData value =
--   let
--     t = Debug.log "setBoolData" value
--   in
--   (\ node ->
--       { node | data =
--           case node.data of
--             BoolData dataValue ->
--               BoolData
--                 { dataValue | value = value }
              
--             _ -> node.data
--       }
--   )


-- setOptionData
--   : String
--   -> ( DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta -> DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta )
-- setOptionData value =
--   (\ node ->
--       { node | data =
--           case node.data of
--             OptionData dataValue ->
--               OptionData
--                 { dataValue | value =
--                     dataValue.value
--                       |> Maybe.map
--                           (\ set ->
--                               if Set.member value set then
--                                 Set.remove value set
--                               else
--                                 Set.insert value set
--                           )
--                       |> Maybe.withDefault ( Set.fromList [ value ] )
--                       |> Just
--                 }
              
--             _ -> node.data
--       }
--   )


-- setTextData
--   : Maybe String
--   -> ( DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta -> DataNode ( BranchFacts meta ) ( LeafFacts meta ) ( DataTypes meta ) meta )
-- setTextData value =
--   (\ node ->
--       { node | data =
--           case node.data of
--             TextData dataValue ->
--               TextData
--                 { dataValue | value = value }
              
--             _ -> node.data
--       }
--   )
