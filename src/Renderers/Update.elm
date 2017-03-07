module Renderers.Update exposing
  ( commandMap
  , eventMap
  )


import CQRS exposing (State)

import FormBuilder exposing (..)
import Renderers.Model exposing (..)


commandMap : model -> Command -> Event
commandMap model command =
  case command of
    
    BoolData_Update path value ->
      BoolData_Updated path value

    CheckboxData_Update path value ->
      CheckboxData_Updated path value

    TextData_Update path value ->
      TextData_Updated path value

    -- CheckboxData_Update (index, value) ->
    --   CheckboxData_Updated (index, value)

    -- InputField_Update id value ->
    --   InputField_Updated id value
    
    -- RadioField_Update id value ->
    --   RadioField_Updated id value


type alias Mapper meta = ( RendererDataNode meta -> RendererDataNode meta )


eventMap
  : Model meta
  -> Event
  ->  ( Model meta
      , Maybe Effect
      )
eventMap model event =
  case event of

    BoolData_Updated path value ->
      ( updateNode model path <| setBoolData ( Just value )
      , Nothing
      )

    CheckboxData_Updated path value ->
      ( updateNode model path <| setOptionData value
      , Nothing
      )

    TextData_Updated path value ->
      ( updateNode model path <| setTextData ( Just value )
      , Nothing
      )


updateNode
  : Model meta
  -> String
  -> Mapper meta
  -> Model meta
updateNode model path mapper =
  model
  -- { model | form = State ( mapDataNodeByPath model.form.state path mapper )
  -- }


setBoolData
  : Maybe Bool
  -> Mapper meta
setBoolData value =
  let
    t = Debug.log "setBoolData" value
  in
  (\ node -> node
      -- { node | data =
      --     case node.data of
      --       BoolData dataValue ->
      --         BoolData
      --           { dataValue | value = value }
              
      --       _ -> node.data
      -- }
  )


setOptionData
  : String
  -> Mapper meta
setOptionData value =
  (\ node -> node
      -- { node | data =
      --     case node.data of
      --       OptionData dataValue ->
      --         OptionData
      --           { dataValue | value =
      --               dataValue.value
      --                 |> Maybe.map
      --                     (\ set ->
      --                         if Set.member value set then
      --                           Set.remove value set
      --                         else
      --                           Set.insert value set
      --                     )
      --                 |> Maybe.withDefault ( Set.fromList [ value ] )
      --                 |> Just
      --           }
              
      --       _ -> node.data
      -- }
  )


setTextData
  : Maybe String
  -> Mapper meta
setTextData value =
  (\ node -> node
      -- { node | data =
      --     case node.data of
      --       TextData dataValue ->
      --         TextData
      --           { dataValue | value = value }
              
      --       _ -> node.data
      -- }
  )


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
