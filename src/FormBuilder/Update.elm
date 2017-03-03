module FormBuilder.Update exposing
  ( commandMap
  , eventMap
  )

import FormBuilder exposing (..)
import FormBuilder.Model exposing (..)

commandMap : Form branch leaf data meta -> model -> Command -> Event
commandMap form model command =
  case command of
    
    BoolData_Update id value ->
      BoolData_Updated id value

    -- InputField_Update id value ->
    --   InputField_Updated id value
    
    -- RadioField_Update id value ->
    --   RadioField_Updated id value



-- eventMap : Form branch leaf data meta -> model -> Event -> ( model, Maybe Effect )
eventMap : Form branch leaf data meta -> model -> Event -> ( model, Maybe Effect )
eventMap form model event =
  let
    -- tree = toTree root
    model_ = case event of

    BoolData_Updated id value ->
      model
  in
    ( model, Nothing )

      -- updateTreeById tree id model
      --   (\ leaf ->
      --     case leaf of
      --       BoolField def -> def.set model value
      --       _ -> model
      --   )

  --   InputField_Updated id value ->
  --     updateTreeById tree id model
  --       (\ leaf ->
  --         case leaf of
  --           InputField def -> def.set model value
  --           _ -> model
  --       )

  --   RadioField_Updated id (index, value) ->
  --     updateTreeById tree id model
  --       (\ leaf ->
  --         case leaf of
  --           RadioField def -> --def.set model value
  --             if Set.member value (def.get model) then
  --               def.set model <| Set.remove value (def.get model)
  --             else
  --               def.set model <| Set.insert value (def.get model)
  --           _ -> model
  --       )
  -- in
  --   ( model_, Nothing )

