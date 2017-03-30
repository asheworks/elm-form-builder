module Renderers.DataCompare exposing (..)

import MultiwayTree exposing (..)
import Set exposing (..)

import FormBuilder exposing (..)
import Renderers.Model exposing (..)

areEqual
  : Maybe RendererForm
  -> Maybe RendererForm
  -> Bool
areEqual left right =
  let
    -- t = Debug.log "LEFT FORM" left
    -- u = Debug.log "RIGHT FORM" right
    -- (Leaf "selected" [] (YesNo "" [])
    -- ,(LeafModel (YesNoControl ({ title = "", value = False },{ visible = True })),{ visible = True })
    -- )
    compares : Maybe ( List Bool )
    compares =
      Maybe.map2
        (\ formLeft formRight ->
          let
            listLeft : List ( RendererNode )
            listLeft = flatten formLeft
            
            listRight : List ( RendererNode )
            listRight = flatten formRight
          in
            if List.length listLeft == 0 || not ( List.length listLeft == List.length listRight ) then
              [ False ]
            else
              List.map2
                (\ ( leftNode, ( leftControl, leftMeta ) ) ( rightNode, ( rightControl, rightMeta ) ) ->
                  let
                    -- t = Debug.log "LEFT" leftNode
                    -- u = Debug.log "RIGHT" rightNode
                    nodesResult = areNodesEqual leftNode rightNode

                    controlResult = areControlsEqual leftControl rightControl
                      -- case ( leftNode, rightNode ) of
                      --   ( Branch _ _ _ _, Branch _ _ _ _ ) ->
                      --     True -- TODO: Check for disparate types / ids etc
                      --   ( Leaf _ _ _, Leaf _ _ _ ) ->
                      --     case ( leftControl, rightControl ) of
                      --       ( CheckboxControl ( leftModel, _ ), CheckboxControl ( rightModel, _ ) ) ->
                      --         True

                      --       ( _, _ ) -> False
                      --   ( _, _ ) ->
                      --     False
                      

                  in
                    nodesResult && controlResult
                )
                listLeft
                listRight
        )
        left
        right
  in
    compares
      |> Maybe.withDefault [ False ]
      |> List.all identity
    -- left_ = flatten left


areNodesEqual : RendererSections -> RendererSections -> Bool
areNodesEqual left right =
  case ( left, right ) of
    ( Branch leftId _ _ _, Branch rightId _ _ _ ) ->
      leftId == rightId -- TODO: Check for disparate types / ids etc
    ( Leaf leftId _ _, Leaf rightId _ _ ) ->
      leftId == rightId
      -- case ( leftControl, rightControl ) of
      --   ( CheckboxControl ( leftModel, _ ), CheckboxControl ( rightModel, _ ) ) ->
      --     True

      --   ( _, _ ) -> False
    ( _, _ ) ->
      False


areControlsEqual : Models -> Models -> Bool
areControlsEqual left right =
  case ( left, right ) of
    ( BranchModel leftBranch, BranchModel rightBranch ) ->
      True
    ( LeafModel leftLeaf, LeafModel rightLeaf ) ->
      case ( leftLeaf, rightLeaf ) of

        ( CheckboxControl ( leftModel, _ ), CheckboxControl ( rightModel, _ ) ) ->
          ( Maybe.map2
            (\ leftValue rightValue ->
              -- leftValue == rightValue
              ( 0 == 
                ( Set.diff leftValue rightValue
                  |> Set.toList
                  |> List.length
                )
              )
            )
            leftModel.values
            rightModel.values
          )
            |> Maybe.withDefault False
        
        ( MultiUploadControl ( leftModel, _ ), MultiUploadControl ( rightModel, _ ) ) ->
          ( 0 == 
            ( Set.diff leftModel.values rightModel.values
              |> Set.toList
              |> List.length
            )
          )

        ( RadioControl ( leftModel, _ ), RadioControl ( rightModel, _ ) ) ->
          leftModel.value == rightModel.value

        ( TextInputControl ( leftModel, _ ), TextInputControl ( rightModel, _ ) ) ->
          leftModel.value == rightModel.value
          
        ( TextLabelControl ( leftModel, _ ), TextLabelControl ( rightModel, _ ) ) ->
          leftModel.value == rightModel.value

        ( YesNoControl ( leftModel, _ ), YesNoControl ( rightModel, _ ) ) ->
          leftModel.value == rightModel.value

        ( YesNoMaybeControl ( leftModel, _ ), YesNoMaybeControl ( rightModel, _ ) ) ->
          ( Maybe.map2
            (\ leftValue rightValue -> leftValue == rightValue )
            leftModel.value
            rightModel.value
          )
            |> Maybe.withDefault False


        ( _, _ ) ->
          False
      -- True
    ( _, _ ) ->
      False