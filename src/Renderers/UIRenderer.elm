module Renderers.UIRenderer exposing
  ( render
  )

import FormBuilder exposing (..)
import FormBuilder.Model exposing (..)
import Renderers.Model exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Css exposing (..)
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)

import UI as UI
-- import UI.Checkbox
import UI.Input
import UI.FieldLabel
-- import UI.LabeledInput as UI
import UI.YesNo as UI


styles : List Mixin -> Html.Attribute msg
styles =
    asPairs >> Attr.style


render
  : Form ContainerFacts (DataFacts ContainerFacts meta) (DataTypes meta) meta
  -> Html Command
render form =
  let
    applyZipper depth index ( ( ( ( Tree node children ) as subtree ), crumbs ) as zipper ) =
      case node of  
        Leaf _ _ _ ->
          renderNode form zipper []

        Branch _ _ _ _ ->
          children
            |> List.indexedMap
                (\ index_ _ ->
                    goToChild index_ zipper
                      |> Maybe.map ( applyZipper ( depth + 1 ) index_ )
                )
            |> keepJusts
            |> renderNode form zipper
  in
    applyZipper 0 0 ( form.def, [] )

  -- MultiwayTree.foldl
  --   (\ node children ->
  --     case node of
  --       Branch _ _ _ _ ->
  --         [ renderNode node children ]
  --       Leaf _ _ _ ->
  --         children ++ [ renderNode node [] ]
  --   )
  --   []
  --   form.def
  --   |> div []


renderNode
  : Form ContainerFacts (DataFacts ContainerFacts meta) (DataTypes meta) meta
  -> Zipper ( Sections ContainerFacts (DataFacts ContainerFacts meta) meta )
  -> List ( Html Command )
  -> Html Command
renderNode form ( ( ( Tree node children_ ) as subtree ), crumbs ) children =
      case node of
        Branch id container mods_ _ ->
          let
            id_ = Maybe.withDefault "" id
          in
            case container of

              BulletList types title ->
                header id_ title children

              Grid ->
                grid id_ "grid" children
              Header title ->
                header id_ title children

              LabeledSection title ->
                header id_ title children

              OrderedList title ->
                header id_ title children


        Leaf id leaf mods_ ->
          let
            id_ = Maybe.withDefault "" id
          in
            case leaf of

              Bool mods control ->
                bool id_

              FileUpload mods control ->
                let
                  t = Debug.log "FileUpload" control
                in
                  div [] [ Html.text <| "FileUpload: " ++ id_ ]

              Option mods values control ->
                checkbox id_ ( Dict.fromList [ ( "one", "one" ) ] )

              Text mods control ->
                case control of
                  TextInput title ctrlMods ->
                    textInput id_ title False ""

                  TextLabel title ->
                    textLabel id_ title "" False


bool : String -> Html Command
bool id =
  UI.yesNoField
    { id = id
    , yesLabel = "Yes"
    , noLabel = "No"
    , value = False
    , onChange = BoolData_Update
    }


checkbox : String -> Dict String String -> Html Command
checkbox id options =
  UI.checkboxControl
    { id = id
    , values = options
      |> Dict.toList
      |> List.map
        (\ (key, value) ->
            { key = key
            , value = value
            , checked = Set.member key (Set.fromList [ "a" ])--(def.get model)
            , error = Nothing
            }
        )
    , error = Nothing
    , onSelect = CheckboxData_Update-- (0, id)
    }


grid : String -> String -> List (Html Command) -> Html Command
grid id title children =
  UI.FieldLabel.view
    { id = id
    , label = title
    , error = False
    }
    [ div
      [ styles
        [ displayFlex
        , flex (int 1)
        , flexDirection column
        , display block
        ]
      ] <|
      List.map
        (\ child ->
            div
              [ styles
                [ float left
                , displayFlex
                , width (pct 50)
                ]
              ]
              [ span
                [ styles
                  [ padding (px 10)
                  , width (pct 100)
                  ]
                ]
                [ child
                ]
              ]
        )
        children
    ]


header : String -> String -> List (Html Command) -> Html Command
header id title children =
  UI.formControl
    { id = id
    , header = Just
      [ Html.text title
      ]
    , section = Just children
    , aside = Nothing
    , footer = Nothing
    }


textInput : String -> String -> Bool -> String -> Html Command
textInput id label error placeholder =
  UI.FieldLabel.view
    { id = id
    , label = label
    , error = error
    }
    [ UI.Input.view
      { id = id
      -- , label = def.label
      , placeholder = placeholder
      , inputType = UI.Input.TextField
      , value = "asdf"--(def.get model)
      , error = False
      , onInput = TextData_Update -- (\ a b -> "" )--InputField_Update
      }
    ]


textLabel : String -> String -> String-> Bool -> Html Command
textLabel id label text error =
  UI.FieldLabel.view
    { id = id
    , label = label
    , error = error
    }
    [ span
        [
        ]
        [ Html.text text
        ]
    ]