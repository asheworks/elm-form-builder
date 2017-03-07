module Renderers.UIRenderer exposing
  ( render
  )

import Char exposing (..)
import Css exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)

import FormBuilder exposing (..)
import Renderers.Model exposing (..)

import UI as UI
import UI.Input
import UI.FieldLabel
import UI.YesNo as UI


styles : List Mixin -> Html.Attribute msg
styles =
    asPairs >> Attr.style


render
  : Model Meta
  -> Html Command
render form =
  applySectionZipper
    ( renderNode form )
    form.sections


checkVisible
  : Html Command
  -> DataValue model Meta
  -> Html Command
  -> Html Command
checkVisible placeholder data control =
  if data.meta.visible then
    control
  else
    placeholder



renderNode
  : Model Meta
  -> ZipperState
  -> String
  -> RendererZipper Meta
  -> List ( Html Command )
  -> Html Command
renderNode form state id zipper children =
  let
    path = appendPath state.path id

    dataNode = getDataNodeByPath form path

    mod =
      checkVisible
        <| div []
        <| [ Html.text "HIDDEN" ] ++ children

    t = Debug.log "renderNode" dataNode
  in
    case dataNode of
      Nothing -> div [] children
      Just dataNode_ ->
      
        case dataNode_ of

          BranchModel branch ->

            case branch of

              BulletListControl data ->
                mod data <| bullets path data.model.title zipper children

              GridControl data ->
                mod data <| grid path data.model.title children

              HeaderControl data ->
                mod data <| header path data.model.title children

              LabeledSectionControl data ->
                mod data <| header path data.model.title children

              OrderedListControl data ->
                mod data <| orderedList path data.model.title False children

          LeafModel leaf ->

            case leaf of

              CheckboxControl data ->
                mod data <| checkbox path ( Dict.fromList data.model.options ) data.model.values

              MultiUploadControl data ->
                mod data <|
                  (
                    ( Set.toList data.model.values ) ++ [ "FileUpload: " ++ path ]
                        |> List.map Html.text
                        |> div []
                  )

              RadioControl data ->
                mod data <| checkbox path ( Dict.fromList data.model.options ) Set.empty--[ model.value ]

              TextInputControl data ->
                mod data <| textInput path data.model.title data.model.value data.model.placeholder False

              TextLabelControl data ->
                mod data <| textLabel path data.model.title data.model.value False

              YesNoControl data ->
                mod data <| bool path data.model.value

              YesNoMaybeControl data ->
                mod data <| bool path <| Maybe.withDefault False data.model.value


bool : String -> Bool -> Html Command
bool id value =
  UI.yesNoField
    { id = id
    , yesLabel = "Yes"
    , noLabel = "No"
    , value = value
    , onChange = BoolData_Update
    }


bullets
  : String
  -> String
  -> RendererZipper meta
  -> List ( Html Command )
  -> Html Command
bullets id title zipper children =
  div
    [ styles
        [ paddingLeft (px 15)
        , marginTop (px 20)
        , marginBottom (px 20)
        , borderLeft3 (px 1) solid (rgba 128 128 128 0.1)
        ]
    ]
    [ span
        [ styles
            [ displayFlex
            -- , paddingRight (px 10)
            , paddingBottom (px 5)
            -- , fontSize (Css.em 1.3)
            , borderBottom3 (px 1) solid (rgba 128 128 128 0.3)
            ]
        ]
        [ span
          [ styles
              [ paddingRight (px 10)
              , fontSize (Css.em 1.3)
              , fontWeight bold
              ]
          ]
          [ Html.text <| (bulletString zipper)
          ]
        , span
          [ styles
              [ flex (int 1)
              , fontSize (Css.em 1.3)
              ]
          ]
          [ Html.text title
          ]
        ]
    , div
        [ styles
            [ paddingTop (px 20)
            , paddingLeft (px 10)
            ]
        ]
        children
    ]

checkbox : String -> Dict String String -> Set String -> Html Command
checkbox id options selected =
  UI.checkboxControl
    { id = id
    , values = options
      |> Dict.toList
      |> List.map
        (\ (key, value) ->
            { key = key
            , value = value
            , checked = Set.member key selected
            , error = Nothing
            }
        )
    , error = Nothing
    , onSelect = CheckboxData_Update
    }


grid
  : String
  -> String
  -> List (Html Command)
  -> Html Command
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


header
  : String
  -> String
  -> List ( Html Command )
  -> Html Command
header id title children =
  UI.formControl
    { id = id
    , header = Just
      [ Html.text title
      ]
    , section = Just
        [ div
            [ styles
                [ paddingLeft (px 10)
                , paddingRight (px 10)
                ]
            ]
            children
        ]
    , aside = Nothing
    , footer = Nothing
    }


orderedList
  : String
  -> String
  -> Bool
  -> List ( Html Command )
  -> Html Command
orderedList id title error children =
  UI.FieldLabel.view
    { id = id
    , label = title
    , error = error
    }
    [ div
        [ styles
          [ displayFlex
          , flex (int 1)
          , flexDirection column
          , display block
          ]
        ] <|
        List.map (\child ->
            div
              [ styles
                  [ float left
                  , displayFlex
                  , width (pct 100)
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
          ) children
    ]

textInput
  : String
  -> String
  -> String
  -> String
  -> Bool
  -> Html Command
textInput id label value placeholder error =
  UI.FieldLabel.view
    { id = id
    , label = label
    , error = error
    }
    [ UI.Input.view
      { id = id
      , placeholder = placeholder
      , inputType = UI.Input.TextField
      , value = value
      , error = False
      , onInput = TextData_Update
      }
    ]


textLabel
  : String
  -> String
  -> String
  -> Bool
  -> Html Command
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


bulletString
  : RendererZipper meta
  -> String
bulletString ctx =
  let
    countPrevZipper ( ( alpha, numeric ) as count ) ( ( subtree, crumbs ) as zipper ) =
      case goLeft zipper of
        Nothing -> count
        Just ( ( ( ( Tree node children ) as subtree_ ), crumbs_ ) as prev ) ->
          let
            count_ = case node of
              Branch _ container _ _ ->

                case container of
                  BulletList mods types _ ->

                    case types of
                      AlphaBullets -> (alpha + 1, numeric)
                      NumericBullets -> (alpha, numeric + 1)

                  _ -> count

              _ -> count
            
          in
            countPrevZipper count_ prev

    bulletStringZipper depth ( ( ( ( Tree node children ) as subtree ), crumbs ) as zipper ) =
      case node of
        Branch _ container _ _ ->
          case container of
            BulletList mods types _ ->
              let
                (alpha, numeric) = countPrevZipper (0, 0) zipper
              in
                case types of
                  AlphaBullets -> String.fromChar <| fromCode (alpha + 65)
                  NumericBullets -> (toString <| numeric + 1)
            
            _ -> ""

        _ -> ""

    applyZipper depth label ( ( subtree, crumbs ) as zipper ) =
      let
        bulletLabel = (bulletStringZipper depth zipper)
        label_ = bulletLabel ++
          if label == "" then
            ""
          else
            if String.startsWith "." label then
              label
            else
              "." ++ label
      in
        case goUp zipper of
          Nothing -> label
          Just parent ->
              applyZipper (depth + 1) label_ parent
  in
    applyZipper 0 "" ctx
