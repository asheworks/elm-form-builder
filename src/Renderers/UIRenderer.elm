module Renderers.UIRenderer exposing
  ( render
  )

import FormBuilder exposing (..)
-- import FormBuilder.Model exposing (..)
import Renderers.Model exposing (..)

import Char exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Css exposing (..)
import MultiwayTree exposing (..)
import MultiwayTreeZipper exposing (..)
import Set exposing (..)

import UI as UI
import UI.Input
import UI.FieldLabel
import UI.YesNo as UI


styles : List Mixin -> Html.Attribute msg
styles =
    asPairs >> Attr.style


render
  : Form
      ( ContainerFacts meta )
      ( DataFacts meta )
      ( DataTypes meta )
      meta
  -> Html Command
render form =
  applySectionZipper ( renderNode form ) form.sections


renderNode
  : Form ( ContainerFacts meta ) ( DataFacts meta ) ( DataTypes meta ) meta
  -> ZipperState
  -> Maybe String
  -> Zipper ( Sections ( ContainerFacts meta ) ( DataFacts meta ) meta )
  -> List ( Html Command )
  -> Html Command
renderNode form state id ( ( ( ( Tree node children_ ) as subtree ), crumbs ) as zipper ) children =
  let
    path = appendPath state.path id

    dataNode = getDataNodeByPath form path
  in
    case node of
      Branch _ container mods_ _ ->
        case container of

          BulletList mods types title ->
            bullets path title zipper children

          Grid mods ->
            grid path "" children

          Header mods title ->
            header path title children

          LabeledSection mods title ->
            header path title children

          OrderedList mods title ->
            orderedList path title False children

      Leaf _ leaf mods_ ->
        case leaf of

          Bool mods control ->
            let
              value = getBoolData dataNode
            in
            bool path value

          FileUpload mods control ->
            let
              values = getListStringData dataNode
            in
              values ++ [ "FileUpload: " ++ path ]
                  |> List.map Html.text
                  |> div []

          Option mods values control ->
            let
              options = Dict.fromList values

              value = getOptionData dataNode
            in
              checkbox path options value

          Text mods control ->
            let
              value = getDataNodeByPath form path |> getTextData
            in
              case control of
                TextInput title ctrlMods ->
                  let
                    state =
                      List.foldl
                        (\ mod state -> mod state )
                        defaultTextInputModel
                        ctrlMods

                    placeholder = Maybe.withDefault "" state.placeholder
                  in
                    textInput path title value placeholder False

                TextLabel title ->
                  textLabel path title value False


-- type DataTypes meta
--   = Meta meta
--   | BoolData ( DataValue meta Bool )
--   | ListStringData ( DataValue meta ( List String )  )
--   | OptionData ( DataValue meta ( Set String ) )
--   | TextData ( DataValue meta String )

getBoolData
  : Maybe ( DataTypes mdeta )
  -> Bool
getBoolData dataNode =
  case dataNode of
    Nothing -> False
    Just data ->
      case data of
        BoolData dataValue ->
          Maybe.withDefault
            ( Maybe.withDefault
                False
                dataValue.default
            )
            dataValue.value
        _ -> False


getListStringData
  : Maybe ( DataTypes mdeta )
  -> List String
getListStringData dataNode =
  case dataNode of
    Nothing -> [ " ** NOT FOUND ** " ]
    Just data ->
      case data of
        ListStringData dataValue ->
          Maybe.withDefault
            ( Maybe.withDefault
                [ " ** NOT SET ** " ]
                dataValue.default
            )
            dataValue.value
        _ -> [ " ** INVALID TYPE ** " ]


getOptionData
  : Maybe (DataTypes meta)
  -> Set String
getOptionData dataNode =
  case dataNode of
    Nothing ->
      Set.empty
    Just data ->
      case data of
        OptionData dataValue ->
          Maybe.withDefault
            ( Maybe.withDefault
                Set.empty
                dataValue.default
            )
            dataValue.value
        _ ->
          Set.empty


getTextData
  : Maybe ( DataTypes mdeta )
  -> String
getTextData dataNode =
  case dataNode of
    Nothing -> " ** NOT FOUND ** "
    Just data ->
      case data of
        TextData dataValue ->
          Maybe.withDefault
            ( Maybe.withDefault
                ""
                dataValue.default
            )
            dataValue.value
        _ -> " ** INVALID TYPE ** "


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
  -> Zipper ( Sections ( ContainerFacts meta ) (DataFacts meta) meta )
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
            , checked = Set.member key selected--(Set.fromList [ "a" ])--(def.get model)
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
  : Zipper ( Sections ( ContainerFacts meta ) (DataFacts meta) meta )
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
