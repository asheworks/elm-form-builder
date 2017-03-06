module Renderers.UIRenderer exposing
  ( render
  )

import FormBuilder exposing (..)
import FormBuilder.Model exposing (..)
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
  : Form ContainerFacts ( DataFacts ContainerFacts meta ) ( DataTypes meta ) meta
  -> Html Command
render form =
  applySectionZipper ( renderNode form ) form.def


renderNode
  : Form ContainerFacts ( DataFacts ContainerFacts meta ) ( DataTypes meta ) meta
  -> ZipperState
  -> Maybe String
  -> Zipper ( Sections ContainerFacts ( DataFacts ContainerFacts meta ) meta )
  -> List ( Html Command )
  -> Html Command
renderNode form state id ( ( ( ( Tree node children_ ) as subtree ), crumbs ) as zipper ) children =
  let
    id_ = Maybe.withDefault "" id

    path = appendPath state.path id

    dataResult = getDataValue form path

    -- data = Result.withDefault ( TextData ( DataValue (Just "") (Just "")  ) )

    -- data = Dict.get path form.indexes.data
    -- form.data
    -- value =
    --   List.filter
    --     (\ data ->
    --         data.path == path
    --     )
    --     form.dataList
    --   |> List.head
    --   |> Maybe.withDefault ( DataNode zipper path id_ Nothing )

  in
    case node of
      Branch _ container mods_ _ ->
        case container of

          BulletList types title ->
            bullets id_ title zipper children

          Grid ->
            grid id_ "" children

          Header title ->
            header id_ title children

          LabeledSection title ->
            header id_ title children

          OrderedList title ->
            orderedList id_ title False children

      Leaf _ leaf mods_ ->
        case leaf of

          Bool mods control ->
            bool id_

          FileUpload mods control ->
              div [] [ Html.text <| "FileUpload: " ++ id_ ]

          Option mods values control ->
            checkbox id_ ( Dict.fromList [ ( "one", "one" ) ] )

          Text mods control ->
            case control of
              TextInput title ctrlMods ->
                textInput id_ title (getTextData dataResult) "" False

              TextLabel title ->
                textLabel id_ title (getTextData dataResult) False


getDataValue
  : Form ContainerFacts ( DataFacts ContainerFacts meta ) ( DataTypes meta ) meta
  -> String
  -> Result String ( DataTypes meta )
getDataValue form path =
  case Dict.get path form.indexes.data of
    Nothing -> Err <| " ** MISSING [ " ++ path ++ " ] ** "
    Just dataNode -> Ok dataNode.data


getTextData
  : Result String ( DataTypes meta )
  -> String
getTextData dataResult =
  case dataResult of
    Err message -> message
    Ok data ->
      case data of
        TextData dataValue ->
          Maybe.withDefault
            ( Maybe.withDefault
                " ** NOT SET ** "
                dataValue.default
            )
            dataValue.value
        _ -> " ** INVALID TYPE ** "
        

bool : String -> Html Command
bool id =
  UI.yesNoField
    { id = id
    , yesLabel = "Yes"
    , noLabel = "No"
    , value = False
    , onChange = BoolData_Update
    }


bullets
  : String
  -> String
  -> Zipper ( Sections ContainerFacts (DataFacts ContainerFacts meta) meta )
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
      -- , label = def.label
      , placeholder = placeholder
      , inputType = UI.Input.TextField
      , value = value--"asdf"--(def.get model)
      , error = False
      , onInput = TextData_Update -- (\ a b -> "" )--InputField_Update
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
  : Zipper ( Sections ContainerFacts (DataFacts ContainerFacts meta) meta )
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
                  BulletList types _ ->

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
            BulletList types _ ->
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
