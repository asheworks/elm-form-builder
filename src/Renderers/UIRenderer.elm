module Renderers.UIRenderer
    exposing
        ( render
        )

import Tuple exposing (..)
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
import DictList exposing (..)
import UI as UI
import UI.Input
import UI.FieldLabel
import UI.YesNo as UI


styles : List Mixin -> Html.Attribute msg
styles =
    asPairs >> Attr.style


render :
    Model
    -> (String -> RendererZipper -> RendererZipper)
    -> Maybe (Html Command)
render model interceptMapper =
    let
        mapper =
            renderNode model interceptMapper
    in
        model.form
            |> Maybe.map
                (\form ->
                    applySectionZipper [] "" mapper ( form, [] )
                )


checkVisible :
    Html Command
    -> Meta
    -> Html Command
    -> Html Command
checkVisible placeholder meta control =
    -- control
    if meta.visible then
        control
    else
        placeholder


preserveOrder : List ( String, String ) -> List ( String, String )
preserveOrder data =
    data
        |> List.indexedMap
            (\index item ->
                Tuple.mapFirst (\key -> (toString index) ++ "_" ++ key) item
            )


renderNode :
    Model
    -> (String -> RendererZipper -> RendererZipper)
    -> String
    -> String
    -> RendererZipper
    -> List (Html Command)
    -> Html Command
renderNode model interceptMapper state id zipper children =
    let
        mod =
            checkVisible (div [] [])

        --children)
        -- <|
        --     div [] <|
        --         [ Html.text "HIDDEN" ]
        --             ++ children
    in
        case MultiwayTreeZipper.datum (interceptMapper id zipper) of
            ( _, ( BranchModel branch, _ ) ) ->
                case branch of
                    BulletListControl ( model, meta ) ->
                        mod meta <| bullets id zipper model.title children

                    GridControl ( model, meta ) ->
                        mod meta <| grid id zipper model.title children

                    HeaderControl ( model, meta ) ->
                        mod meta <| header id zipper model.title children

                    LabeledSectionControl ( model, meta ) ->
                        mod meta <| header id zipper model.title children

                    OrderedListControl ( model, meta ) ->
                        mod meta <| orderedList id zipper model.title False children

            ( _, ( (LeafModel leaf) as model, _ ) ) ->
                case leaf of
                    CheckboxControl ( model, meta ) ->
                        -- mod meta <| checkbox id zipper (Dict.fromList <| preserveOrder model.options) (Maybe.withDefault Set.empty model.values)
                        mod meta <| checkbox id zipper (DictList.fromList model.options) (Maybe.withDefault Set.empty model.values)

                    MultiUploadControl ( model, meta ) ->
                        mod meta <| div [] [ Html.text "PLEASE SUBMIT RELEVANT SUPPORTING FILES VIA EMAIL TO:  ADMIN@ASHELABS.COM" ]

                    -- (
                    --   ( Set.toList model.values ) ++ [ "FileUpload: " ++ id ]
                    --       |> List.map Html.text
                    --       |> div []
                    -- )
                    RadioControl ( model, meta ) ->
                        -- mod meta <| checkbox id zipper (Dict.fromList <| preserveOrder model.options) Set.empty
                        mod meta <| checkbox id zipper (DictList.fromList model.options) Set.empty

                    --[ model.value ]
                    TextInputControl ( model, meta ) ->
                        mod meta <| textInput id zipper model.title model.value model.placeholder meta.labelColorHex False

                    TextLabelControl ( model, meta ) ->
                        mod meta <| textLabel id zipper model.title (Maybe.withDefault "" model.default) Nothing False

                    -- mod meta <| textLabel id zipper model.title model.value False
                    YesNoControl ( model, meta ) ->
                        mod meta <| yesNo id zipper model.value

                    YesNoMaybeControl ( model, meta ) ->
                        mod meta <| yesNoMaybe id zipper model.value


yesNo :
    String
    -> RendererZipper
    -> Bool
    -> Html Command
yesNo id zipper value =
    UI.yesNoField
        { id = id
        , yesLabel = "Yes"
        , noLabel = "No"
        , value = Just value
        , onChange = YesNo_Update zipper
        }


yesNoMaybe :
    String
    -> RendererZipper
    -> Maybe Bool
    -> Html Command
yesNoMaybe id zipper value =
    UI.yesNoField
        { id = id
        , yesLabel = "Yes"
        , noLabel = "No"
        , value = value
        , onChange = (YesNoMaybe_Update zipper)
        }



-- , value = Maybe.withDefault False value


bullets :
    String
    -> RendererZipper
    -> String
    -> List (Html Command)
    -> Html Command
bullets id zipper title children =
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


checkbox :
    String
    -> RendererZipper
    -> DictList String String
    -> Set String
    -> Html Command
checkbox id zipper options selected =
    UI.checkboxControl
        { id = id
        , values =
            options
                |> DictList.toList
                |> List.map
                    (\( key, value ) ->
                        { key = key
                        , value = value
                        , checked = Set.member key selected
                        , error = Nothing
                        }
                    )
        , error = Nothing
        , onSelect = Checkbox_Update zipper
        }


grid :
    String
    -> RendererZipper
    -> String
    -> List (Html Command)
    -> Html Command
grid id zipper title children =
    UI.FieldLabel.view
        { id = id
        , label = title
        , error = False
        , labelColorHex = Nothing
        }
        [ div
            [ styles
                [ displayFlex
                , flex (int 1)
                , flexDirection column
                , display block
                ]
            ]
          <|
            List.map
                (\child ->
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


header :
    String
    -> RendererZipper
    -> String
    -> List (Html Command)
    -> Html Command
header id zipper title children =
    UI.formControl
        { id = id
        , header =
            Just
                [ Html.text title
                ]
        , section =
            Just
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


orderedList :
    String
    -> RendererZipper
    -> String
    -> Bool
    -> List (Html Command)
    -> Html Command
orderedList id zipper title error children =
    UI.FieldLabel.view
        { id = id
        , label = title
        , error = error
        , labelColorHex = Nothing
        }
        [ div
            [ styles
                [ displayFlex
                , flex (int 1)
                , flexDirection column
                , display block
                ]
            ]
          <|
            List.map
                (\child ->
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
                )
                children
        ]


textInput :
    String
    -> RendererZipper
    -> String
    -> String
    -> String
    -> Maybe String
    -> Bool
    -> Html Command
textInput id zipper label value placeholder labelColorHex error =
    UI.FieldLabel.view
        { id = id
        , label = label
        , error = error
        , labelColorHex = labelColorHex
        }
        [ UI.Input.view
            { id = id
            , placeholder = placeholder
            , inputType = UI.Input.TextField
            , value = value
            , error = False
            , onInput = TextInput_Update zipper
            }
        ]


textLabel :
    String
    -> RendererZipper
    -> String
    -> String
    -> Maybe String
    -> Bool
    -> Html Command
textLabel id zipper label text labelColorHex error =
    UI.FieldLabel.view
        { id = id
        , label = label
        , error = error
        , labelColorHex = labelColorHex
        }
        [ span
            []
            [ Html.text text
            ]
        ]


bulletString :
    RendererZipper
    -> String
bulletString zipper =
    let
        countPrevZipper : ( Int, Int ) -> RendererZipper -> ( Int, Int )
        countPrevZipper (( alpha, numeric ) as count) (( subtree, crumbs ) as zipper) =
            case goLeft zipper of
                Nothing ->
                    count

                Just (( (Tree node children) as subtree_, crumbs_ ) as prev) ->
                    let
                        count_ =
                            case node of
                                ( Branch _ _ container _, _ ) ->
                                    case container of
                                        BulletList mods types _ ->
                                            case types of
                                                AlphaBullets ->
                                                    ( alpha + 1, numeric )

                                                NumericBullets ->
                                                    ( alpha, numeric + 1 )

                                        _ ->
                                            count

                                _ ->
                                    count
                    in
                        countPrevZipper count_ prev

        bulletStringZipper : Int -> RendererZipper -> String
        bulletStringZipper depth (( (Tree node children) as subtree, crumbs ) as zipper) =
            case node of
                ( Branch _ _ container _, _ ) ->
                    case container of
                        BulletList mods types _ ->
                            let
                                ( alpha, numeric ) =
                                    countPrevZipper ( 0, 0 ) zipper
                            in
                                case types of
                                    AlphaBullets ->
                                        String.fromChar <| fromCode (alpha + 65)

                                    NumericBullets ->
                                        (toString <| numeric + 1)

                        _ ->
                            ""

                _ ->
                    ""

        applyZipper : Int -> String -> RendererZipper -> String
        applyZipper depth label (( subtree, crumbs ) as zipper) =
            let
                bulletLabel =
                    (bulletStringZipper depth zipper)

                label_ =
                    bulletLabel
                        ++ if label == "" then
                            ""
                           else if String.startsWith "." label then
                            label
                           else
                            "." ++ label
            in
                case goUp zipper of
                    Nothing ->
                        label

                    Just parent ->
                        applyZipper (depth + 1) label_ parent
    in
        applyZipper 0 "" zipper
