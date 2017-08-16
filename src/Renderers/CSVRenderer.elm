module Renderers.CSVRenderer
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
    control


type alias PartialDataTemplate =
    { id : String
    , section : String
    , title : String
    , fieldType : String
    , fieldValues : String
    , default : String
    , helpText : String
    }


type alias ControlDataTemplate =
    { fieldType : String
    , fieldValues : String
    , default : String
    , helpText : String
    }


type alias FullDataTemplate =
    { id : String
    , section : String
    , title : String
    , fieldType : String
    , fieldValues : String
    , default : String
    , instructions : String
    , requiredOptions : String
    , conditions : String
    , constraints : String
    , errorMessage : String
    , helpText : String
    , notes : String
    , mobileKeypad : String
    }


type DataTemplates
    = Partial PartialDataTemplate
    | Control ControlDataTemplate
    | Full FullDataTemplate


stripChars : String -> String
stripChars value =
    if not <| String.contains "- " value then
        value
    else
        String.dropLeft 2 value


toDataTemplate :
    DataTemplates
    -> Html msg
toDataTemplate template =
    case template of
        Partial data ->
            div [] [ Html.text <| "\"" ++ data.id ++ "\",\"" ++ data.section ++ "\",\"" ++ data.title ++ "\",,,\"" ++ data.fieldType ++ "\",\"" ++ data.fieldValues ++ "\",,,\"" ++ (stripChars data.default) ++ "\",\"" ++ data.helpText ++ "\"" ]

        Control data ->
            div [] [ Html.text <| ",,,,,\"" ++ data.fieldType ++ "\",\"" ++ data.fieldValues ++ "\",,,\"" ++ (stripChars data.default) ++ "\",\"" ++ data.helpText ++ "\"" ]

        Full data ->
            div [] []


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
                        mod meta <| checkbox id zipper (DictList.fromList model.options) (Maybe.withDefault Set.empty model.values)

                    MultiUploadControl ( model, meta ) ->
                        mod meta <| div [] []

                    -- Html.text "PLEASE SUBMIT RELEVANT SUPPORTING FILES VIA EMAIL TO:  INFOSEC@ASHELABS.COM" ]
                    RadioControl ( model, meta ) ->
                        mod meta <| checkbox id zipper (DictList.fromList model.options) Set.empty

                    TextInputControl ( model, meta ) ->
                        mod meta <| textInput id zipper model.title model.value model.placeholder meta.labelColorHex False

                    TextLabelControl ( model, meta ) ->
                        mod meta <| textLabel id zipper model.title (Maybe.withDefault "" model.default) Nothing False

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
    toDataTemplate <| Control <| ControlDataTemplate "YesNo" "No" "No" ""


yesNoMaybe :
    String
    -> RendererZipper
    -> Maybe Bool
    -> Html Command
yesNoMaybe id zipper value =
    toDataTemplate <| Control <| ControlDataTemplate "Tri-State" "Un-set / No" "Un-set / No" ""


bulletSection : String -> String -> String
bulletSection key title =
    if String.contains "." key then
        ""
    else
        title


bulletValue : String -> String -> String
bulletValue key title =
    if String.contains "." key then
        title
    else
        ""


bullets :
    String
    -> RendererZipper
    -> String
    -> List (Html Command)
    -> Html Command
bullets id zipper title children =
    let
        key =
            (bulletString zipper)
    in
        div [] <|
            [ toDataTemplate <| Partial <| PartialDataTemplate key (bulletSection key title) (bulletValue key title) "Label" "" "" "" ]
                ++ children


checkboxValues : DictList String String -> String
checkboxValues options =
    options
        |> DictList.toList
        |> List.foldr
            (\( key, value ) out ->
                out ++ " [ " ++ key ++ " | " ++ value ++ " ]"
            )
            ""


checkbox :
    String
    -> RendererZipper
    -> DictList String String
    -> Set String
    -> Html Command
checkbox id zipper options selected =
    toDataTemplate <| Control <| ControlDataTemplate "Checkbox" (checkboxValues options) (checkboxValues (DictList.take 1 (DictList.reverse options))) ""


grid :
    String
    -> RendererZipper
    -> String
    -> List (Html Command)
    -> Html Command
grid id zipper title children =
    div [] children


header :
    String
    -> RendererZipper
    -> String
    -> List (Html Command)
    -> Html Command
header id zipper title children =
    div [] children


orderedList :
    String
    -> RendererZipper
    -> String
    -> Bool
    -> List (Html Command)
    -> Html Command
orderedList id zipper title error children =
    div [] <| [ toDataTemplate <| Control <| ControlDataTemplate "Label" title "N/A" "" ] ++ children


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
    toDataTemplate <| Partial <| PartialDataTemplate "" "" label "TextInput" value "" placeholder


textLabel :
    String
    -> RendererZipper
    -> String
    -> String
    -> Maybe String
    -> Bool
    -> Html Command
textLabel id zipper label text labelColorHex error =
    toDataTemplate <| Control <| ControlDataTemplate "TextLabel" label text ""


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
