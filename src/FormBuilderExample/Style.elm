module FormBuilderExample.Style
    exposing
        ( CssClasses(..)
        , CssIds(..)
        , cssNamespace
        , css
        )

import Css exposing (..)
import Html.CssHelpers exposing (Namespace, withNamespace)
import Css.Namespace exposing (namespace)


type CssClasses
    = Component
    | Container
    | Label


type CssIds
    = None


cssNamespace : Namespace String a b c
cssNamespace =
    withNamespace "FormBuilderExample_"


css : Stylesheet
css =
    (stylesheet << namespace cssNamespace.name)
        [ (.) Component
            [ displayFlex
            , flexDirection column
            ]
        , (.) Container
            [ displayFlex
            , flexDirection column
            ]
        ]
