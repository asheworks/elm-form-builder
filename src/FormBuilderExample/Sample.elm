module FormBuilderExample.Sample exposing (..)

import FormBuilder exposing (..)
import Renderers.Model exposing (..)


defaultDescription : String
defaultDescription =
  "Provide an excuse"


defaultRisk : String
defaultRisk =
  "Provide a reason"


-- infoSection
--   : String
--   -> String
--   -> Maybe String
--   -> Maybe String
--   -> List ( RendererSections Meta )
--   -> RendererSections Meta
-- infoSection key title descriptionLabel riskLabel children =
--   section key []
--     ( BulletList title NumericBullets [] ) <|

--     [ field "selected" [] <|
--         YesNo "" []

--     , field "description"
--         [ visible False
--         --visible (byId <| key ++ ".selected") <| boolIs True
--         ] <|
--         TextInput ( Maybe.withDefault defaultDescription descriptionLabel )
--           [ visible False
--           ]

--     , field "risk"
--         [ --visible (byId <| key ++ ".selected") <| boolIs False
--         ] <|
--         TextInput ( Maybe.withDefault defaultRisk riskLabel ) []

--     ] ++ children


infoSection
  : String
  -> String
  -> Maybe String
  -> Maybe String
  -> List RendererSections
  -> RendererSections
infoSection key title descriptionLabel riskLabel children =
  Branch key [] ( BulletList title NumericBullets [] ) <|

    [ Leaf "selected" [] <|
        YesNo "" []

    , Leaf "description"
        [ visible False
        --visible (byId <| key ++ ".selected") <| boolIs True
        ] <|
        TextInput ( Maybe.withDefault defaultDescription descriptionLabel )
          [ visible False
          ]

    , Leaf "risk"
        [ --visible (byId <| key ++ ".selected") <| boolIs False
        ] <|
        TextInput ( Maybe.withDefault defaultRisk riskLabel ) []

    ] ++ children


sample : RendererSections
sample =
  Branch "sample" []
    ( Header "Sample Form" [] )

    [ compositeSection
    , menu
    , details
    , definitions
    , uploadSection
    , questionsSection
    , optionalListSection
    , version
    ]


version : RendererSections
version =
  Leaf "version" [] <|
    TextLabel "Version"
      [ default "v1.0.0"
      ]


details : RendererSections
details =
  Branch "details" []
    ( Grid "" [] )

    [ Leaf "companyName" [] <|
        TextInput "Company Name" []
    
    , Leaf "contactName" [] <|
        TextInput "Contact Name" []

    , Leaf "emailAddress" [] <|
        TextInput "Email Address"
          [ placeholder "user@email.com"
          , default "user@gmail.com"
          ]
      
    , Leaf "date" [] <|
        TextInput "Date"
          [ placeholder "mm-dd-yyyy"
          ]

    , Leaf "title" [] <|
        TextInput "Title" []

    , Leaf "telephone" [] <|
        TextInput "Telephone"
              [ placeholder "(xxx) xxx-xxxx"
              ]
    ]


definitions : RendererSections
definitions =
  Branch "definitions" []
    ( OrderedList "Definitions:" [] )

    [ Leaf "rules" [] <|
        TextLabel "Rules"
          [ default "You must follow all of my rules"
          ]

    , Leaf "punishments" [] <|
        TextLabel "Punishments"
          [ default "The punishment will be harsh..."
          ]
    ]


menu : RendererSections
menu =
  Branch "menu" []
    ( BulletList "Meal Description:" AlphaBullets []
    )

    [ Branch "offered" []
        ( BulletList "Check all the kinds of food you serve:"
            NumericBullets []
        )

        [ Leaf "selected" [] <|
            Checkbox ""
              [ ( "italian", "Italian" )
              , ( "mexican", "Mexican" )
              , ( "french", "French" )
              , ( "other", "Other" )
              ]
              []

        , Leaf "preparation" [] <|
            TextInput "Please describe preparation options offered:" []

        , Branch "compliance" []
            ( BulletList "Check all boxes which describe how the food is prepared:"
                NumericBullets []
            )
            [ Leaf "selected" [] <|
                Checkbox ""
                  [ ( "deepfry", "Deep Fried" )
                  , ( "grill", "Grilled" )
                  , ( "frozen", "Frozen" )
                  ]
                  []

            , Branch "deepfry"
                [ --visible (byId "infosec.services.compliance.selected") <| boolIs False
                ]
                ( BulletList "Do you enjoy deep fried food?"
                    NumericBullets []
                )
                [ Leaf "enjoy" [] <|
                    YesNoMaybe "" []
                ]

            , Branch "grill"
                [ --visible (byId "infosec.services.compliance.selected") <| boolIs False
                ]
                ( BulletList "Do you enjoy grilled food?"
                    NumericBullets []
                )
                [ Leaf "enjoy" [] <|
                    YesNoMaybe "" []
                ]

            ]
        ]
    ]


uploadSection : RendererSections
uploadSection =
  Branch "uploadSection" []
    ( BulletList "Upload Section:" NumericBullets []
    )

    [ Leaf "haveUploads" [] <|
        YesNoMaybe "" []

    , Leaf "files" [] <|
        MultiUpload "" []
    ]


compositeSection : RendererSections
compositeSection =
  Branch "compositeSection" []
    ( BulletList "COMPOSITE SECTION"
        NumericBullets []
    )

    [ infoSection
        "ownHome"
        "Do you own your home?"
        Nothing
        Nothing

        [ infoSection
            "mortgage"
            "Do you have a mortgage?"
            Nothing
            Nothing
            []

        , infoSection
            "appreciation"
            "Has your home appreciated in value?"
            Nothing
            Nothing
            []

        ]
    ]


questionsSection : RendererSections
questionsSection =
    Branch "questionsSection" []
      ( BulletList "COMPOSITE SET SECTION"
          NumericBullets []
      )

      [ infoSection
          "information"
          "Some questions need to be answered"
          ( Just "With a custom message applied..." )
          Nothing

          [ Branch "included" []
              ( BulletList "What do you think is important (check all that apply):"
                  NumericBullets []
              )
              [ Leaf "selected" [] <|
                  Checkbox ""
                    [ ( "happyness", "Happyness is important?" )
                    , ( "money", "Money is important?" )
                    , ( "drugs", "Drugs are important?" )
                    ]
                    []
              ]
          ]


      ]


optionalListSection : RendererSections
optionalListSection =
    Branch "optionalListSection"
      [ --visible (byId "infosec.customSoftware.provided") <| boolIs False
      ]
      ( BulletList "Does you do some things you're not proud of?" NumericBullets []
      )

      [ Leaf "selected" [] <|
          YesNo ""
            [ -- default False
            ]

      , Branch "details"
          [ -- visible (byId "infosec.storeData.selected") <| boolIs False
          ]
          ( OrderedList "What are they?" [] )

          [ Leaf "guilts" [] <|
              Checkbox ""
                [ ( "thoughts", "Thoughts" )
                , ( "actions", "Actions" )
                ]
                []
          ]
      ]
