module FormBuilderExample.Sample exposing (..)

import FormBuilder exposing (..)
import Renderers.Model exposing (..)


defaultDescription : String
defaultDescription =
  "Provide an excuse"


defaultRisk : String
defaultRisk =
  "Provide a reason"


infoSection
  : String
  -> String
  -> Maybe String
  -> Maybe String
  -> List ( RendererSections Meta )
  -> RendererSections Meta
infoSection key title descriptionLabel riskLabel children =
  section key []
    ( BulletList title NumericBullets [] ) <|

    [ field "selected" [] <|
        YesNo "" []

    , field "description"
        [ visible False
        --visible (byId <| key ++ ".selected") <| boolIs True
        ] <|
        TextInput ( Maybe.withDefault defaultDescription descriptionLabel )
          [ visible False
          ]

    , field "risk"
        [ --visible (byId <| key ++ ".selected") <| boolIs False
        ] <|
        TextInput ( Maybe.withDefault defaultRisk riskLabel ) []

    ] ++ children


sample : RendererSections Meta
sample =
  section "sample" []
    ( Header "Sample Form" [] )

    [ version
    , details
    , definitions
    , menu
    , uploadSection
    , compositeSection
    , questionsSection
    , optionalListSection
    ]


version : RendererSections Meta
version =
  field "version" [] <|
    TextLabel "Version"
      [ default "v1.0.0"
      ]


details : RendererSections Meta
details =
  section "details" []
    ( Grid "" [] )

    [ field "companyName" [] <|
        TextInput "Company Name" []
    
    , field "contactName" [] <|
        TextInput "Contact Name" []

    , field "emailAddress" [] <|
        TextInput "Email Address"
          [ placeholder "user@email.com"
          -- , default "asdf"
          ]
      
    , field "date" [] <|
        TextInput "Date"
          [ placeholder "mm-dd-yyyy"
          ]

    , field "title" [] <|
        TextInput "Title" []

    , field "telephone" [] <|
        TextInput "Telephone"
              [ placeholder "(xxx) xxx-xxxx"
              ]
    ]


definitions : RendererSections Meta
definitions =
  section "definitions" []
    ( OrderedList "Definitions:" [] )

    [ field "rules" [] <|
        TextLabel "Rules"
          [ default "You must follow all of my rules"
          ]

    , field "punishments" [] <|
        TextLabel "Punishments"
          [ default "The punishment will be harsh..."
          ]
    ]


menu : RendererSections Meta
menu =
  section "menu" []
    ( BulletList "Meal Description:" AlphaBullets []
    )

    [ section "offered" []
        ( BulletList "Check all the kinds of food you serve:"
            NumericBullets []
        )

        [ field "selected" [] <|
            Checkbox ""
              [ ( "italian", "Italian" )
              , ( "mexican", "Mexican" )
              , ( "french", "French" )
              , ( "other", "Other" )
              ]
              []

        , field "preparation" [] <|
            TextInput "Please describe preparation options offered:" []

        , section "compliance" []
            ( BulletList "Check all boxes which describe how the food is prepared:"
                NumericBullets []
            )
            [ field "selected" [] <|
                Checkbox ""
                  [ ( "deepfry", "Deep Fried" )
                  , ( "grill", "Grilled" )
                  , ( "frozen", "Frozen" )
                  ]
                  []

            , section "deepfry"
                [ --visible (byId "infosec.services.compliance.selected") <| boolIs False
                ]
                ( BulletList "Do you enjoy deep fried food?"
                    NumericBullets []
                )
                [ field "enjoy" [] <|
                    YesNoMaybe "" []
                ]

            , section "grill"
                [ --visible (byId "infosec.services.compliance.selected") <| boolIs False
                ]
                ( BulletList "Do you enjoy grilled food?"
                    NumericBullets []
                )
                [ field "enjoy" [] <|
                    YesNoMaybe "" []
                ]

            ]
        ]
    ]


uploadSection : RendererSections Meta
uploadSection =
  section "uploadSection" []
    ( BulletList "Upload Section:" NumericBullets []
    )

    [ field "haveUploads" [] <|
        YesNoMaybe "" []

    , field "files" [] <|
        MultiUpload "" []
    ]


compositeSection : RendererSections Meta
compositeSection =
  section "compositeSection" []
    ( BulletList "COMPOSITE SECTION"
        NumericBullets []
    )

    [ infoSection
        "ownHome"
        "Does you own your home?"
        Nothing
        Nothing

        [ infoSection
            "mortgage"
            "Does you have a mortgage?"
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


questionsSection : RendererSections Meta
questionsSection =
    section "questionsSection" []
      ( BulletList "COMPOSITE SET SECTION"
          NumericBullets []
      )

      [ infoSection
          "information"
          "Some questions need to be answered"
          ( Just "With a custom message applied..." )
          Nothing

          [ section "included" []
              ( BulletList "What do you think is important (check all that apply):"
                  NumericBullets []
              )
              [ field "selected" [] <|
                  Checkbox ""
                    [ ( "happyness", "Happyness is important?" )
                    , ( "money", "Money is important?" )
                    , ( "drugs", "Drugs are important?" )
                    ]
                    []
              ]
          ]


      ]


optionalListSection : RendererSections Meta
optionalListSection =
    section "optionalListSection"
      [ --visible (byId "infosec.customSoftware.provided") <| boolIs False
      ]
      ( BulletList "Does you do some things you're not proud of?" NumericBullets []
      )

      [ field "selected" [] <|
          YesNo ""
            [ -- default False
            ]

      , section "details"
          [ -- visible (byId "infosec.storeData.selected") <| boolIs False
          ]
          ( OrderedList "What are they?" [] )

          [ field "guilts" [] <|
              Checkbox ""
                [ ( "thoughts", "Thoughts" )
                , ( "actions", "Actions" )
                ]
                []
          ]
      ]
