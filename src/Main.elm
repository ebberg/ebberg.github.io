module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, type_)
import Html.Events exposing (onClick)
import Markdown
import Random
import Time
import Url exposing (Url)
import Url.Builder as Builder exposing (Root(..))
import Url.Parser
    exposing
        ( (</>)
        , Parser
        , fragment
        , map
        , oneOf
        , parse
        , s
        , string
        , top
        )
import Url.Parser.Query as Query


-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { title : String
    , version : String
    , url : Url
    , navKey : Nav.Key
    , ericberg : String
    , quest : Int
    }


type Msg
    = NoOp
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | EricBerg Time.Posix
    | IntegerQuest Time.Posix


type Route
    = Hypertext (Maybe String)
    | ExternalLink


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Hypertext <| top </> fragment identity
        ]


parseRoute : Url.Url -> Route
parseRoute url =
    Maybe.withDefault ExternalLink <| parse routeParser url


init : { v : String } -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( Model "Eric Berg Eric Berg" flags.v url navKey "Eric Berg" 0, Cmd.none )



-- UPDATE


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case parseRoute url of
                        ExternalLink ->
                            ( model
                            , Nav.load (Url.toString url)
                            )

                        _ ->
                            ( model
                            , Nav.pushUrl model.navKey (Url.toString url)
                            )

                Browser.External href ->
                    ( model, Nav.load href )

        EricBerg newTime ->
            ( { model
                | ericberg =
                    model.ericberg ++ " " ++ model.ericberg
              }
            , Cmd.none
            )

        IntegerQuest newTime ->
            ( { model
                | quest =
                    model.quest + 1
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ case parseRoute model.url of
            Hypertext maybeQ ->
                case maybeQ of
                    Nothing ->
                        Sub.none

                    Just q ->
                        case q of
                            "ericberg" ->
                                Time.every 1000 EricBerg

                            "integer-quest" ->
                                Time.every 1 IntegerQuest

                            _ ->
                                Sub.none

            _ ->
                Sub.none
        ]



-- VIEW


view model =
    Document
        model.title
        (viewContainer model)


viewContainer model =
    [ div [ class "container" ]
        (case parseRoute model.url of
            Hypertext maybeQ ->
                case maybeQ of
                    Nothing ->
                        viewHome model

                    Just q ->
                        viewHypertext model q

            ExternalLink ->
                [ text "wait...what" ]
        )
    ]


viewHypertext model q =
    case q of
        "ericberg" ->
            [ viewNav model
            , br [] []
            , p [] [ text "'Eric Berg Eric Berg' by Eric Berg" ]
            , p [] [ text model.ericberg ]
            ]

        "integer-quest" ->
            [ viewNav model
            , br [] []
            , p [] [ text "Integer Quest: Collect All The Integers" ]
            , p [] [ text <| String.fromInt model.quest ]
            ]

        "the-guild" ->
            [ viewNav model
            , br [] []
            , p [] [ text "The Guild" ]
            , p [] [ text "Coming soon..." ]
            ]

        "space-crawler" ->
            [ viewNav model
            , br [] []
            , p [] [ text "Space Crawler" ]
            , p [] [ text "Coming soon..." ]
            ]

        _ ->
            [ viewNav model
            , br [] []
            , text "hypertext not found"
            ]


viewHome model =
    [ viewNav model
    , br [] []
    , div [ class "row" ]
        [ div [ class "col-lg" ] <|
            viewIntro model
        , div [ class "col-lg" ] <|
            viewHyperlinks model
        ]
    ]


viewIntro model =
    [ h1 [ class "display-4" ] [ text model.title ]
    , Markdown.toHtml [ class "lead" ]
        contentLead
    , hr [ class "my-4" ] []
    , Markdown.toHtml [] contentAfterFold
    ]


viewHyperlinks model =
    [ h2 [ class "display-4" ]
        [ text "Hypertext" ]
    , div [ class "list-group" ]
        [ viewListLink
            model
            (Builder.custom Absolute [] [] <| Just "ericberg")
            "'Eric Berg Eric Berg' by Eric Berg"
        , viewListLink
            model
            (Builder.custom Absolute [] [] <| Just "integer-quest")
            "Integer Quest"
        , viewListLink
            model
            (Builder.custom Absolute [] [] <| Just "the-guild")
            "The Guild"
        , viewListLink
            model
            (Builder.custom Absolute [] [] <| Just "space-crawler")
            "Space Crawler"
        ]
    ]


viewListLink model link title =
    a
        [ href link
        , class "list-group-item list-group-item-action"
        ]
        [ text title ]


viewNav model =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-primary" ]
        [ a [ class "btn btn-link navbar-brand", href "/" ]
            [ text model.title ]
        , span [ class "navbar-text ml-auto" ]
            [ text <| "v" ++ model.version ]
        ]


contentLead =
    """
Welcome to my doubly concatenated website. It was a blog for a while, then a book-like interface to a blog, and finally a browser-crashing poem. Then it vanished, unregistered.
"""


contentAfterFold =
    """
"""
