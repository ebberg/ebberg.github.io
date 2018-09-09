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
import Url.Builder as Builder exposing (absolute)
import Url.Parser
    exposing
        ( (<?>)
        , Parser
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
        [ map Hypertext <| top <?> Query.string "q"
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

        "apocalypse-move" ->
            [ viewNav model
            , br [] []
            , p [] [ text "Apocalypse Move" ]
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
        [ text "Stories and Poems" ]
    , div [ class "list-group" ]
        (List.map
            (\( link, title ) ->
                a
                    [ href link
                    , class "list-group-item list-group-item-action"
                    ]
                    [ text title ]
            )
            [ ( absolute [] [ Builder.string "q" "ericberg" ]
              , "'Eric Berg Eric Berg' by Eric Berg"
              )
            , ( absolute [] [ Builder.string "q" "integer-quest" ]
              , "Integer Quest"
              )
            , ( absolute [] [ Builder.string "q" "apocalypse-move" ]
              , "Apocalypse Move"
              )
            ]
        )
    ]


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
I'm going to be publishing stories and poems here, coming in all forms. What are REST API logs but a hypertext story? What is a program but a poem?

In the first batch, there's the browser-crashing one, one about addictive internet games, and one that's Powered by the Apocalypse inspired.
"""
