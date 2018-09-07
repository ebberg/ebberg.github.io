module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, type_)
import Html.Events exposing (onClick)
import Markdown
import Random


-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { title : String
    }


type Msg
    = NoOp


init : Int -> ( Model, Cmd Msg )
init currentTime =
    ( Model "Eric Berg Eric Berg", Cmd.none )



-- UPDATE


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions model =
    Sub.none



-- VIEW


view model =
    Document
        model.title
        (viewContainer model)


viewContainer model =
    [ div [ class "container" ]
        [ viewNav model
        , br [] []
        , div [ class "row" ]
            [ div [ class "col-lg" ] <|
                viewIntro model
            , div [ class "col-lg" ] <|
                viewHyperlinks model
            ]
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
            [ ( "#ericberg", "'Eric Berg Eric Berg' by Eric Berg" )
            , ( "#integer-quest", "Integer Quest" )
            , ( "#apocalypse-move", "Apocalypse Move" )
            ]
        )
    ]


viewNav model =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-primary" ]
        [ a [ class "btn btn-link navbar-brand", href "/" ]
            [ text model.title ]
        ]


contentLead =
    """
Welcome to my doubly concatenated website. It was a blog for a while, then a book-like interface to a blog, and finally a browser-crashing poem. Then it vanished, unregistered.
"""


contentAfterFold =
    """
I'm going to be publishing stories and poems here, coming in all forms. What are REST API logs but a hypertext story? What is a program but a poem?

The first batch are short. There's the browser-crashing one, one about addictive internet games, and one that's Powered by the Apocalypse inspired.
"""
