module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, type_)
import Html.Events exposing (onClick)
import Markdown


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { title : String
    }


type Msg
    = EricBergEricBerg


init : Int -> ( Model, Cmd Msg )
init currentTime =
    ( Model "Eric Berg Eric Berg", Cmd.none )


update msg model =
    case msg of
        EricBergEricBerg ->
            ( { model | title = model.title ++ " " ++ model.title }
            , Cmd.none
            )


view model =
    Document
        "Eric Berg Eric Berg"
        (viewContainer model)


viewContainer model =
    [ div [ class "container" ]
        [ viewNav model
        , br [] []
        , h1 [] [ text "Eric Berg" ]
        , div [ class "row" ]
            [ div [ class "col-md" ]
                [ h2 [] [ text "Eric Berg" ]
                , Markdown.toHtml [] model.title
                ]
            , div [ class "col-md" ]
                [ h2 [] [ text "Eric Berg" ]
                , Markdown.toHtml [] model.title
                ]
            ]
        ]
    ]


viewNav model =
    nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
        [ button [ class "btn btn-link navbar-brand", onClick EricBergEricBerg ] [ text "Eric Berg" ]
        , span [ class "navbar-text" ] [ text model.title ]
        ]


subscriptions model =
    Sub.none
