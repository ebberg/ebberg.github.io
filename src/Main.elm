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
    , count : Int
    }


type Msg
    = EricBergEricBerg


init : Int -> ( Model, Cmd Msg )
init currentTime =
    ( Model "Eric Berg Eric Berg" 0, Cmd.none )


contentPersonalBrand =
    """
[Etym Press](https://etympress.com)
"""


contentUnfolding =
    """
[Jagged Letter](https://jaggedletter.com)
"""


contentSpirals =
    """
[bergd.net](https://bergd.net)
"""


update msg model =
    case msg of
        EricBergEricBerg ->
            ( { model
                | title =
                    case model.count of
                        1 ->
                            model.title ++ " " ++ contentPersonalBrand ++ " " ++ model.title

                        3 ->
                            model.title ++ " " ++ contentUnfolding ++ " " ++ model.title

                        5 ->
                            model.title ++ " " ++ contentSpirals ++ " " ++ model.title

                        _ ->
                            model.title ++ " " ++ model.title
                , count = model.count + 1
              }
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
        , span [ class "navbar-text" ] [ Markdown.toHtml [] model.title ]
        ]


subscriptions model =
    Sub.none
