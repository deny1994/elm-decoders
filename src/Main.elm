module Main exposing (main)

import Browser
import Data.User
import Html exposing (Html)



---- MODEL ----


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoUpdate


update : Msg -> Model -> ( Model, Cmd Msg )
update NoUpdate model =
    ( model
    , Cmd.none
    )



---- VIEW ----


view : Model -> Html Msg
view _ =
    Html.h1 [] [ Html.text "Hello world!" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
