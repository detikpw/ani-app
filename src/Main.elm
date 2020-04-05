module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Test


update : Msg -> Model -> Model
update msg model =
    case msg of
        Test ->
            Debug.todo "Test dulu gan"


navMenuItems : List String
navMenuItems =
    [ "Transport", "Tickets" ]


viewNavItem : String -> Html msg
viewNavItem label =
    li
        [ class "text-white px-2 flex items-center"
        , class "border-b border-red-500 h-full"
        ]
        [ text label ]


viewNavMenu : Html msg
viewNavMenu =
    ul [ class "inline-flex items-center list-none" ] <|
        List.map viewNavItem navMenuItems


viewNav : Html msg
viewNav =
    div [ class "flex flex-row mx-auto h-full" ]
        [ viewNavMenu ]


viewHeader : Html msg
viewHeader =
    header [ class "flex flex-row items-center bg-alt-1 h-12" ]
        [ viewNav ]


view : Model -> Html msg
view model =
    viewHeader
