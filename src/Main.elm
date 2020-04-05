module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type NavItem
    = Transport
    | Tickets
    | Hotels
    | Cars
    | More



-- Model


type alias Model =
    { activeNavItem : NavItem }


init : Model
init =
    { activeNavItem = Transport }


type Msg
    = SetNavItem NavItem



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetNavItem activeNav ->
            { model | activeNavItem = activeNav }



-- Converter


navItemsToLabel : NavItem -> String
navItemsToLabel navItem =
    case navItem of
        Transport ->
            "Transport"

        Tickets ->
            "Tickets"

        Hotels ->
            "Hotels"

        Cars ->
            "Cars"

        More ->
            "More"



-- View


viewNavItem : NavItem -> NavItem -> Html Msg
viewNavItem activeNavItem navItem =
    let
        isActiveNav =
            activeNavItem == navItem
    in
    li
        [ classList
            [ ( "text-white px-2 inline-flex items-center", True )
            , ( "h-full cursor-pointer hover:text-alt-2", True )
            , ( "border-b-2 border-alt-2", isActiveNav )
            ]
        , onClick <| SetNavItem navItem
        ]
        [ text <| navItemsToLabel navItem ]


viewNavMenu : NavItem -> Html Msg
viewNavMenu activeNavItem =
    ul [ class "inline list-none mr-auto" ] <|
        List.map (viewNavItem activeNavItem) navMenuItems


viewNavMenu2 : NavItem -> Html Msg
viewNavMenu2 activeNavItem =
    ul [ class "inline list-none ml-auto" ] <|
        List.map (viewNavItem activeNavItem) navMenuItems


viewNav : NavItem -> Html Msg
viewNav activeNavItem =
    div [ class "flex flex-row mx-auto h-full w-10/12" ]
        [ viewNavMenu activeNavItem
        , viewNavMenu2 activeNavItem
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "flex flex-row items-center bg-alt-1 h-12" ]
        [ viewNav model.activeNavItem ]


view : Model -> Html Msg
view model =
    viewHeader model



-- Const


navMenuItems : List NavItem
navMenuItems =
    [ Transport, Tickets, Hotels, Cars, More ]
