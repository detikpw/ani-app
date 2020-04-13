module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
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


viewNavMenuItem : NavItem -> NavItem -> Html Msg
viewNavMenuItem activeNavItem navItem =
    let
        isActiveNav =
            activeNavItem == navItem
    in
    li
        [ classList
            [ ( "text-white px-2 inline-flex items-center", True )
            , ( "relative h-full cursor-pointer hover:text-alt-2", True )
            , ( "selected-nav-menu-item", isActiveNav )
            ]
        , onClick <| SetNavItem navItem
        ]
        [ text <| navItemsToLabel navItem ]


viewNavUserItem : String -> Html msg
viewNavUserItem navItem =
    li
        [ classList
            [ ( "text-white px-2 inline-flex items-center", True )
            , ( "h-full cursor-pointer", True )
            , ( "hover:text-alt-2", navItem /= "Sign In" )
            , ( "bg-primary hover:text-white hover:bg-alt-3", navItem == "Sign In" )
            ]
        ]
        [ text <| navItem ]


viewNavMenu : NavItem -> Html Msg
viewNavMenu activeNavItem =
    ul [ class "inline list-none mr-auto" ] <|
        List.map (viewNavMenuItem activeNavItem) navMenuItems


viewNavMenuUser : Html msg
viewNavMenuUser =
    ul [ class "inline list-none ml-auto" ] <|
        List.map viewNavUserItem navUserItems


viewNav : NavItem -> Html Msg
viewNav activeNavItem =
    div [ class "flex flex-row mx-auto h-full w-10/12" ]
        [ viewNavMenu activeNavItem
        , viewNavMenuUser
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "flex flex-row items-center bg-alt-1 h-12" ]
        [ viewNav model.activeNavItem ]


viewMain : Html msg
viewMain =
    div [ class "flex flex-col items-center justify-center h-full" ]
        [ div [ class "flex w-1/2 shadow-alt-1 bg-alt-4 rounded opacity-96 py-5 px-4" ]
            [ input [ class "text-lg px-2 mr-2 flex-grow" ] []
            , button [ class "bg-primary px-4 py-2 text-white uppercase" ] [ text "Search" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col" ]
        [ viewHeader model
        , viewMain
        ]



-- Const


navMenuItems : List NavItem
navMenuItems =
    [ Transport, Tickets, Hotels, Cars, More ]


navUserItems : List String
navUserItems =
    [ "IDR / EN", "Sign In" ]
