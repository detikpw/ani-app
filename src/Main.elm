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
        Transport -> "Transport"
            
        Tickets -> "Tickets"
            
    

-- View

viewNavItem : NavItem -> NavItem -> Html Msg
viewNavItem activeNavItem navItem =
    let
        isActiveNav =
            activeNavItem == navItem
    in
    
    li
        [ classList
            [ ("text-white px-2 inline-flex items-center h-full cursor-pointer", True)
            , ("border-b-2 border-red-500", isActiveNav)
            ]
        , onClick <| SetNavItem navItem
        ]
        [ text <| navItemsToLabel navItem ]


viewNavMenu : NavItem -> Html Msg
viewNavMenu activeNavItem =
    ul [ class "inline list-none" ] <|
        List.map (viewNavItem activeNavItem) navMenuItems


viewNav : NavItem -> Html Msg
viewNav activeNavItem =
    div [ class "flex flex-row mx-auto h-full" ]
        [ viewNavMenu activeNavItem ]


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
    [ Transport, Tickets ]