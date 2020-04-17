module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Process
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type NavItem
    = Transport
    | Tickets
    | Hotels
    | Cars
    | More



-- Model


type alias Model =
    { activeNavItem : NavItem
    , input : String
    , testDebounce : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeNavItem = Transport
      , input = ""
      , testDebounce = ""
      }
    , Cmd.none
    )


type Msg
    = SetNavItem NavItem
    | InputOccurred String
    | TimePassed String



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNavItem activeNav ->
            ( { model | activeNavItem = activeNav }, Cmd.none )

        InputOccurred str ->
            ( { model | input = str }
            , enqueueDebounceFor str
            )

        TimePassed debouncedString ->
            if debouncedString == model.input then
                ( { model | testDebounce = model.input }, Cmd.none )

            else
                ( model, Cmd.none )



-- Subcriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
            [ ( "text-primary px-2 inline-flex items-center", True )
            , ( "relative h-full cursor-pointer hover:text-alt-2", True )
            , ( "selected-nav-menu-item", isActiveNav )
            ]
        , onClick <| SetNavItem navItem
        ]
        [ text <| navItemsToLabel navItem ]


viewNavMenu : NavItem -> Html Msg
viewNavMenu activeNavItem =
    ul [ class "inline list-none mr-auto" ] <|
        List.map (viewNavMenuItem activeNavItem) navMenuItems


viewNav : NavItem -> Html Msg
viewNav activeNavItem =
    div [ class "flex flex-row mx-auto h-full w-10/12" ]
        [ viewNavMenu activeNavItem ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "flex flex-row items-center bg-bg-2 h-12" ]
        [ viewNav model.activeNavItem ]


viewMain : Html Msg
viewMain =
    div [ class "flex flex-col items-center justify-center h-full bg-bg-1" ]
        [ div [ class "flex w-1/2 bg-bg-2 rounded py-5 px-4" ]
            [ input
                [ class "text-lg px-2 mr-2 flex-grow bg-bg-1 text-primary"
                , onInput InputOccurred
                ]
                []
            , button [ class "bg-alt-4 px-4 py-2 text-white uppercase" ] [ text "Search" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col" ]
        [ viewHeader model
        , viewMain
        ]



-- Helper


enqueueDebounceFor : String -> Cmd Msg
enqueueDebounceFor str =
    Process.sleep debounceTimeOut
        |> Task.perform (\_ -> TimePassed str)



-- Const


debounceTimeOut : Float
debounceTimeOut =
    200


navMenuItems : List NavItem
navMenuItems =
    [ Transport, Tickets, Hotels, Cars, More ]


navUserItems : List String
navUserItems =
    [ "IDR / EN", "Sign In" ]
