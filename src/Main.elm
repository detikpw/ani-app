module Main exposing (main)

import Browser
import GraphQl exposing (Named, Operation, Query, Variables)
import GraphQl.Http exposing (Options)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, field, int, maybe, string)
import Json.Encode as Encode
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
    , animeList : List Media
    }


type alias QueryCollection =
    { page : Page }


type alias Page =
    { media : List Media
    }


type alias Media =
    { id : Int
    , title : Title
    }


type alias Title =
    { romaji : String
    , english : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeNavItem = Transport
      , input = ""
      , animeList = []
      }
    , Cmd.none
    )


type Msg
    = SetNavItem NavItem
    | InputOccurred String
    | TimePassed String
    | GraphQlMsg (Result Error QueryCollection)



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
                ( model, sendRequest model.input )

            else
                ( model, Cmd.none )

        GraphQlMsg arg ->
            let
                a =
                    Debug.log "arg" arg
            in
            case arg of
                Ok value ->
                    ( { model | animeList = value.page.media }
                    , Cmd.none
                    )

                Err e ->
                    Debug.todo "Next gan"



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


viewAutoCompleteItem : Media -> Html msg
viewAutoCompleteItem media =
    div [ class "h-8 flex items-center" ]
        [ text media.title.romaji ]


viewAutoComplete : List Media -> Html msg
viewAutoComplete mediaList =
    case mediaList of
        [] ->
            div [] []

        _ ->
            div
                [ class "absolute z-10 inset-x-0 top-1 px-2"
                , class "bg-bg-1 text-lg text-primary w-full h-full"
                ]
                (List.map viewAutoCompleteItem mediaList)


viewMain : Model -> Html Msg
viewMain model =
    div [ class "flex flex-col items-center justify-center h-full bg-bg-1 px-4" ]
        [ div
            [ class "mr-2 relative"
            , class "flex w-11/12 bg-bg-2 rounded py-5 px-4"
            ]
            [ div [ class "relative w-full" ]
                [ input
                    [ class "text-lg bg-bg-1 text-primary w-full px-2 h-8"
                    , onInput InputOccurred
                    ]
                    []
                , viewAutoComplete model.animeList
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col" ]
        [ viewHeader model
        , viewMain model
        ]



-- Decoder


decodeQueryCollection : Decoder QueryCollection
decodeQueryCollection =
    Decode.map QueryCollection
        (field "Page" decodePage)


decodePage : Decoder Page
decodePage =
    Decode.map Page
        (field "media" decodeListMedia)


decodeMedia : Decoder Media
decodeMedia =
    Decode.map2 Media
        (field "id" int)
        (field "title" decodeTitle)


decodeTitle : Decoder Title
decodeTitle =
    Decode.map2 Title
        (field "romaji" string)
        (field "english" string)


decodeListMedia : Decoder (List Media)
decodeListMedia =
    Decode.list decodeMedia



-- Helper


animeRequest : Operation Query Variables
animeRequest =
    GraphQl.named "AnimeQuery"
        [ GraphQl.field "Page"
            |> GraphQl.withArgument "page" (GraphQl.int 1)
            |> GraphQl.withArgument "perPage" (GraphQl.int 7)
            |> GraphQl.withSelectors
                [ GraphQl.field "media"
                    |> GraphQl.withArgument "search" (GraphQl.variable "search")
                    |> GraphQl.withArgument "sort" (GraphQl.type_ "POPULARITY_DESC")
                    |> GraphQl.withArgument "type" (GraphQl.type_ "ANIME")
                    |> GraphQl.withSelectors
                        [ GraphQl.field "id"
                        , GraphQl.field "title"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "romaji"
                                , GraphQl.field "english"
                                ]
                        ]
                ]
        ]
        |> GraphQl.withVariables [ ( "search", "String" ) ]


options : Options
options =
    { url = "https://graphql.anilist.co", headers = [] }


sendRequest : String -> Cmd Msg
sendRequest search =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "search", Encode.string search ) ]
        |> GraphQl.Http.send options GraphQlMsg decodeQueryCollection


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
