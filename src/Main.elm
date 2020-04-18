module Main exposing (main)

import Browser
import Task
import Process
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string)
import GraphQl exposing (Operation, Variables, Query, Named)
import GraphQl.Http exposing (Options)



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
    }

type alias QueryCollection =
    { page: Page }

type alias Page = 
    { media: Maybe (List Media)
    }

type alias Media =
    { id: Maybe Int
    , title: Maybe Title
    }

type alias Title =
    { romaji: Maybe String
    , english: Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeNavItem = Transport
      , input = ""
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
                a = Debug.log "arg" arg
            in
            (model, Cmd.none)



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
            [ div
                [class "mr-2 flex-grow relative"]
                [ input
                    [ class "text-lg bg-bg-1 text-primary w-full h-full px-2"
                    , onInput InputOccurred
                    ]
                    []
                , div
                    [ class "absolute z-10 inset-x-0 top-1 px-2"
                    , class "bg-bg-1 text-lg text-primary w-full h-full"
                    ]
                    [ div [class "h-8 flex items-center"]
                        [ text "test"]
                    , div [class "h-8 flex items-center"]
                        [ text "test"]
                    , div [class "h-8 flex items-center"]
                        [ text "test"]
                    ]
                ]
                 
            , button [ class "bg-alt-4 px-4 py-2 text-white uppercase" ] [ text "Search" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "h-screen flex flex-col" ]
        [ viewHeader model
        , viewMain
        ]



-- Decoder

decodeQueryCollection : Decoder QueryCollection
decodeQueryCollection =
    Decode.map QueryCollection
        (field "Page" decodePage)
        
decodePage : Decoder Page
decodePage =
    Decode.map Page
        (maybe (field "media" decodeListMedia ))
decodeMedia : Decoder Media
decodeMedia =
    Decode.map2 Media
        (maybe (field "id" int))
        (maybe (field "title" decodeTitle))

decodeTitle : Decoder Title
decodeTitle =
    Decode.map2 Title
        (maybe (field "romaji" string))
        (maybe (field "english" string))

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
    |> GraphQl.withVariables [ ("search", "String") ]

options : Options
options =
    {url = "https://graphql.anilist.co", headers = []}

sendRequest : String -> Cmd Msg
sendRequest search =
  GraphQl.query animeRequest
  |> GraphQl.addVariables [ ("search", Encode.string search) ]
  |> GraphQl.Http.send options  GraphQlMsg decodeQueryCollection

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


