module Main exposing (main)

import Browser
import Compare exposing (Comparator, by, concat)
import GraphQl exposing (Operation, Query, Variables)
import GraphQl.Http exposing (Options)
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, value)
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
    , animeList : List MediaWithRelations
    , relatedAnime : List Media
    , relationsEdge : List Edge
    }


type alias QueryCollection =
    { page : Page }


type alias Page =
    { media : List MediaWithRelations
    }


type alias Media =
    { id : Int
    , title : Title
    , startDate : StartDate
    }


type alias MediaWithRelations =
    { id : Int
    , title : Title
    , startDate : StartDate
    , relations : Relations
    }


type alias Title =
    { romaji : String
    , english : Maybe String
    }


type alias StartDate =
    { year : Maybe Int
    , month : Maybe Int
    , day : Maybe Int
    }


type alias Relations =
    { edges : List Edge }


type alias Edge =
    { relationType : RelationType
    , node : Media
    }


type RelationType
    = Prequel
    | Sequel
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeNavItem = Transport
      , input = ""
      , animeList = []
      , relatedAnime = []
      , relationsEdge = []
      }
    , Cmd.none
    )


type Msg
    = SetNavItem NavItem
    | InputOccurred String
    | TimePassed String
    | QueryAnimeListBySearch (Result Error QueryCollection)
    | QueryRelatedAnime (Result Error QueryCollection)
    | QueryPrequelAnime (Result Error QueryCollection)
    | QuerySequelAnime (Result Error QueryCollection)
    | SearchRelatedAnime Int String



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
                ( model, searchAnimeBySearch model.input )

            else
                ( model, Cmd.none )

        QueryAnimeListBySearch arg ->
            case arg of
                Ok value ->
                    ( { model | animeList = value.page.media }
                    , Cmd.none
                    )

                Err e ->
                    let
                        a =
                            Debug.log "Query" e
                    in
                    Debug.todo "QueryAnimeListBySearch gan"

        QueryRelatedAnime arg ->
            case arg of
                Ok value ->
                    let
                        selectedMedia =
                            List.head value.page.media

                        edges =
                            case selectedMedia of
                                Just media ->
                                    media.relations.edges

                                Nothing ->
                                    []

                        prequel =
                            List.filter (\edge -> .relationType edge == Prequel) edges

                        sequel =
                            List.filter (\edge -> .relationType edge == Sequel) edges

                        currentMedia =
                            List.map
                                (\media ->
                                    { id = media.id
                                    , title = media.title
                                    , startDate = media.startDate
                                    }
                                )
                                value.page.media

                        prequelMedia =
                            List.map (\edge -> .node edge) prequel

                        sequelMedia =
                            List.map (\edge -> .node edge) sequel
                    in
                    ( { model
                        | relatedAnime = prequelMedia ++ currentMedia ++ sequelMedia
                      }
                    , Cmd.batch
                        [ case prequel of
                            [ edge ] ->
                                searchPrequelAnime [ edge.node.id ]

                            _ ->
                                Cmd.none
                        , case sequel of
                            [ edge ] ->
                                searchSequelAnime [ edge.node.id ]

                            _ ->
                                Cmd.none
                        ]
                    )

                Err e ->
                    let
                        a =
                            Debug.log "Query" e
                    in
                    Debug.todo "QueryRelatedAnime gan"

        QueryPrequelAnime arg ->
            case arg of
                Ok value ->
                    let
                        relations =
                            case value.page.media of
                                [] ->
                                    Nothing

                                [ media ] ->
                                    Just media

                                mediaList ->
                                    Debug.todo "QueryPrequelAnime nanti ya gan"

                        edges =
                            case relations of
                                Just media ->
                                    media.relations.edges

                                Nothing ->
                                    []

                        prequel =
                            List.filter (\edge -> .relationType edge == Prequel) edges
                    in
                    ( { model
                        | relatedAnime = List.map (\edge -> .node edge) prequel ++ model.relatedAnime
                      }
                    , case prequel of
                        [ edge ] ->
                            searchPrequelAnime [ edge.node.id ]

                        _ ->
                            Cmd.none
                    )

                Err e ->
                    Debug.todo "QueryPrequelAnime nanti ya gan"

        QuerySequelAnime arg ->
            case arg of
                Ok value ->
                    let
                        relations =
                            case value.page.media of
                                [] ->
                                    Nothing

                                [ media ] ->
                                    Just media

                                mediaList ->
                                    Debug.todo "QuerySequelAnime nanti ya gan"

                        edges =
                            case relations of
                                Just media ->
                                    media.relations.edges

                                Nothing ->
                                    []

                        sequel =
                            List.filter (\edge -> .relationType edge == Sequel) edges
                    in
                    ( { model
                        | relatedAnime = model.relatedAnime ++ List.map (\edge -> .node edge) sequel
                      }
                    , case sequel of
                        [ edge ] ->
                            searchSequelAnime [ edge.node.id ]

                        _ ->
                            Cmd.none
                    )

                Err e ->
                    Debug.todo "QuerySequelAnime nanti ya gan"

        SearchRelatedAnime id title ->
            ( { model
                | animeList = []
                , input = title
              }
            , searchAnimeByIds [ id ]
            )



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
    header [ class "flex flex-row items-center bg-bg-2 h-12" ] []


viewAutoCompleteItem : MediaWithRelations -> Html Msg
viewAutoCompleteItem media =
    let
        romajiTitle =
            media.title.romaji

        englishTitle =
            case media.title.english of
                Just str ->
                    str

                Nothing ->
                    ""

        title =
            if romajiTitle == englishTitle then
                romajiTitle

            else
                romajiTitle ++ " / " ++ englishTitle
    in
    div
        [ class "flex items-center my-2 cursor-pointer"
        , onClick (SearchRelatedAnime media.id title)
        ]
        [ text title ]


viewAutoComplete : List MediaWithRelations -> Html Msg
viewAutoComplete mediaList =
    case mediaList of
        [] ->
            div [] []

        _ ->
            div
                [ class "absolute z-10 inset-x-0 top-1 px-2"
                , class "bg-bg-1 text-sm text-primary w-full h-full"
                ]
                (List.map viewAutoCompleteItem mediaList)


viewMain : Model -> Html Msg
viewMain model =
    div [ class "flex flex-col items-center h-full bg-bg-1 px-4" ]
        [ div
            [ class "flex flex-col w-11/12 bg-bg-2 rounded pt-1 pb-4 px-4 mt-8" ]
            [ span [ class "text-alt-2 items-center px-2 text-base" ]
                [ text "Please type an anime title" ]
            , div [ class "relative w-full" ]
                [ input
                    [ class "text-lg bg-bg-1 text-primary w-full px-2 h-8"
                    , value model.input
                    , placeholder "e.g Code Geass"
                    , onInput InputOccurred
                    ]
                    []
                , viewAutoComplete model.animeList
                , div [] [ text <| Debug.toString (List.map (.title >> .romaji) (sortByReleaseDate model.relatedAnime)) ]
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
        (field "media" decodeListMediaWithRelations)


decodeMediaWithRelations : Decoder MediaWithRelations
decodeMediaWithRelations =
    Decode.map4 MediaWithRelations
        (field "id" int)
        (field "title" decodeTitle)
        (field "startDate" decodeStartDate)
        (field "relations" decodeRelations)


decodeMedia : Decoder Media
decodeMedia =
    Decode.map3 Media
        (field "id" int)
        (field "title" decodeTitle)
        (field "startDate" decodeStartDate)


decodeTitle : Decoder Title
decodeTitle =
    Decode.map2 Title
        (field "romaji" string)
        (maybe (field "english" string))


decodeStartDate : Decoder StartDate
decodeStartDate =
    Decode.map3 StartDate
        (maybe (field "year" int))
        (maybe (field "month" int))
        (maybe (field "day" int))


decodeRelations : Decoder Relations
decodeRelations =
    Decode.map Relations
        (field "edges" decodeListEdge)


decodeEdge : Decoder Edge
decodeEdge =
    Decode.map2 Edge
        (field "relationType" decodeRelationType)
        (field "node" decodeMedia)


decodeRelationType : Decoder RelationType
decodeRelationType =
    Decode.map stringToRelationType string


stringToRelationType : String -> RelationType
stringToRelationType value =
    case value of
        "PREQUEL" ->
            Prequel

        "SEQUEL" ->
            Sequel

        _ ->
            Other


decodeListEdge : Decoder (List Edge)
decodeListEdge =
    Decode.list decodeEdge


decodeListMediaWithRelations : Decoder (List MediaWithRelations)
decodeListMediaWithRelations =
    Decode.list decodeMediaWithRelations


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
                    |> GraphQl.withArgument "id_in" (GraphQl.variable "ids")
                    |> GraphQl.withArgument "sort" (GraphQl.type_ "POPULARITY_DESC")
                    |> GraphQl.withArgument "type" (GraphQl.type_ "ANIME")
                    |> GraphQl.withSelectors
                        (queryMedia
                            ++ [ GraphQl.field "relations"
                                    |> GraphQl.withSelectors
                                        [ GraphQl.field "edges"
                                            |> GraphQl.withSelectors
                                                [ GraphQl.field "relationType"
                                                , GraphQl.field "node"
                                                    |> GraphQl.withSelectors
                                                        queryMedia
                                                ]
                                        ]
                               ]
                        )
                ]
        ]
        |> GraphQl.withVariables [ ( "search", "String" ), ( "ids", "[Int]" ) ]


queryMedia : List (GraphQl.Field a)
queryMedia =
    [ GraphQl.field "id"
    , GraphQl.field "title"
        |> GraphQl.withSelectors
            [ GraphQl.field "romaji"
            , GraphQl.field "english"
            ]
    , GraphQl.field "startDate"
        |> GraphQl.withSelectors
            [ GraphQl.field "year"
            , GraphQl.field "month"
            , GraphQl.field "day"
            ]
    ]


options : Options
options =
    { url = "https://graphql.anilist.co", headers = [] }


searchAnimeBySearch : String -> Cmd Msg
searchAnimeBySearch value =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "search", Encode.string value ) ]
        |> GraphQl.Http.send options QueryAnimeListBySearch decodeQueryCollection


prepareSearchAnimeByIds : List Int -> GraphQl.Request Query Variables
prepareSearchAnimeByIds values =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "ids", Encode.list Encode.int values ) ]


searchAnimeByIds : List Int -> Cmd Msg
searchAnimeByIds values =
    prepareSearchAnimeByIds values
        |> GraphQl.Http.send options QueryRelatedAnime decodeQueryCollection


searchPrequelAnime : List Int -> Cmd Msg
searchPrequelAnime values =
    prepareSearchAnimeByIds values
        |> GraphQl.Http.send options QueryPrequelAnime decodeQueryCollection


searchSequelAnime : List Int -> Cmd Msg
searchSequelAnime values =
    prepareSearchAnimeByIds values
        |> GraphQl.Http.send options QuerySequelAnime decodeQueryCollection


enqueueDebounceFor : String -> Cmd Msg
enqueueDebounceFor str =
    Process.sleep debounceTimeOut
        |> Task.perform (\_ -> TimePassed str)


sortByReleaseDate : List Media -> List Media
sortByReleaseDate media =
    List.sortWith
        (concat
            [ by
                (\a ->
                    case a.startDate.year of
                        Just value ->
                            value

                        Nothing ->
                            0
                )
            , by
                (\a ->
                    case a.startDate.month of
                        Just value ->
                            value

                        Nothing ->
                            0
                )
            ]
        )
        media



-- Const


debounceTimeOut : Float
debounceTimeOut =
    200


navMenuItems : List NavItem
navMenuItems =
    [ Transport, Tickets, Hotels, Cars, More ]
