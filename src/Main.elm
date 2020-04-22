module Main exposing (main)

import Array exposing (Array)
import Browser
import Compare exposing (by, concat)
import GraphQl exposing (Operation, Query, Variables)
import GraphQl.Http exposing (Options)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, placeholder, src, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, at, field, int, maybe, string)
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
    , selectedTab : Tab
    , input : String
    , animeList : List Media
    , relatedAnime : List BasicInfo
    , relationsEdge : List Edge
    }


type Media
    = Standard BasicInfo
    | Extended BasicInfo (Maybe (List Edge))


type alias BasicInfo =
    { id : Int
    , title : Title
    , startDate : StartDate
    , coverImage : String
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


type alias Edge =
    { relationType : RelationType
    , node : BasicInfo
    }


type RelationType
    = Prequel
    | Sequel
    | Other


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activeNavItem = Transport
      , selectedTab = ByStoryTimeline
      , input = ""
      , animeList = []
      , relatedAnime = []
      , relationsEdge = []
      }
    , Cmd.none
    )


initBasicInfo : BasicInfo
initBasicInfo =
    BasicInfo 0 (Title "" Nothing) (StartDate Nothing Nothing Nothing) ""


type Msg
    = SetNavItem NavItem
    | SelectTab Tab
    | InputOccurred String
    | TimePassed String
    | QueryAnimeListBySearch (Result Error (List Media))
    | QueryRelatedAnime (Result Error (List Media))
    | QueryPrequelAnime (Result Error (List Media))
    | QuerySequelAnime (Result Error (List Media))
    | SearchRelatedAnime Int String



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNavItem activeNav ->
            ( { model | activeNavItem = activeNav }, Cmd.none )

        SelectTab tab ->
            ( { model | selectedTab = tab }, Cmd.none )

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
            let
                z =
                    Debug.log "test" arg
            in
            case arg of
                Ok value ->
                    ( { model | animeList = value }
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
                        ( node, edges ) =
                            List.head value
                                |> Maybe.andThen
                                    (\media ->
                                        case media of
                                            Standard v ->
                                                Just ( v, Just [] )

                                            Extended v z ->
                                                Just ( v, z )
                                    )
                                |> Maybe.withDefault ( initBasicInfo, Nothing )

                        relations =
                            edges
                                |> Maybe.andThen
                                    (\items ->
                                        Just
                                            ( List.filter (\edge -> .relationType edge == Prequel) items
                                                |> List.map .node
                                            , [ node ]
                                            , List.filter (\edge -> .relationType edge == Sequel) items
                                                |> List.map .node
                                            )
                                    )
                                |> Maybe.withDefault ( [], [], [] )

                        ( prequel, current, sequel ) =
                            relations
                    in
                    ( { model
                        | relatedAnime = prequel ++ current ++ sequel
                      }
                    , Cmd.batch
                        [ case prequel of
                            [ p ] ->
                                searchPrequelAnime [ p.id ]

                            _ ->
                                Cmd.none
                        , case sequel of
                            [ s ] ->
                                searchSequelAnime [ s.id ]

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
                        prequel =
                            List.head value
                                |> Maybe.andThen
                                    (\sm ->
                                        case sm of
                                            Standard _ ->
                                                Just []

                                            Extended _ z ->
                                                z
                                    )
                                |> Maybe.withDefault []
                                |> List.filter (\edge -> .relationType edge == Prequel)
                                |> List.map .node
                    in
                    ( { model
                        | relatedAnime = prequel ++ model.relatedAnime
                      }
                    , case prequel of
                        [ p ] ->
                            searchPrequelAnime [ p.id ]

                        _ ->
                            Cmd.none
                    )

                Err e ->
                    Debug.todo "QueryPrequelAnime nanti ya gan"

        QuerySequelAnime arg ->
            case arg of
                Ok value ->
                    let
                        sequel =
                            List.head value
                                |> Maybe.andThen
                                    (\sm ->
                                        case sm of
                                            Standard _ ->
                                                Just []

                                            Extended _ z ->
                                                z
                                    )
                                |> Maybe.withDefault []
                                |> List.filter (\edge -> .relationType edge == Sequel)
                                |> List.map .node
                    in
                    ( { model
                        | relatedAnime = model.relatedAnime ++ sequel
                      }
                    , case sequel of
                        [ s ] ->
                            searchSequelAnime [ s.id ]

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


view : Model -> Html Msg
view model =
    div [ class "flex flex-col bg-bg-1 h-screen" ]
        [ viewHeader model
        , viewMain model
        , viewFooter
        ]


viewFooter : Html msg
viewFooter =
    div
        [ class "h-12 flex-shrink-0 bg-bg-2 flex flex-col items-center"
        , class "justify-center text-alt-2 text-xs mt-4"
        ]
        [ span []
            [ text "Data provided by "
            , a [ href "https://anilist.co" ] [ text "Anilist. " ]
            , text "Color theme "
            , a [ href "https://ethanschoonover.com/solarized/" ] [ text "solarized" ]
            ]
        , span []
            [ a [ href "https://github.com/detikpw/ani-app" ] [ text "Site Source " ]
            , text "© 2020 "
            , a [ href "https://twitter.com/2nd_frozenheart" ] [ text "2nd_frozenheart" ]
            ]
        ]


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
    header [ class "flex flex-col items-center justify-center bg-bg-2 h-12 flex-shrink-0" ]
        [ div [ class "w-10/12 text-lg text-alt-2" ]
            [ text "AniApp" ]
        ]


viewAutoCompleteItem : Media -> Html Msg
viewAutoCompleteItem value =
    let
        media =
            case value of
                Extended v _ ->
                    v

                Standard v ->
                    v

        title =
            ( media.title.romaji, media.title.english )

        label =
            case title of
                ( romaji, Just english ) ->
                    if romaji == english then
                        romaji

                    else
                        romaji ++ " / " ++ english

                ( romaji, Nothing ) ->
                    romaji
    in
    div
        [ class "flex items-center my-2 cursor-pointer"
        , onClick (SearchRelatedAnime media.id label)
        ]
        [ text label ]


viewAutoComplete : List Media -> Html Msg
viewAutoComplete mediaList =
    case mediaList of
        [] ->
            div [] []

        _ ->
            div
                [ class "z-10 inset-x-0 top-1 px-2 flex-grow"
                , class "bg-bg-1 text-sm text-primary w-full h-full"
                ]
                (List.map viewAutoCompleteItem mediaList)


viewMain : Model -> Html Msg
viewMain model =
    div [ class "flex flex-col items-center flex-grow bg-bg-1 px-4" ]
        [ div [ class "w-11/12 text-left text-primary text-lg mt-4" ]
            [ text "AniChrono"
            , div [ class "text-sm" ] [ text "Get anime list in the order release date or story timeline" ]
            ]
        , div
            [ class "flex flex-col w-11/12 bg-bg-2 rounded pt-1 pb-4 px-4 mt-4" ]
            [ span [ class "text-alt-2 items-center px-2 text-base" ]
                [ text "Please type an anime title" ]
            , div [ class "relative w-full flex flex-col" ]
                [ input
                    [ class "text-lg bg-bg-1 text-primary w-full px-2 h-8"
                    , value model.input
                    , placeholder "e.g Code Geass"
                    , onInput InputOccurred
                    ]
                    []
                , viewAutoComplete model.animeList
                ]
            ]
        , viewTabs model
        ]


viewTabs : Model -> Html Msg
viewTabs { relatedAnime, animeList, selectedTab } =
    case ( relatedAnime, animeList ) of
        ( [], [] ) ->
            Html.text ""

        ( ra, [] ) ->
            div [ class "flex flex-col w-11/12 bg-bg-2 rounded pt-1 pb-4 px-4 mt-4" ]
                [ ul [ class "inline-flex list-none mr-auto w-full" ] <|
                    List.map (viewTab selectedTab) tabs
                , div [ class "flex flex-col w-full" ]
                    (case selectedTab of
                        ByStoryTimeline ->
                            List.map viewCard relatedAnime

                        ByReleaseDate ->
                            List.map viewCard (sortByReleaseDate relatedAnime)
                    )
                ]

        _ ->
            Html.text ""


viewCard : BasicInfo -> Html msg
viewCard basicInfo =
    div [ class "flex w-full mt-4" ]
        [ img
            [ src basicInfo.coverImage
            , class "w-1/2 mr-2"
            ]
            []
        , div [ class "flex flex-col w-1/2 text-primary text-sm" ]
            [ text ("title: " ++ basicInfo.title.romaji) ]
        ]


viewTab : Tab -> Tab -> Html Msg
viewTab selectedTab tab =
    li
        [ classList
            [ ( "text-primary px-2 inline-flex items-center w-1/2", True )
            , ( "relative h-full cursor-pointer hover:text-alt-2", True )
            , ( "selected-nav-menu-item", selectedTab == tab )
            ]
        , onClick (SelectTab tab)
        ]
        [ span [ class "mx-auto text-sm" ] [ text (tabToLabel tab) ] ]



-- Decoder


mediaListDecoder : Decoder (List Media)
mediaListDecoder =
    at [ "Page", "media" ] (Decode.list mediaDecoder)


mediaDecoder : Decoder Media
mediaDecoder =
    relationsDecoder
        |> Decode.andThen chooseFromRelations


chooseFromRelations : Maybe (List Edge) -> Decoder Media
chooseFromRelations relations =
    case relations of
        Just _ ->
            extendedMediaDecoder

        Nothing ->
            standardMediaDecoder


standardMediaDecoder : Decoder Media
standardMediaDecoder =
    Decode.map Standard basicMediaDecoder


extendedMediaDecoder : Decoder Media
extendedMediaDecoder =
    Decode.map2 Extended basicMediaDecoder relationsDecoder


basicMediaDecoder : Decoder BasicInfo
basicMediaDecoder =
    Decode.map4 BasicInfo
        (field "id" int)
        (field "title" titleDecoder)
        (field "startDate" startDateDecoder)
        (field "coverImage" coverImageDecoder)


coverImageDecoder : Decoder String
coverImageDecoder =
    field "large" string


titleDecoder : Decoder Title
titleDecoder =
    Decode.map2 Title
        (field "romaji" string)
        (maybe (field "english" string))


startDateDecoder : Decoder StartDate
startDateDecoder =
    Decode.map3 StartDate
        (maybe (field "year" int))
        (maybe (field "month" int))
        (maybe (field "day" int))


relationsDecoder : Decoder (Maybe (List Edge))
relationsDecoder =
    maybe (at [ "relations", "edges" ] (Decode.list edgeDecoder))


edgeDecoder : Decoder Edge
edgeDecoder =
    Decode.map2 Edge
        (field "relationType" relationTypeDecoder)
        (field "node" basicMediaDecoder)


relationTypeDecoder : Decoder RelationType
relationTypeDecoder =
    Decode.map stringToRelationType string



-- Helper


stringToRelationType : String -> RelationType
stringToRelationType value =
    case value of
        "PREQUEL" ->
            Prequel

        "SEQUEL" ->
            Sequel

        _ ->
            Other


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
    , GraphQl.field "coverImage"
        |> GraphQl.withSelectors
            [ GraphQl.field "large" ]
    ]


options : Options
options =
    { url = "https://graphql.anilist.co", headers = [] }


searchAnimeBySearch : String -> Cmd Msg
searchAnimeBySearch value =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "search", Encode.string value ) ]
        |> GraphQl.Http.send options QueryAnimeListBySearch mediaListDecoder


prepareSearchAnimeByIds : List Int -> GraphQl.Request Query Variables
prepareSearchAnimeByIds values =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "ids", Encode.list Encode.int values ) ]


searchAnimeByIds : List Int -> Cmd Msg
searchAnimeByIds values =
    prepareSearchAnimeByIds values
        |> GraphQl.Http.send options QueryRelatedAnime mediaListDecoder


searchPrequelAnime : List Int -> Cmd Msg
searchPrequelAnime values =
    prepareSearchAnimeByIds values
        |> GraphQl.Http.send options QueryPrequelAnime mediaListDecoder


searchSequelAnime : List Int -> Cmd Msg
searchSequelAnime values =
    prepareSearchAnimeByIds values
        |> GraphQl.Http.send options QuerySequelAnime mediaListDecoder


enqueueDebounceFor : String -> Cmd Msg
enqueueDebounceFor str =
    Process.sleep debounceTimeOut
        |> Task.perform (\_ -> TimePassed str)


sortByReleaseDate : List BasicInfo -> List BasicInfo
sortByReleaseDate basicInfoList =
    List.sortWith
        (concat
            [ by
                (\basicInfo ->
                    basicInfo
                        |> .startDate
                        |> .year
                        |> Maybe.withDefault 0
                )
            , by
                (\basicInfo ->
                    basicInfo
                        |> .startDate
                        |> .month
                        |> Maybe.withDefault 0
                )
            ]
        )
        basicInfoList


mediaToBasicInfo : Media -> BasicInfo
mediaToBasicInfo media =
    case media of
        Standard v ->
            v

        Extended v _ ->
            v


tabToLabel : Tab -> String
tabToLabel tab =
    case tab of
        ByReleaseDate ->
            "By Release Date"

        ByStoryTimeline ->
            "By Story Timeline"



-- Const


months : Array String
months =
    Array.fromList
        [ "Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Des" ]


type Tab
    = ByReleaseDate
    | ByStoryTimeline


debounceTimeOut : Float
debounceTimeOut =
    200


navMenuItems : List NavItem
navMenuItems =
    [ Transport, Tickets, Hotels, Cars, More ]


tabs : List Tab
tabs =
    [ ByStoryTimeline, ByReleaseDate ]
