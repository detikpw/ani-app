module Main exposing (main)

import Array exposing (Array)
import Browser
import Compare exposing (by, concat)
import GraphQl exposing (Operation, Query, Variables)
import GraphQl.Http exposing (Options)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, placeholder, property, src, value)
import Html.Events exposing (onClick, onInput)
import Html.Parser
import Html.Parser.Util
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder, at, field, int, maybe, nullable, string)
import Json.Encode as Encode
import Process
import Regex
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Model


type alias Model =
    { selectedTab : Tab
    , selectedTitle : String
    , input : String
    , error : String
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
    , format : String
    , episodes : Maybe Int
    , description : String
    , studio : String
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
    ( { selectedTab = ByStoryTimeline
      , input = ""
      , error = ""
      , animeList = []
      , relatedAnime = []

      --   , relatedAnime = [
      --       BasicInfo 0 (Title "Test" Nothing) (StartDate Nothing Nothing Nothing)
      --       "https://s4.anilist.co/file/anilistcdn/media/anime/cover/medium/bx101922-PEn1CTc93blC.jpg"
      --   ]
      , relationsEdge = []
      , selectedTitle = ""
      }
    , Cmd.none
    )


initBasicInfo : BasicInfo
initBasicInfo =
    -- { id = 0
    -- , title = initTitle
    -- , startDate = initStartDate
    -- , coverImage = ""
    -- , episodes = 0
    -- , format = ""
    -- , description = ""
    -- }
    BasicInfo 0 (Title "" Nothing) (StartDate Nothing Nothing Nothing) "" "" Nothing "" ""


initTitle : Title
initTitle =
    { romaji = ""
    , english = Nothing
    }


initStartDate : StartDate
initStartDate =
    { year = Nothing
    , month = Nothing
    , day = Nothing
    }


type Msg
    = SelectTab Tab
    | InputOccurred String
    | TimePassed String
    | QueryMsg QueryMsg
    | SearchRelatedAnime Int String


type QueryMsg
    = QueryAnimeListBySearch (Result Error (List Media))
    | QueryRelatedAnime (Result Error (List Media))
    | QueryPrequelAnime (Result Error (List Media))
    | QuerySequelAnime (Result Error (List Media))



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        SelectTab tab ->
            ( { model | selectedTab = tab }, Cmd.none )

        InputOccurred str ->
            case str of
                "" ->
                    ( { model
                        | input = str
                        , animeList = []
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | input = str }
                    , enqueueDebounceFor str
                    )

        TimePassed debouncedString ->
            if debouncedString == model.input then
                ( model, requestAnimeBySearch model.input )

            else
                ( model, Cmd.none )

        QueryMsg query ->
            updateQuery query model

        SearchRelatedAnime id title ->
            ( { model
                | animeList = []
                , input = title
              }
            , requestAnimeByIds [ id ]
            )


updateQuery : QueryMsg -> Model -> ( Model, Cmd Msg )
updateQuery query model =
    case query of
        QueryAnimeListBySearch arg ->
            case ( model.input, arg ) of
                ( "", _ ) ->
                    ( { model | animeList = [] }
                    , Cmd.none
                    )

                ( _, Ok value ) ->
                    ( { model
                        | animeList = value
                        , error = ""
                      }
                    , Cmd.none
                    )

                ( _, Err e ) ->
                    ( { model | error = "Oops somthing went wrong" }, Cmd.none )

        QueryRelatedAnime (Ok value) ->
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

                currentTitle =
                    List.head current
                        |> Maybe.andThen
                            (\basicInfo ->
                                Just (.romaji (.title basicInfo))
                            )
                        |> Maybe.withDefault ""
            in
            ( { model
                | relatedAnime = prequel ++ current ++ sequel
                , selectedTitle = currentTitle
                , input = ""
                , error = ""
              }
            , Cmd.batch
                [ case prequel of
                    [ p ] ->
                        requestPrequelAnime [ p.id ]

                    _ ->
                        Cmd.none
                , case sequel of
                    [ s ] ->
                        requestSequelAnime [ s.id ]

                    _ ->
                        Cmd.none
                ]
            )

        QueryRelatedAnime (Err e) ->
            ( { model | error = "Oops somthing went wrong" }, Cmd.none )

        QueryPrequelAnime (Ok value) ->
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
                , error = ""
              }
            , case prequel of
                [ p ] ->
                    requestPrequelAnime [ p.id ]

                _ ->
                    Cmd.none
            )

        QueryPrequelAnime (Err _) ->
            ( { model | error = "Oops somthing went wrong" }, Cmd.none )

        QuerySequelAnime (Ok value) ->
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
                , error = ""
              }
            , case sequel of
                [ s ] ->
                    requestSequelAnime [ s.id ]

                _ ->
                    Cmd.none
            )

        QuerySequelAnime (Err _) ->
            ( { model | error = "Oops somthing went wrong" }, Cmd.none )



-- Subcriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- View


view : Model -> Html Msg
view model =
    div [ class "flex flex-col bg-bg-1 h-screen" ]
        [ viewHeader
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


viewHeader : Html Msg
viewHeader =
    header [ class "flex flex-col items-center justify-center bg-bg-2 h-12 flex-shrink-0" ]
        [ div [ class "w-10/12 text-lg text-alt-2" ]
            [ text "AniApp" ]
        ]


viewAutoCompleteItem : Media -> Html Msg
viewAutoCompleteItem value =
    let
        basicInfo =
            mediaToBasicInfo value

        title =
            ( basicInfo.title.romaji, basicInfo.title.english )

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
        , onClick (SearchRelatedAnime basicInfo.id label)
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
        , viewInput model
        , div [ class "text-alt-2 w-10/12 mt-2 px-2" ] [ text model.selectedTitle ]
        , viewTabs model
        ]


viewInput : Model -> Html Msg
viewInput model =
    div [ class "flex flex-col w-11/12 bg-bg-2 rounded pt-1 pb-4 px-4 mt-4" ]
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
            , span [ class "text-xs text-alt-4 px-2" ] [ text model.error ]
            , viewAutoComplete model.animeList
            ]
        ]


viewTabs : Model -> Html Msg
viewTabs { relatedAnime, animeList, selectedTab, error } =
    case ( relatedAnime, animeList, error ) of
        ( [], [], _ ) ->
            Html.text ""

        ( ra, [], "" ) ->
            div [ class "flex flex-col w-11/12 bg-bg-2 rounded pt-1 pb-4 px-4 mt-2" ]
                [ div [ class "inline-flex list-none mr-auto w-full" ] <|
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
    div [ class "w-full mt-4 flex flex-col" ]
        [ div [ class "w-full text-primary card-info leading-tight" ]
            [ img
                [ src basicInfo.coverImage
                , class "mr-2 float-left w-1/3"
                ]
                []
            , pre [ class "whitespace-pre-wrap" ]
                ([]
                    ++ viewInfoDetail "title" basicInfo.title.romaji
                    ++ viewInfoDetail "studio" basicInfo.studio
                    ++ viewInfoDetail "start date" (dateToString basicInfo.startDate)
                    ++ viewInfoDetail "format" basicInfo.format
                    ++ viewInfoDetail "episodes"
                        (Maybe.andThen (Just << String.fromInt) basicInfo.episodes
                            |> Maybe.withDefault ""
                        )
                )
            , pre
                [ class "whitespace-pre-wrap" ]
                (textHtml (removeBr basicInfo.description))
            ]
        , hr [ class "mt-2 border border-alt-1 border-dashed" ] []
        ]


viewInfoDetail : String -> String -> List (Html msg)
viewInfoDetail label value =
    case value of
        "" ->
            [ text "" ]

        _ ->
            [ span [] [ text (label ++ ": ") ]
            , text (value ++ "\n")
            ]


viewTab : Tab -> Tab -> Html Msg
viewTab selectedTab tab =
    button
        [ classList
            [ ( "text-primary px-2 inline-flex items-center w-1/2", True )
            , ( "relative h-full cursor-pointer hover:text-alt-2 focus:outline-none", True )
            , ( "selected-nav-menu-item", selectedTab == tab )
            ]
        , onClick (SelectTab tab)
        ]
        [ span [ class "mx-auto text-sm cursor-pointer" ] [ text (tabToLabel tab) ] ]



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
    Decode.map8 BasicInfo
        (field "id" int)
        (field "title" titleDecoder)
        (field "startDate" startDateDecoder)
        (field "coverImage" coverImageDecoder)
        (field "format" string)
        (field "episodes" (nullable int))
        (field "description" string)
        (field "studios" mainStudioDecoder)


mainStudioDecoder : Decoder String
mainStudioDecoder =
    Decode.map setStudio studioListDecoder


setStudio : List String -> String
setStudio value =
    case value of
        [] ->
            ""

        studios ->
            Maybe.withDefault "" (List.head studios)


studioListDecoder : Decoder (List String)
studioListDecoder =
    field "nodes" (Decode.list studioDecoder)


studioDecoder : Decoder String
studioDecoder =
    field "name" string


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



-- Request


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
    , GraphQl.field "format"
    , GraphQl.field "episodes"
    , GraphQl.field "description"
    , GraphQl.field "studios"
        |> GraphQl.withArgument "isMain" (GraphQl.bool True)
        |> GraphQl.withSelectors
            [ GraphQl.field "nodes"
                |> GraphQl.withSelectors
                    [ GraphQl.field "name" ]
            ]
    ]


options : Options
options =
    { url = "https://graphql.anilist.co", headers = [] }


requestAnimeBySearch : String -> Cmd Msg
requestAnimeBySearch value =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "search", Encode.string value ) ]
        |> GraphQl.Http.send options (QueryMsg << QueryAnimeListBySearch) mediaListDecoder


prepareRequestAnimeByIds : List Int -> GraphQl.Request Query Variables
prepareRequestAnimeByIds values =
    GraphQl.query animeRequest
        |> GraphQl.addVariables [ ( "ids", Encode.list Encode.int values ) ]


requestAnimeByIds : List Int -> Cmd Msg
requestAnimeByIds values =
    prepareRequestAnimeByIds values
        |> GraphQl.Http.send options (QueryMsg << QueryRelatedAnime) mediaListDecoder


requestPrequelAnime : List Int -> Cmd Msg
requestPrequelAnime values =
    prepareRequestAnimeByIds values
        |> GraphQl.Http.send options (QueryMsg << QueryPrequelAnime) mediaListDecoder


requestSequelAnime : List Int -> Cmd Msg
requestSequelAnime values =
    prepareRequestAnimeByIds values
        |> GraphQl.Http.send options (\a -> QueryMsg (QuerySequelAnime a)) mediaListDecoder



-- Helper

textHtml : String -> List (Html.Html msg)
textHtml t =
    case Html.Parser.run t of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes

        Err _ ->
            []


removeBr : String -> String
removeBr string =
    userReplace "<br>" (\_ -> "") string


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


dateToString : StartDate -> String
dateToString { year, month, day } =
    case ( year, month, day ) of
        ( Just y, Just m, Just d ) ->
            let
                m_ =
                    Array.get (m - 1) months

                d_ =
                    String.fromInt d

                y_ =
                    String.fromInt y
            in
            case m_ of
                Just monthInString ->
                    monthInString ++ " " ++ d_ ++ ", " ++ y_

                _ ->
                    ""

        _ ->
            ""


stringToRelationType : String -> RelationType
stringToRelationType value =
    case value of
        "PREQUEL" ->
            Prequel

        "SEQUEL" ->
            Sequel

        _ ->
            Other


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


tabs : List Tab
tabs =
    [ ByStoryTimeline, ByReleaseDate ]
