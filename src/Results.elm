module Results exposing (init)

import Browser
import Html exposing (Html, a, button, div, h3, input, label, li, p, small, table, td, text, tr, ul)
import Html.Attributes exposing (class, classList, href, id, placeholder, style, target, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Svg
import Svg.Attributes



-- MODEL


type alias Model =
    { flags : Flags
    , fullScreen : Bool
    , items : WebData (List Item)
    , search : String
    , event : WebData Event
    , onEventSection : Maybe String
    , errorMsg : Maybe String
    }


type alias Flags =
    { host : Maybe String
    , hash : Maybe String
    , apiKey : String
    , section : ItemsSection
    , registration : Bool
    , pageSize : Int
    , excludeEventSections : List String
    , eventId : Maybe Int
    }


type ItemsSection
    = LeaguesSection
    | CompetitionsSection
    | ProductsSection


type alias Item =
    { id : Int
    , name : String
    , summary : Maybe String
    , occursOn : Maybe String
    , location : Maybe String
    , noRegistrationMessage : Maybe String
    , price : Maybe String
    , purchaseUrl : Maybe String
    , publishResults : Bool
    }


type alias Event =
    { id : Int
    , name : String
    , summary : Maybe String
    , startsOn : String
    , endsOn : String
    , endScoresEnabled : Bool
    , shotByShotEnabled : Bool
    , numberOfEnds : Int
    , topRock : String
    , botRock : String
    , sheetNames : List String
    , teams : List Team
    , stages : List Stage
    , draws : List Draw
    , games : List Game
    }


type EventSection
    = EventSectionDetails
    | EventSectionNotes
    | EventSectionRegistrations
    | EventSectionSpares
    | EventSectionTeams
    | EventSectionSchedule
    | EventSectionStandings
    | EventSectionReports


type alias Team =
    { id : Int
    , name : String
    , shortName : String
    , lineup : List TeamCurler
    }


type alias TeamCurler =
    { curlerId : Int
    , position : TeamPosition
    , skip : Bool
    , name : String
    , delivery : Maybe RockDelivery
    , clubName : Maybe String
    , clubCity : Maybe String
    }


type TeamPosition
    = TeamFourth
    | TeamThird
    | TeamSecond
    | TeamFirst
    | TeamAlternate


type RockDelivery
    = RockDeliveryRight
    | RockDeliveryLeft


type alias Stage =
    { id : Int
    , stageType : StageType
    , name : String
    , rankingMethod : Maybe RankingMethod
    , pointsPerWin : Float
    , pointsPerTie : Float
    , pointsPerLoss : Float
    , pointsPerEnd : Float
    , tiebreaker : Tiebreaker
    }


type StageType
    = RoundRobin
    | Bracket


type RankingMethod
    = PointsRanking
    | SkinsRanking
    | ScoresRanking


type Tiebreaker
    = TiebreakerNone
    | TiebreakerHeadToHead
    | TiebreakerHeadToHeadThenSkillRank
    | TiebreakerScores


type alias Draw =
    { id : Int
    , startsAt : String
    , label : String
    , attendance : Maybe Int
    , drawSheets : List String
    }


type alias Game =
    { id : String
    , name : String
    , stageName : String
    , state : GameState
    , sides : List Side
    }


type GameState
    = GamePending
    | GameActive
    | GameComplete


type alias Side =
    { teamId : Maybe Int
    , topRock : Bool
    , firstHammer : Bool
    }



-- DECODERS


isLocalMode : String -> Bool
isLocalMode url =
    String.contains "localhost" url


decodeFlags : Decoder Flags
decodeFlags =
    let
        decodeSection : Decoder ItemsSection
        decodeSection =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "competitions" ->
                                Decode.succeed CompetitionsSection

                            "products" ->
                                Decode.succeed ProductsSection

                            _ ->
                                Decode.succeed LeaguesSection
                    )
    in
    Decode.succeed Flags
        |> optional "host" (nullable string) Nothing
        |> optional "hash" (nullable string) Nothing
        |> required "apiKey" string
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "pageSize" int 10
        |> optional "excludeEventSections" (list string) []
        |> optional "eventId" (nullable int) Nothing


decodeItems : Decoder (List Item)
decodeItems =
    list decodeItem


decodeItem : Decoder Item
decodeItem =
    Decode.succeed Item
        |> required "id" int
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> optional "occurs_on" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> optional "no_registration_message" (nullable string) Nothing
        |> optional "price" (nullable string) Nothing
        |> optional "url" (nullable string) Nothing
        |> optional "publish_results" bool False


decodeEvent : Decoder Event
decodeEvent =
    Decode.succeed Event
        |> required "id" int
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> required "starts_on" string
        |> required "ends_on" string
        |> optional "end_scores_enabled" bool False
        |> optional "shot_by_shot_enabled" bool False
        |> optional "number_of_ends" int 10
        |> optional "top_rock" string "red"
        |> optional "bot_rock" string "yellow"
        |> required "sheet_names" (list string)
        |> optional "teams" (list decodeTeam) []
        |> optional "stages" (list decodeStage) []
        |> optional "draws" (list decodeDraw) []
        |> optional "games" (list decodeGame) []


decodeTeam : Decoder Team
decodeTeam =
    Decode.succeed Team
        |> required "id" int
        |> required "name" string
        |> required "short_name" string
        |> optional "lineup" (list decodeTeamCurler) []


decodeTeamCurler : Decoder TeamCurler
decodeTeamCurler =
    let
        decodeTeamPosition : Decoder TeamPosition
        decodeTeamPosition =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "fourth" ->
                                Decode.succeed TeamFourth

                            "third" ->
                                Decode.succeed TeamThird

                            "second" ->
                                Decode.succeed TeamSecond

                            "first" ->
                                Decode.succeed TeamFirst

                            _ ->
                                Decode.succeed TeamAlternate
                    )

        decodeDelivery : Decoder RockDelivery
        decodeDelivery =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "left" ->
                                Decode.succeed RockDeliveryLeft

                            _ ->
                                Decode.succeed RockDeliveryRight
                    )
    in
    Decode.succeed TeamCurler
        |> required "curler_id" int
        |> required "position" decodeTeamPosition
        |> optional "skip" bool False
        |> required "name" string
        |> optional "delivery" (nullable decodeDelivery) Nothing
        |> optional "club_name" (nullable string) Nothing
        |> optional "club_city" (nullable string) Nothing


decodeStage : Decoder Stage
decodeStage =
    let
        decodeStageType : Decoder StageType
        decodeStageType =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "bracket" ->
                                Decode.succeed Bracket

                            _ ->
                                Decode.succeed RoundRobin
                    )

        decodeRankingMethod : Decoder RankingMethod
        decodeRankingMethod =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "skins" ->
                                Decode.succeed SkinsRanking

                            "scores" ->
                                Decode.succeed ScoresRanking

                            _ ->
                                Decode.succeed PointsRanking
                    )

        decodeTiebreaker : Decoder Tiebreaker
        decodeTiebreaker =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "head_to_head" ->
                                Decode.succeed TiebreakerHeadToHead

                            "head_to_head_then_skill_rank" ->
                                Decode.succeed TiebreakerHeadToHeadThenSkillRank

                            "scores" ->
                                Decode.succeed TiebreakerScores

                            _ ->
                                Decode.succeed TiebreakerNone
                    )
    in
    Decode.succeed Stage
        |> required "id" int
        |> required "type" decodeStageType
        |> required "name" string
        |> optional "ranking_method" (nullable decodeRankingMethod) Nothing
        |> optional "points_per_win" float 0
        |> optional "points_per_tie" float 0
        |> optional "points_per_loss" float 0
        |> optional "points_per_end" float 0
        |> optional "tiebreaker" decodeTiebreaker TiebreakerNone


decodeDraw : Decoder Draw
decodeDraw =
    Decode.succeed Draw
        |> required "id" int
        |> required "starts_at" string
        |> required "label" string
        |> optional "attendance" (nullable int) Nothing
        |> required "draw_sheets" (list string)


decodeGame : Decoder Game
decodeGame =
    let
        decodeGameState : Decoder GameState
        decodeGameState =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "active" ->
                                Decode.succeed GameActive

                            "complete" ->
                                Decode.succeed GameComplete

                            _ ->
                                Decode.succeed GamePending
                    )
    in
    Decode.succeed Game
        |> required "id" string
        |> required "name" string
        |> required "stage_name" string
        |> required "state" decodeGameState
        |> required "game_positions" (list decodeSide)


decodeSide : Decoder Side
decodeSide =
    Decode.succeed Side
        |> optional "team_id" (nullable int) Nothing
        |> optional "top_rock" bool False
        |> optional "first_hammer" bool False



-- HELPERS


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            ( Model flags False NotAsked "" NotAsked Nothing Nothing
            , case flags.eventId of
                Just eventId ->
                    getEvent flags eventId

                Nothing ->
                    case flags.hash of
                        -- It's kind of annoying, but document.location.hash is always a string, and it will contain the #,
                        -- To keep our flag setting simple (eventId: document.location.hash), we replace the # and concert to an integer in elm.
                        -- I'd rather do this in the flags decoder, but I'm not sure how to yet.
                        Just hash ->
                            case String.toInt (String.replace "#" "" hash) of
                                Just eventId ->
                                    getEvent flags eventId

                                Nothing ->
                                    getItems flags

                        Nothing ->
                            getItems flags
            )

        Err error ->
            ( Model (Flags Nothing Nothing "" LeaguesSection False 10 [] Nothing) False NotAsked "" NotAsked Nothing (Just (Decode.errorToString error))
            , Cmd.none
            )


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl string ->
            "Bad URL used: " ++ string

        Http.Timeout ->
            "Network timeout. Please check your internet connection."

        Http.NetworkError ->
            "Network error. Please check your internet connection."

        Http.BadStatus int ->
            "Bad status response from server. Please contact Curling I/O support if the issue persists for more than a few minutes."

        Http.BadBody string ->
            "Bad body response from server. Please contact Curling I/O support if the issue persists for more than a few minutes. Details: \"" ++ string ++ "\""


baseClubUrl : Flags -> String
baseClubUrl { host, apiKey } =
    let
        devUrl =
            "http://api.curling.test:3000"

        productionUrl =
            "https://api-curlingio.global.ssl.fastly.net"

        baseUrl =
            case host of
                Just h ->
                    if String.contains "localhost" h || String.contains ".curling.test" h then
                        devUrl

                    else
                        productionUrl

                Nothing ->
                    productionUrl
    in
    baseUrl ++ "/clubs/" ++ apiKey ++ "/"


getItems : Flags -> Cmd Msg
getItems flags =
    let
        url =
            baseClubUrl flags
                ++ (case flags.section of
                        LeaguesSection ->
                            "leagues"

                        CompetitionsSection ->
                            "competitions"

                        ProductsSection ->
                            "products"
                   )
    in
    RemoteData.Http.get url GotItems decodeItems


getEvent : Flags -> Int -> Cmd Msg
getEvent flags id =
    let
        url =
            baseClubUrl flags
                ++ "events/"
                ++ String.fromInt id
    in
    RemoteData.Http.get url GotEvent decodeEvent


eventSections : List String -> List String
eventSections excludeEventSections =
    let
        notExcluded section =
            not (List.member section excludeEventSections)
    in
    [ "Details", "Notes", "Registrations", "Spares", "Schedule", "Standings", "Teams" ]
        |> List.filter notExcluded



-- SVG


svgScreenResize path =
    Svg.svg
        [ Svg.Attributes.width "32"
        , Svg.Attributes.height "32"
        , Svg.Attributes.viewBox "0 0 24 24"
        ]
        [ Svg.path [ Svg.Attributes.d path ] [] ]


svgExitFullScreen =
    svgScreenResize "M.172 15.828a.5.5 0 0 0 .707 0l4.096-4.096V14.5a.5.5 0 1 0 1 0v-3.975a.5.5 0 0 0-.5-.5H1.5a.5.5 0 0 0 0 1h2.768L.172 15.121a.5.5 0 0 0 0 .707zM15.828.172a.5.5 0 0 0-.707 0l-4.096 4.096V1.5a.5.5 0 1 0-1 0v3.975a.5.5 0 0 0 .5.5H14.5a.5.5 0 0 0 0-1h-2.768L15.828.879a.5.5 0 0 0 0-.707z"


svgFullScreen =
    svgScreenResize "M5.828 10.172a.5.5 0 0 0-.707 0l-4.096 4.096V11.5a.5.5 0 0 0-1 0v3.975a.5.5 0 0 0 .5.5H4.5a.5.5 0 0 0 0-1H1.732l4.096-4.096a.5.5 0 0 0 0-.707zm4.344-4.344a.5.5 0 0 0 .707 0l4.096-4.096V4.5a.5.5 0 1 0 1 0V.525a.5.5 0 0 0-.5-.5H11.5a.5.5 0 0 0 0 1h2.768l-4.096 4.096a.5.5 0 0 0 0 .707z"



-- UPDATE


type Msg
    = ToggleFullScreen
    | GotItems (WebData (List Item))
    | ReloadItems
    | UpdateSearch String
    | SelectEvent Int
    | GotEvent (WebData Event)
    | UpdateEventSection String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        GotItems response ->
            ( { model | items = response, errorMsg = Nothing }, Cmd.none )

        ReloadItems ->
            ( model, getItems model.flags )

        UpdateSearch val ->
            ( { model | search = String.toLower val }, Cmd.none )

        SelectEvent id ->
            ( model, getEvent model.flags id )

        GotEvent response ->
            let
                onEventSection =
                    case model.onEventSection of
                        Just eventSection ->
                            Just eventSection

                        Nothing ->
                            -- Get the default section
                            eventSections model.flags.excludeEventSections
                                |> List.head
            in
            ( { model
                | onEventSection = onEventSection
                , event = response
              }
            , Cmd.none
            )

        UpdateEventSection eventSection ->
            ( model, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    div
        (List.append
            [ id "curlingio__results"
            , style "position" "relative"
            ]
            (if model.fullScreen then
                [ style "width" "100%"
                , style "height" "100%"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "z-index" "100"
                , style "overflow-y" "auto"
                , style "backgroup-color" "#fff"
                ]

             else
                []
            )
        )
        [ div
            [ style "cursor" "pointer"
            , style "position" "absolute"
            , style "top" "10px"
            , style "right" "10px"
            , onClick ToggleFullScreen
            ]
            [ if model.fullScreen then
                svgExitFullScreen

              else
                svgFullScreen
            ]
        , case model.errorMsg of
            Just errorMsg ->
                viewNotReady errorMsg

            Nothing ->
                case model.event of
                    Success event ->
                        viewEvent model.flags model.onEventSection event

                    Loading ->
                        viewNotReady "Loading..."

                    Failure error ->
                        viewFetchError (errorMessage error)

                    NotAsked ->
                        case model.items of
                            NotAsked ->
                                viewNotReady "Initializing..."

                            Loading ->
                                viewNotReady "Loading..."

                            Failure error ->
                                viewFetchError (errorMessage error)

                            Success items ->
                                viewItems model items
        ]


viewNotReady : String -> Html Msg
viewNotReady message =
    p [ class "p-3" ] [ text message ]


viewFetchError : String -> Html Msg
viewFetchError message =
    div
        [ class "p-3" ]
        [ p [] [ text message ]
        , button [ class "btn btn-primary", onClick ReloadItems ] [ text "Reload" ]
        ]


viewItems : Model -> List Item -> Html Msg
viewItems { flags, fullScreen, search } items =
    let
        filteredItems =
            case String.trim search of
                "" ->
                    items

                _ ->
                    let
                        matches item =
                            String.contains search (String.toLower item.name)
                                || (case item.location of
                                        Just location ->
                                            String.contains search (String.toLower location)

                                        Nothing ->
                                            False
                                   )
                    in
                    List.filter matches items

        sectionTitle =
            case flags.section of
                LeaguesSection ->
                    "Leagues"

                CompetitionsSection ->
                    "Competitions"

                ProductsSection ->
                    "Products"

        viewItem item =
            tr []
                ([ td []
                    [ if item.publishResults then
                        a [ href ("#" ++ String.fromInt item.id), onClick (SelectEvent item.id) ] [ text item.name ]

                      else
                        text item.name
                    , small [ class "d-block" ] [ text (Maybe.withDefault "" item.summary) ]
                    ]
                 , td [] [ text (Maybe.withDefault "" item.occursOn) ]
                 ]
                    ++ (if flags.registration then
                            [ td [ class "text-right" ]
                                [ case item.noRegistrationMessage of
                                    Just msg ->
                                        case item.purchaseUrl of
                                            Just url ->
                                                a [ href url, target "_blank" ] [ text (msg ++ " →") ]

                                            Nothing ->
                                                text msg

                                    Nothing ->
                                        case ( item.price, item.purchaseUrl ) of
                                            ( Just price, Just url ) ->
                                                div []
                                                    [ a [ href url, target "_blank" ] [ text "Register →" ]
                                                    , small [ class "d-block" ] [ text price ]
                                                    ]

                                            _ ->
                                                text ""
                                ]
                            ]

                        else
                            [ td [] [ text (Maybe.withDefault "" item.location) ]
                            ]
                       )
                )
    in
    div
        [ class "p-3" ]
        [ div
            [ class "d-flex justify-content-between" ]
            [ div [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Search"
                    , value search
                    , onInput UpdateSearch
                    ]
                    []
                ]

            -- I don't think we want the user initiating reloads, unless the initial load fails maybe, but even then I think
            -- we're better off having a ticker doing reloads every 30s if they're on an event.
            -- , div [ class "d-flex text-right" ]
            -- [ button [ class "btn btn-sm btn-primary mr-2", onClick ReloadItems ] [ text "Reload" ]
            ]
        , div
            [ class "table-responsive" ]
            [ table
                [ class "table" ]
                (List.map viewItem filteredItems)
            ]
        ]


viewEvent : Flags -> Maybe String -> Event -> Html Msg
viewEvent { registration, excludeEventSections } onEventSection event =
    let
        viewNavItem section =
            let
                isActive =
                    Just section == onEventSection
            in
            li [ class "nav-item" ]
                [ a
                    [ classList
                        [ ( "nav-link", True )
                        , ( "active", isActive )
                        ]
                    , onClick (UpdateEventSection section)
                    , href ("#" ++ String.fromInt event.id ++ "/" ++ String.toLower section)
                    ]
                    [ text section ]
                ]
    in
    div [ class "p-3" ]
        [ h3 [ class "mb-3" ] [ text event.name ]
        , ul [ class "nav nav-pills" ]
            (List.map viewNavItem (eventSections excludeEventSections))
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
