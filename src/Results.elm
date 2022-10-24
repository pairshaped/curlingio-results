module Results exposing (init)

import Browser
import Html exposing (Html, a, button, div, h3, h5, input, label, li, p, small, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, href, id, placeholder, style, target, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
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

    -- I don't love the name: SelectedChild...
    , selectedChild : Maybe SelectedChild
    , errorMsg : Maybe String
    }


type SelectedChild
    = SelectedChildDraw Draw
    | SelectedChildGame Game
    | SelectedChildTeam Team


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


type alias UrlState =
    { eventId : Maybe Int
    , eventSection : Maybe String
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
    }


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
    , rankingMethod : RankingMethod
    , pointsPerWin : Float
    , pointsPerTie : Float
    , pointsPerLoss : Float
    , pointsPerEnd : List Float
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
    { startsAt : String
    , label : String
    , attendance : Int
    , drawSheets : List (Maybe Game)
    }


type alias Game =
    { name : String
    , stageId : Int
    , nameWithResult : String
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
    , result : Maybe SideResult
    , endScores : List Int
    }


type SideResult
    = SideResultWon
    | SideResultLost
    | SideResultTied
    | SideResultConceded
    | SideResultForfeited
    | SideResultTimePenalized


type alias TeamResult =
    { team : Team
    , wins : Int
    , losses : Int
    , ties : Int
    , points : Float
    }



-- DECODERS


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

        decodePointsPerEnd : Decoder (List Float)
        decodePointsPerEnd =
            string
                |> Decode.andThen
                    (\str ->
                        String.replace " " "" str
                            |> String.split ","
                            |> List.map (\s -> String.toFloat s)
                            |> List.filterMap identity
                            |> Decode.succeed
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
        |> optional "ranking_method" decodeRankingMethod PointsRanking
        |> optional "points_per_win" float 0
        |> optional "points_per_tie" float 0
        |> optional "points_per_loss" float 0
        |> optional "points_per_end" decodePointsPerEnd []
        |> optional "tiebreaker" decodeTiebreaker TiebreakerNone


decodeDraw : Decoder Draw
decodeDraw =
    let
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

                decodeSide : Decoder Side
                decodeSide =
                    let
                        decodeSideResult : Decoder (Maybe SideResult)
                        decodeSideResult =
                            string
                                |> Decode.andThen
                                    (\str ->
                                        case str of
                                            "won" ->
                                                Decode.succeed (Just SideResultWon)

                                            "lost" ->
                                                Decode.succeed (Just SideResultLost)

                                            "tied" ->
                                                Decode.succeed (Just SideResultTied)

                                            "conceded" ->
                                                Decode.succeed (Just SideResultConceded)

                                            "forfeited" ->
                                                Decode.succeed (Just SideResultForfeited)

                                            "time_penalized" ->
                                                Decode.succeed (Just SideResultTimePenalized)

                                            _ ->
                                                Decode.succeed Nothing
                                    )
                    in
                    Decode.succeed Side
                        |> optional "team_id" (nullable int) Nothing
                        |> optional "top_rock" bool False
                        |> optional "first_hammer" bool False
                        |> optional "result" decodeSideResult Nothing
                        |> optional "end_scores" (list int) []
            in
            Decode.succeed Game
                |> required "name" string
                |> required "stage_id" int
                |> required "name_with_result" string
                |> required "state" decodeGameState
                |> required "game_positions" (list decodeSide)
    in
    Decode.succeed Draw
        |> required "starts_at" string
        |> required "label" string
        |> optional "attendance" int 0
        |> required "games" (list (nullable decodeGame))



-- HELPERS


isLocalMode : String -> Bool
isLocalMode url =
    String.contains "localhost" url


parseUrlHashFragments : Maybe String -> UrlState
parseUrlHashFragments maybeHash =
    case maybeHash of
        -- It's kind of annoying, but document.location.hash is always a string, and it will contain the #,
        -- To keep our flag setting simple (eventId: document.location.hash), we replace the # and concert to an integer in elm.
        -- I'd rather do this in the flags decoder, but I'm not sure how to yet.
        Just hash ->
            let
                fragments =
                    String.split "/" hash

                eventId =
                    case List.head fragments of
                        Just id ->
                            String.replace "#" "" id
                                |> String.toInt

                        Nothing ->
                            Nothing

                eventSection =
                    List.drop 1 fragments
                        |> List.head
            in
            UrlState eventId eventSection

        Nothing ->
            UrlState Nothing Nothing


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            ( Model flags False NotAsked "" NotAsked Nothing Nothing Nothing
            , case flags.eventId of
                Just eventId ->
                    getEvent flags eventId

                Nothing ->
                    let
                        urlState =
                            parseUrlHashFragments flags.hash
                    in
                    case urlState.eventId of
                        Just eventId ->
                            getEvent flags eventId

                        Nothing ->
                            getItems flags
            )

        Err error ->
            ( Model (Flags Nothing Nothing "" LeaguesSection False 10 [] Nothing) False NotAsked "" NotAsked Nothing Nothing (Just (Decode.errorToString error))
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
        -- Check if a section is included (not in the explicitly excluded sections list).
        included section =
            List.map String.toLower excludeEventSections
                |> List.member (String.toLower section)
                |> not
    in
    [ "Details", "Notes", "Registrations", "Spares", "Schedule", "Standings", "Teams", "Reports" ]
        |> List.filter included


gamesByStage : Stage -> List Draw -> List Game
gamesByStage stage draws =
    -- Get the draw sheets (games) from each draw,
    -- concat (flatten) to a list of games
    -- keep the games that aren't Nothing
    -- keep the games that are complete
    List.map .drawSheets draws
        |> List.concat
        |> List.filterMap identity
        |> List.filter (\g -> g.stageId == stage.id)


teamResultsForGames : Stage -> List Team -> List Game -> List TeamResult
teamResultsForGames stage teams games =
    let
        sides team =
            List.filter (\g -> g.state == GameComplete) games
                |> List.map .sides
                |> List.concat
                |> List.filter (\side -> side.teamId == Just team.id)

        results team =
            sides team
                |> List.map (\side -> side.result)
                |> List.filterMap identity

        wins team =
            results team
                |> List.filter (\result -> result == SideResultWon)
                |> List.length

        losses team =
            let
                isLoss result =
                    (result == SideResultLost)
                        || (result == SideResultConceded)
                        || (result == SideResultForfeited)
                        || (result == SideResultTimePenalized)
            in
            results team
                |> List.filter isLoss
                |> List.length

        ties team =
            results team
                |> List.filter (\result -> result == SideResultTied)
                |> List.length

        assignPoints teamResult =
            { teamResult
                | points =
                    case stage.rankingMethod of
                        PointsRanking ->
                            (toFloat (wins teamResult.team) * stage.pointsPerWin)
                                + (toFloat (ties teamResult.team) * stage.pointsPerTie)
                                + (toFloat (losses teamResult.team) * stage.pointsPerLoss)

                        SkinsRanking ->
                            let
                                pointsPerEnd index score =
                                    if score > 0 then
                                        List.Extra.getAt index stage.pointsPerEnd
                                            |> Maybe.withDefault 0.0

                                    else
                                        0.0
                            in
                            List.map (\side -> side.endScores) (sides teamResult.team)
                                |> List.map (\endScores -> List.indexedMap pointsPerEnd endScores)
                                |> List.concat
                                |> List.sum

                        ScoresRanking ->
                            List.map (\side -> side.endScores) (sides teamResult.team)
                                |> List.concat
                                |> List.sum
                                |> toFloat
            }
    in
    List.map (\team -> TeamResult team 0 0 0 0) teams
        |> List.map (\teamResult -> { teamResult | wins = wins teamResult.team })
        |> List.map (\teamResult -> { teamResult | losses = losses teamResult.team })
        |> List.map (\teamResult -> { teamResult | ties = ties teamResult.team })
        |> List.map assignPoints


teamResultsRankedByPoints : List TeamResult -> List TeamResult
teamResultsRankedByPoints teamResults =
    List.sortBy .points teamResults
        |> List.reverse



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
    | SelectDraw Draw
    | SelectGame Game
    | SelectTeam Team


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
                            -- Check if there's a section from the UrlState
                            let
                                urlState =
                                    parseUrlHashFragments model.flags.hash
                            in
                            case urlState.eventSection of
                                Just eventSection ->
                                    Just eventSection

                                Nothing ->
                                    -- Get the first included section
                                    eventSections model.flags.excludeEventSections
                                        |> List.map String.toLower
                                        |> List.head
            in
            ( { model
                | onEventSection = onEventSection
                , event = response
              }
            , Cmd.none
            )

        UpdateEventSection eventSection ->
            ( { model
                | onEventSection = Just (String.toLower eventSection)
                , selectedChild = Nothing
              }
            , Cmd.none
            )

        SelectDraw draw ->
            ( { model | selectedChild = Just (SelectedChildDraw draw) }
            , Cmd.none
            )

        SelectGame game ->
            ( { model | selectedChild = Just (SelectedChildGame game) }
            , Cmd.none
            )

        SelectTeam team ->
            ( { model | selectedChild = Just (SelectedChildTeam team) }
            , Cmd.none
            )



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
                        viewEvent model event

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


viewEvent : Model -> Event -> Html Msg
viewEvent { flags, onEventSection, selectedChild } event =
    let
        viewNavItem section =
            let
                isActive =
                    Just (String.toLower section) == onEventSection
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
        , ul [ class "nav nav-pills mb-3" ]
            (List.map viewNavItem (eventSections flags.excludeEventSections))
        , case selectedChild of
            Just (SelectedChildDraw draw) ->
                viewDraw draw

            Just (SelectedChildGame game) ->
                viewGame game

            Just (SelectedChildTeam team) ->
                viewTeam team

            Nothing ->
                case String.toLower (Maybe.withDefault "details" onEventSection) of
                    "schedule" ->
                        viewDrawSchedule event

                    "teams" ->
                        viewTeams event

                    "standings" ->
                        case List.head event.stages of
                            Just stage ->
                                viewStandings event stage

                            Nothing ->
                                p [] [ text "No stages" ]

                    _ ->
                        p [] [ text "Missing section view" ]
        ]


viewDrawSchedule : Event -> Html Msg
viewDrawSchedule { id, sheetNames, draws } =
    let
        isDrawActive draw =
            List.any
                (\g ->
                    case g of
                        Just g_ ->
                            g_.state == GameActive

                        Nothing ->
                            False
                )
                draw.drawSheets

        drawLink drawIndex draw label =
            a
                [ href ("#" ++ String.fromInt id ++ "/schedule/draws/" ++ String.fromInt (drawIndex + 1))
                , onClick (SelectDraw draw)
                ]
                [ text label ]

        gameLink drawIndex sheetIndex game =
            case game of
                Just game_ ->
                    let
                        stateClass =
                            case game_.state of
                                GamePending ->
                                    "text-primary"

                                GameComplete ->
                                    "text-secondary"

                                GameActive ->
                                    "text-primary font-weight-bold"
                    in
                    a
                        [ href ("#" ++ String.fromInt id ++ "/schedule/draws/" ++ String.fromInt (drawIndex + 1) ++ "/sheets/" ++ String.fromInt (sheetIndex + 1))
                        , onClick (SelectGame game_)
                        , class stateClass
                        ]
                        [ text game_.nameWithResult ]

                Nothing ->
                    text ""

        viewTableSchedule =
            let
                viewDrawRow drawIndex draw =
                    let
                        viewDrawSheet sheetIndex game =
                            td [ class "text-center" ]
                                [ gameLink drawIndex sheetIndex game ]
                    in
                    tr
                        [ classList
                            [ ( "bg-light", isDrawActive draw )
                            ]
                        ]
                        ([ td [] [ drawLink drawIndex draw draw.label ] ]
                            ++ [ td [] [ drawLink drawIndex draw draw.startsAt ]
                               ]
                            ++ List.indexedMap viewDrawSheet draw.drawSheets
                        )
            in
            table [ class "table" ]
                [ thead []
                    [ tr []
                        ([ th [ style "min-width" "65px" ] [ text "Draw" ] ]
                            ++ [ th [ style "min-width" "220px" ] [ text "Starts at" ] ]
                            ++ List.map (\sheetName -> th [ class "text-center", style "min-width" "198px" ] [ text sheetName ]) sheetNames
                        )
                    ]
                , tbody []
                    (List.indexedMap viewDrawRow draws)
                ]

        viewListSchedule =
            let
                viewDrawListItem drawIndex draw =
                    let
                        viewDrawSheet sheetIndex game =
                            li []
                                [ gameLink drawIndex sheetIndex game ]
                    in
                    div []
                        [ h5 [ class "mb-0" ] [ drawLink drawIndex draw ("Draw " ++ draw.label) ]
                        , small [] [ drawLink drawIndex draw draw.startsAt ]
                        , ul [ class "mt-2" ] (List.indexedMap viewDrawSheet draw.drawSheets)
                        ]
            in
            div [] (List.indexedMap viewDrawListItem draws)
    in
    div []
        [ div [ class "table-responsive d-none d-md-block" ] [ viewTableSchedule ]
        , div [ class "d-md-none" ] [ viewListSchedule ]
        ]


viewTeams : Event -> Html Msg
viewTeams { teams } =
    let
        viewTeamRow team =
            li [] [ text team.name ]
    in
    div []
        [ h5 [] [ text "Teams" ]
        , ul [] (List.map viewTeamRow teams)
        ]


viewStandings : Event -> Stage -> Html Msg
viewStandings { draws, teams } stage =
    let
        viewRow teamResult =
            tr []
                [ td [] [ text teamResult.team.name ]
                , td [] [ text (String.fromInt teamResult.wins) ]
                , td [] [ text (String.fromInt teamResult.losses) ]
                , td [] [ text (String.fromInt teamResult.ties) ]
                , td [] [ text (String.fromFloat teamResult.points) ]
                ]

        teamResults =
            gamesByStage stage draws
                |> teamResultsForGames stage teams
                |> teamResultsRankedByPoints

        hasTies =
            List.any (\teamResult -> teamResult.ties > 0) teamResults
    in
    div []
        [ h5 [] [ text "Standings" ]
        , table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Team" ]
                    , th [] [ text "Wins" ]
                    , th [] [ text "Losses" ]
                    , th [] [ text "Ties" ]
                    , th [] [ text "Points" ]
                    ]
                ]
            , tbody [] (List.map viewRow teamResults)
            ]
        ]


viewDraw : Draw -> Html Msg
viewDraw draw =
    div [] [ text ("Draw " ++ draw.label) ]


viewGame : Game -> Html Msg
viewGame game =
    div [] [ text ("Game " ++ game.name) ]


viewTeam : Team -> Html Msg
viewTeam team =
    div [] [ text ("Team " ++ team.name) ]



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
