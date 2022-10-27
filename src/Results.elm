module Results exposing (init)

import Browser
import Html exposing (Html, a, button, div, h3, h5, input, label, li, p, small, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, href, id, placeholder, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Svg
import Svg.Attributes
import Url
import Url.Parser exposing ((</>), Parser)



-- MODEL


type alias Model =
    { flags : Flags
    , path : Maybe String
    , fullScreen : Bool
    , translations : WebData (List Translation)
    , items : WebData (List Item)
    , search : String
    , product : WebData Product
    , event : WebData Event
    , errorMsg : Maybe String
    }


type Route
    = ItemsRoute
    | ProductRoute Int
    | EventRoute Int NestedEventRoute


type NestedEventRoute
    = DrawsRoute
    | DrawRoute Int
    | GameRoute Int Int
    | StagesRoute
    | StageRoute Int
    | TeamsRoute
    | TeamRoute Int
    | ReportsRoute
    | ReportRoute String


type alias Translation =
    { key : String
    , label : String
    }


type alias Flags =
    { host : Maybe String
    , hash : Maybe String
    , lang : Maybe String
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


type alias Product =
    { id : Int
    , name : String
    , summary : Maybe String
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
    , coach : Maybe String
    , affiliation : Maybe String
    , location : Maybe String
    , contactName : Maybe String
    , contactEmail : Maybe String
    , contactPhone : Maybe String
    , imageUrl : Maybe String
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
    , photoUrl : Maybe String
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
    , gamesPlayed : Int
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
        |> optional "lang" (nullable string) Nothing
        |> required "apiKey" string
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "pageSize" int 10
        |> optional "excludeEventSections" (list string) []
        |> optional "eventId" (nullable int) Nothing


decodeTranslations : Decoder (List Translation)
decodeTranslations =
    list decodeTranslation


decodeTranslation : Decoder Translation
decodeTranslation =
    Decode.succeed Translation
        |> required "key" string
        |> required "label" string


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


decodeProduct : Decoder Product
decodeProduct =
    Decode.succeed Product
        |> required "id" int
        |> required "name" string
        |> optional "summary" (nullable string) Nothing


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
        |> optional "coach" (nullable string) Nothing
        |> optional "affiliation" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> optional "contact_name" (nullable string) Nothing
        |> optional "contact_email" (nullable string) Nothing
        |> optional "contact_phone" (nullable string) Nothing
        |> optional "image_url" (nullable string) Nothing
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
        |> optional "photo_url" (nullable string) Nothing


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


matchRoute : Parser (Route -> a) a
matchRoute =
    Url.Parser.oneOf
        [ Url.Parser.map ItemsRoute Url.Parser.top
        , Url.Parser.map ProductRoute (Url.Parser.s "products" </> Url.Parser.int)
        , Url.Parser.map EventRoute (Url.Parser.s "events" </> Url.Parser.int </> matchNestedEventRoute)
        ]


matchNestedEventRoute : Parser (NestedEventRoute -> a) a
matchNestedEventRoute =
    Url.Parser.oneOf
        [ Url.Parser.map DrawsRoute Url.Parser.top
        , Url.Parser.map DrawsRoute (Url.Parser.s "draws")
        , Url.Parser.map StagesRoute (Url.Parser.s "stages")
        , Url.Parser.map TeamsRoute (Url.Parser.s "teams")
        , Url.Parser.map ReportsRoute (Url.Parser.s "reports")
        , Url.Parser.map DrawRoute (Url.Parser.s "draws" </> Url.Parser.int)
        , Url.Parser.map GameRoute (Url.Parser.s "draws" </> Url.Parser.int </> Url.Parser.s "sheets" </> Url.Parser.int)
        , Url.Parser.map StageRoute (Url.Parser.s "stages" </> Url.Parser.int)
        , Url.Parser.map TeamRoute (Url.Parser.s "teams" </> Url.Parser.int)
        , Url.Parser.map ReportRoute (Url.Parser.s "reports" </> Url.Parser.string)
        ]


toRoute : Maybe String -> Route
toRoute path =
    let
        url =
            let
                fixedPath =
                    path
                        |> Maybe.withDefault ""
                        |> String.replace "#" ""
            in
            { host = "api.curling.io"
            , port_ = Nothing
            , protocol = Url.Https
            , query = Nothing
            , fragment = Nothing
            , path = fixedPath
            }

        _ =
            Debug.log (Url.toString url) { url = url, parsed = Url.Parser.parse matchRoute url }
    in
    Maybe.withDefault ItemsRoute (Url.Parser.parse matchRoute url)


translate : WebData (List Translation) -> String -> String
translate translationsData key =
    -- Translates the passed key to the current labels (server determines locale by url).
    case translationsData of
        Success translations ->
            case List.Extra.find (\translation -> String.toLower translation.key == String.toLower key) translations of
                Just translation ->
                    translation.label

                Nothing ->
                    key

        _ ->
            key


isLocalMode : String -> Bool
isLocalMode url =
    String.contains "localhost" url


pathToSectionName : Maybe String -> String
pathToSectionName path =
    case path of
        Just h ->
            if String.contains "/reports" h then
                "reports"

            else if String.contains "/teams" h then
                "teams"

            else if String.contains "/stages" h then
                "stages"

            else
                "draws"

        Nothing ->
            "draws"


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            let
                hash =
                    case flags.hash of
                        Just "" ->
                            Nothing

                        Nothing ->
                            Nothing

                        Just _ ->
                            flags.hash

                path =
                    case hash of
                        Just h ->
                            Just h

                        Nothing ->
                            case flags.eventId of
                                Just eventId ->
                                    Just ("#/events/" ++ String.fromInt eventId ++ "/draws")

                                Nothing ->
                                    Nothing
            in
            ( Model flags path False NotAsked NotAsked "" NotAsked NotAsked Nothing
            , Cmd.batch
                [ getTranslations flags
                , case toRoute path of
                    ItemsRoute ->
                        getItems flags

                    ProductRoute id ->
                        getProduct flags id

                    EventRoute id _ ->
                        let
                            _ =
                                Debug.log "init with event:" id
                        in
                        getEvent flags id
                ]
            )

        Err error ->
            let
                flags =
                    Flags Nothing Nothing Nothing "" LeaguesSection False 10 [] Nothing
            in
            ( Model flags Nothing False NotAsked NotAsked "" NotAsked NotAsked (Just (Decode.errorToString error))
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


baseUrl : Flags -> String
baseUrl { host, lang } =
    let
        devUrl =
            "http://api.curling.test:3000/" ++ Maybe.withDefault "en" lang

        productionUrl =
            "https://api-curlingio.global.ssl.fastly.net/" ++ Maybe.withDefault "en" lang
    in
    case host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                devUrl

            else
                productionUrl

        Nothing ->
            productionUrl


baseClubUrl : Flags -> String
baseClubUrl flags =
    baseUrl flags ++ "/clubs/" ++ flags.apiKey ++ "/"


getTranslations : Flags -> Cmd Msg
getTranslations flags =
    let
        url =
            baseUrl flags ++ "/translations"
    in
    RemoteData.Http.get url GotTranslations decodeTranslations


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


getProduct : Flags -> Int -> Cmd Msg
getProduct flags id =
    let
        url =
            baseClubUrl flags
                ++ "products/"
                ++ String.fromInt id
    in
    RemoteData.Http.get url GotProduct decodeProduct


eventSections : List String -> List String
eventSections excludeEventSections =
    let
        -- Check if a section is included (not in the explicitly excluded sections list).
        included section =
            List.map String.toLower excludeEventSections
                |> List.member (String.toLower section)
                |> not
    in
    [ "details", "notes", "registrations", "spares", "draws", "stages", "teams", "reports" ]
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

        gamesPlayed team =
            List.length (sides team)

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
    List.map (\team -> TeamResult team 0 0 0 0 0) teams
        |> List.map (\teamResult -> { teamResult | gamesPlayed = gamesPlayed teamResult.team })
        |> List.map (\teamResult -> { teamResult | wins = wins teamResult.team })
        |> List.map (\teamResult -> { teamResult | losses = losses teamResult.team })
        |> List.map (\teamResult -> { teamResult | ties = ties teamResult.team })
        |> List.map assignPoints


teamResultsRankedByPoints : List TeamResult -> List TeamResult
teamResultsRankedByPoints teamResults =
    List.sortBy .points teamResults
        |> List.reverse


teamHasDetails : Team -> Bool
teamHasDetails team =
    -- Check if there are any team details to show.
    not (List.isEmpty team.lineup)
        || (team.contactName /= Nothing)
        || (team.contactEmail /= Nothing)
        || (team.contactPhone /= Nothing)



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
    | GotTranslations (WebData (List Translation))
    | GotItems (WebData (List Item))
    | ReloadItems
    | UpdateSearch String
    | SelectEvent Int
    | GotEvent (WebData Event)
    | GotProduct (WebData Product)
    | UpdateRoute String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        GotTranslations response ->
            ( { model | translations = response, errorMsg = Nothing }, Cmd.none )

        GotItems response ->
            ( { model | items = response, errorMsg = Nothing }, Cmd.none )

        ReloadItems ->
            ( model, getItems model.flags )

        UpdateSearch val ->
            ( { model | search = String.toLower val }, Cmd.none )

        SelectEvent id ->
            ( model, getEvent model.flags id )

        GotEvent response ->
            ( { model | event = response }
            , Cmd.none
            )

        GotProduct response ->
            ( { model | product = response }
            , Cmd.none
            )

        UpdateRoute path ->
            ( { model | path = Just path }
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
viewItems { flags, fullScreen, translations, search } items =
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

        viewItem item =
            tr []
                ([ td []
                    [ if item.publishResults then
                        a [ href ("#/events/" ++ String.fromInt item.id), onClick (SelectEvent item.id) ] [ text item.name ]

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
                                                    [ a [ href url, target "_blank" ] [ text (translate translations "register" ++ " →") ]
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
                    , placeholder (translate translations "search")
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
viewEvent { flags, path, translations } event =
    let
        viewNavItem eventSection =
            let
                isActiveRoute =
                    -- TODO: This needs a bit of work. I don't like the string pattern matching, would prefer patterning on toRoute result.
                    case path of
                        Just p ->
                            if String.contains "stages" p then
                                eventSection == "stages"

                            else if String.contains "teams" p then
                                eventSection == "teams"

                            else if String.contains "reports" p then
                                eventSection == "reports"

                            else
                                eventSection == "draws"

                        Nothing ->
                            eventSection == "draws"

                newPath =
                    "#/events/" ++ String.fromInt event.id ++ "/" ++ eventSection
            in
            li [ class "nav-item" ]
                [ a
                    [ classList
                        [ ( "nav-link", True )
                        , ( "active", isActiveRoute )
                        ]
                    , onClick (UpdateRoute newPath)
                    , href newPath
                    ]
                    [ text (translate translations eventSection) ]
                ]
    in
    div [ class "p-3" ]
        [ h3 [ class "mb-3" ] [ text event.name ]
        , ul [ class "nav nav-tabs mb-3" ]
            (List.map viewNavItem (eventSections flags.excludeEventSections))
        , case toRoute path of
            EventRoute _ nestedRoute ->
                case nestedRoute of
                    DrawsRoute ->
                        viewDrawSchedule translations event

                    DrawRoute idx ->
                        case List.Extra.getAt (idx - 1) event.draws of
                            Just draw ->
                                viewDraw translations event draw

                            Nothing ->
                                -- TODO Maybe an error instead since we didn't find the corresponding draw?
                                viewDrawSchedule translations event

                    GameRoute drawIndex sheetIndex ->
                        case List.Extra.getAt (drawIndex - 1) event.draws of
                            Just draw ->
                                case List.Extra.getAt (sheetIndex - 1) draw.drawSheets of
                                    Just (Just game) ->
                                        let
                                            sheetLabel =
                                                List.Extra.getAt sheetIndex event.sheetNames
                                        in
                                        viewGame translations event sheetLabel game

                                    _ ->
                                        -- TODO Maybe an error instead since we didn't find the corresponding game?
                                        viewDraw translations event draw

                            Nothing ->
                                -- TODO Maybe an error instead since we didn't find the corresponding draw?
                                viewDrawSchedule translations event

                    StagesRoute ->
                        case List.head event.stages of
                            Just stage ->
                                viewStandings translations event stage

                            Nothing ->
                                p [] [ text "No stages found" ]

                    StageRoute id ->
                        case List.Extra.find (\s -> s.id == id) event.stages of
                            Just stage ->
                                viewStandings translations event stage

                            Nothing ->
                                p [] [ text "Stage not found." ]

                    TeamsRoute ->
                        viewTeams translations event

                    TeamRoute id ->
                        case List.Extra.find (\t -> t.id == id) event.teams of
                            Just team ->
                                viewTeam translations team

                            Nothing ->
                                p [] [ text "Team not found." ]

                    ReportsRoute ->
                        viewReports translations event

                    ReportRoute report ->
                        viewReport translations event report

            _ ->
                viewDrawSchedule translations event
        ]


viewDrawSchedule : WebData (List Translation) -> Event -> Html Msg
viewDrawSchedule translations { id, endScoresEnabled, sheetNames, draws } =
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
            if endScoresEnabled then
                let
                    newPath =
                        "#/events/" ++ String.fromInt id ++ "/draws/" ++ String.fromInt (drawIndex + 1)
                in
                a [ href newPath, onClick (UpdateRoute newPath) ] [ text label ]

            else
                text label

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
                    if endScoresEnabled then
                        let
                            newPath =
                                "#/events/" ++ String.fromInt id ++ "/draws/" ++ String.fromInt (drawIndex + 1) ++ "/sheets/" ++ String.fromInt (sheetIndex + 1)
                        in
                        a
                            [ href newPath
                            , onClick (UpdateRoute newPath)
                            , class stateClass
                            ]
                            [ text game_.nameWithResult ]

                    else
                        text game_.nameWithResult

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
                        ([ th [ style "border-top" "none", style "min-width" "65px" ] [ text (translate translations "draw") ] ]
                            ++ [ th [ style "border-top" "none", style "min-width" "220px" ] [ text (translate translations "starts_at") ] ]
                            ++ List.map (\sheetName -> th [ class "text-center", style "border-top" "none", style "min-width" "198px" ] [ text sheetName ]) sheetNames
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


viewTeams : WebData (List Translation) -> Event -> Html Msg
viewTeams translations { id, teams } =
    let
        viewTeamRow team =
            tr []
                [ td []
                    [ if teamHasDetails team then
                        let
                            newPath =
                                "#/events/" ++ String.fromInt id ++ "/teams/" ++ String.fromInt team.id
                        in
                        a [ href newPath, onClick (UpdateRoute newPath) ] [ text team.name ]

                      else
                        -- No point in linking to team details if there are no more details.
                        text team.name
                    ]
                , td [] []
                , td [] []
                , td [] []
                ]
    in
    table [ class "table table-striped" ]
        [ thead []
            [ tr []
                [ th [ style "border-top" "none" ] [ text (translate translations "team") ]
                , th [ style "border-top" "none" ] [ text (translate translations "coach") ]
                , th [ style "border-top" "none" ] [ text (translate translations "affiliation") ]
                , th [ style "border-top" "none" ] [ text (translate translations "location") ]
                ]
            ]
        , tbody [] (List.map viewTeamRow teams)
        ]


viewStandings : WebData (List Translation) -> Event -> Stage -> Html Msg
viewStandings translations { id, draws, teams, stages } onStage =
    let
        viewStageLink stage =
            let
                newPath =
                    "#/events/" ++ String.fromInt id ++ "/standings/" ++ String.fromInt stage.id
            in
            li [ class "nav-item" ]
                [ a
                    [ href newPath
                    , onClick (UpdateRoute newPath)
                    , classList
                        [ ( "nav-link", True )
                        , ( "active", stage.id == onStage.id )
                        ]
                    ]
                    [ text stage.name ]
                ]

        viewRoundRobin =
            let
                teamResults =
                    gamesByStage onStage draws
                        |> teamResultsForGames onStage teams
                        |> teamResultsRankedByPoints

                hasTies =
                    List.any (\teamResult -> teamResult.ties > 0) teamResults

                viewRow teamResult =
                    let
                        newPath =
                            "#/events/" ++ String.fromInt id ++ "/teams/" ++ String.fromInt teamResult.team.id
                    in
                    tr []
                        [ td []
                            [ if teamHasDetails teamResult.team then
                                a
                                    [ href newPath
                                    , onClick (UpdateRoute newPath)
                                    ]
                                    [ text teamResult.team.name ]

                              else
                                text teamResult.team.name
                            ]
                        , td [ class "text-right" ] [ text (String.fromInt teamResult.gamesPlayed) ]
                        , td [ class "text-right" ] [ text (String.fromInt teamResult.wins) ]
                        , td [ class "text-right" ] [ text (String.fromInt teamResult.losses) ]
                        , if hasTies then
                            td [ class "text-right" ] [ text (String.fromInt teamResult.ties) ]

                          else
                            text ""
                        , td [ class "text-right" ] [ text (String.fromFloat teamResult.points) ]
                        ]
            in
            table [ class "table table-striped" ]
                [ thead []
                    [ tr []
                        [ th [ style "border-top" "none" ] [ text (translate translations "team") ]
                        , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "games") ]
                        , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "wins") ]
                        , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "losses") ]
                        , if hasTies then
                            th [ class "text-right", style "border-top" "none" ] [ text (translate translations "ties") ]

                          else
                            text ""
                        , th [ class "text-right", style "border-top" "none" ] [ text (translate translations "points") ]
                        ]
                    ]
                , tbody [] (List.map viewRow teamResults)
                ]

        viewBracket =
            div [] [ text "Coming Soon." ]
    in
    div []
        [ div [ class "nav nav-pills mb-3" ] (List.map viewStageLink stages)
        , case onStage.stageType of
            RoundRobin ->
                viewRoundRobin

            Bracket ->
                viewBracket
        ]


viewReports : WebData (List Translation) -> Event -> Html Msg
viewReports translations { id } =
    div [] [ text "Coming Soon." ]


viewDraw : WebData (List Translation) -> Event -> Draw -> Html Msg
viewDraw translations event draw =
    let
        viewDrawSheet index drawSheet =
            case drawSheet of
                Just game ->
                    let
                        sheetLabel =
                            List.Extra.getAt index event.sheetNames
                    in
                    div [ class "mt-4" ] [ viewGame translations event sheetLabel game ]

                Nothing ->
                    text ""
    in
    div []
        ([ h5 [] [ text ("Draw " ++ draw.label ++ " - " ++ draw.startsAt) ]
         ]
            ++ List.indexedMap viewDrawSheet draw.drawSheets
        )


viewGame : WebData (List Translation) -> Event -> Maybe String -> Game -> Html Msg
viewGame translations event sheetLabel game =
    let
        numberOfEnds =
            List.map (\s -> List.length s.endScores) game.sides
                |> List.maximum
                |> Maybe.withDefault 0
                |> max event.numberOfEnds

        viewEndHeader endNumber =
            th [ style "border-top" "none" ] [ text (String.fromInt endNumber) ]

        viewSide side =
            let
                viewEndScore endNumber =
                    let
                        endScore =
                            case List.Extra.getAt (endNumber - 1) side.endScores of
                                Just es ->
                                    String.fromInt es

                                Nothing ->
                                    "-"
                    in
                    td [] [ text endScore ]

                teamName =
                    case side.teamId of
                        Just id ->
                            case List.Extra.find (\team -> team.id == id) event.teams of
                                Just team ->
                                    team.name

                                Nothing ->
                                    "TBD"

                        Nothing ->
                            "TBD"
            in
            tr []
                ([ td [] [ text teamName ]
                 , td []
                    [ text
                        (if side.firstHammer then
                            "*"

                         else
                            ""
                        )
                    ]
                 ]
                    ++ List.map viewEndScore (List.range 1 numberOfEnds)
                    ++ [ td [] [ text (String.fromInt (List.sum side.endScores)) ] ]
                )
    in
    div []
        [ table [ class "table" ]
            [ thead []
                [ tr []
                    ([ th [ style "border-top" "none" ] [ text (Maybe.withDefault game.name sheetLabel) ]
                     , th [ style "border-top" "none" ] [ text "LSFE" ]
                     ]
                        ++ List.map viewEndHeader (List.range 1 numberOfEnds)
                        ++ [ th [ style "border-top" "none" ] [ text "Tot" ] ]
                    )
                ]
            , tbody [] (List.map viewSide game.sides)
            ]
        ]


viewTeam : WebData (List Translation) -> Team -> Html Msg
viewTeam translations team =
    div []
        [ h5 [] [ text team.name ]
        , div [] [ text (Maybe.withDefault "" team.coach) ]
        ]


viewReport : WebData (List Translation) -> Event -> String -> Html Msg
viewReport translations { id } report =
    div [] [ text ("Coming Soon: " ++ report) ]



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
