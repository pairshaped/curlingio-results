module Results exposing (init)

import Browser
import Html exposing (Html, a, button, caption, div, h3, h4, h5, i, input, label, li, nav, ol, p, small, span, strong, sup, table, tbody, td, text, th, thead, tr, u, ul)
import Html.Attributes exposing (attribute, class, classList, colspan, href, id, placeholder, rowspan, style, target, title, type_, value)
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
    , scoringHilight : Maybe ScoringHilight
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


type ScoringHilight
    = HilightHammers
    | HilightStolenEnds
    | HilightBlankEnds
    | Hilight1PointEnds
    | Hilight2PointEnds
    | Hilight3PointEnds
    | Hilight4PointEnds
    | Hilight5PlusPointEnds


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
    { drawNumber : Int
    , startsAt : String
    , label : String
    , attendance : Int
    , drawSheets : List (Maybe Game)
    }


type alias Game =
    { drawNumber : Int
    , sheetNumber : Int
    , name : String
    , stageId : Int
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
    , score : Maybe Int
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
                        |> optional "score" (nullable int) Nothing
                        |> optional "end_scores" (list int) []
            in
            Decode.succeed Game
                |> required "draw_number" int
                |> required "sheet_number" int
                |> required "name" string
                |> required "stage_id" int
                |> required "state" decodeGameState
                |> required "game_positions" (list decodeSide)
    in
    Decode.succeed Draw
        |> required "draw_number" int
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

        parsed =
            Maybe.withDefault ItemsRoute (Url.Parser.parse matchRoute url)
    in
    parsed


drawUrl : Int -> Draw -> String
drawUrl eventId draw =
    let
        eventIdStr =
            String.fromInt eventId

        drawNumberStr =
            String.fromInt draw.drawNumber
    in
    "#/events/" ++ eventIdStr ++ "/draws/" ++ drawNumberStr


gameUrl : Int -> Game -> String
gameUrl eventId game =
    let
        eventIdStr =
            String.fromInt eventId

        drawNumberStr =
            String.fromInt game.drawNumber

        sheetNumberStr =
            String.fromInt game.sheetNumber
    in
    "#/events/" ++ eventIdStr ++ "/draws/" ++ drawNumberStr ++ "/sheets/" ++ sheetNumberStr


teamUrl : Int -> Team -> String
teamUrl eventId team =
    let
        eventIdStr =
            String.fromInt eventId

        teamIdStr =
            String.fromInt team.id
    in
    "#/events/" ++ eventIdStr ++ "/teams/" ++ teamIdStr


stageUrl : Int -> Stage -> String
stageUrl eventId stage =
    let
        eventIdStr =
            String.fromInt eventId

        stageIdStr =
            String.fromInt stage.id
    in
    "#/events/" ++ eventIdStr ++ "/stages/" ++ stageIdStr


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


colorNameToRGB : String -> String
colorNameToRGB color =
    case color of
        "red" ->
            "rgb(204, 0, 0)"

        "yellow" ->
            "rgb(204, 204, 0)"

        _ ->
            color


sideResultToString : WebData (List Translation) -> Maybe SideResult -> String
sideResultToString translations result =
    translate translations
        (case result of
            Just SideResultWon ->
                "won"

            Just SideResultLost ->
                "lost"

            Just SideResultTied ->
                "tied"

            Just SideResultConceded ->
                "conceded"

            Just SideResultForfeited ->
                "forfeited"

            Just SideResultTimePenalized ->
                "was time penalized"

            Nothing ->
                "unknown"
        )


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
            ( Model flags path False NotAsked NotAsked "" NotAsked NotAsked Nothing Nothing
            , Cmd.batch
                [ getTranslations flags
                , case toRoute path of
                    ItemsRoute ->
                        getItems flags

                    ProductRoute id ->
                        getProduct flags id

                    EventRoute id _ ->
                        getEvent flags id
                ]
            )

        Err error ->
            let
                flags =
                    Flags Nothing Nothing Nothing "" LeaguesSection False 10 [] Nothing
            in
            ( Model flags Nothing False NotAsked NotAsked "" NotAsked NotAsked Nothing (Just (Decode.errorToString error))
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


gameNameWithResult : List Team -> Game -> String
gameNameWithResult teams game =
    case game.state of
        GameComplete ->
            let
                teamNameForSide side =
                    List.Extra.find (\t -> Just t.id == side.teamId) teams
                        |> Maybe.map .shortName

                winningSide =
                    List.Extra.find (\s -> s.result == Just SideResultWon) game.sides

                losingSide =
                    case winningSide of
                        Just winningSide_ ->
                            List.Extra.find (\s -> s.teamId /= winningSide_.teamId) game.sides

                        Nothing ->
                            Nothing

                tied =
                    List.any (\s -> s.result == Just SideResultTied) game.sides

                sortedTeamNames =
                    if tied then
                        List.map teamNameForSide game.sides
                            |> List.filterMap identity

                    else
                        [ winningSide, losingSide ]
                            |> List.filterMap identity
                            |> List.map teamNameForSide
                            |> List.filterMap identity
            in
            String.join
                (if tied then
                    " tied "

                 else
                    " defeated "
                )
                sortedTeamNames

        _ ->
            game.name


findTeamForSide teams side =
    let
        findTeamById teamId =
            List.Extra.find (\team -> team.id == teamId) teams
    in
    side.teamId
        |> Maybe.andThen findTeamById



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
    | ToggleScoringHilight ScoringHilight


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

        ToggleScoringHilight scoringHilight ->
            ( { model
                | scoringHilight =
                    if model.scoringHilight == Just scoringHilight then
                        Nothing

                    else
                        Just scoringHilight
              }
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
viewEvent { flags, path, translations, scoringHilight } event =
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
                        viewDrawSchedule translations scoringHilight event

                    DrawRoute drawNumber ->
                        case List.Extra.find (\d -> d.drawNumber == drawNumber) event.draws of
                            Just draw ->
                                viewDraw translations scoringHilight event draw

                            Nothing ->
                                -- TODO Maybe an error instead since we didn't find the corresponding draw?
                                viewDrawSchedule translations scoringHilight event

                    GameRoute drawNumber sheetNumber ->
                        case List.Extra.find (\d -> d.drawNumber == drawNumber) event.draws of
                            Just draw ->
                                let
                                    matchingGame =
                                        List.filterMap identity draw.drawSheets
                                            |> List.Extra.find (\g -> g.drawNumber == drawNumber && g.sheetNumber == sheetNumber)
                                in
                                case matchingGame of
                                    Just game ->
                                        let
                                            sheetLabel =
                                                List.Extra.getAt (sheetNumber - 1) event.sheetNames
                                                    |> Maybe.withDefault ""
                                        in
                                        viewGame translations scoringHilight event sheetLabel True draw game

                                    _ ->
                                        -- TODO Maybe an error instead since we didn't find the corresponding game?
                                        viewDraw translations scoringHilight event draw

                            Nothing ->
                                -- TODO Maybe an error instead since we didn't find the corresponding draw?
                                viewDrawSchedule translations scoringHilight event

                    StagesRoute ->
                        case List.head event.stages of
                            Just stage ->
                                viewStages translations event stage

                            Nothing ->
                                p [] [ text "No stages found" ]

                    StageRoute id ->
                        case List.Extra.find (\s -> s.id == id) event.stages of
                            Just stage ->
                                viewStages translations event stage

                            Nothing ->
                                p [] [ text "Stage not found." ]

                    TeamsRoute ->
                        viewTeams translations event

                    TeamRoute id ->
                        case List.Extra.find (\t -> t.id == id) event.teams of
                            Just team ->
                                viewTeam translations event team

                            Nothing ->
                                p [] [ text "Team not found." ]

                    ReportsRoute ->
                        viewReports translations event

                    ReportRoute report ->
                        viewReport translations scoringHilight event report

            _ ->
                viewDrawSchedule translations scoringHilight event
        ]


viewDrawSchedule : WebData (List Translation) -> Maybe ScoringHilight -> Event -> Html Msg
viewDrawSchedule translations scoringHilight { id, endScoresEnabled, sheetNames, teams, draws } =
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

        drawLink draw label =
            if endScoresEnabled then
                let
                    newPath =
                        drawUrl id draw
                in
                a [ href newPath, onClick (UpdateRoute newPath) ] [ text label ]

            else
                text label

        gameLink game =
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
                                gameUrl id game_
                        in
                        a
                            [ href newPath
                            , onClick (UpdateRoute newPath)
                            , class stateClass
                            , title (gameNameWithResult teams game_)
                            ]
                            [ text game_.name ]

                    else
                        text (gameNameWithResult teams game_)

                Nothing ->
                    text ""

        viewTableSchedule =
            let
                viewDrawRow draw =
                    let
                        viewDrawSheet game =
                            td [ class "text-center" ]
                                [ gameLink game ]
                    in
                    tr
                        [ classList
                            [ ( "bg-light", isDrawActive draw )
                            ]
                        ]
                        ([ td [] [ drawLink draw draw.label ] ]
                            ++ [ td [] [ drawLink draw draw.startsAt ]
                               ]
                            ++ List.map viewDrawSheet draw.drawSheets
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
                    (List.map viewDrawRow draws)
                ]

        viewListSchedule =
            let
                viewDrawListItem draw =
                    let
                        viewDrawSheet game =
                            li []
                                [ gameLink game ]
                    in
                    div []
                        [ h5 [ class "mb-0" ] [ drawLink draw (translate translations "draw" ++ " " ++ draw.label) ]
                        , small [] [ drawLink draw draw.startsAt ]
                        , ul [ class "mt-2" ] (List.map viewDrawSheet draw.drawSheets)
                        ]
            in
            div [] (List.map viewDrawListItem draws)
    in
    div []
        [ div [ class "table-responsive d-none d-md-block" ] [ viewTableSchedule ]
        , div [ class "d-md-none" ] [ viewListSchedule ]
        ]


viewTeams : WebData (List Translation) -> Event -> Html Msg
viewTeams translations event =
    let
        viewTeamRow team =
            tr []
                [ td []
                    [ if teamHasDetails team then
                        let
                            newPath =
                                teamUrl event.id team
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
        , tbody [] (List.map viewTeamRow event.teams)
        ]


viewStages : WebData (List Translation) -> Event -> Stage -> Html Msg
viewStages translations { id, draws, teams, stages } onStage =
    let
        viewStageLink stage =
            let
                newPath =
                    stageUrl id stage
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
                            teamUrl id teamResult.team
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
viewReports translations event =
    let
        hasAttendance =
            (List.map .attendance event.draws |> List.sum) > 0

        scoringAnalysisLink =
            "#/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"
    in
    ul []
        [ if hasAttendance then
            li [] [ text "Attendance" ]

          else
            text ""
        , li [] [ text "Competition Matrix" ]
        , if event.endScoresEnabled then
            li []
                [ a
                    [ href scoringAnalysisLink
                    , onClick (UpdateRoute scoringAnalysisLink)
                    ]
                    [ text "Scoring Analysis" ]
                ]

          else
            text ""
        , if event.endScoresEnabled then
            li [] [ text "Scoring Analysis by Hammer" ]

          else
            text ""
        , li [] [ text "Team Rosters" ]
        ]


viewDraw : WebData (List Translation) -> Maybe ScoringHilight -> Event -> Draw -> Html Msg
viewDraw translations scoringHilight event draw =
    let
        viewDrawSheet drawSheet =
            case drawSheet of
                Just game ->
                    let
                        sheetLabel =
                            List.Extra.getAt (game.sheetNumber - 1) event.sheetNames
                                |> Maybe.withDefault ""
                    in
                    viewGame translations scoringHilight event sheetLabel False draw game

                Nothing ->
                    text ""
    in
    div []
        ([ nav
            [ attribute "aria-label" "breadcrumb" ]
            [ ol [ class "breadcrumb" ]
                [ li
                    [ class "breadcrumb-item active"
                    , attribute "aria-current" "page"
                    ]
                    [ text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt) ]
                ]
            ]
         ]
            ++ List.map viewDrawSheet draw.drawSheets
        )


viewGame : WebData (List Translation) -> Maybe ScoringHilight -> Event -> String -> Bool -> Draw -> Game -> Html Msg
viewGame translations scoringHilight event sheetLabel detailed draw game =
    let
        maxNumberOfEnds =
            List.map (\s -> List.length s.endScores) game.sides
                |> List.maximum
                |> Maybe.withDefault 0
                |> max event.numberOfEnds

        viewEndHeader endNumber =
            th [ class "text-center", style "width" "50px" ] [ text (String.fromInt endNumber) ]

        teamName side =
            findTeamForSide event.teams side
                |> Maybe.map .name
                |> Maybe.withDefault "TBD"

        viewSide side =
            let
                wonOrTied =
                    (side.result == Just SideResultWon)
                        || (side.result == Just SideResultTied)

                viewEndScore endNumber =
                    let
                        hasHammer =
                            if endNumber == 1 && side.firstHammer then
                                True

                            else
                                case List.Extra.getAt (endNumber - 2) side.endScores of
                                    Just es ->
                                        es == 0

                                    Nothing ->
                                        False

                        endScoreStr =
                            case List.Extra.getAt (endNumber - 1) side.endScores of
                                Just es ->
                                    String.fromInt es

                                Nothing ->
                                    "-"

                        endScoreInt =
                            List.Extra.getAt (endNumber - 1) side.endScores
                                |> Maybe.withDefault 0

                        stolenEnd =
                            (endScoreInt > 0) && not hasHammer

                        blankEnd =
                            case List.Extra.getAt (endNumber - 1) side.endScores of
                                Just 0 ->
                                    True

                                _ ->
                                    False

                        isHilighted =
                            (scoringHilight == Just HilightHammers && hasHammer)
                                || (scoringHilight == Just HilightStolenEnds && stolenEnd)
                                || (scoringHilight == Just HilightBlankEnds && blankEnd)
                                || (scoringHilight == Just Hilight1PointEnds && (endScoreInt == 1))
                                || (scoringHilight == Just Hilight2PointEnds && (endScoreInt == 2))
                                || (scoringHilight == Just Hilight3PointEnds && (endScoreInt == 3))
                                || (scoringHilight == Just Hilight4PointEnds && (endScoreInt == 4))
                                || (scoringHilight == Just Hilight5PlusPointEnds && (endScoreInt > 4))
                    in
                    td
                        [ classList
                            [ ( "text-center", True )
                            , ( "bg-light", hasHammer && not isHilighted )
                            , ( "bg-secondary", isHilighted )
                            , ( "text-light", isHilighted )
                            , ( "font-weight-bolder", isHilighted )
                            ]
                        ]
                        [ text endScoreStr ]

                rockStyles =
                    [ style "border-bottom"
                        ("solid 3px "
                            ++ colorNameToRGB
                                (if side.topRock then
                                    event.topRock

                                 else
                                    event.botRock
                                )
                        )
                    ]
            in
            tr []
                ([ td
                    [ classList
                        [ ( "font-weight-bold", wonOrTied )
                        , ( "text-primary", side.firstHammer && scoringHilight == Just HilightHammers )
                        ]
                    ]
                    [ span rockStyles
                        [ text
                            (teamName side
                                ++ (if side.firstHammer then
                                        " *"

                                    else
                                        ""
                                   )
                            )
                        ]
                    ]
                 ]
                    ++ List.map viewEndScore (List.range 1 maxNumberOfEnds)
                    ++ [ td
                            [ classList
                                [ ( "text-center", True )
                                , ( "font-weight-bold", wonOrTied )
                                , ( "text-success", wonOrTied )
                                ]
                            ]
                            [ text (String.fromInt (List.sum side.endScores)) ]
                       ]
                )

        viewGameCaption =
            let
                label =
                    i []
                        [ case game.state of
                            GameComplete ->
                                let
                                    winner =
                                        List.Extra.find (\s -> s.result == Just SideResultWon) game.sides

                                    loser =
                                        List.Extra.find (\s -> s.result /= Just SideResultWon) game.sides

                                    sideResultText result =
                                        sideResultToString translations result
                                in
                                if List.any (\s -> s.result == Just SideResultTied) game.sides then
                                    let
                                        joinedResult =
                                            List.map teamName game.sides
                                                |> String.join (" " ++ sideResultText (Just SideResultTied) ++ " ")
                                    in
                                    text (joinedResult ++ ".")

                                else
                                    case ( winner, loser ) of
                                        ( Just w, Just l ) ->
                                            text (teamName w ++ " " ++ sideResultText w.result ++ ", " ++ teamName l ++ " " ++ sideResultText l.result ++ ".")

                                        _ ->
                                            text "Unknown result."

                            GameActive ->
                                text (translate translations "game_in_progress" ++ ": " ++ game.name)

                            GamePending ->
                                text (translate translations "upcoming_game" ++ ": " ++ game.name)
                        ]
            in
            if detailed then
                label

            else
                a
                    [ href gameLink
                    , onClick (UpdateRoute gameLink)
                    ]
                    [ label ]

        viewGameHilight =
            case scoringHilight of
                Just hilight ->
                    button
                        [ style "cursor" "pointer"
                        , class "btn btn-sm btn-secondary"
                        , onClick (ToggleScoringHilight hilight)
                        ]
                        [ span []
                            [ text
                                (case hilight of
                                    HilightHammers ->
                                        "Hammers"

                                    HilightStolenEnds ->
                                        "Stolen ends"

                                    HilightBlankEnds ->
                                        "Blank ends"

                                    Hilight1PointEnds ->
                                        "1 point ends"

                                    Hilight2PointEnds ->
                                        "2 point ends"

                                    Hilight3PointEnds ->
                                        "3 point ends"

                                    Hilight4PointEnds ->
                                        "4 point ends"

                                    Hilight5PlusPointEnds ->
                                        "5+ point ends"
                                )
                            ]
                        ]

                Nothing ->
                    text ""

        gameLink =
            gameUrl event.id game

        drawLink =
            drawUrl event.id draw
    in
    div []
        [ if detailed then
            nav [ attribute "aria-label" "breadcrumb" ]
                [ ol [ class "breadcrumb" ]
                    [ li [ class "breadcrumb-item" ]
                        [ a
                            [ href drawLink
                            , onClick (UpdateRoute drawLink)
                            ]
                            [ text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt) ]
                        ]
                    , li
                        [ class "breadcrumb-item active"
                        , attribute "aria-current" "page"
                        ]
                        [ text game.name ]
                    ]
                ]

          else
            text ""
        , div [ class "table-responsive mt-4" ]
            [ table [ class "table table-bordered mb-0" ]
                [ caption []
                    [ div [ class "d-flex justify-content-between" ]
                        [ viewGameCaption
                        , viewGameHilight
                        ]
                    ]
                , thead []
                    [ tr []
                        ([ th [ style "min-width" "160px" ]
                            [ span [] [ text sheetLabel ]
                            , if detailed then
                                text ""

                              else
                                small [ class "ml-3" ]
                                    [ a [ href gameLink, onClick (UpdateRoute gameLink) ] [ text game.name ]
                                    ]
                            ]
                         ]
                            ++ List.map viewEndHeader (List.range 1 maxNumberOfEnds)
                            ++ [ th [ class "text-center", style "width" "64px" ] [ text "Total" ] ]
                        )
                    ]
                , tbody [] (List.map viewSide game.sides)
                ]
            ]
        , if detailed then
            div [ class "mt-3" ]
                [ List.map (findTeamForSide event.teams) game.sides
                    |> List.filterMap identity
                    |> viewReportScoringAnalysis translations scoringHilight event
                ]

          else
            text ""
        ]


viewTeam : WebData (List Translation) -> Event -> Team -> Html Msg
viewTeam translations event team =
    let
        hasSideForTeam game =
            List.any (\s -> s.teamId == Just team.id) game.sides

        draws =
            let
                hasGameForTeam draw =
                    List.filterMap identity draw.drawSheets
                        |> List.filter hasSideForTeam
                        |> List.isEmpty
                        |> not
            in
            List.filter hasGameForTeam event.draws

        viewTeamDraw : Draw -> List (Html Msg)
        viewTeamDraw draw =
            let
                games : List Game
                games =
                    List.filterMap identity draw.drawSheets
                        |> List.filter hasSideForTeam

                viewTeamGame : Game -> Html Msg
                viewTeamGame game =
                    let
                        drawPath =
                            drawUrl event.id draw

                        gamePath =
                            gameUrl event.id game

                        resultLabel =
                            let
                                resultText =
                                    let
                                        sideResultText res =
                                            sideResultToString translations res
                                    in
                                    case game.state of
                                        GameComplete ->
                                            List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                                                |> Maybe.map .result
                                                |> Maybe.map sideResultText

                                        GameActive ->
                                            Just (translate translations "game_in_progress")

                                        GamePending ->
                                            Nothing
                            in
                            case resultText of
                                Just t ->
                                    a [ href gamePath, onClick (UpdateRoute gamePath) ] [ text t ]

                                Nothing ->
                                    text "-"

                        gameScoreLabel =
                            let
                                gameScore =
                                    List.map (\s -> s.score) game.sides
                                        |> List.filterMap identity
                                        |> List.sort
                                        |> List.reverse
                                        |> List.map String.fromInt
                                        |> String.join " - "
                            in
                            case gameScore of
                                "" ->
                                    text ""

                                _ ->
                                    a [ href gamePath, onClick (UpdateRoute gamePath) ] [ text gameScore ]

                        opponentLabel =
                            let
                                opponent =
                                    List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                                        |> Maybe.andThen (findTeamForSide event.teams)
                            in
                            case opponent of
                                Just oppo ->
                                    let
                                        oppoPath =
                                            teamUrl event.id oppo
                                    in
                                    a
                                        [ href oppoPath
                                        , onClick (UpdateRoute oppoPath)
                                        ]
                                        [ text oppo.name
                                        ]

                                Nothing ->
                                    text ""
                    in
                    tr []
                        [ td [] [ a [ href drawPath, onClick (UpdateRoute drawPath) ] [ text draw.label ] ]
                        , td [] [ a [ href drawPath, onClick (UpdateRoute drawPath) ] [ text draw.startsAt ] ]
                        , td [] [ resultLabel ]
                        , td [] [ gameScoreLabel ]
                        , td [] [ opponentLabel ]
                        ]
            in
            List.map viewTeamGame games
    in
    div []
        [ h5 [ class "mb-3" ] [ text team.name ]
        , div [] [ text (Maybe.withDefault "" team.coach) ]
        , table [ class "table" ]
            [ thead []
                [ tr []
                    [ th [ colspan 2 ] [ text "Draw" ]
                    , th [] [ text "Result" ]
                    , th [] [ text "Score" ]
                    , th [] [ text "Opponent" ]
                    ]
                ]
            , tbody [] (List.map viewTeamDraw draws |> List.concat)
            ]
        ]


viewReportScoringAnalysis : WebData (List Translation) -> Maybe ScoringHilight -> Event -> List Team -> Html Msg
viewReportScoringAnalysis translations scoringHilight event teams =
    let
        isHilighted onHilight =
            scoringHilight == Just onHilight

        isForGame =
            List.length teams == 2

        fullReportUrl =
            "#/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"
    in
    div
        [ class "table-responsive" ]
        [ div
            [ class "d-flex justify-content-between" ]
            [ h5 [ class "mb-3" ] [ text (translate translations "scoring_analysis_report") ]
            , if isForGame then
                a
                    [ href fullReportUrl
                    , onClick (UpdateRoute fullReportUrl)
                    ]
                    [ small [] [ text (translate translations "full_report" ++ " →") ] ]

              else
                text ""
            ]
        , table [ class "table table-sm table-bordered small" ]
            ([ caption []
                [ ol [ class "pl-3" ]
                    [ li [] [ text " LSFE - Last Shot in the First End" ]
                    , li [] [ text "SE - Stolen Ends" ]
                    , li [] [ text "BE - Blank Ends" ]
                    , li [] [ text "SP - Stolen Points" ]
                    ]
                ]
             , thead [ class "thead-light" ]
                [ tr []
                    [ th [] [ text "Team" ]
                    , th [ class "text-center" ] [ text "Games" ]
                    , th [ class "text-center" ] [ text "Ends" ]
                    , th [] []
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightHammers) )
                            , ( "text-danger", isForGame && isHilighted HilightHammers )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightHammers)
                        ]
                        [ span [] [ text "LSFE" ], sup [] [ text "1" ] ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightStolenEnds) )
                            , ( "text-danger", isForGame && isHilighted HilightStolenEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightStolenEnds)
                        ]
                        [ span [] [ text "SE" ], sup [] [ text "2" ] ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightBlankEnds) )
                            , ( "text-danger", isForGame && isHilighted HilightBlankEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightBlankEnds)
                        ]
                        [ span [] [ text "BE" ], sup [] [ text "3" ] ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight1PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight1PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight1PointEnds)
                        ]
                        [ text "1pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight2PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight2PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight2PointEnds)
                        ]
                        [ text "2pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight3PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight3PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight3PointEnds)
                        ]
                        [ text "3pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight4PointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight4PointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight4PointEnds)
                        ]
                        [ text "4pt" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted Hilight5PlusPointEnds) )
                            , ( "text-danger", isForGame && isHilighted Hilight5PlusPointEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight Hilight5PlusPointEnds)
                        ]
                        [ text "5pt+" ]
                    , th [ class "text-right" ] [ text "Tot" ]
                    , th [ class "text-right" ] [ text "Avg" ]
                    , th
                        [ classList
                            [ ( "text-right", True )
                            , ( "text-primary", isForGame && not (isHilighted HilightStolenEnds) )
                            , ( "text-danger", isForGame && isHilighted HilightStolenEnds )
                            ]
                        , style "cursor" "pointer"
                        , onClick (ToggleScoringHilight HilightStolenEnds)
                        ]
                        [ span [] [ text "SP" ], sup [] [ text "4" ] ]
                    ]
                ]
             ]
                ++ List.map (viewTeamScoringAnalysis event) teams
            )
        ]


viewTeamScoringAnalysis : Event -> Team -> Html Msg
viewTeamScoringAnalysis event team =
    let
        games =
            let
                participatedIn sides =
                    List.any (\s -> s.teamId == Just team.id) sides
            in
            List.map .drawSheets event.draws
                |> List.concat
                |> List.filterMap identity
                |> List.filter (\g -> g.state /= GamePending)
                |> List.filter (\g -> participatedIn g.sides)

        sidesFor =
            List.map .sides games
                |> List.concat
                |> List.filter (\s -> s.teamId == Just team.id)

        sidesAgainst =
            List.map .sides games
                |> List.concat
                |> List.filter (\s -> s.teamId /= Just team.id)

        endsFor =
            List.map .endScores sidesFor
                |> List.concat

        endsAgainst =
            List.map .endScores sidesAgainst
                |> List.concat

        firstHammerCountFor =
            List.filter (\s -> s.firstHammer == True) sidesFor
                |> List.length

        firstHammerCountAgainst =
            List.filter (\s -> s.firstHammer == True) sidesAgainst
                |> List.length

        blankEndsFor =
            List.filter (\i -> i /= 0) endsFor
                |> List.length

        blankEndsAgainst =
            List.filter (\i -> i == 0) endsAgainst
                |> List.length

        onePointEndsFor =
            List.filter (\i -> i == 1) endsFor
                |> List.length

        onePointEndsAgainst =
            List.filter (\i -> i == 1) endsAgainst
                |> List.length

        twoPointEndsFor =
            List.filter (\i -> i == 2) endsFor
                |> List.length

        twoPointEndsAgainst =
            List.filter (\i -> i == 2) endsAgainst
                |> List.length

        threePointEndsFor =
            List.filter (\i -> i == 3) endsFor
                |> List.length

        threePointEndsAgainst =
            List.filter (\i -> i == 3) endsAgainst
                |> List.length

        fourPointEndsFor =
            List.filter (\i -> i == 4) endsFor
                |> List.length

        fourPointEndsAgainst =
            List.filter (\i -> i == 4) endsAgainst
                |> List.length

        moreThanFourPointEndsFor =
            List.filter (\i -> i > 4) endsFor
                |> List.length

        moreThanFourPointEndsAgainst =
            List.filter (\i -> i > 4) endsAgainst
                |> List.length

        totalPointsFor =
            List.sum endsFor

        totalPointsAgainst =
            List.sum endsAgainst

        averagePointsFor =
            -- total points divided by number of ends. We multiple by 100 then round then divide by 100 so that we get 2 decimal places.
            toFloat (round ((toFloat totalPointsFor / toFloat (List.length endsFor)) * 100)) / 100

        averagePointsAgainst =
            -- see averagePointsFor
            toFloat (round ((toFloat totalPointsAgainst / toFloat (List.length endsAgainst)) * 100)) / 100

        stolenEnds : Bool -> List Int
        stolenEnds for =
            -- The for is a bool to indicate for (True) or against (False)
            let
                -- We need to look at each end score, and whether or not they had the hammer.
                stolenEndsForGame game =
                    let
                        -- We don't know if the team side is the first or second, so build a tuple so we know.
                        sides =
                            if for then
                                ( List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                                , List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                                )

                            else
                                ( List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                                , List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                                )

                        -- We don't care about the event's number of ends, just the max of either side.
                        numberOfEnds =
                            List.map (\side -> List.length side.endScores) game.sides
                                |> List.maximum
                                |> Maybe.withDefault 0

                        stolenEnd : Side -> Side -> Int -> Maybe Int
                        stolenEnd sideFor sideAgainst endIndex =
                            let
                                hasHammer =
                                    if endIndex > 0 then
                                        -- Check if they lost the previous end (thus getting hammer for this end)
                                        Maybe.withDefault 0 (List.Extra.getAt (endIndex - 1) sideAgainst.endScores) > 0

                                    else
                                        -- First end and first hammer?
                                        sideFor.firstHammer == True
                            in
                            if hasHammer then
                                Nothing

                            else
                                List.Extra.getAt endIndex sideFor.endScores
                    in
                    case sides of
                        ( Just sideFor, Just sideAgainst ) ->
                            List.range 0 numberOfEnds
                                |> List.map (stolenEnd sideFor sideAgainst)
                                |> List.filterMap identity
                                |> List.filter (\e -> e > 0)

                        _ ->
                            []
            in
            List.map (\game -> stolenEndsForGame game) games
                |> List.concat

        stolenEndsCount for =
            List.length (stolenEnds for)

        stolenPoints for =
            List.sum (stolenEnds for)
    in
    tbody []
        [ tr []
            [ td [ rowspan 2 ]
                [ a
                    [ class "d-block mt-3"
                    , href (teamUrl event.id team)
                    , onClick (UpdateRoute (teamUrl event.id team))
                    ]
                    [ text team.name ]
                ]
            , td [ rowspan 2, class "text-center" ] [ div [ class "mt-3" ] [ text (String.fromInt (List.length games)) ] ]
            , td [ rowspan 2, class "text-center" ] [ div [ class "mt-3" ] [ text (String.fromInt (List.length endsFor)) ] ]
            , td [ class "pl-2" ] [ text "For" ]
            , td [ class "text-right" ] [ text (String.fromInt firstHammerCountFor) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenEndsCount True)) ]
            , td [ class "text-right" ] [ text (String.fromInt blankEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt onePointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt twoPointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt threePointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt fourPointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt moreThanFourPointEndsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt totalPointsFor) ]
            , td [ class "text-right" ] [ text (String.fromFloat averagePointsFor) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenPoints True)) ]
            ]
        , tr []
            [ td [ class "pl-2" ] [ text "Against" ]
            , td [ class "text-right" ] [ text (String.fromInt firstHammerCountAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenEndsCount False)) ]
            , td [ class "text-right" ] [ text (String.fromInt blankEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt onePointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt twoPointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt threePointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt fourPointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt moreThanFourPointEndsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt totalPointsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromFloat averagePointsAgainst) ]
            , td [ class "text-right" ] [ text (String.fromInt (stolenPoints False)) ]
            ]
        ]


viewReport : WebData (List Translation) -> Maybe ScoringHilight -> Event -> String -> Html Msg
viewReport translations scoringHilight event report =
    case report of
        "scoring_analysis" ->
            viewReportScoringAnalysis translations scoringHilight event event.teams

        _ ->
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
