module Results.Rest exposing (..)

import Browser.Dom
import Element exposing (Device)
import Http
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Results.Types exposing (..)
import Shared.Theme exposing (Theme, decodeTheme, defaultTheme)
import Shared.Translation exposing (Translation, decodeTranslations)
import Task
import Url
import Url.Parser exposing ((</>), Parser)


init : Decode.Value -> ( Model, Cmd Msg )
init flags_ =
    let
        device =
            Element.classifyDevice { width = 800, height = 600 }
    in
    case Decode.decodeValue decodeFlags flags_ of
        Ok flags ->
            let
                newHash =
                    let
                        eventRouteMaybe =
                            case flags.eventId of
                                Just eventId ->
                                    let
                                        section =
                                            case flags.defaultEventSection of
                                                Nothing ->
                                                    "details"

                                                Just "" ->
                                                    "details"

                                                Just defaultEventSection ->
                                                    defaultEventSection
                                    in
                                    "/events/" ++ String.fromInt eventId ++ "/" ++ section

                                Nothing ->
                                    ""
                    in
                    case flags.hash of
                        Just "" ->
                            eventRouteMaybe

                        Nothing ->
                            eventRouteMaybe

                        Just hash ->
                            hash

                newModel =
                    { flags = flags
                    , hash = newHash
                    , translations = NotAsked
                    , items = NotAsked
                    , itemFilter = ItemFilter 1 0 "" False
                    , product = NotAsked
                    , event = NotAsked
                    , eventConfig = EventConfig Nothing Nothing False Nothing False
                    , errorMsg = Nothing
                    }
            in
            ( newModel
            , Cmd.batch
                [ Task.perform InitDevice Browser.Dom.getViewport
                , getTranslations flags
                , Tuple.second (getItemsMaybe newModel newHash False)
                , Tuple.second (getEventMaybe newModel newHash False)
                , Tuple.second (getProductMaybe newModel newHash)
                ]
            )

        Err error ->
            let
                flags =
                    { host = Nothing
                    , hash = Nothing
                    , lang = "en"
                    , apiKey = Nothing
                    , subdomain = Nothing
                    , fullScreenToggle = False
                    , fullScreen = False
                    , searchable = True
                    , section = LeaguesSection
                    , registration = False
                    , showWaiversForTeams = False
                    , excludeEventSections = []
                    , defaultEventSection = Nothing
                    , eventId = Nothing
                    , theme = defaultTheme
                    , loggedInCurlerIds = []
                    , device = device
                    }
            in
            ( { flags = flags
              , hash = ""
              , translations = NotAsked
              , items = NotAsked
              , itemFilter = ItemFilter 1 0 "" False
              , product = NotAsked
              , event = NotAsked
              , eventConfig = EventConfig Nothing Nothing False Nothing False
              , errorMsg = Just (Decode.errorToString error)
              }
            , Cmd.none
            )



-- ROUTING


matchRoute : Maybe String -> Parser (Route -> a) a
matchRoute defaultEventSection =
    Url.Parser.oneOf
        [ Url.Parser.map ItemsRoute Url.Parser.top
        , Url.Parser.map ProductRoute (Url.Parser.s "products" </> Url.Parser.int)
        , Url.Parser.map EventRoute (Url.Parser.s "events" </> Url.Parser.int </> matchNestedEventRoute defaultEventSection)
        ]


matchNestedEventRoute : Maybe String -> Parser (NestedEventRoute -> a) a
matchNestedEventRoute defaultEventSection =
    let
        defaultRoute =
            case defaultEventSection of
                Just section ->
                    case section of
                        "registrations" ->
                            RegistrationsRoute

                        "spares" ->
                            SparesRoute

                        "draws" ->
                            DrawsRoute

                        "stages" ->
                            StagesRoute

                        "teams" ->
                            TeamsRoute

                        "reports" ->
                            ReportsRoute

                        _ ->
                            DetailsRoute

                Nothing ->
                    DetailsRoute
    in
    Url.Parser.oneOf
        [ Url.Parser.map defaultRoute Url.Parser.top
        , Url.Parser.map DetailsRoute (Url.Parser.s "details")
        , Url.Parser.map RegistrationsRoute (Url.Parser.s "registrations")
        , Url.Parser.map SparesRoute (Url.Parser.s "spares")
        , Url.Parser.map DrawsRoute (Url.Parser.s "draws")
        , Url.Parser.map StagesRoute (Url.Parser.s "stages")
        , Url.Parser.map TeamsRoute (Url.Parser.s "teams")
        , Url.Parser.map ReportsRoute (Url.Parser.s "reports")
        , Url.Parser.map DrawRoute (Url.Parser.s "draws" </> Url.Parser.int)
        , Url.Parser.map GameRoute (Url.Parser.s "games" </> Url.Parser.string)
        , Url.Parser.map StageRoute (Url.Parser.s "stages" </> Url.Parser.int)
        , Url.Parser.map TeamRoute (Url.Parser.s "teams" </> Url.Parser.int)
        , Url.Parser.map ReportRoute (Url.Parser.s "reports" </> Url.Parser.string)
        ]


toRoute : Maybe String -> String -> Route
toRoute defaultEventSection hash =
    let
        url =
            let
                fixedHash =
                    hash
                        |> String.replace "#" ""
            in
            { host = "api-curlingio.global.ssl.fastly.net"
            , port_ = Nothing
            , protocol = Url.Https
            , query = Nothing
            , fragment = Nothing
            , path = fixedHash
            }

        parsed =
            Maybe.withDefault ItemsRoute (Url.Parser.parse (matchRoute defaultEventSection) url)
    in
    parsed


eventSectionForRoute : NestedEventRoute -> String
eventSectionForRoute route =
    case route of
        DetailsRoute ->
            "details"

        RegistrationsRoute ->
            "registrations"

        SparesRoute ->
            "spares"

        DrawsRoute ->
            "draws"

        DrawRoute _ ->
            "draws"

        StagesRoute ->
            "stages"

        StageRoute _ ->
            "stages"

        GameRoute _ ->
            "draws"

        TeamsRoute ->
            "teams"

        TeamRoute _ ->
            "teams"

        ReportsRoute ->
            "reports"

        ReportRoute _ ->
            "reports"



-- FETCHING


drawUrl : Int -> Int -> String
drawUrl eventId drawId =
    "/events/" ++ String.fromInt eventId ++ "/draws/" ++ String.fromInt drawId


gameUrl : Int -> String -> String
gameUrl eventId gameId =
    "/events/" ++ String.fromInt eventId ++ "/games/" ++ gameId


teamUrl : Int -> Int -> String
teamUrl eventId teamId =
    "/events/" ++ String.fromInt eventId ++ "/teams/" ++ String.fromInt teamId


stageUrl : Int -> Stage -> String
stageUrl eventId stage =
    let
        eventIdStr =
            String.fromInt eventId

        stageIdStr =
            String.fromInt stage.id
    in
    "/events/" ++ String.fromInt eventId ++ "/stages/" ++ String.fromInt stage.id


baseUrl : Flags -> String
baseUrl { host, lang } =
    let
        devUrl =
            -- Development
            "http://api.curling.test:3000/" ++ lang

        -- "https://api-curlingio.global.ssl.fastly.net/" ++ lang
        -- productionUrl =
        --     -- Production without caching
        --     "https://api.curling.io/" ++ lang
        --
        productionCachedUrl =
            -- Production cached via CDN (Fastly)
            "https://api-curlingio.global.ssl.fastly.net/" ++ lang
    in
    case host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                devUrl
                --
                -- else if String.contains ".curling.io" h then
                --     productionUrl

            else
                productionCachedUrl

        Nothing ->
            productionCachedUrl


clubId : Flags -> String
clubId { apiKey, subdomain } =
    case ( apiKey, subdomain ) of
        ( _, Just subdomain_ ) ->
            subdomain_

        ( Just apiKey_, _ ) ->
            apiKey_

        _ ->
            ""


baseClubUrl : Flags -> String
baseClubUrl flags =
    baseUrl flags ++ "/clubs/" ++ clubId flags ++ "/"


baseClubSubdomainUrl : Flags -> String
baseClubSubdomainUrl flags =
    let
        devUrl =
            -- Development
            "http://" ++ clubId flags ++ ".curling.test:3000/" ++ flags.lang

        productionUrl =
            "https://" ++ clubId flags ++ ".curling.io/" ++ flags.lang
    in
    case flags.host of
        Just h ->
            if String.contains "localhost" h || String.contains ".curling.test" h then
                devUrl

            else
                productionUrl

        Nothing ->
            productionUrl


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


getItemsMaybe : Model -> String -> Bool -> ( WebData ItemsResult, Cmd Msg )
getItemsMaybe { flags, itemFilter, items } hash reload =
    case toRoute flags.defaultEventSection hash of
        ItemsRoute ->
            case ( items, reload ) of
                ( Success _, False ) ->
                    -- Already have the items.
                    ( items, Cmd.none )

                _ ->
                    -- On items route and haven't loaded them yet.
                    ( Loading, getItems flags itemFilter )

        _ ->
            -- Not on items route.
            ( items, Cmd.none )


getEventMaybe : Model -> String -> Bool -> ( WebData Event, Cmd Msg )
getEventMaybe { flags, event } hash reload =
    case toRoute flags.defaultEventSection hash of
        EventRoute eventId _ ->
            if reload then
                ( Loading, getEvent flags eventId )

            else
                case event of
                    Success event_ ->
                        if event_.id /= eventId then
                            -- Different event selected, load it.
                            ( Loading, getEvent flags eventId )

                        else
                            -- Already have the event.
                            ( event, Cmd.none )

                    _ ->
                        -- Haven't loaded an event yet.
                        ( Loading, getEvent flags eventId )

        _ ->
            -- Not on event route.
            ( event, Cmd.none )


getProductMaybe : Model -> String -> ( WebData Product, Cmd Msg )
getProductMaybe { flags, product } hash =
    case toRoute flags.defaultEventSection hash of
        ProductRoute productId ->
            case product of
                Success product_ ->
                    if product_.id /= productId then
                        -- Different product selected, load it.
                        ( Loading, getProduct flags productId )

                    else
                        -- Already have the product.
                        ( product, Cmd.none )

                _ ->
                    -- Haven't loaded a product yet.
                    ( Loading, getProduct flags productId )

        _ ->
            -- Not o product route.
            ( product, Cmd.none )


getTranslations : Flags -> Cmd Msg
getTranslations flags =
    let
        url =
            baseUrl flags ++ "/translations"
    in
    RemoteData.Http.get url GotTranslations decodeTranslations


getItems : Flags -> ItemFilter -> Cmd Msg
getItems flags itemFilter =
    let
        itemsSectionName : ItemsSection -> String
        itemsSectionName section =
            case section of
                LeaguesSection ->
                    "leagues"

                CompetitionsSection ->
                    "competitions"

                ProductsSection ->
                    "products"

        params =
            "?occurred="
                ++ String.fromInt itemFilter.seasonDelta
                ++ (case ( flags.section, flags.registration ) of
                        ( ProductsSection, _ ) ->
                            ""

                        ( _, False ) ->
                            "&registrations=f"

                        _ ->
                            ""
                   )

        url =
            baseClubUrl flags
                ++ itemsSectionName flags.section
                ++ params
    in
    RemoteData.Http.get url GotItems decodeItemsResult


getEvent : Flags -> Int -> Cmd Msg
getEvent flags id =
    let
        includeRegistrations =
            if List.member "registrations" flags.excludeEventSections then
                Nothing

            else
                Just "registrations"

        includeSpares =
            if List.member "spares" flags.excludeEventSections then
                Nothing

            else
                Just "spares"

        includeParamVal =
            [ includeRegistrations, includeSpares ]
                |> List.filterMap identity
                |> String.join ","

        includeParam =
            if includeParamVal == "" then
                ""

            else
                "?include=" ++ includeParamVal

        url =
            baseClubUrl flags
                ++ "events/"
                ++ String.fromInt id
                ++ includeParam
    in
    RemoteData.Http.get url GotEvent decodeEvent


reloadEvent : Flags -> Int -> Cmd Msg
reloadEvent flags id =
    let
        url =
            baseClubUrl flags
                ++ "events/"
                ++ String.fromInt id
    in
    RemoteData.Http.get url ReloadedEvent decodeEvent


getProduct : Flags -> Int -> Cmd Msg
getProduct flags id =
    let
        url =
            baseClubUrl flags
                ++ "products/"
                ++ String.fromInt id
    in
    RemoteData.Http.get url GotProduct decodeProduct



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

                            "competition" ->
                                Decode.succeed CompetitionsSection

                            "products" ->
                                Decode.succeed ProductsSection

                            "product" ->
                                Decode.succeed ProductsSection

                            _ ->
                                Decode.succeed LeaguesSection
                    )
    in
    Decode.succeed Flags
        |> optional "host" (nullable string) Nothing
        |> required "hash" (nullable string)
        |> optional "lang" string "en"
        |> optional "apiKey" (nullable string) Nothing
        |> optional "subdomain" (nullable string) Nothing
        |> optional "fullScreenToggle" bool False
        |> hardcoded False
        |> optional "searchable" bool True
        |> optional "section" decodeSection LeaguesSection
        |> optional "registration" bool False
        |> optional "showWaiversForTeams" bool False
        |> optional "excludeEventSections" (list string) []
        |> optional "defaultEventSection" (nullable string) Nothing
        |> optional "eventId" (nullable int) Nothing
        |> optional "theme" decodeTheme defaultTheme
        |> optional "loggedInCurlerIds" (list int) []
        |> hardcoded (Element.classifyDevice { width = 800, height = 600 })


decodeItemsResult : Decoder ItemsResult
decodeItemsResult =
    let
        decodeSeason : Decoder Season
        decodeSeason =
            Decode.succeed Season
                |> required "display" string
                |> required "delta" int

        decodeItem : Decoder Item
        decodeItem =
            Decode.succeed Item
                |> required "id" int
                |> required "name" string
                |> optional "summary" (nullable string) Nothing
                |> optional "occurs_on" (nullable string) Nothing
                |> optional "time_zone_short" (nullable string) Nothing
                |> optional "location" (nullable string) Nothing
                |> optional "venue" (nullable string) Nothing
                |> optional "no_registration_message" (nullable string) Nothing
                |> optional "price" (nullable string) Nothing
                |> optional "add_to_cart_url" (nullable string) Nothing
                |> optional "add_to_cart_text" (nullable string) Nothing
                |> optional "publish_results" bool False
    in
    Decode.succeed ItemsResult
        |> optional "seasons" (list decodeSeason) []
        |> required "items" (list decodeItem)


decodeSponsor : Decoder Sponsor
decodeSponsor =
    Decode.succeed Sponsor
        |> required "logo_url" string
        |> optional "name" (nullable string) Nothing
        |> optional "url" (nullable string) Nothing


decodeProduct : Decoder Product
decodeProduct =
    Decode.succeed Product
        |> required "id" int
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing
        |> optional "sponsor" (nullable decodeSponsor) Nothing
        |> optional "add_to_cart_url" (nullable string) Nothing
        |> optional "add_to_cart_text" (nullable string) Nothing
        |> optional "total" (nullable string) Nothing
        |> optional "potential_discounts" (list string) []


decodeEvent : Decoder Event
decodeEvent =
    let
        decodeEventType : Decoder EventType
        decodeEventType =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "competition" ->
                                Decode.succeed Competition

                            _ ->
                                Decode.succeed League
                    )

        decodeEventState : Decoder EventState
        decodeEventState =
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "pending" ->
                                Decode.succeed EventStatePending

                            "active" ->
                                Decode.succeed EventStateActive

                            _ ->
                                Decode.succeed EventStateComplete
                    )

        decodeSpare : Decoder Spare
        decodeSpare =
            Decode.succeed Spare
                |> required "name" string
                |> required "positions" (list string)
    in
    Decode.succeed Event
        |> required "id" int
        |> required "event_type" decodeEventType
        |> required "name" string
        |> optional "summary" (nullable string) Nothing
        |> optional "description" (nullable string) Nothing
        |> optional "note" (nullable string) Nothing
        |> required "team_restriction" string
        |> optional "mixed_doubles" bool False
        |> required "age_range" string
        |> optional "sponsor" (nullable decodeSponsor) Nothing
        |> required "starts_on" string
        |> required "ends_on" string
        |> optional "state" decodeEventState EventStateComplete
        |> optional "time_zone" (nullable string) Nothing
        |> optional "time_zone_short" (nullable string) Nothing
        |> optional "location" (nullable string) Nothing
        |> optional "venue" (nullable string) Nothing
        |> optional "video_url" (nullable string) Nothing
        |> optional "no_registration_message" (nullable string) Nothing
        |> optional "registration_opens_at" (nullable string) Nothing
        |> optional "registration_closes_at" (nullable string) Nothing
        |> optional "spots_available" (nullable int) Nothing
        |> optional "spots_remaining" (nullable int) Nothing
        |> optional "add_to_cart_url" (nullable string) Nothing
        |> optional "add_to_cart_text" (nullable string) Nothing
        |> optional "total" (nullable string) Nothing
        |> optional "potential_discounts" (list string) []
        |> optional "end_scores_enabled" bool False
        |> optional "shot_by_shot_enabled" bool False
        |> optional "last_stone_draw_enabled" bool False
        |> optional "number_of_ends" int 10
        |> optional "top_rock" string "red"
        |> optional "bot_rock" string "yellow"
        |> optional "sheet_names" (list string) []
        |> optional "teams" (list decodeTeam) []
        |> optional "registrations" (list decodeRegistration) []
        |> optional "spares" (list decodeSpare) []
        |> optional "stages" (list decodeStage) []
        |> optional "draws" (list decodeDraw) []
        |> optional "current_draw_id" (nullable int) Nothing


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
        |> optional "email" (nullable string) Nothing
        |> optional "phone" (nullable string) Nothing
        |> optional "image_url" (nullable string) Nothing
        |> optional "lineup" (list decodeTeamCurler) []


decodeTeamCurler : Decoder TeamCurler
decodeTeamCurler =
    let
        decodeTeamPosition : Decoder Int
        decodeTeamPosition =
            -- This can be made obsolute, if we pass position integers instead of strings.
            string
                |> Decode.andThen
                    (\str ->
                        case String.toLower str of
                            "fourth" ->
                                Decode.succeed 4

                            "third" ->
                                Decode.succeed 3

                            "second" ->
                                Decode.succeed 2

                            "first" ->
                                Decode.succeed 1

                            "alternate_1" ->
                                Decode.succeed 5

                            "alternate_2" ->
                                Decode.succeed 6

                            "alternate_3" ->
                                Decode.succeed 7

                            "alternate_4" ->
                                Decode.succeed 8

                            "alternate_5" ->
                                Decode.succeed 9

                            "alternate_6" ->
                                Decode.succeed 10

                            _ ->
                                -- Sketch
                                Decode.succeed 5
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
        |> optional "position" (nullable decodeTeamPosition) Nothing
        |> optional "skip" bool False
        |> required "name" string
        |> optional "delivery" (nullable decodeDelivery) Nothing
        |> optional "photo_url" (nullable string) Nothing
        |> optional "waiver" bool False


decodeRegistration : Decoder Registration
decodeRegistration =
    let
        decodeLineup : Decoder Lineup
        decodeLineup =
            Decode.succeed Lineup
                |> optional "first" (nullable string) Nothing
                |> optional "second" (nullable string) Nothing
                |> optional "third" (nullable string) Nothing
                |> optional "fourth" (nullable string) Nothing
                |> optional "alternate_1" (nullable string) Nothing
    in
    Decode.succeed Registration
        |> optional "curler_name" (nullable string) Nothing
        |> optional "team_name" (nullable string) Nothing
        |> optional "skip_name" (nullable string) Nothing
        |> optional "position" (nullable string) Nothing
        |> optional "lineup" (nullable decodeLineup) Nothing


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

        decodeGroup : Decoder Group
        decodeGroup =
            Decode.succeed Group
                |> required "id" int
                |> required "name" string

        decodeStanding : Decoder Standing
        decodeStanding =
            Decode.succeed Standing
                |> required "id" int
                |> required "rank" int
                |> required "played" int
                |> required "wins" int
                |> required "losses" int
                |> optional "ties" int 0
                |> optional "points" float 0.0
                |> optional "lsd" (nullable float) Nothing
                |> optional "lsd_rank" (nullable int) Nothing
    in
    Decode.succeed Stage
        |> required "id" int
        |> required "type" decodeStageType
        |> required "name" string
        |> optional "groups" (nullable (list decodeGroup)) Nothing
        |> optional "games" (list decodeGame) []
        |> optional "standings" (list decodeStanding) []


decodeDraw : Decoder Draw
decodeDraw =
    Decode.succeed Draw
        |> required "id" int
        |> required "epoch" int
        |> required "starts_at" string
        |> required "label" string
        |> optional "attendance" int 0
        |> required "draw_sheets" (list (nullable string))


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

        decodeGameCoords : Decoder GameCoords
        decodeGameCoords =
            Decode.succeed GameCoords
                |> required "group_id" int
                |> required "row" int
                |> required "col" int

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

                                    "unnecessary" ->
                                        Decode.succeed (Just SideResultUnnecessary)

                                    "conceded" ->
                                        Decode.succeed (Just SideResultForfeited)

                                    "forfeited" ->
                                        Decode.succeed (Just SideResultForfeited)

                                    "time_penalized" ->
                                        Decode.succeed (Just SideResultTimePenalized)

                                    _ ->
                                        Decode.succeed Nothing
                            )

                decodeShot : Decoder Shot
                decodeShot =
                    Decode.succeed Shot
                        |> required "end_number" int
                        |> required "shot_number" int
                        |> optional "curler_id" (nullable int) Nothing
                        |> optional "turn" (nullable string) Nothing
                        |> optional "throw" (nullable string) Nothing
                        |> optional "rating" (nullable string) Nothing
            in
            Decode.succeed Side
                |> optional "team_id" (nullable int) Nothing
                |> optional "top_rock" bool False
                |> optional "first_hammer" bool False
                |> optional "result" decodeSideResult Nothing
                |> optional "score" (nullable int) Nothing
                |> optional "end_scores" (list int) []
                |> optional "shots" (list decodeShot) []
                |> optional "time_remaining" (nullable string) Nothing
                |> optional "lsd" (nullable float) Nothing
    in
    Decode.succeed Game
        |> required "id" string
        |> required "name" string
        |> required "state" decodeGameState
        |> optional "video_url" (nullable string) Nothing
        |> optional "coords" (nullable decodeGameCoords) Nothing
        |> required "game_positions" (list decodeSide)
        |> optional "winner_to_game_id" (nullable string) Nothing
        |> optional "winner_to_side" (nullable int) Nothing
        |> optional "loser_to_game_id" (nullable string) Nothing
        |> optional "loser_to_side" (nullable int) Nothing
