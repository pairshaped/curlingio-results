module Results.Reports.Helpers exposing (..)

import List.Extra
import Results.Helpers exposing (..)
import Results.Types exposing (..)



-- VIEW MODELS AND HELPERS


type DrawState
    = DrawPending
    | DrawActive
    | DrawComplete


type alias EndStat =
    { teamId : Int
    , hammer : Bool
    , firstHammerFor : Bool
    , firstHammerAgainst : Bool
    , scoreFor : Int
    , scoreAgainst : Int
    , blankFor : Bool
    , blankAgainst : Bool
    , stolenFor : Bool
    , stolenAgainst : Bool
    , onePointFor : Bool
    , onePointAgainst : Bool
    , multiPointFor : Bool
    , multiPointAgainst : Bool
    }


type alias ShotExpanded =
    { gameId : String
    , drawId : Int
    , drawEpoch : Int
    , drawLabel : String
    , stageId : Int
    , sideNumber : Int
    , teamId : Int
    , teamShortName : String
    , curlerId : Int
    , curlerName : String
    , gender : Gender
    , endNumber : Int
    , lineupPosition : Int
    , position : Int
    , turn : Maybe String
    , throw : Maybe String
    , rating : Maybe String
    }


type alias ShotSummaryByPosition =
    { position : Int
    , lineupPosition : Int
    , gameId : String
    , drawId : Int
    , drawEpoch : Int
    , sideNumber : Int
    , teamId : Int
    , teamName : String
    , curlerId : Int
    , curlerName : String
    , gender : Gender
    , numberOfShots : Int
    , totalRatings : Int
    , percentage : Float
    , overUnder : Float
    , plusMinus : Int
    }


type alias TeamShot =
    { curlerId : Int
    , curlerName : String
    , throw : String
    , turn : String
    , rating : String
    }


type alias Throws =
    { throw : String
    , name : String
    , inTurn : String
    , inTurnPercentage : String
    , outTurn : String
    , outTurnPercentage : String
    , total : String
    , totalPercentage : String
    }


type alias LineConnector =
    { gameResult : GameResult
    , fromCoords : ( Int, Int )
    , toCoords : ( Int, Int )
    }


expandShotsForGame : Event -> Game -> List ShotExpanded
expandShotsForGame { mixedDoubles, teams, draws, stages } game =
    let
        shotNumberToPosition : Int -> Int
        shotNumberToPosition shotNumber =
            if mixedDoubles then
                case shotNumber of
                    1 ->
                        1

                    5 ->
                        -- In mixed doubles, the lead throws 1 and 5.
                        1

                    _ ->
                        2

            else
                case shotNumber of
                    1 ->
                        1

                    2 ->
                        1

                    3 ->
                        2

                    4 ->
                        2

                    5 ->
                        3

                    6 ->
                        3

                    _ ->
                        4

        stageWithGame =
            let
                hasGame stage =
                    List.any (\g -> g.id == game.id) stage.games
            in
            List.Extra.find hasGame stages

        drawWithGame =
            drawWithGameId draws game.id
    in
    case ( stageWithGame, drawWithGame ) of
        ( Just stage, Just draw ) ->
            game.sides
                |> List.indexedMap
                    (\sideNumber side ->
                        case teamForSide teams side of
                            Just team ->
                                side.shots
                                    |> List.map
                                        (\shot ->
                                            case shot.curlerId of
                                                Just curlerId ->
                                                    case List.Extra.find (\c -> c.curlerId == curlerId) team.lineup of
                                                        Just curler ->
                                                            Just
                                                                { gameId = game.id
                                                                , drawId = draw.id
                                                                , drawEpoch = draw.epoch
                                                                , drawLabel = draw.label
                                                                , stageId = stage.id
                                                                , sideNumber = sideNumber
                                                                , teamId = team.id
                                                                , teamShortName = team.shortName
                                                                , curlerId = curlerId
                                                                , curlerName = curler.name
                                                                , gender = curler.gender
                                                                , endNumber = shot.endNumber
                                                                , lineupPosition = List.Extra.find (\l -> l.curlerId == curlerId) team.lineup |> Maybe.map .position |> Maybe.withDefault (Just 5) |> Maybe.withDefault 5
                                                                , position = shotNumberToPosition shot.shotNumber
                                                                , turn = shot.turn
                                                                , throw = shot.throw
                                                                , rating = shot.rating
                                                                }

                                                        Nothing ->
                                                            Nothing

                                                Nothing ->
                                                    Nothing
                                        )
                                    |> List.filterMap identity

                            Nothing ->
                                []
                    )
                -- Lift the shots up to the sides
                |> List.concat

        _ ->
            []


summarizeShotsByPositionForGame : Event -> Game -> List ShotSummaryByPosition
summarizeShotsByPositionForGame event game =
    let
        toSummary : ( ShotExpanded, List ShotExpanded ) -> ShotSummaryByPosition
        toSummary ( shotsHead, shotsTail ) =
            let
                positionForCurler : Int
                positionForCurler =
                    -- The position is determine by where they throw the most rocks, or if the same, the
                    (shotsHead :: shotsTail)
                        |> List.map .position
                        |> List.Extra.group
                        |> List.map (\tu -> ( Tuple.first tu, List.length (Tuple.second tu) ))
                        |> List.sortBy (\tu -> Tuple.second tu)
                        |> List.reverse
                        |> List.map (\tu -> Tuple.first tu)
                        |> List.head
                        |> Maybe.withDefault 0

                totalRatings : Int
                totalRatings =
                    List.map .rating (shotsHead :: shotsTail)
                        |> List.filterMap identity
                        |> List.map String.toInt
                        |> List.filterMap identity
                        |> List.sum

                numberOfShots =
                    List.length shotsTail + 1
            in
            { position = positionForCurler
            , lineupPosition = shotsHead.lineupPosition
            , gameId = shotsHead.gameId
            , drawId = shotsHead.drawId
            , drawEpoch = shotsHead.drawEpoch
            , sideNumber = shotsHead.sideNumber
            , teamId = shotsHead.teamId
            , teamName = shotsHead.teamShortName
            , curlerId = shotsHead.curlerId
            , curlerName = shotsHead.curlerName
            , gender = shotsHead.gender
            , numberOfShots = numberOfShots
            , totalRatings = totalRatings
            , percentage = toFloat totalRatings / toFloat numberOfShots * 100 / 4
            , overUnder = 0
            , plusMinus = 0
            }

        addPlusMinuses : List ShotSummaryByPosition -> List ShotSummaryByPosition
        addPlusMinuses shotSummaries =
            let
                addPlusMinus : ShotSummaryByPosition -> ShotSummaryByPosition
                addPlusMinus shotSummary =
                    let
                        opponent =
                            shotSummaries
                                |> List.filter (\ss -> ss.position == shotSummary.position && ss.gameId == shotSummary.gameId && ss.teamId /= shotSummary.teamId)
                                |> List.head
                    in
                    -- TODO
                    case opponent of
                        Just opp ->
                            let
                                overUnder =
                                    shotSummary.percentage - opp.percentage

                                plusMinus =
                                    if overUnder >= 5 then
                                        1

                                    else if overUnder <= -5 then
                                        -1

                                    else
                                        0
                            in
                            { shotSummary | overUnder = overUnder, plusMinus = plusMinus }

                        Nothing ->
                            shotSummary
            in
            List.map addPlusMinus shotSummaries
    in
    expandShotsForGame event game
        -- Remove throw throughs (X in throw)
        |> List.filter (\s -> s.throw /= Nothing && s.throw /= Just "X")
        -- We need to sort by side number and curler since the groupWhile only examines adjacent items. (annoying!)
        |> List.sortBy .curlerId
        -- Then sort by side so we keep the two sides separate
        |> List.sortBy .sideNumber
        -- Group by side and curler
        |> List.Extra.groupWhile (\a b -> a.sideNumber == b.sideNumber && a.curlerId == b.curlerId)
        -- Build out a shot summary for each
        |> List.map toSummary
        -- Sort by position within each side.
        |> List.sortBy .position
        |> List.sortBy .sideNumber
        |> addPlusMinuses
