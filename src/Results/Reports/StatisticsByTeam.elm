module Results.Reports.StatisticsByTeam exposing (view)

import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html.Attributes exposing (class)
import List.Extra
import Results.Helpers exposing (..)
import Results.Reports.Helpers exposing (..)
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


view : Theme -> List Translation -> EventConfig -> Event -> Bool -> Element Msg
view theme translations eventConfig event cumulative =
    let
        draw =
            if cumulative then
                Nothing

            else
                case eventConfig.drawSelected of
                    Just drawId ->
                        List.Extra.find (\draw_ -> draw_.id == drawId) event.draws

                    Nothing ->
                        List.head event.draws

        viewDrawSelector =
            let
                drawOption draw_ =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateDrawSelected draw_.id)
                        , if Just draw_.id == eventConfig.drawSelected then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text (translate translations "draw" ++ " " ++ draw_.label))

                drawOptions =
                    if eventConfig.drawSelectionOpen then
                        let
                            scrolling =
                                if List.length event.draws > 5 then
                                    [ El.height (El.fill |> El.minimum 210), El.scrollbarY ]

                                else
                                    []
                        in
                        column
                            ([ El.width El.fill
                             , Border.width 1
                             , Border.color theme.grey
                             , Background.color theme.white
                             ]
                                ++ scrolling
                            )
                            (List.map drawOption event.draws)

                    else
                        El.none
            in
            row
                [ El.width (El.px 150)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleDrawSelection
                , El.below drawOptions
                , El.htmlAttribute (class "cio__draw_dropdown")
                ]
                [ el []
                    (text
                        (case draw of
                            Just draw_ ->
                                translate translations "draw" ++ " " ++ draw_.label

                            Nothing ->
                                "-"
                        )
                    )
                , el [ El.alignRight ]
                    (text
                        (if eventConfig.drawSelectionOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]

        selectableTeams =
            case draw of
                Just draw_ ->
                    draw_.drawSheets
                        |> List.filterMap identity
                        |> List.map
                            (\drawSheet ->
                                findGameById event drawSheet
                                    |> Maybe.map
                                        (\game ->
                                            game.sides
                                                |> List.map
                                                    (\side ->
                                                        side.teamId
                                                            |> Maybe.andThen (\teamId -> List.Extra.find (\t -> t.id == teamId) event.teams)
                                                    )
                                        )
                                    |> Maybe.withDefault []
                            )
                        |> List.concat
                        |> List.filterMap identity

                Nothing ->
                    event.teams

        team =
            case eventConfig.teamSelected of
                Just teamId ->
                    case List.Extra.find (\team_ -> team_.id == teamId) selectableTeams of
                        Just team_ ->
                            Just team_

                        Nothing ->
                            List.head selectableTeams

                Nothing ->
                    List.head selectableTeams

        viewTeamSelector =
            let
                teamOption team_ =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateTeamSelected team_.id)
                        , if Just team_.id == eventConfig.teamSelected then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text team_.name)

                teamOptions =
                    if eventConfig.teamSelectionOpen then
                        let
                            scrolling =
                                if List.length selectableTeams >= 8 then
                                    [ El.height (El.fill |> El.minimum 260), El.scrollbarY ]

                                else
                                    []
                        in
                        column
                            ([ El.width El.fill
                             , Border.width 1
                             , Border.color theme.grey
                             , Background.color theme.white
                             ]
                                ++ scrolling
                            )
                            (List.map teamOption selectableTeams)

                    else
                        El.none
            in
            row
                [ El.width (El.px 260)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleTeamSelection
                , El.below teamOptions
                , El.htmlAttribute (class "cio__team_dropdown")
                ]
                [ el []
                    (text
                        (case team of
                            Just team_ ->
                                team_.name

                            Nothing ->
                                "-"
                        )
                    )
                , el [ El.alignRight ]
                    (text
                        (if eventConfig.teamSelectionOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]

        throws : Team -> Maybe TeamCurler -> List Throws
        throws team_ curler =
            let
                shots : List TeamShot
                shots =
                    let
                        teamShots : List TeamShot
                        teamShots =
                            let
                                teamShot : Shot -> Maybe TeamShot
                                teamShot { curlerId, throw, turn, rating } =
                                    case curlerId of
                                        Just curlerId_ ->
                                            case List.Extra.find (\c -> c.curlerId == curlerId_) team_.lineup of
                                                Just curler_ ->
                                                    case [ throw, turn, rating ] of
                                                        [ Just throw_, Just turn_, Just rating_ ] ->
                                                            Just
                                                                { curlerId = curlerId_
                                                                , curlerName = curler_.name
                                                                , throw = throw_
                                                                , turn = turn_
                                                                , rating = rating_
                                                                }

                                                        _ ->
                                                            Nothing

                                                _ ->
                                                    Nothing

                                        _ ->
                                            Nothing
                            in
                            List.map
                                (\stage ->
                                    List.map
                                        (\game ->
                                            if game.state /= GamePending then
                                                let
                                                    hasGame =
                                                        case draw of
                                                            Just draw_ ->
                                                                let
                                                                    matching gameId =
                                                                        gameId == Just game.id
                                                                in
                                                                List.any matching draw_.drawSheets

                                                            Nothing ->
                                                                False
                                                in
                                                if draw == Nothing || hasGame then
                                                    List.map
                                                        (\side ->
                                                            if side.teamId == Just team_.id then
                                                                List.map
                                                                    (\shot ->
                                                                        teamShot shot
                                                                    )
                                                                    side.shots

                                                            else
                                                                []
                                                        )
                                                        game.sides

                                                else
                                                    []

                                            else
                                                []
                                        )
                                        stage.games
                                )
                                event.stages
                                |> List.concat
                                |> List.concat
                                |> List.concat
                                |> List.filterMap identity
                    in
                    (case curler of
                        Just curler_ ->
                            teamShots
                                |> List.filter (\teamShot -> teamShot.curlerId == curler_.curlerId)

                        Nothing ->
                            teamShots
                    )
                        -- |> List.filter (\teamShot -> teamShot.turn == "I" || teamShot.turn == "O")
                        |> List.filter (\teamShot -> teamShot.throw /= "X")

                shotsByThrow : String -> List TeamShot -> List TeamShot
                shotsByThrow throw shots_ =
                    shots_
                        |> List.filter (\shot -> shot.throw == throw)

                shotsByTurn : String -> List TeamShot -> List TeamShot
                shotsByTurn turn shots_ =
                    shots_
                        |> List.filter (\shot -> shot.turn == turn)

                shotsByAllDraws : List TeamShot -> List TeamShot
                shotsByAllDraws shots_ =
                    shots_
                        |> List.filter (\shot -> String.contains shot.throw "EFGHJ")

                shotsByAllTakeouts : List TeamShot -> List TeamShot
                shotsByAllTakeouts shots_ =
                    shots_
                        |> List.filter (\shot -> String.contains shot.throw "ABCD")

                shotsToPoints shots_ =
                    shots_ |> List.map .rating |> List.map String.toInt |> List.filterMap identity |> List.sum

                throwLabels : List ( String, String )
                throwLabels =
                    [ ( "A", "Takeout" )
                    , ( "B", "Hit and roll" )
                    , ( "C", "Clear front" )
                    , ( "D", "Raise take out" )
                    , ( "E", "Draw" )
                    , ( "F", "Front stone" )
                    , ( "G", "Guard" )
                    , ( "H", "Freeze" )
                    , ( "J", "Tap Back" )
                    ]
            in
            (throwLabels
                |> List.map
                    (\throwLabel ->
                        let
                            throw =
                                Tuple.first throwLabel

                            inTurn =
                                shots |> shotsByThrow throw |> shotsByTurn "I" |> List.length

                            inTurnPoints =
                                shots |> shotsByThrow throw |> shotsByTurn "I" |> shotsToPoints

                            outTurn =
                                shots |> shotsByThrow throw |> shotsByTurn "O" |> List.length

                            outTurnPoints =
                                shots |> shotsByThrow throw |> shotsByTurn "O" |> shotsToPoints

                            total =
                                shots |> shotsByThrow throw |> List.length

                            totalPoints =
                                shots |> shotsByThrow throw |> shotsToPoints
                        in
                        { throw = throw
                        , name = Tuple.second throwLabel
                        , inTurn = inTurn |> String.fromInt
                        , inTurnPercentage = round ((toFloat inTurnPoints / toFloat (inTurn * 4)) * 100) |> String.fromInt
                        , outTurn = outTurn |> String.fromInt
                        , outTurnPercentage = round ((toFloat outTurnPoints / toFloat (outTurn * 4)) * 100) |> String.fromInt
                        , total = total |> String.fromInt
                        , totalPercentage = round ((toFloat totalPoints / toFloat (total * 4)) * 100) |> String.fromInt
                        }
                    )
            )
                ++ [ { throw = ""
                     , name = "All Draws"
                     , inTurn = shots |> shotsByAllDraws |> shotsByTurn "I" |> List.length |> String.fromInt
                     , inTurnPercentage =
                        round ((toFloat (shots |> shotsByAllDraws |> shotsByTurn "I" |> shotsToPoints) / toFloat ((shots |> shotsByAllDraws |> shotsByTurn "I" |> List.length) * 4)) * 100) |> String.fromInt
                     , outTurn = shots |> shotsByAllDraws |> shotsByTurn "O" |> List.length |> String.fromInt
                     , outTurnPercentage =
                        round ((toFloat (shots |> shotsByAllDraws |> shotsByTurn "O" |> shotsToPoints) / toFloat ((shots |> shotsByAllDraws |> shotsByTurn "O" |> List.length) * 4)) * 100) |> String.fromInt
                     , total = shots |> shotsByAllDraws |> List.length |> String.fromInt
                     , totalPercentage =
                        round ((toFloat (shots |> shotsByAllDraws |> shotsToPoints) / toFloat ((shots |> shotsByAllDraws |> List.length) * 4)) * 100) |> String.fromInt
                     }
                   , { throw = ""
                     , name = "All Takeouts"
                     , inTurn = shots |> shotsByAllTakeouts |> shotsByTurn "I" |> List.length |> String.fromInt
                     , inTurnPercentage =
                        round ((toFloat (shots |> shotsByAllTakeouts |> shotsByTurn "I" |> shotsToPoints) / toFloat ((shots |> shotsByAllTakeouts |> shotsByTurn "I" |> List.length) * 4)) * 100) |> String.fromInt
                     , outTurn = shots |> shotsByAllTakeouts |> shotsByTurn "O" |> List.length |> String.fromInt
                     , outTurnPercentage =
                        round ((toFloat (shots |> shotsByAllTakeouts |> shotsByTurn "O" |> shotsToPoints) / toFloat ((shots |> shotsByAllTakeouts |> shotsByTurn "O" |> List.length) * 4)) * 100) |> String.fromInt
                     , total = shots |> shotsByAllTakeouts |> List.length |> String.fromInt
                     , totalPercentage =
                        round ((toFloat (shots |> shotsByAllTakeouts |> shotsToPoints) / toFloat ((shots |> shotsByAllTakeouts |> List.length) * 4)) * 100) |> String.fromInt
                     }
                   , { throw = ""
                     , name = "Total"
                     , inTurn = shots |> shotsByTurn "I" |> List.length |> String.fromInt
                     , inTurnPercentage =
                        round ((toFloat (shots |> shotsByTurn "I" |> shotsToPoints) / toFloat ((shots |> shotsByTurn "I" |> List.length) * 4)) * 100) |> String.fromInt
                     , outTurn = shots |> shotsByTurn "O" |> List.length |> String.fromInt
                     , outTurnPercentage =
                        round ((toFloat (shots |> shotsByTurn "O" |> shotsToPoints) / toFloat ((shots |> shotsByTurn "O" |> List.length) * 4)) * 100) |> String.fromInt
                     , total = shots |> List.length |> String.fromInt
                     , totalPercentage =
                        round ((toFloat (shots |> shotsToPoints) / toFloat ((shots |> List.length) * 4)) * 100) |> String.fromInt
                     }
                   ]

        viewHeader align label =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                , Font.semiBold
                , Background.color theme.greyLight
                ]
                (el [ align ] (text (translate translations label)))

        viewCell align content onType =
            el
                [ El.padding 15
                , Border.widthEach
                    { top =
                        if onType == "" then
                            1

                        else
                            0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.color theme.grey
                , if onType == "" then
                    Font.semiBold

                  else
                    Font.regular
                ]
                (el [ align ]
                    (text
                        (if content == "0" || content == "NaN" then
                            "-"

                         else
                            content
                        )
                    )
                )

        viewThrowsTable throws_ =
            El.table []
                { data = throws_
                , columns =
                    [ { header = viewHeader El.alignLeft "Type"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignLeft throw.name throw.throw
                      }
                    , { header = viewHeader El.alignRight "Inturn"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.inTurn throw.throw
                      }
                    , { header = viewHeader El.alignRight "Inturn %"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.inTurnPercentage throw.throw
                      }
                    , { header = viewHeader El.alignRight "Outturn"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.outTurn throw.throw
                      }
                    , { header = viewHeader El.alignRight "Outturn %"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.outTurnPercentage throw.throw
                      }
                    , { header = viewHeader El.alignRight "Total"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.total throw.throw
                      }
                    , { header = viewHeader El.alignRight "Total %"
                      , width = El.fill
                      , view =
                            \throw ->
                                viewCell El.alignRight throw.totalPercentage throw.throw
                      }
                    ]
                }
    in
    column [ El.width El.fill, El.spacing 30, El.paddingEach { top = 0, right = 0, bottom = 140, left = 0 } ]
        [ row [ El.width El.fill ]
            [ el [ Font.size 24 ]
                (text
                    (translate translations
                        ((if cumulative then
                            "cumulative_"

                          else
                            ""
                         )
                            ++ "statistics_by_team"
                        )
                    )
                )
            , column [ El.spacing 10, El.alignRight ]
                [ if cumulative then
                    El.none

                  else
                    el [ El.alignRight ] viewDrawSelector
                , el [ El.alignRight ] viewTeamSelector
                ]
            ]
        , column [ El.width El.fill, El.spacing 20 ]
            (case team of
                Just team_ ->
                    [ el [ Font.size 20 ] (text team_.name)
                    , viewThrowsTable (throws team_ Nothing)
                    , el [] (text "")
                    ]
                        ++ List.map
                            (\curler ->
                                column [ El.width El.fill, El.spacing 20 ]
                                    [ el [ Font.size 20, El.paddingEach { top = 10, right = 0, bottom = 0, left = 0 } ] (text curler.name)
                                    , viewThrowsTable (throws team_ (Just curler))
                                    ]
                            )
                            team_.lineup

                Nothing ->
                    []
            )
        ]
