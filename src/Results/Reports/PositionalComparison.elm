module Results.Reports.PositionalComparison exposing (viewPercentage, viewPlusMinus)

import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
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


viewPercentage : Theme -> List Translation -> Event -> Element Msg
viewPercentage theme translations event =
    -- We need to figure out how many shots were taken per position per game,
    -- then we'll want to use that to determine if the curler has taken at least
    -- 50% of those shots to determine if they'll be listed in the positional
    -- percentages or as an alternate (less than 50% means unranked / alternate).
    -- Right now we're doing this based on the predetermined lineup position, which is
    -- incorrect (but probably right most of the time).
    let
        draws =
            drawsWithCompletedGames event

        shotSummariesByPosition : Int -> List ShotSummaryByPosition
        shotSummariesByPosition position =
            List.map (gamesInDraw event) draws
                |> List.concat
                |> List.map (summarizeShotsByPositionForGame event)
                |> List.concat
                |> List.filter
                    (\ss ->
                        if position <= 4 then
                            ss.position == position && ss.lineupPosition <= 4

                        else
                            ss.lineupPosition == position
                    )

        viewPosition position =
            let
                groupSummarizedShotsByCurler : List (List ShotSummaryByPosition)
                groupSummarizedShotsByCurler =
                    shotSummariesByPosition position
                        |> List.sortBy (\ss -> ss.drawEpoch)
                        |> List.sortBy (\ss -> ss.curlerId)
                        |> List.Extra.groupWhile (\a b -> a.curlerId == b.curlerId)
                        |> List.map fromNonempty
                        |> List.sortBy (\g -> List.sum (List.map .percentage g) / toFloat (List.length g))
                        |> List.reverse

                data : List ShotSummaryByPosition
                data =
                    []

                viewHeader align label =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 2, left = 0 }
                        , Border.color theme.grey
                        , Font.semiBold
                        ]
                        (el [ align ] (text (translate translations label)))

                viewCell i align content =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        (el [ align ] (text content))

                viewDrawCell draw =
                    { header = viewHeader El.centerX draw.label
                    , width = El.fill
                    , view =
                        \i shotSummary ->
                            viewCell i
                                El.centerX
                                (case List.Extra.find (\ss -> ss.drawId == draw.id) shotSummary of
                                    Just ss ->
                                        String.fromInt (round ss.percentage)

                                    _ ->
                                        " "
                                )

                    -- (String.fromInt shotSummary.totalRatings)
                    }
            in
            El.indexedTable []
                { data = groupSummarizedShotsByCurler
                , columns =
                    [ { header = viewHeader El.alignLeft (positionNumberToString translations (Just position))
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    (Maybe.map .curlerName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    , { header = viewHeader El.alignLeft (translate translations "team")
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    (Maybe.map .teamName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    ]
                        ++ List.map viewDrawCell draws
                        ++ [ { header = viewHeader El.centerX (translate translations "total")
                             , width = El.fill
                             , view =
                                \i shotSummaries ->
                                    viewCell i
                                        El.centerX
                                        (String.fromFloat
                                            (toFloat (round (List.sum (List.map .percentage shotSummaries) / toFloat (List.length shotSummaries))))
                                        )
                             }
                           ]
                }
    in
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "positional_percentage_comparison"))
        , column [ El.spacing 60 ] (List.map viewPosition [ 4, 3, 2, 1, 5 ])
        , el [] (text "")
        ]


viewPlusMinus : Theme -> List Translation -> Event -> Element Msg
viewPlusMinus theme translations event =
    -- We need to figure out how many shots were taken per position per game,
    -- then we'll want to use that to determine if the curler has taken at least
    -- 50% of those shots to determine if they'll be listed in the positional
    -- percentages or as an alternate (less than 50% means unranked / alternate).
    -- Right now we're doing this based on the predetermined lineup position, which is
    -- incorrect (but probably right most of the time).
    let
        draws =
            drawsWithCompletedGames event

        shotSummariesByPosition : Int -> List ShotSummaryByPosition
        shotSummariesByPosition position =
            List.map (gamesInDraw event) draws
                |> List.concat
                |> List.map (summarizeShotsByPositionForGame event)
                |> List.concat
                |> List.filter
                    (\ss ->
                        if position <= 4 then
                            ss.position == position && ss.lineupPosition <= 4

                        else
                            ss.lineupPosition == position
                    )

        viewPosition position =
            let
                groupSummarizedShotsByCurler : List (List ShotSummaryByPosition)
                groupSummarizedShotsByCurler =
                    shotSummariesByPosition position
                        |> List.sortBy (\ss -> ss.drawEpoch)
                        |> List.sortBy (\ss -> ss.curlerId)
                        |> List.Extra.groupWhile (\a b -> a.curlerId == b.curlerId)
                        |> List.map fromNonempty
                        |> List.sortBy (\g -> List.sum (List.map .overUnder g))
                        |> List.sortBy (\g -> List.sum (List.map .plusMinus g))
                        |> List.reverse

                data : List ShotSummaryByPosition
                data =
                    []

                viewHeader align label =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 2, left = 0 }
                        , Border.color theme.grey
                        , Font.semiBold
                        ]
                        (el [ align ] (text (translate translations label)))

                viewCell i align fontWeight fontColor content =
                    el
                        [ El.padding 15
                        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                        , Border.color theme.grey
                        , Maybe.withDefault Font.regular fontWeight
                        , Font.color (Maybe.withDefault (El.rgb 0 0 0) fontColor)
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        (el [ align ] (text content))

                viewDrawCell draw =
                    { header = viewHeader El.centerX draw.label
                    , width = El.fill
                    , view =
                        \i shotSummary ->
                            case List.Extra.find (\ss -> ss.drawId == draw.id) shotSummary of
                                Just ss ->
                                    if ss.plusMinus > 0 then
                                        viewCell i El.centerX (Just Font.semiBold) Nothing "+"

                                    else if ss.plusMinus < 0 then
                                        viewCell i El.centerX (Just Font.bold) Nothing "-"

                                    else
                                        viewCell i El.centerX Nothing Nothing "0"

                                _ ->
                                    viewCell i El.centerX Nothing (Just (El.rgb 0.8 0.8 0.8)) "*"
                    }
            in
            El.indexedTable []
                { data = groupSummarizedShotsByCurler
                , columns =
                    [ { header = viewHeader El.alignLeft (positionNumberToString translations (Just position))
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    Nothing
                                    Nothing
                                    (Maybe.map .curlerName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    , { header = viewHeader El.alignLeft (translate translations "team")
                      , width = El.fill
                      , view =
                            \i shotSummary ->
                                viewCell i
                                    El.alignLeft
                                    Nothing
                                    Nothing
                                    (Maybe.map .teamName (List.head shotSummary) |> Maybe.withDefault "")
                      }
                    ]
                        ++ List.map viewDrawCell draws
                        ++ [ { header = viewHeader El.alignRight (translate translations "total")
                             , width = El.fill
                             , view =
                                \i shotSummaries ->
                                    let
                                        tally =
                                            let
                                                plusMinus ss =
                                                    if ss.overUnder >= 5 then
                                                        1

                                                    else if ss.overUnder <= -5 then
                                                        -1

                                                    else
                                                        0
                                            in
                                            List.map plusMinus shotSummaries |> List.sum
                                    in
                                    viewCell i
                                        El.alignRight
                                        Nothing
                                        Nothing
                                        ((if tally > 0 then
                                            "+"

                                          else
                                            ""
                                         )
                                            ++ String.fromInt tally
                                        )
                             }
                           ]
                }
    in
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "positional_plus_minus_comparison"))
        , column [ El.spacing 60 ] (List.map viewPosition [ 4, 3, 2, 1, 5 ])
        , el [] (text "")
        ]
