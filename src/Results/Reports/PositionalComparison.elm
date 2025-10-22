module Results.Reports.PositionalComparison exposing (viewPercentage, viewPlusMinus)

import Element as El exposing (Element, column, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import List.Extra
import Results.Helpers exposing (..)
import Results.Reports.Helpers exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)
import Html.Attributes exposing (class)


viewGenderTabs : Theme -> List Translation -> PositionalComparisonGender -> Bool -> Element Msg
viewGenderTabs theme translations selected hasTabs =
    if hasTabs then
        let
            tabConfig =
                [ ( GenderFemale, translate translations "womens" )
                , ( GenderMale, translate translations "mens" )
                ]

            viewTab ( selection, label ) =
                let
                    isSelected =
                        selection == selected
                in
                button
                    [ El.paddingXY 18 10
                    , El.focused [ Background.color theme.transparent ]
                    , Border.color theme.grey
                    , Font.color
                        (if isSelected then
                            theme.defaultText

                         else
                            theme.primary
                        )
                    , Border.widthEach
                        (if isSelected then
                            { bottom = 0, left = 1, right = 1, top = 1 }

                         else
                            { bottom = 1, left = 0, right = 0, top = 0 }
                        )
                    , Border.rounded 3
                    , El.htmlAttribute (class "cio__event_stage_link")
                    ]
                    { onPress = Just (SetPositionalComparisonGender selection)
                    , label = text label
                    }
        in
        El.row [] (List.map viewTab tabConfig)

    else
        El.none


viewPercentage : Theme -> List Translation -> EventConfig -> Event -> Element Msg
viewPercentage theme translations eventConfig event =
    -- We need to figure out how many shots were taken per position per game,
    -- then we'll want to use that to determine if the curler has taken at least
    -- 50% of those shots to determine if they'll be listed in the positional
    -- percentages or as an alternate (less than 50% means unranked / alternate).
    -- Right now we're doing this based on the predetermined lineup position, which is
    -- incorrect (but probably right most of the time).
    let
        draws =
            drawsWithCompletedGames event

        positions =
            [ 4, 3, 2, 1, 5 ]

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

        allShotSummaries =
            positions
                |> List.concatMap shotSummariesByPosition

        hasMale =
            List.any (\ss -> ss.gender == Male) allShotSummaries

        hasFemale =
            List.any (\ss -> ss.gender == Female) allShotSummaries

        tabsEnabled =
            hasMale && hasFemale

        selectedGender =
            eventConfig.positionalComparisonGender

        filterByGender summaries =
            if tabsEnabled then
                case selectedGender of
                    GenderFemale ->
                        List.filter (\ss -> ss.gender == Female) summaries

                    GenderMale ->
                        List.filter (\ss -> ss.gender == Male || ss.gender == Unknown) summaries

            else
                summaries

        viewPosition position =
            let
                summariesForPosition =
                    shotSummariesByPosition position
                        |> filterByGender

                groupSummarizedShotsByCurler : List (List ShotSummaryByPosition)
                groupSummarizedShotsByCurler =
                    summariesForPosition
                        |> List.sortBy (\ss -> ss.drawEpoch)
                        |> List.sortBy (\ss -> ss.curlerId)
                        |> List.Extra.groupWhile (\a b -> a.curlerId == b.curlerId)
                        |> List.map fromNonempty
                        |> List.sortBy (\g -> List.sum (List.map .percentage g) / toFloat (List.length g))
                        |> List.reverse

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
        , viewGenderTabs theme translations selectedGender tabsEnabled
        , column [ El.spacing 60 ] (List.map viewPosition positions)
        , el [] (text "")
        ]


viewPlusMinus : Theme -> List Translation -> EventConfig -> Event -> Element Msg
viewPlusMinus theme translations eventConfig event =
    -- We need to figure out how many shots were taken per position per game,
    -- then we'll want to use that to determine if the curler has taken at least
    -- 50% of those shots to determine if they'll be listed in the positional
    -- percentages or as an alternate (less than 50% means unranked / alternate).
    -- Right now we're doing this based on the predetermined lineup position, which is
    -- incorrect (but probably right most of the time).
    let
        draws =
            drawsWithCompletedGames event

        positions =
            [ 4, 3, 2, 1, 5 ]

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

        allShotSummaries =
            positions
                |> List.concatMap shotSummariesByPosition

        hasMale =
            List.any (\ss -> ss.gender == Male) allShotSummaries

        hasFemale =
            List.any (\ss -> ss.gender == Female) allShotSummaries

        tabsEnabled =
            hasMale && hasFemale

        selectedGender =
            eventConfig.positionalComparisonGender

        filterByGender summaries =
            if tabsEnabled then
                case selectedGender of
                    GenderFemale ->
                        List.filter (\ss -> ss.gender == Female) summaries

                    GenderMale ->
                        List.filter (\ss -> ss.gender == Male || ss.gender == Unknown) summaries

            else
                summaries

        viewPosition position =
            let
                summariesForPosition =
                    shotSummariesByPosition position
                        |> filterByGender

                groupSummarizedShotsByCurler : List (List ShotSummaryByPosition)
                groupSummarizedShotsByCurler =
                    summariesForPosition
                        |> List.sortBy (\ss -> ss.drawEpoch)
                        |> List.sortBy (\ss -> ss.curlerId)
                        |> List.Extra.groupWhile (\a b -> a.curlerId == b.curlerId)
                        |> List.map fromNonempty
                        |> List.sortBy (\g -> List.sum (List.map .overUnder g))
                        |> List.sortBy (\g -> List.sum (List.map .plusMinus g))
                        |> List.reverse

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
        , viewGenderTabs theme translations selectedGender tabsEnabled
        , column [ El.spacing 60 ] (List.map viewPosition positions)
        , el [] (text "")
        ]
