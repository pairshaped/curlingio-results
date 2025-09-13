module Results.Reports.ScoringAnalysis exposing (view)

import Element as El exposing (Element, column, el, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html.Attributes exposing (class)
import List.Extra
import Results.Helpers exposing (..)
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme)
import Shared.Translation exposing (Translation, translate)


view : Theme -> List Translation -> Event -> Maybe (List Team) -> Element Msg
view theme translations event restrictToTeams =
    let
        teams =
            case restrictToTeams of
                Just teams_ ->
                    teams_

                Nothing ->
                    let
                        teamHasGames team =
                            not (List.isEmpty (gamesWithTeam event team))
                    in
                    List.filter teamHasGames event.teams

        rows =
            -- TODO: Structure the data so that for and against are just rows, but when rendering we know due to missing data or a flag which is which.
            List.Extra.interweave teams teams

        isForGame =
            List.length teams == 2

        games team =
            let
                participatedIn sides =
                    List.any (\s -> s.teamId == Just team.id) sides
            in
            List.filter (\g -> g.state /= GamePending) (gamesInEvent event)
                |> List.filter (\g -> participatedIn g.sides)

        sidesFor team =
            List.map .sides (games team)
                |> List.concat
                |> List.filter (\s -> s.teamId == Just team.id)

        sidesAgainst team =
            List.map .sides (games team)
                |> List.concat
                |> List.filter (\s -> s.teamId /= Just team.id)

        statsFor team =
            List.map .endScores (sidesFor team)
                |> List.concat

        statsAgainst team =
            List.map .endScores (sidesAgainst team)
                |> List.concat

        firstHammerCountFor team =
            List.filter (\s -> s.firstHammer == True) (sidesFor team)
                |> List.length

        firstHammerCountAgainst team =
            List.filter (\s -> s.firstHammer == True) (sidesAgainst team)
                |> List.length

        blankEndsFor : Bool -> Team -> Int
        blankEndsFor for team =
            -- For is when you get the hammer after the blank end, which depends on whether it's a mixed doubles.
            -- Against is when you lose the hammer after the blank end, which depends on whether it's a mixed doubles.
            let
                -- A blank end is when neither teams gets a point.
                blankEndsForGame game =
                    let
                        -- We don't know if the team side is the first or second, so build a tuple so we know.
                        ( sideFor, sideAgainst ) =
                            ( List.Extra.find (\s -> s.teamId == Just team.id) game.sides
                            , List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                            )

                        -- We don't care about the event's number of ends, just the max of either side.
                        numberOfEnds =
                            List.map (\side -> List.length side.endScores) game.sides
                                |> List.maximum
                                |> Maybe.withDefault 0

                        isBlankEndFor : Int -> Bool
                        isBlankEndFor endIndex =
                            case ( sideFor, sideAgainst ) of
                                ( Just sideFor_, Just sideAgainst_ ) ->
                                    let
                                        hasHammer =
                                            hasHammerInEnd event.mixedDoubles sideFor_ sideAgainst_ endIndex
                                    in
                                    -- This is ridiculous (but correct) and needs to be refactored.
                                    if for then
                                        isBlankEnd sideFor sideAgainst endIndex
                                            && ((not event.mixedDoubles && hasHammer)
                                                    || (event.mixedDoubles && not hasHammer)
                                               )

                                    else
                                        isBlankEnd sideFor sideAgainst endIndex
                                            && ((not event.mixedDoubles && not hasHammer)
                                                    || (event.mixedDoubles && hasHammer)
                                               )

                                _ ->
                                    False
                    in
                    List.range 0 numberOfEnds
                        |> List.filter isBlankEndFor
            in
            List.map (\game -> blankEndsForGame game) (games team)
                |> List.concat
                |> List.length

        onePointEndsFor team =
            List.filter (\i -> i == 1) (statsFor team)
                |> List.length

        onePointEndsAgainst team =
            List.filter (\i -> i == 1) (statsAgainst team)
                |> List.length

        twoPointEndsFor team =
            List.filter (\i -> i == 2) (statsFor team)
                |> List.length

        twoPointEndsAgainst team =
            List.filter (\i -> i == 2) (statsAgainst team)
                |> List.length

        threePointEndsFor team =
            List.filter (\i -> i == 3) (statsFor team)
                |> List.length

        threePointEndsAgainst team =
            List.filter (\i -> i == 3) (statsAgainst team)
                |> List.length

        fourPointEndsFor team =
            List.filter (\i -> i == 4) (statsFor team)
                |> List.length

        fourPointEndsAgainst team =
            List.filter (\i -> i == 4) (statsAgainst team)
                |> List.length

        fivePlusPointEndsFor team =
            List.filter (\i -> i > 4) (statsFor team)
                |> List.length

        fivePlusPointEndsAgainst team =
            List.filter (\i -> i > 4) (statsAgainst team)
                |> List.length

        totalPointsFor team =
            List.sum (statsFor team)

        totalPointsAgainst team =
            List.sum (statsAgainst team)

        averagePointsFor team =
            -- total points divided by number of ends. We multiple by 100 then round then divide by 100 so that we get 2 decimal places.
            if totalPointsFor team == 0 then
                0.0

            else
                toFloat (round ((toFloat (totalPointsFor team) / toFloat (List.length (statsFor team))) * 100)) / 100

        averagePointsAgainst team =
            -- see averagePointsFor
            toFloat (round ((toFloat (totalPointsAgainst team) / toFloat (List.length (statsAgainst team))) * 100)) / 100

        stolenEnds : Team -> Bool -> List Int
        stolenEnds team for =
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

                        stolenEnd : Side -> Side -> Int -> Maybe Int
                        stolenEnd sideFor sideAgainst endIndex =
                            if hasHammerInEnd event.mixedDoubles sideFor sideAgainst endIndex then
                                Nothing

                            else
                                List.Extra.getAt endIndex sideFor.endScores
                    in
                    case sides of
                        ( Just sideFor, Just sideAgainst ) ->
                            let
                                -- We don't care about the event's number of ends, just the max of either side.
                                numberOfEnds =
                                    List.map (\side -> List.length side.endScores) game.sides
                                        |> List.maximum
                                        |> Maybe.withDefault 0
                            in
                            List.range 0 numberOfEnds
                                |> List.map (stolenEnd sideFor sideAgainst)
                                |> List.filterMap identity
                                |> List.filter (\e -> e > 0)

                        _ ->
                            []
            in
            List.map (\game -> stolenEndsForGame game) (games team)
                |> List.concat

        stolenEndsCount team for =
            stolenEnds team for
                |> List.length

        stolenPoints team for =
            stolenEnds team for
                |> List.sum

        tableHeader content align onPress sup =
            el
                [ El.width El.fill
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , Background.color theme.greyLight
                ]
                (button
                    [ El.focused [ Background.color theme.transparent ]
                    , align
                    , Font.semiBold
                    , Font.color
                        (case onPress of
                            Just _ ->
                                if isForGame then
                                    theme.primary

                                else
                                    theme.defaultText

                            Nothing ->
                                theme.defaultText
                        )
                    ]
                    { onPress =
                        if isForGame then
                            onPress

                        else
                            Nothing
                    , label =
                        el
                            [ El.onRight
                                (case sup of
                                    Just sup_ ->
                                        el [ Font.size 10 ] (text sup_)

                                    Nothing ->
                                        El.none
                                )
                            ]
                            (text content)
                    }
                )

        tableCell align bottomBorder content =
            el
                [ El.width El.fill
                , El.padding 10
                , Border.widthEach { top = 0, right = 1, bottom = bottomBorder, left = 1 }
                , Border.color theme.grey
                ]
                (el [ align ] content)
    in
    column
        [ El.width El.fill, El.htmlAttribute (class "cio__event_reports_scoring_analysis") ]
        [ El.indexedTable [ El.width El.fill, Font.size 13 ]
            { data = rows
            , columns =
                [ { header = tableHeader (translate translations "team") El.alignLeft Nothing Nothing
                  , width = El.fillPortion 5 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                button
                                    [ Font.color theme.primary
                                    , El.focused [ Background.color theme.transparent ]
                                    ]
                                    { onPress = Just (NavigateTo (teamUrl event.id team.id))
                                    , label =
                                        team.name
                                            |> text
                                            |> tableCell El.alignLeft 0
                                    }

                            else
                                tableCell El.centerX 1 (text " ")
                  }
                , { header = tableHeader (translate translations "games") El.centerX Nothing Nothing
                  , width = El.fillPortion 2 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                games team
                                    |> List.length
                                    |> String.fromInt
                                    |> text
                                    |> tableCell El.centerX 0

                            else
                                tableCell El.centerX 1 (text " ")
                  }
                , { header = tableHeader (translate translations "ends") El.centerX Nothing Nothing
                  , width = El.fillPortion 2 |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- It doesn't matter if we use statsFor or statsAgainst, the count is the same since these the ends they participated in, just whether it's the top or bot.
                                statsFor team
                                    |> List.length
                                    |> String.fromInt
                                    |> text
                                    |> tableCell El.centerX 0

                            else
                                tableCell El.centerX 1 (text " ")
                  }
                , { header = tableHeader " " El.alignLeft Nothing Nothing
                  , width = El.fillPortion 2 |> El.minimum 40
                  , view =
                        \i _ ->
                            if modBy 2 i == 0 then
                                tableCell El.centerX 1 (text (translate translations "for"))

                            else
                                tableCell El.centerX 1 (text (translate translations "against"))
                  }

                -- LSFE / First Hammer
                , { header =
                        tableHeader (translate translations "lsfe") El.centerX (Just (ToggleScoringHilight HilightHammers)) (Just "1")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (firstHammerCountFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (firstHammerCountAgainst team)))
                  }

                -- Stolen Ends
                , { header =
                        tableHeader (translate translations "se") El.centerX (Just (ToggleScoringHilight HilightStolenEnds)) (Just "2")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (stolenEndsCount team True)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (stolenEndsCount team False)))
                  }

                -- Blank Ends
                , { header = tableHeader (translate translations "be") El.centerX (Just (ToggleScoringHilight HilightBlankEnds)) (Just "3")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (blankEndsFor True team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (blankEndsFor False team)))
                  }

                -- 1 point ends
                , { header = tableHeader "1pt" El.centerX (Just (ToggleScoringHilight Hilight1PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (onePointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (onePointEndsAgainst team)))
                  }

                -- 2 point ends
                , { header = tableHeader "2pt" El.centerX (Just (ToggleScoringHilight Hilight2PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (twoPointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (twoPointEndsAgainst team)))
                  }

                -- 3 point ends
                , { header = tableHeader "3pt" El.centerX (Just (ToggleScoringHilight Hilight3PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (threePointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (threePointEndsAgainst team)))
                  }

                -- 4 point ends
                , { header = tableHeader "4pt" El.centerX (Just (ToggleScoringHilight Hilight4PointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (fourPointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (fourPointEndsAgainst team)))
                  }

                -- 5+ point ends
                , { header = tableHeader "5pt" El.centerX (Just (ToggleScoringHilight Hilight5PlusPointEnds)) Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (fivePlusPointEndsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (fivePlusPointEndsAgainst team)))
                  }

                -- Total points
                , { header = tableHeader "Tot" El.centerX Nothing Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (totalPointsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (totalPointsAgainst team)))
                  }

                -- Average points
                , { header = tableHeader (translate translations "avg") El.centerX Nothing Nothing
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromFloat (averagePointsFor team)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromFloat (averagePointsAgainst team)))
                  }

                -- Stolen points
                , { header = tableHeader (translate translations "sp") El.centerX (Just (ToggleScoringHilight HilightStolenPoints)) (Just "4")
                  , width = El.fill |> El.minimum 40
                  , view =
                        \i team ->
                            if modBy 2 i == 0 then
                                -- For
                                tableCell El.centerX 1 (text (String.fromInt (stolenPoints team True)))

                            else
                                -- Against
                                tableCell El.centerX 1 (text (String.fromInt (stolenPoints team False)))
                  }
                ]
            }
        , El.column [ El.paddingXY 0 20, El.spacing 5, Font.size 12 ]
            [ el [] (text ("1 - " ++ translate translations "lsfe" ++ ": " ++ translate translations "last_shot_first_end"))
            , el [] (text ("2 - " ++ translate translations "se" ++ ": " ++ translate translations "stolen_ends"))
            , el [] (text ("3 - " ++ translate translations "be" ++ ": " ++ translate translations "blank_ends"))
            , el [] (text ("4 - " ++ translate translations "sp" ++ ": " ++ translate translations "stolen_points"))
            ]
        ]
