module Results.Reports.CompetitionMatrix exposing (view)

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


view : Theme -> List Translation -> Event -> Element Msg
view theme translations event =
    let
        viewStageMatrix stage =
            let
                -- Only the teams participating in this stage.
                teams =
                    -- Filter the teams so only team ids included in sides included in games included in the passed stage.
                    let
                        teamIncluded team =
                            let
                                inSide side =
                                    side.teamId == Just team.id

                                inGame game =
                                    List.any inSide game.sides
                            in
                            List.any inGame stage.games
                    in
                    -- Filter the event.teams to include only those teams involved in games that are in this stage.
                    List.filter teamIncluded event.teams

                viewTeamCell : Team -> Team -> Element Msg
                viewTeamCell teamA teamB =
                    let
                        teamIdsForGame g =
                            List.map .teamId g.sides
                                |> List.filterMap identity

                        matchingGame =
                            List.Extra.find (\g -> [ teamA.id, teamB.id ] == teamIdsForGame g || [ teamB.id, teamA.id ] == teamIdsForGame g) stage.games
                    in
                    case matchingGame of
                        Just game ->
                            el
                                [ El.clip
                                , El.width (El.px 115)
                                , El.height (El.px 47)
                                , El.padding 7
                                , Border.widthEach { top = 1, right = 1, bottom = 0, left = 0 }
                                , Border.color theme.grey
                                ]
                                (el [ El.centerX, El.centerY ]
                                    (case gameScore game (Just ( teamA.id, teamB.id )) of
                                        Just score ->
                                            if event.endScoresEnabled then
                                                let
                                                    -- Only link if the game has been scheduled
                                                    gameHasBeenScheduled =
                                                        case drawWithGameId event.draws game.id of
                                                            Just _ ->
                                                                True

                                                            Nothing ->
                                                                False

                                                    gamePath =
                                                        gameUrl event.id game.id
                                                in
                                                if gameHasBeenScheduled then
                                                    button
                                                        [ Font.color theme.primary
                                                        , El.focused [ Background.color theme.transparent ]
                                                        ]
                                                        { onPress = Just (NavigateTo gamePath)
                                                        , label = text score
                                                        }

                                                else
                                                    text score

                                            else
                                                text score

                                        Nothing ->
                                            text
                                                (case game.state of
                                                    GameComplete ->
                                                        " "

                                                    _ ->
                                                        " "
                                                )
                                    )
                                )

                        Nothing ->
                            el
                                [ El.width (El.px 115)
                                , El.height (El.px 47)
                                , Background.color theme.greyLight
                                , El.padding 20
                                , Border.widthEach { top = 1, right = 1, bottom = 0, left = 0 }
                                , Border.color theme.grey
                                ]
                                (text " ")

                viewHeader team =
                    el
                        [ El.clip
                        , El.width (El.px 115)
                        , El.height (El.px 47)
                        , El.padding 7
                        , Border.widthEach { top = 1, right = 1, bottom = 0, left = 0 }
                        , Border.color theme.grey
                        ]
                        (case team of
                            Just t ->
                                button
                                    [ El.centerX
                                    , El.centerY
                                    , Font.color theme.primary
                                    , El.focused [ Background.color theme.transparent ]
                                    ]
                                    { onPress = Just (NavigateTo (teamUrl event.id t.id))
                                    , label = text t.shortName
                                    }

                            Nothing ->
                                text " "
                        )

                viewTableColumn idx =
                    let
                        team =
                            List.Extra.getAt idx teams
                    in
                    { header = viewHeader team
                    , width = El.px 115
                    , view =
                        \teamA ->
                            case team of
                                Just teamB ->
                                    viewTeamCell teamA teamB

                                Nothing ->
                                    viewHeader Nothing
                    }
            in
            if List.isEmpty stage.games then
                El.none

            else
                column [ El.spacing 10, El.alignTop ]
                    [ el [ Font.size 20 ] (text stage.name)
                    , El.table [ Border.widthEach { top = 0, right = 0, bottom = 1, left = 1 }, Border.color theme.grey, Font.size 14 ]
                        { data = teams
                        , columns =
                            [ { header = viewHeader Nothing
                              , width = El.px 115
                              , view = \team -> viewHeader (Just team)
                              }
                            ]
                                ++ List.map viewTableColumn (List.range 0 (List.length teams - 1))
                        }
                    ]
    in
    column [ El.width El.fill, El.spacing 30, El.htmlAttribute (class "cio__event_reports_competition_matrix") ]
        [ el [ Font.size 24 ] (text (translate translations "competition_matrix"))
        , El.wrappedRow [ El.spacing 30 ] (List.filter (\s -> s.stageType == RoundRobin) event.stages |> List.map viewStageMatrix)
        ]
