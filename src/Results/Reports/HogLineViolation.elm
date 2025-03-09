module Results.Reports.HogLineViolation exposing (view)

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


view : Theme -> List Translation -> Event -> Element Msg
view theme translations event =
    let
        hogLineViolations : List ShotExpanded
        hogLineViolations =
            let
                expandShotsForEvent : List ShotExpanded
                expandShotsForEvent =
                    let
                        expandShotsForStage : Stage -> List ShotExpanded
                        expandShotsForStage stage =
                            -- Kind of annoying, but we need to dig down to shots while picking up the draw label, and side's team id.
                            -- Ignore it if we're missing a draw (unscheduled), team (no point in reporting), curler (no point in reporting).
                            List.map (expandShotsForGame event) stage.games
                                -- Lift the games up to the root
                                |> List.concat
                    in
                    -- Kind of annoying, but we need to dig down to shots while picking up the draw label, and side's team id.
                    -- Ignore it if we're missing a draw (unscheduled), team (no point in reporting), curler (no point in reporting).
                    event.stages
                        |> List.map expandShotsForStage
                        -- Lift the stages up to the root
                        |> List.concat
            in
            expandShotsForEvent
                |> List.filter (\s -> s.rating == Just "V")

        viewHeader align label =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                , Font.semiBold
                , Background.color theme.greyLight
                ]
                (el [ align ] (text (translate translations label)))

        viewCell align content =
            el
                [ El.padding 15
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.grey
                ]
                (el [ align ] content)
    in
    -- We are looking for any shots with a rating value of "V" (for violation)
    -- Then we want to report the athlete name, the team name, the draw label, and the end number.
    column [ El.spacing 20, El.width El.fill ]
        [ el [ Font.size 24 ] (text (translate translations "hog_line_violation"))
        , El.table []
            { data = hogLineViolations
            , columns =
                [ { header = viewHeader El.alignLeft "curler"
                  , width = El.fill
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignLeft (text hogLineViolation.curlerName)
                  }
                , { header = viewHeader El.alignLeft "team"
                  , width = El.fill
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignLeft
                                (button
                                    [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo (teamUrl event.id hogLineViolation.teamId))
                                    , label = text hogLineViolation.teamShortName
                                    }
                                )
                  }
                , { header = viewHeader El.alignRight "draw"
                  , width = El.fill |> El.maximum 100
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignRight
                                (button
                                    [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo (drawUrl event.id hogLineViolation.drawId))
                                    , label = text hogLineViolation.drawLabel
                                    }
                                )
                  }
                , { header = viewHeader El.alignRight "end_number"
                  , width = El.fill |> El.maximum 100
                  , view =
                        \hogLineViolation ->
                            viewCell El.alignRight
                                (button
                                    [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo (gameUrl event.id hogLineViolation.gameId))
                                    , label = text (String.fromInt hogLineViolation.endNumber)
                                    }
                                )
                  }
                ]
            }
        ]
