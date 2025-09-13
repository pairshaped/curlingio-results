port module Results.Helpers exposing (..)

import Element
import Html.Attributes
import List.Extra
import Results.Types exposing (..)
import Shared.Translation exposing (Translation, translate)



-- PORTS


port navigateTo : String -> Cmd msg


port hashChangeReceiver : (String -> msg) -> Sub msg



-- HELPERS


fromNonempty : ( a, List a ) -> List a
fromNonempty ( x, xs ) =
    x :: xs


attrNone : Element.Attribute msg
attrNone =
    Element.htmlAttribute (Html.Attributes.classList [])



-- FETCHING


rockColorNameToRGB : String -> Element.Color
rockColorNameToRGB color =
    case color of
        "red" ->
            Element.rgb255 204 0 0

        "blue" ->
            Element.rgb255 0 0 204

        "green" ->
            Element.rgb255 0 204 0

        "yellow" ->
            Element.rgb255 204 204 0

        "black" ->
            Element.rgb255 0 0 0

        "white" ->
            Element.rgb255 255 255 255

        _ ->
            Element.rgb255 204 204 0


sideResultToString : List Translation -> Maybe SideResult -> String
sideResultToString translations result =
    translate translations
        (case result of
            Just SideResultWon ->
                "won"

            Just SideResultLost ->
                "lost"

            Just SideResultTied ->
                "tied"

            Just SideResultUnnecessary ->
                "unnecessary"

            Just SideResultForfeited ->
                "forfeited"

            Just SideResultTimePenalized ->
                "was time penalized"

            Nothing ->
                "unknown"
        )


positionNumberToString : List Translation -> Maybe Int -> String
positionNumberToString translations position =
    translate translations
        (case position of
            Just 1 ->
                "first"

            Just 2 ->
                "second"

            Just 3 ->
                "third"

            Just 4 ->
                "fourth"

            Just _ ->
                "alternate"

            Nothing ->
                " "
        )


deliveryToString : List Translation -> Maybe RockDelivery -> String
deliveryToString translations delivery =
    case delivery of
        Just RockDeliveryRight ->
            translate translations "right"

        Just RockDeliveryLeft ->
            translate translations "left"

        Nothing ->
            "-"


gamesInEvent : Event -> List Game
gamesInEvent event =
    let
        gamesFromStages : List Stage -> List Game
        gamesFromStages stages =
            List.map .games stages
                |> List.concat
    in
    gamesFromStages event.stages


findGameById : Event -> String -> Maybe Game
findGameById event id =
    gamesInEvent event
        |> List.Extra.find (\g -> g.id == id)


drawWithGameId : List Draw -> String -> Maybe Draw
drawWithGameId draws id =
    let
        hasGame draw =
            let
                matching gameId =
                    gameId == Just id
            in
            List.any matching draw.drawSheets
    in
    List.Extra.find hasGame draws


gamesInDraw : Event -> Draw -> List Game
gamesInDraw event draw =
    draw.drawSheets
        |> List.filterMap identity
        |> List.map (findGameById event)
        |> List.filterMap identity


drawHasCompletedGame : Event -> Draw -> Bool
drawHasCompletedGame event draw =
    gamesInDraw event draw
        |> List.filter (\g -> g.state == GameComplete)
        |> List.isEmpty
        |> not


drawHasActiveGame : Event -> Draw -> Bool
drawHasActiveGame event draw =
    gamesInDraw event draw
        |> List.filter (\g -> g.state == GameActive)
        |> List.isEmpty
        |> not


drawsWithCompletedGames : Event -> List Draw
drawsWithCompletedGames event =
    List.filter (drawHasCompletedGame event) event.draws


currentDrawForEvent : Event -> Maybe Draw
currentDrawForEvent event =
    case List.Extra.find (\g -> g.state == GameActive) (gamesInEvent event) of
        Just game ->
            let
                findGameInDraw draw =
                    List.any (\ds -> ds == Just game.id) draw.drawSheets
            in
            List.filter findGameInDraw event.draws
                |> List.head

        Nothing ->
            case event.currentDrawId of
                Just currentDrawId ->
                    List.Extra.find (\d -> d.id == currentDrawId) event.draws

                Nothing ->
                    Nothing


teamForSide : List Team -> Side -> Maybe Team
teamForSide teams side =
    List.Extra.find (\t -> Just t.id == side.teamId) teams


gamesWithTeam : Event -> Team -> List Game
gamesWithTeam event team =
    let
        participatedIn sides =
            List.any (\s -> s.teamId == Just team.id) sides
    in
    gamesInEvent event
        |> List.filter (\g -> participatedIn g.sides)


sheetNameForGame : Event -> Game -> String
sheetNameForGame event game =
    let
        drawHasGame : Game -> Draw -> Bool
        drawHasGame game_ draw =
            List.any (\gameId -> gameId == Just game_.id) draw.drawSheets

        sheetNumber : Game -> Draw -> Maybe Int
        sheetNumber game_ draw =
            let
                matching gameId =
                    gameId == Just game_.id
            in
            List.Extra.findIndex matching draw.drawSheets
    in
    case List.Extra.find (drawHasGame game) event.draws of
        Just draw ->
            case sheetNumber game draw of
                Just index ->
                    case List.Extra.getAt index event.sheetNames of
                        Just sheetName ->
                            sheetName

                        Nothing ->
                            ""

                Nothing ->
                    ""

        Nothing ->
            ""


hasHammerInEnd : Bool -> Side -> Side -> Int -> Bool
hasHammerInEnd mixedDoubles sideFor sideAgainst endIndex =
    let
        previousEndScore s =
            List.Extra.getAt (endIndex - 1) s.endScores
    in
    if endIndex == 0 then
        -- If we're in the first end, then we check the firstHammer field.
        sideFor.firstHammer

    else
        -- If we're not in the first end, then we compare the scores from the previous end.
        case ( previousEndScore sideFor, previousEndScore sideAgainst ) of
            ( Just 0, Just 0 ) ->
                -- If the previous end was 0, we go back another end via recursion
                if mixedDoubles then
                    -- For mixed doubles, a 0 0 end does the opposite and switches hammer
                    not (hasHammerInEnd mixedDoubles sideFor sideAgainst (endIndex - 1))

                else
                    hasHammerInEnd mixedDoubles sideFor sideAgainst (endIndex - 1)

            ( Just 0, _ ) ->
                True

            _ ->
                False


isBlankEnd : Maybe Side -> Maybe Side -> Int -> Bool
isBlankEnd sideA sideB endIndex =
    let
        end side =
            List.Extra.getAt endIndex side.endScores
    in
    case ( sideA, sideB ) of
        ( Just a, Just b ) ->
            ( end a, end b ) == ( Just 0, Just 0 )

        _ ->
            False


gameScore : Game -> Maybe ( Int, Int ) -> Maybe String
gameScore game orderByTeamIds =
    let
        sides =
            -- If we passed team ids to order by, use them. Otherwise just use the default side positions.
            case orderByTeamIds of
                Just teamIds ->
                    [ List.Extra.find (\side -> side.teamId == Just (Tuple.first teamIds)) game.sides
                    , List.Extra.find (\side -> side.teamId == Just (Tuple.second teamIds)) game.sides
                    ]
                        |> List.filterMap identity

                Nothing ->
                    game.sides

        sideResults =
            List.map (\s -> s.result) sides
                |> List.filterMap identity

        intScores =
            List.map (\s -> s.score) sides
                |> List.filterMap identity

        strScores =
            List.map String.fromInt intScores

        fromScores =
            case game.state of
                GameComplete ->
                    case ( Maybe.withDefault 0 (List.head intScores), Maybe.withDefault 0 (List.Extra.getAt 1 intScores) ) of
                        ( 0, 0 ) ->
                            -- Display a W if a won, an L if a lost, or a T if they tied.
                            case List.head sideResults of
                                Nothing ->
                                    "-"

                                Just SideResultWon ->
                                    "W"

                                Just SideResultTied ->
                                    "T"

                                Just SideResultUnnecessary ->
                                    "U"

                                _ ->
                                    "L"

                        _ ->
                            -- if a > b then
                            --     String.join " > " strScores
                            --
                            -- else if a < b then
                            --     String.join " < " strScores
                            --
                            -- else
                            --     String.join " = " strScores
                            String.join " - " strScores

                _ ->
                    ""
    in
    case fromScores of
        "" ->
            Nothing

        score ->
            Just score
