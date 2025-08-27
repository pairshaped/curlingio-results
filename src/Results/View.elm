module Results.View exposing (view)

import Array
import Element as El exposing (Device, Element, column, el, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, style)
import Html.Events
import List.Extra
import Markdown
import RemoteData exposing (RemoteData(..))
import Results.CustomSvg exposing (..)
import Results.Helpers exposing (..)
import Results.Reports.Helpers exposing (..)
import Results.Reports.ScoringAnalysis
import Results.Reports.ScoringAndPercentages
import Results.Reports.View
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Shared.Theme exposing (Theme, defaultTheme)
import Shared.Translation exposing (Translation, decodeTranslations, translate)



-- HELPER FUNCTIONS


findSourceConnection : List Game -> String -> Int -> Maybe ( Game, GameResult )
findSourceConnection games targetGameId targetPosition =
    let
        checkGame game =
            if game.winnerToGameId == Just targetGameId && game.winnerToSide == Just targetPosition then
                Just ( game, Winner )

            else if game.loserToGameId == Just targetGameId && game.loserToSide == Just targetPosition then
                Just ( game, Loser )

            else
                Nothing
    in
    games
        |> List.filterMap checkGame
        |> List.head



-- VIEWS


viewButtonPrimary theme content msg =
    button
        [ Background.color theme.primary

        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
        , El.htmlAttribute (attribute "style" "color: white !important;")
        , El.paddingXY 12 10
        , Border.rounded 4
        , El.focused [ Background.color theme.primary ]
        ]
        { onPress = Just msg
        , label = text content
        }


view : Model -> Html Msg
view model =
    let
        { device, fullScreenToggle, fullScreen } =
            model.flags

        viewMain =
            el
                [ El.htmlAttribute (class "cio__main")
                , El.width
                    (if fullScreen then
                        El.fill

                     else
                        El.fill
                            |> El.maximum
                                (case device.class of
                                    El.Phone ->
                                        599

                                    El.Tablet ->
                                        1199

                                    El.Desktop ->
                                        1200

                                    El.BigDesktop ->
                                        1920
                                )
                    )
                , El.scrollbarX
                , El.clipY
                , El.centerX
                ]
                (case model.errorMsg of
                    Just errorMsg ->
                        viewNotReady fullScreen errorMsg

                    Nothing ->
                        case model.translations of
                            Success translations ->
                                viewRoute translations model

                            Failure error ->
                                Lazy.lazy2 viewFetchError model.flags.theme (errorMessage error)

                            _ ->
                                viewNotReady fullScreen "Loading..."
                )

        theme =
            model.flags.theme
    in
    El.layout
        [ Font.size 16
        , Font.color theme.defaultText
        , El.width El.fill
        , El.padding 10
        , if fullScreen then
            El.htmlAttribute (style "z-index" "2001")

          else
            attrNone
        , Font.family
            [ Font.typeface "-apple-system"
            , Font.typeface "BlinkMacSystemFont"
            , Font.typeface "Segoe UI"
            , Font.typeface "Roboto"
            , Font.typeface "Helvetica Neue"
            , Font.typeface "Arial"
            , Font.typeface "Noto Sans"
            , Font.typeface "Noto Sans"
            , Font.typeface "Liberation Sans"
            , Font.typeface "Apple Color Emoji"
            , Font.typeface "Segoe UI Emoji"
            , Font.typeface "Segoe UI Symbol"
            , Font.typeface "Noto Color Emoji"
            , Font.sansSerif
            ]
        , El.htmlAttribute (class "cio__container")
        , if fullScreen then
            El.inFront
                (el
                    [ El.width El.fill
                    , El.height El.fill
                    , El.padding 10
                    , El.scrollbarY
                    , Background.color theme.white
                    ]
                    viewMain
                )

          else
            attrNone
        ]
    <|
        if fullScreen then
            El.none

        else
            viewMain


viewRoute : List Translation -> Model -> Element Msg
viewRoute translations { flags, hash, itemFilter, eventConfig, items, product, event } =
    let
        { device, fullScreen } =
            flags

        viewLoading =
            Lazy.lazy2 viewNotReady fullScreen "Loading..."
    in
    case toRoute flags.defaultEventSection hash of
        ItemsRoute ->
            case items of
                Success items_ ->
                    Lazy.lazy5 viewItems flags device translations itemFilter items_

                Failure error ->
                    Lazy.lazy2 viewFetchError flags.theme (errorMessage error)

                _ ->
                    viewLoading

        ProductRoute id ->
            case product of
                Success product_ ->
                    Lazy.lazy4 viewProduct flags translations fullScreen product_

                Failure error ->
                    Lazy.lazy2 viewFetchError flags.theme (errorMessage error)

                _ ->
                    viewLoading

        EventRoute id nestedRoute ->
            case event of
                Success event_ ->
                    Lazy.lazy5 viewEvent flags translations eventConfig nestedRoute event_

                Failure error ->
                    Lazy.lazy2 viewFetchError flags.theme (errorMessage error)

                _ ->
                    viewLoading


viewReloadStatus : Flags -> List Translation -> NestedEventRoute -> Event -> Element Msg
viewReloadStatus flags translations nestedRoute event =
    let
        { device, theme } =
            flags

        shouldReload =
            (event.state == EventStateActive)
                && event.endScoresEnabled
                && not (List.member nestedRoute [ DetailsRoute, RegistrationsRoute, SparesRoute, TeamsRoute ])
    in
    if shouldReload && device.class /= El.Phone then
        el
            [ El.paddingXY 10 15
            , El.alignTop
            , Font.size 12
            , Font.color theme.secondary
            , El.htmlAttribute (class "cio__reload_button")
            ]
            (text (translate translations "refreshes_in" ++ " 30s"))

    else
        El.none


viewFullScreenButton : Theme -> List Translation -> Bool -> Element Msg
viewFullScreenButton theme translations fullScreen =
    button
        [ El.paddingXY 5 4
        , Border.rounded 4
        , Font.size 12
        , Font.color theme.white
        , Background.color theme.secondary
        , El.focused [ Background.color theme.secondary ]
        ]
        { onPress = Just ToggleFullScreen
        , label =
            if fullScreen then
                -- El.html svgExitFullScreen
                text (translate translations "minimize" ++ " ↙")

            else
                -- El.html svgFullScreen
                text (translate translations "fullscreen" ++ " ↗")
        }


viewNotReady : Bool -> String -> Element Msg
viewNotReady fullScreen message =
    el [ El.htmlAttribute (class "cio__not_ready") ] (text message)


viewFetchError : Theme -> String -> Element Msg
viewFetchError theme message =
    row
        [ El.htmlAttribute (class "cio__fetch_error") ]
        [ column [ El.spacing 10 ]
            [ el [] (text message)
            , viewButtonPrimary theme "Reload" Reload
            ]
        ]


viewItems : Flags -> Device -> List Translation -> ItemFilter -> ItemsResult -> Element Msg
viewItems flags device translations itemFilter items =
    let
        { theme, fullScreen, section, registration } =
            flags

        viewPaging =
            let
                viewPageButton content msg =
                    button
                        [ Font.size 14
                        , El.padding 8
                        , Border.rounded 3

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important;")
                        , Background.color theme.primary
                        , El.focused [ Background.color theme.primary ]
                        ]
                        { onPress = Just msg
                        , label = text content
                        }
            in
            row [ El.htmlAttribute (class "cio__paging"), El.spacing 10 ]
                [ if itemFilter.page > 1 then
                    viewPageButton "< Previous" (IncrementPageBy -1)

                  else
                    El.none
                , if List.length filteredItems > (itemFilter.page * 10) then
                    viewPageButton "Next >" (IncrementPageBy 1)

                  else
                    El.none
                ]

        viewSeasonDropDown =
            let
                seasonOption { display, delta } =
                    el
                        [ El.width El.fill
                        , El.padding 10
                        , Events.onClick (UpdateSeasonDelta delta)
                        , if delta == itemFilter.seasonDelta then
                            Background.color theme.greyLight

                          else
                            Background.color theme.transparent
                        ]
                        (text (translate translations display))

                seasonOptions =
                    if itemFilter.seasonSearchOpen then
                        let
                            scrolling =
                                if List.length items.seasons > 5 then
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
                            (List.map seasonOption items.seasons)

                    else
                        El.none

                seasonSelected =
                    List.filter (\season -> season.delta == itemFilter.seasonDelta) items.seasons
                        |> List.map .display
                        |> List.head
                        |> Maybe.withDefault "-"
            in
            row
                [ El.width (El.px 150)
                , El.padding 10
                , Border.width 1
                , Border.color theme.grey
                , El.pointer
                , Events.onClick ToggleSeasonSearch
                , El.below seasonOptions
                , El.htmlAttribute (class "cio__season_dropdown")
                ]
                [ el [] (text seasonSelected)
                , el [ El.alignRight ]
                    (text
                        (if itemFilter.seasonSearchOpen then
                            "▼"

                         else
                            "►"
                        )
                    )
                ]

        filteredItems =
            case String.trim itemFilter.search of
                "" ->
                    items.items

                _ ->
                    let
                        matches item =
                            String.contains itemFilter.search (String.toLower item.name)
                                || (case item.location of
                                        Just location ->
                                            String.contains itemFilter.search (String.toLower location)

                                        Nothing ->
                                            False
                                   )
                    in
                    List.filter matches items.items

        pagedItems =
            filteredItems
                |> Array.fromList
                |> Array.slice ((itemFilter.page - 1) * 10) (itemFilter.page * 10)
                |> Array.toList
    in
    column [ El.spacing 10, El.width El.fill ]
        [ row [ El.spacing 20, El.htmlAttribute (class "cio__filter_container") ]
            [ if flags.searchable then
                El.html
                    (Html.input
                        [ Html.Attributes.placeholder (translate translations "search")
                        , style "box-sizing" "border-box"
                        , style "height" "38px"
                        , style "padding" "3px 10px"
                        , style "margin-right" "10px"
                        , style "border" "solid lightgray 1px"
                        , style "width" "170px"
                        , Html.Events.onInput UpdateSearch
                        ]
                        []
                    )

              else
                El.none
            , if List.length items.seasons > 1 then
                viewSeasonDropDown

              else
                El.none
            ]
        , if List.isEmpty filteredItems then
            el [ El.padding 10 ] (text "No results found.")

          else
            let
                viewItemName item =
                    let
                        newPath =
                            case section of
                                ProductsSection ->
                                    "/products/" ++ String.fromInt item.id

                                _ ->
                                    "/events/" ++ String.fromInt item.id
                    in
                    column
                        [ El.spacingXY 0 5
                        , El.paddingXY 10 15
                        , Border.color theme.grey
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class "cio__item_name")
                        ]
                        [ el [ Font.color theme.primary, El.pointer, Events.onClick (NavigateTo newPath) ] (text item.name)
                        , el [ Font.size 13 ] (text (Maybe.withDefault " " item.summary))
                        ]

                viewItemCell content =
                    el
                        [ El.paddingXY 10 24
                        , Border.color theme.grey
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class "cio__item_cell")
                        ]
                        content

                viewItemOccursOn item =
                    viewItemCell (el [ El.centerX, El.htmlAttribute (class "cio__item_occurs_on") ] (text (Maybe.withDefault " " item.occursOn)))

                viewItemLocation item =
                    column
                        [ El.spacingXY 0 5
                        , El.paddingXY 10 15
                        , Border.color theme.grey
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class "cio__item_location")
                        ]
                        [ el [] (text (Maybe.withDefault " " item.location))
                        , el [ Font.size 13 ] (text (Maybe.withDefault " " item.venue))
                        ]

                viewItemPrice item =
                    if registration then
                        viewItemCell (el [ El.alignRight, El.htmlAttribute (class "cio__item_price") ] (text (Maybe.withDefault " " item.price)))

                    else
                        viewItemCell (text " ")

                viewItemRegister item =
                    if registration then
                        case item.noRegistrationMessage of
                            Just msg ->
                                viewItemCell (el [ El.alignRight, El.htmlAttribute (class "cio__item_register") ] (text msg))

                            Nothing ->
                                case ( item.addToCartUrl, item.addToCartText ) of
                                    ( Just addToCartUrl, Just addToCartText ) ->
                                        el
                                            [ El.paddingXY 10 17
                                            , Border.color theme.grey
                                            , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                                            , El.htmlAttribute (class "cio__item_register")
                                            ]
                                            (button
                                                [ Background.color theme.primary

                                                -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                                                , El.htmlAttribute (attribute "style" "color: white !important;")
                                                , Font.size 14
                                                , El.alignRight
                                                , El.padding 8
                                                , Border.rounded 3
                                                , El.focused [ Background.color theme.primary ]
                                                ]
                                                { onPress = Just (NavigateOut addToCartUrl)
                                                , label = text addToCartText
                                                }
                                            )

                                    _ ->
                                        viewItemCell (text " ")

                    else
                        viewItemCell (text " ")
            in
            row [ El.width El.fill ]
                [ El.table [ El.spacingXY 0 15, El.htmlAttribute (class "cio__items_table") ]
                    { data = pagedItems
                    , columns =
                        [ { header = El.none
                          , width = El.fill
                          , view = viewItemName
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemOccursOn
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemLocation
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemPrice
                          }
                        , { header = El.none
                          , width = El.fill
                          , view = viewItemRegister
                          }
                        ]
                    }
                ]
        , viewPaging
        ]


viewNoDataForRoute : List Translation -> Element Msg
viewNoDataForRoute translations =
    el [] (text (translate translations "no_data_for_route"))


viewSponsor : Sponsor -> Element Msg
viewSponsor sponsor =
    column [ El.spacing 10, El.width El.fill, El.alignTop, El.htmlAttribute (class "cio__sponsor") ]
        [ case sponsor.url of
            Just url ->
                el [ El.width El.fill, El.pointer, Events.onClick (NavigateTo url) ]
                    (El.image [ El.alignRight ] { src = sponsor.logoUrl, description = Maybe.withDefault "" sponsor.name })

            Nothing ->
                El.image [] { src = sponsor.logoUrl, description = Maybe.withDefault "" sponsor.name }
        , case sponsor.name of
            Just name ->
                el [ El.alignRight ] (text name)

            Nothing ->
                El.none
        ]


viewProduct : Flags -> List Translation -> Bool -> Product -> Element Msg
viewProduct { theme } translations fullScreen product =
    row
        [ El.width El.fill
        , El.height El.fill
        , El.paddingXY 0 20
        , El.spacing 20
        , El.htmlAttribute (class "cio__product")
        ]
        [ column
            [ El.spacing 20
            , El.width El.fill
            , El.height El.fill
            , El.alignTop
            ]
            [ el [ Font.size 28 ] (text product.name)
            , case product.summary of
                Just summary ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_summary") ] [ text summary ]

                Nothing ->
                    El.none
            , case product.description of
                Just description ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_description") ] [ El.html (Markdown.toHtml [] description) ]

                Nothing ->
                    El.none
            , case product.total of
                Just total ->
                    column [ El.spacing 8, El.htmlAttribute (class "cio__product_total") ]
                        [ el [ Font.bold ] (text (translate translations "total"))
                        , el [] (text total)
                        ]

                _ ->
                    El.none
            , case ( product.addToCartUrl, product.addToCartText ) of
                ( Just addToCartUrl, Just addToCartText ) ->
                    El.paragraph [ El.htmlAttribute (class "cio__product_add_to_cart") ]
                        [ viewButtonPrimary theme addToCartText (NavigateOut addToCartUrl)
                        ]

                _ ->
                    El.none
            , if not (List.isEmpty product.potentialDiscounts) then
                column [ El.spacing 5, El.htmlAttribute (class "cio__product_discounts") ]
                    [ el [ Font.bold ] (text (translate translations "potential_discounts"))
                    , column [] (List.map (\d -> el [] (text d)) product.potentialDiscounts)
                    ]

              else
                El.none
            ]
        , case product.sponsor of
            Just sponsor ->
                viewSponsor sponsor

            Nothing ->
                El.none
        ]


viewEvent : Flags -> List Translation -> EventConfig -> NestedEventRoute -> Event -> Element Msg
viewEvent flags translations eventConfig nestedRoute event =
    let
        { device, theme, fullScreenToggle, fullScreen } =
            flags

        eventSections : List String
        eventSections =
            let
                -- Check if a section is included (not in the explicitly excluded sections list).
                included section =
                    List.map String.toLower flags.excludeEventSections
                        |> List.member (String.toLower section)
                        |> not

                hasData section =
                    let
                        hasRegistrations =
                            not (List.isEmpty event.registrations)

                        hasSpares =
                            not (List.isEmpty event.spares)

                        hasDraws =
                            not (List.isEmpty event.draws)

                        hasStages =
                            not (List.isEmpty event.stages)

                        hasTeams =
                            not (List.isEmpty event.teams)

                        hasCompletedGames =
                            List.any (\g -> g.state == GameComplete) (gamesInEvent event)

                        hasEndScores =
                            event.endScoresEnabled
                    in
                    case section of
                        "registrations" ->
                            hasRegistrations

                        "spares" ->
                            hasSpares

                        "draws" ->
                            hasDraws

                        "stages" ->
                            hasStages

                        "teams" ->
                            hasTeams

                        "reports" ->
                            (hasDraws && hasTeams)
                                || hasCompletedGames

                        _ ->
                            True
            in
            [ "details", "registrations", "spares", "draws", "stages", "teams", "reports" ]
                |> List.filter included
                |> List.filter hasData

        viewNavItem eventSection =
            let
                isActiveRoute =
                    -- TODO: This needs a bit of work. I don't like the string pattern matching, would prefer patterning on toRoute result.
                    eventSection == eventSectionForRoute nestedRoute

                newPath =
                    "/events/" ++ String.fromInt event.id ++ "/" ++ eventSection
            in
            el [ El.htmlAttribute (class "cio__event_nav_item") ]
                (if isActiveRoute then
                    button
                        [ El.paddingXY 16 12
                        , Border.rounded 4
                        , Background.color theme.primary

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important;")
                        , El.focused [ Background.color theme.primary ]
                        ]
                        { onPress = Just (NavigateTo newPath)
                        , label = text (translate translations eventSection)
                        }

                 else
                    button
                        [ El.paddingXY 16 12
                        , Font.color theme.primary
                        , Border.rounded 4
                        , El.focused [ Background.color theme.transparent ]
                        ]
                        { onPress = Just (NavigateTo newPath)
                        , label = text (translate translations eventSection)
                        }
                )
    in
    column
        [ El.width El.fill
        , El.height El.fill
        , El.spacing 20
        , El.htmlAttribute (class "cio__event")
        ]
        [ El.row [ El.spacing 10 ]
            [ el
                [ Font.size
                    (case device.class of
                        El.Phone ->
                            22

                        _ ->
                            28
                    )
                , El.width El.fill
                , Font.medium
                , El.htmlAttribute (class "cio__event_name")
                ]
                (text event.name)
            , el [ El.alignTop ] (viewReloadStatus flags translations nestedRoute event)
            , if fullScreenToggle then
                el [] (viewFullScreenButton theme translations fullScreen)

              else
                El.none
            ]
        , El.row [ El.width El.fill, El.htmlAttribute (class "cio__event_nav") ]
            (List.map viewNavItem eventSections
                ++ (case event.videoUrl of
                        Just videoUrl ->
                            [ el [ El.padding 8 ] (text "")
                            , El.newTabLink
                                [ El.padding 8
                                , Border.rounded 4
                                , Font.color theme.white
                                , Background.color theme.secondary
                                ]
                                { url = videoUrl
                                , label = text (translate translations "video" ++ " ▶")
                                }
                            ]

                        Nothing ->
                            []
                   )
            )
        , case nestedRoute of
            DetailsRoute ->
                Lazy.lazy4 viewDetails theme device translations event

            RegistrationsRoute ->
                Lazy.lazy3 viewRegistrations theme translations event.registrations

            SparesRoute ->
                Lazy.lazy3 viewSpares flags translations event

            DrawsRoute ->
                Lazy.lazy4 viewDraws theme translations eventConfig event

            DrawRoute drawId ->
                case List.Extra.find (\d -> d.id == drawId) event.draws of
                    Just draw ->
                        Lazy.lazy5 viewDraw theme translations eventConfig event draw

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            GameRoute gameId ->
                let
                    drawWithGame =
                        drawWithGameId event.draws gameId
                in
                case ( drawWithGame, findGameById event gameId ) of
                    ( Just draw, Just game ) ->
                        let
                            sheetLabel =
                                sheetNameForGame event game
                        in
                        viewGame theme translations eventConfig event sheetLabel True draw game

                    _ ->
                        Lazy.lazy viewNoDataForRoute translations

            StagesRoute ->
                case List.head event.stages of
                    Just stage ->
                        Lazy.lazy5 viewStages theme device translations event stage

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            StageRoute id ->
                case List.Extra.find (\s -> s.id == id) event.stages of
                    Just stage ->
                        Lazy.lazy5 viewStages theme device translations event stage

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            TeamsRoute ->
                Lazy.lazy3 viewTeams theme translations event

            TeamRoute id ->
                case List.Extra.find (\t -> t.id == id) event.teams of
                    Just team ->
                        Lazy.lazy5 viewTeam theme translations flags event team

                    Nothing ->
                        Lazy.lazy viewNoDataForRoute translations

            ReportsRoute ->
                Lazy.lazy3 Results.Reports.View.viewReports theme translations event

            ReportRoute report ->
                Lazy.lazy5 Results.Reports.View.viewReport theme translations eventConfig event report
        ]


viewDetails : Theme -> Device -> List Translation -> Event -> Element Msg
viewDetails theme device translations event =
    row [ El.spacing 20, El.htmlAttribute (class "cio__event_details") ]
        [ column [ El.spacing 20, El.width El.fill, El.alignTop ]
            [ case event.summary of
                Just summary ->
                    El.paragraph [ El.htmlAttribute (class "cio__event_summary") ] [ text summary ]

                Nothing ->
                    El.none
            , case event.description of
                Just description ->
                    El.paragraph [ El.htmlAttribute (class "cio__event_description") ] [ El.html (Markdown.toHtml [] description) ]

                Nothing ->
                    El.none
            , case event.total of
                Just total ->
                    column [ El.spacing 8, El.htmlAttribute (class "cio__event_total") ]
                        [ el [ Font.bold ] (text (translate translations "total"))
                        , el [] (text total)
                        ]

                _ ->
                    El.none
            , case ( event.addToCartUrl, event.addToCartText ) of
                ( Just addToCartUrl, Just addToCartText ) ->
                    El.paragraph [ El.paddingEach { top = 10, right = 0, bottom = 20, left = 0 }, El.htmlAttribute (class "cio__event_add_to_cart") ]
                        [ viewButtonPrimary theme addToCartText (NavigateOut addToCartUrl)
                        ]

                _ ->
                    El.none
            , row [ El.width El.fill, El.spacing 20 ]
                [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_starts_on") ]
                    [ el [ Font.bold ] (text (translate translations "starts_on"))
                    , el [] (text event.startsOn)
                    ]
                , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_ends_on") ]
                    [ el [ Font.bold ] (text (translate translations "ends_on"))
                    , el [] (text event.endsOn)
                    ]
                ]
            , case ( event.registrationOpensAt, event.registrationClosesAt ) of
                ( Just registrationOpensAt, Just registrationClosesAt ) ->
                    row [ El.width El.fill, El.spacing 20 ]
                        [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_registration_opens_at") ]
                            [ el [ Font.bold ] (text (translate translations "registration_opens_at"))
                            , el [] (text registrationOpensAt)
                            ]
                        , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_registration_closes_at") ]
                            [ el [ Font.bold ] (text (translate translations "registration_closes_at"))
                            , el [] (text registrationClosesAt)
                            ]
                        ]

                _ ->
                    El.none
            , case event.timeZone of
                Just timeZone ->
                    row [ El.width El.fill, El.spacing 20 ]
                        [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_time_zone") ]
                            [ el [ Font.bold ] (text (translate translations "time_zone"))
                            , el [] (text timeZone)
                            ]
                        ]

                Nothing ->
                    El.none
            , case ( event.location, event.venue ) of
                ( Just location, Just venue ) ->
                    row [ El.width El.fill, El.spacing 20 ]
                        [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_location") ]
                            [ el [ Font.bold ] (text (translate translations "location"))
                            , el [] (text location)
                            , el [ Font.size 13 ] (text venue)
                            ]
                        ]

                _ ->
                    El.none
            , row [ El.width El.fill, El.spacing 20 ]
                [ column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_team_restriction") ]
                    [ el [ Font.bold ] (text (translate translations "team_restriction"))
                    , el [] (text event.teamRestriction)
                    ]
                , column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_age_range") ]
                    [ el [ Font.bold ] (text (translate translations "age_range"))
                    , el [] (text event.ageRange)
                    ]
                ]
            , row [ El.width El.fill, El.spacing 20 ]
                [ case ( event.spotsAvailable, event.spotsRemaining ) of
                    ( Just spotsAvailable, Just spotsRemaining ) ->
                        column [ El.width El.fill, El.spacing 10, El.htmlAttribute (class "cio__event_spots_available") ]
                            [ el [ Font.bold ] (text (translate translations "spots_available"))
                            , el [] (text (String.fromInt spotsRemaining ++ " / " ++ String.fromInt spotsAvailable))
                            ]

                    _ ->
                        El.none
                , if not (List.isEmpty event.potentialDiscounts) then
                    column [ El.width El.fill, El.spacing 5, El.htmlAttribute (class "cio__event_discounts") ]
                        [ el [ Font.bold ] (text (translate translations "potential_discounts"))
                        , column [ El.spacing 5, El.paddingXY 4 5 ] (List.map (\d -> el [] (text ("• " ++ d))) event.potentialDiscounts)
                        ]

                  else
                    El.none
                ]
            ]
        , case event.sponsor of
            Just sponsor ->
                if device.class == El.Phone then
                    El.none

                else
                    viewSponsor sponsor

            Nothing ->
                El.none
        ]


viewRegistrations : Theme -> List Translation -> List Registration -> Element Msg
viewRegistrations theme translations registrations =
    let
        hasCurlers =
            List.any (\r -> r.curlerName /= Nothing) registrations

        hasTeamNames =
            List.any (\r -> r.teamName /= Nothing) registrations

        hasSkipNames =
            List.any (\r -> r.skipName /= Nothing) registrations

        hasPositions =
            List.any (\r -> r.position /= Nothing) registrations

        hasLineups =
            List.any (\r -> r.lineup /= Nothing) registrations

        tableHeader content =
            el
                [ Font.bold
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                , Border.color theme.grey
                , El.padding 12
                ]
                (text (translate translations content))

        tableCell i content =
            el
                [ El.padding 12
                , Background.color
                    (if modBy 2 i == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                (text content)

        curlerColumn =
            if hasCurlers then
                Just
                    { header = tableHeader "curler"
                    , width = El.fill
                    , view = \i reg -> tableCell i (Maybe.withDefault "-" reg.curlerName)
                    }

            else
                Nothing

        teamColumn =
            if hasTeamNames then
                Just
                    { header = tableHeader "team"
                    , width = El.fill
                    , view = \i reg -> tableCell i (Maybe.withDefault "-" reg.teamName)
                    }

            else
                Nothing

        skipColumn =
            if hasSkipNames then
                Just
                    { header = tableHeader "skip"
                    , width = El.fill
                    , view = \i reg -> tableCell i (Maybe.withDefault "-" reg.skipName)
                    }

            else
                Nothing

        positionColumn =
            if hasPositions then
                Just
                    { header = tableHeader "position"
                    , width = El.fill
                    , view =
                        \i reg ->
                            tableCell i
                                (case reg.position of
                                    Just pos ->
                                        translate translations pos

                                    Nothing ->
                                        "-"
                                )
                    }

            else
                Nothing

        lineupColumn =
            if hasLineups then
                Just
                    { header = tableHeader "lineup"
                    , width = El.fill
                    , view =
                        \i reg ->
                            tableCell i
                                (case reg.lineup of
                                    Just lineup ->
                                        [ lineup.first
                                        , lineup.second
                                        , lineup.third
                                        , lineup.fourth
                                        , lineup.alternate
                                        ]
                                            |> List.filterMap identity
                                            |> String.join ", "

                                    Nothing ->
                                        "-"
                                )
                    }

            else
                Nothing

        tableColumns =
            List.filterMap identity [ curlerColumn, teamColumn, skipColumn, positionColumn, lineupColumn ]
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_registrations") ]
        (if List.isEmpty registrations then
            El.paragraph [] [ text (translate translations "no_registrations") ]

         else
            El.indexedTable [ El.htmlAttribute (class "cio__event_registrations_table") ]
                { data = registrations
                , columns = tableColumns
                }
        )


viewSpares : Flags -> List Translation -> Event -> Element Msg
viewSpares flags translations event =
    let
        theme =
            flags.theme

        tableHeader content =
            el
                [ Font.bold
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                , Border.color theme.grey
                , El.padding 12
                ]
                (text (translate translations content))

        tableCell i content =
            el
                [ El.padding 12
                , Background.color
                    (if modBy 2 i == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                (text content)

        nameColumn =
            { header = tableHeader "curler"
            , width = El.fill
            , view = \i spare -> tableCell i spare.name
            }

        positionsColumn =
            { header = tableHeader "position"
            , width = El.fill
            , view =
                \i spare ->
                    tableCell i
                        (if List.isEmpty spare.positions then
                            "-"

                         else
                            List.map (\pos -> translate translations pos) spare.positions
                                |> String.join ", "
                        )
            }
    in
    column [ El.width El.fill, El.spacing 30, El.htmlAttribute (class "cio__event_spares") ]
        [ if List.isEmpty event.spares then
            El.paragraph [] [ text (translate translations "no_spares") ]

          else
            El.indexedTable [ El.htmlAttribute (class "cio__event_spares_table") ]
                { data = event.spares
                , columns = [ nameColumn, positionsColumn ]
                }
        , button
            [ Font.color theme.primary, El.focused [ Background.color theme.white ] ]
            { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/events/" ++ String.fromInt event.id ++ "/spares"))
            , label = text (translate translations "members_login_to_see_contact_info")
            }
        ]


viewDraws : Theme -> List Translation -> EventConfig -> Event -> Element Msg
viewDraws theme translations eventConfig event =
    let
        currentDraw =
            currentDrawForEvent event

        drawLink : Draw -> String -> DrawState -> Element Msg
        drawLink draw label drawState_ =
            if event.endScoresEnabled then
                button
                    [ case drawState_ of
                        DrawPending ->
                            Font.color theme.secondary

                        _ ->
                            Font.color theme.primary
                    , El.focused [ Background.color theme.white ]
                    ]
                    { onPress = Just (NavigateTo (drawUrl event.id draw.id))
                    , label = text label
                    }

            else
                text label

        gameLink : Game -> DrawState -> Element Msg
        gameLink game drawState_ =
            let
                teamNameForSide side =
                    teamForSide event.teams side
                        |> Maybe.map .shortName

                teamNames =
                    List.map teamNameForSide game.sides
                        |> List.filterMap identity

                hasScores =
                    List.any (\s -> s.score /= Nothing) game.sides

                teamNameForSideWithScore side =
                    teamForSide event.teams side
                        |> Maybe.map
                            (\team ->
                                case side.score of
                                    Just score ->
                                        -- Bold the score
                                        row [ El.spacing 2 ] [ text team.shortName, el [ Font.bold ] (text (String.fromInt score)) ]

                                    Nothing ->
                                        text team.shortName
                            )

                teamNamesWithScores =
                    List.map teamNameForSideWithScore game.sides
                        |> List.filterMap identity

                gameNameWithScores =
                    -- Show the teams that are playing once a game is active.
                    case game.state of
                        GameComplete ->
                            -- Show the scores for the teams that played once a game is complete.
                            if hasScores then
                                row [ El.spacing 6 ] teamNamesWithScores

                            else
                                text (String.join " v " teamNames)

                        _ ->
                            if List.length teamNames == 2 then
                                text (String.join " v " teamNames)

                            else
                                text game.name
            in
            if event.endScoresEnabled then
                button
                    [ case drawState_ of
                        DrawPending ->
                            Font.color theme.secondary

                        _ ->
                            Font.color theme.primary
                    , El.focused [ Background.color theme.white ]
                    ]
                    { onPress = Just (NavigateTo (gameUrl event.id game.id))
                    , label = gameNameWithScores
                    }

            else
                el [] gameNameWithScores

        tableColumns onActive =
            let
                drawState : Draw -> DrawState
                drawState draw =
                    if drawHasCompletedGame event draw then
                        DrawComplete

                    else if (Maybe.map .id currentDraw == Just draw.id) || drawHasActiveGame event draw then
                        -- Highlight the active (closest) draw if there are no active games, or the current
                        -- draw is it has an active game.
                        DrawActive

                    else
                        DrawPending

                hasAttendance =
                    List.any (\d -> d.attendance > 0) event.draws

                tableHeader align content =
                    let
                        contentId =
                            String.replace " " "_" content
                                |> String.toLower
                    in
                    row
                        [ Font.bold
                        , El.paddingXY 12 16
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , El.htmlAttribute (class ("cio__event_draws_header cio__event_draws_header_" ++ contentId))
                        , Border.color theme.grey
                        ]
                        [ el [ align ] (text (translate translations content))
                        ]

                tableCell align drawState_ content =
                    row
                        ([ El.paddingXY 12 16
                         , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                         , Border.color theme.grey
                         , El.htmlAttribute (class "cio__event_draws_cell")
                         ]
                            ++ (case drawState_ of
                                    DrawActive ->
                                        [ Background.color theme.greyLight
                                        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 1 }
                                        ]

                                    _ ->
                                        []
                               )
                        )
                        [ el [ align ] content ]

                labelColumn =
                    Just
                        { header = tableHeader El.alignLeft " "
                        , width = El.px 35
                        , view = \draw -> tableCell El.alignLeft (drawState draw) (drawLink draw draw.label (drawState draw))
                        }

                startsAtColumn =
                    Just
                        { header =
                            tableHeader El.alignLeft
                                (if onActive then
                                    translate translations "current_draw"

                                 else
                                    translate translations "all_draws"
                                )
                        , width = El.px 180
                        , view = \draw -> tableCell El.alignLeft (drawState draw) (drawLink draw draw.startsAt (drawState draw))
                        }

                attendanceColumn =
                    if hasAttendance then
                        Just
                            { header =
                                tableHeader El.alignLeft
                                    (if onActive then
                                        "Att"

                                     else
                                        "Att"
                                    )
                            , width = El.px 65
                            , view =
                                \draw ->
                                    tableCell El.alignLeft (drawState draw) (text (String.fromInt draw.attendance))
                            }

                    else
                        Nothing

                sheetColumn columnIndex sheetName =
                    Just
                        { header =
                            tableHeader El.centerX
                                (if sheetName == "" then
                                    " "

                                 else
                                    sheetName
                                )
                        , width = El.fill
                        , view =
                            \draw ->
                                tableCell El.centerX
                                    (drawState draw)
                                    (case List.Extra.getAt columnIndex draw.drawSheets of
                                        Just (Just gameId) ->
                                            case List.Extra.find (\g -> g.id == gameId) (gamesInEvent event) of
                                                Just game ->
                                                    gameLink game (drawState draw)

                                                Nothing ->
                                                    text "-"

                                        _ ->
                                            text "-"
                                    )
                        }

                sheetColumns =
                    List.indexedMap sheetColumn event.sheetNames
            in
            ([ labelColumn, startsAtColumn ] ++ sheetColumns ++ [ attendanceColumn ])
                |> List.filterMap identity
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_draws") ]
        (if List.isEmpty event.draws then
            El.paragraph [] [ text (translate translations "no_draws") ]

         else
            column [ El.width El.fill, El.spacing 10 ]
                [ if event.eventType == Competition then
                    case currentDraw of
                        Just draw ->
                            El.table [ El.htmlAttribute (class "cio__event_draws_table") ]
                                { data = [ draw ]
                                , columns = tableColumns True
                                }

                        Nothing ->
                            El.none

                  else
                    El.none
                , El.table [ El.paddingXY 0 10, El.htmlAttribute (class "cio__event_draws_table") ]
                    { data = event.draws
                    , columns = tableColumns False
                    }
                , case event.timeZone of
                    Just timeZone ->
                        el [ Font.italic, Font.color theme.greyDark, El.paddingXY 10 10 ] (text ("* " ++ timeZone))

                    Nothing ->
                        El.none
                , case event.note of
                    Just note ->
                        El.paragraph [ El.paddingXY 10 10 ]
                            [ el
                                [ Font.italic
                                , Font.color theme.greyDark
                                ]
                                (text note)
                            ]

                    Nothing ->
                        El.none
                ]
        )


viewTeams : Theme -> List Translation -> Event -> Element Msg
viewTeams theme translations event =
    let
        hasCoaches =
            List.any (\t -> t.coach /= Nothing) event.teams

        hasAffiliations =
            List.any (\t -> t.affiliation /= Nothing) event.teams

        hasLocations =
            List.any (\t -> t.location /= Nothing) event.teams

        tableHeader content =
            el
                [ Font.bold
                , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                , Border.color theme.grey
                , El.paddingXY 12 16
                , El.htmlAttribute (class "cio__event_teams_header")
                ]
                (text (translate translations content))

        tableCell i content =
            el
                [ El.paddingXY 12 16
                , El.htmlAttribute (class "cio__event_teams_cell")
                , Background.color
                    (if modBy 2 i == 0 then
                        theme.greyLight

                     else
                        theme.transparent
                    )
                ]
                content

        teamColumn =
            Just
                { header = tableHeader "team"
                , width = El.fill
                , view =
                    \i team ->
                        tableCell i
                            (button
                                [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (teamUrl event.id team.id))
                                , label = text team.name
                                }
                            )
                }

        coachColumn =
            if hasCoaches then
                Just
                    { header = tableHeader "coach"
                    , width = El.fill
                    , view = \i team -> tableCell i (text (Maybe.withDefault "-" team.coach))
                    }

            else
                Nothing

        affiliationColumn =
            if hasAffiliations then
                Just
                    { header = tableHeader "affiliation"
                    , width = El.fill
                    , view = \i team -> tableCell i (text (Maybe.withDefault "-" team.affiliation))
                    }

            else
                Nothing

        locationColumn =
            if hasLocations then
                Just
                    { header = tableHeader "location"
                    , width = El.fill
                    , view = \i team -> tableCell i (text (Maybe.withDefault "-" team.location))
                    }

            else
                Nothing

        tableColumns =
            List.filterMap identity [ teamColumn, coachColumn, affiliationColumn, locationColumn ]
    in
    el [ El.width El.fill, El.htmlAttribute (class "cio__event_teams") ]
        (if List.isEmpty event.teams then
            El.paragraph [] [ text (translate translations "no_teams") ]

         else
            El.indexedTable [ El.htmlAttribute (class "cio__event_teams_table") ]
                { data = event.teams
                , columns = tableColumns
                }
        )


viewStages : Theme -> El.Device -> List Translation -> Event -> Stage -> Element Msg
viewStages theme device translations event onStage =
    let
        viewStageLink stage =
            button
                [ El.paddingXY 18 10
                , El.focused [ Background.color theme.transparent ]
                , Border.color theme.grey
                , Font.color
                    (if stage.id == onStage.id then
                        theme.defaultText

                     else
                        theme.primary
                    )
                , Border.widthEach
                    (if stage.id == onStage.id then
                        { bottom = 0, left = 1, right = 1, top = 1 }

                     else
                        { bottom = 1, left = 0, right = 0, top = 0 }
                    )
                , Border.rounded 3
                , El.htmlAttribute (class "cio__event_stage_link")
                ]
                { onPress = Just (NavigateTo (stageUrl event.id stage))
                , label = text stage.name
                }

        viewRoundRobin =
            let
                teams =
                    -- Only teams that have IDs in the current stages standings.
                    List.filter (\t -> List.any (\s -> s.teamId == t.id) onStage.standings) event.teams

                teamForStanding standing =
                    List.Extra.find (\t -> t.id == standing.teamId) teams

                hasTies =
                    List.any (\standing -> standing.ties > 0) onStage.standings

                hasPoints =
                    List.any (\standing -> standing.points > 0) onStage.standings

                hasLsdCumulative =
                    List.any (\standing -> standing.lsd /= Nothing) onStage.standings

                hasLsdRank =
                    List.any (\standing -> standing.lsdRank /= Nothing) onStage.standings

                tableHeader align content =
                    el
                        [ Font.bold
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        , El.paddingXY 12 16
                        ]
                        (el [ align ] (text (translate translations content)))

                tableCell i align content =
                    el
                        [ El.paddingXY 12 16
                        , Background.color
                            (if modBy 2 i == 0 then
                                theme.greyLight

                             else
                                theme.transparent
                            )
                        ]
                        (el [ align ] content)

                teamColumn =
                    Just
                        { header = tableHeader El.alignLeft " "
                        , width = El.fill
                        , view =
                            \i standing ->
                                case teamForStanding standing of
                                    Just team ->
                                        let
                                            teamName =
                                                if device.class == El.Phone then
                                                    team.shortName

                                                else
                                                    team.name
                                        in
                                        tableCell i
                                            El.alignLeft
                                            (button
                                                [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateTo (teamUrl event.id standing.teamId))
                                                , label = text teamName
                                                }
                                            )

                                    Nothing ->
                                        text " "
                        }

                lsdCumulativeColumn =
                    if hasLsdCumulative then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "LSD"

                                     else
                                        -- "lsd_cumulative"
                                        "LSD Rank Distance"
                                    )
                            , width = El.fill
                            , view =
                                \i standing ->
                                    tableCell i
                                        El.alignRight
                                        (text
                                            (case standing.lsd of
                                                Just lsd ->
                                                    String.fromFloat lsd

                                                Nothing ->
                                                    "-"
                                            )
                                        )
                            }

                    else
                        Nothing

                lsdRankColumn =
                    if hasLsdRank then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "LSD #"

                                     else
                                        -- "lsd_standings"
                                        "LSD Standings"
                                    )
                            , width = El.fill
                            , view =
                                \i standing ->
                                    tableCell i
                                        El.alignRight
                                        (text
                                            (case standing.lsdRank of
                                                Just lsdRank ->
                                                    String.fromInt lsdRank

                                                Nothing ->
                                                    "-"
                                            )
                                        )
                            }

                    else
                        Nothing

                gamesColumn =
                    Just
                        { header =
                            tableHeader El.alignRight
                                (if device.class == El.Phone then
                                    "G"

                                 else
                                    "games"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.played))
                        }

                winsColumn =
                    Just
                        { header =
                            tableHeader El.alignRight
                                (if device.class == El.Phone then
                                    "W"

                                 else
                                    "wins"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.wins))
                        }

                lossesColumn =
                    Just
                        { header =
                            tableHeader El.alignRight
                                (if device.class == El.Phone then
                                    "L"

                                 else
                                    "losses"
                                )
                        , width = El.fill
                        , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.losses))
                        }

                tiesColumn =
                    if hasTies then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "T"

                                     else
                                        "ties"
                                    )
                            , width = El.fill
                            , view = \i standing -> tableCell i El.alignRight (text (String.fromInt standing.ties))
                            }

                    else
                        Nothing

                pointsColumn =
                    if hasPoints then
                        Just
                            { header =
                                tableHeader El.alignRight
                                    (if device.class == El.Phone then
                                        "P"

                                     else
                                        "points"
                                    )
                            , width = El.fill
                            , view = \i standing -> tableCell i El.alignRight (text (String.fromFloat standing.points))
                            }

                    else
                        Nothing

                tableColumns =
                    List.filterMap identity [ teamColumn, gamesColumn, winsColumn, lossesColumn, tiesColumn, pointsColumn, lsdCumulativeColumn, lsdRankColumn ]
            in
            El.indexedTable [ El.htmlAttribute (class "cio__event_round_robin_table") ]
                { data = onStage.standings
                , columns = tableColumns
                }

        viewBracket =
            let
                gridSize : Int
                gridSize =
                    50

                viewGroup group =
                    let
                        gamesForGroup =
                            onStage.games
                                |> List.filter (\g -> Maybe.map (\c -> c.groupId) g.coords == Just group.id)

                        colsForGames =
                            gamesForGroup
                                |> List.map (\g -> Maybe.map (\coords -> coords.col + 5) g.coords |> Maybe.withDefault 0)
                                |> List.maximum
                                |> Maybe.withDefault 10

                        rowsForGroup =
                            gamesForGroup
                                |> List.filter (\g -> Maybe.map (\coords -> coords.groupId == group.id) g.coords |> Maybe.withDefault False)
                                |> List.map (\g -> Maybe.map (\coords -> coords.row + 3) g.coords |> Maybe.withDefault 4)
                                |> List.maximum
                                |> Maybe.withDefault 4

                        viewGroupGame game =
                            case game.coords of
                                Just coords ->
                                    let
                                        viewSide position side =
                                            let
                                                label =
                                                    case side.teamId of
                                                        Just id ->
                                                            case List.Extra.find (\t -> t.id == id) event.teams of
                                                                Just team ->
                                                                    team.name

                                                                Nothing ->
                                                                    "TBD"

                                                        Nothing ->
                                                            case findSourceConnection onStage.games game.id position of
                                                                Just ( sourceGame, Winner ) ->
                                                                    "W: " ++ sourceGame.name

                                                                Just ( sourceGame, Loser ) ->
                                                                    "L: " ++ sourceGame.name

                                                                Nothing ->
                                                                    "TBD"
                                            in
                                            el
                                                ([ El.width El.fill
                                                 , El.height (El.px 25)
                                                 , El.clip
                                                 , El.paddingEach { left = 3, right = 0, top = 6, bottom = 0 }
                                                 , if position == 0 then
                                                    Border.widthEach { left = 0, right = 0, top = 0, bottom = 1 }

                                                   else
                                                    Border.width 0
                                                 , Border.color theme.grey
                                                 ]
                                                    ++ (if side.result == Just SideResultWon then
                                                            [ Font.bold ]

                                                        else
                                                            [ Font.regular ]
                                                       )
                                                )
                                                (el
                                                    [ El.width El.fill
                                                    , El.inFront
                                                        (if side.result == Just SideResultWon then
                                                            el
                                                                [ El.alignRight
                                                                , El.htmlAttribute (style "margin-right" "3px")
                                                                , El.htmlAttribute (style "margin-top" "-2px")
                                                                , El.htmlAttribute (style "padding" "3px")
                                                                , Background.color (El.rgb255 0 128 0)
                                                                , Font.color theme.white
                                                                , Font.size 10
                                                                , Border.rounded 10
                                                                ]
                                                                (text "W")

                                                         else
                                                            El.none
                                                        )
                                                    ]
                                                    (el
                                                        [ if side.result == Just SideResultWon then
                                                            El.paddingEach { left = 0, right = 20, top = 0, bottom = 0 }

                                                          else
                                                            El.paddingEach { left = 0, right = 0, top = 0, bottom = 0 }
                                                        , El.width El.fill
                                                        ]
                                                        (el
                                                            [ Border.widthEach { left = 0, right = 0, top = 0, bottom = 2 }
                                                            , Border.color
                                                                (rockColorNameToRGB
                                                                    (if side.topRock then
                                                                        event.topRock

                                                                     else
                                                                        event.botRock
                                                                    )
                                                                )
                                                            ]
                                                            (text label)
                                                        )
                                                    )
                                                )

                                        -- Only link if the game has been scheduled
                                        gameHasBeenScheduled =
                                            case drawWithGameId event.draws game.id of
                                                Just _ ->
                                                    True

                                                Nothing ->
                                                    False
                                    in
                                    column []
                                        ([ button 
                                            [ El.focused [ Background.color theme.transparent ]
                                            , El.htmlAttribute (style "cursor" 
                                                (if gameHasBeenScheduled && event.endScoresEnabled then
                                                    "pointer"
                                                 else
                                                    "default"
                                                )
                                              )
                                            ]
                                            { onPress =
                                                if gameHasBeenScheduled && event.endScoresEnabled then
                                                    Just (NavigateTo (gameUrl event.id game.id))

                                                else
                                                    Nothing
                                            , label =
                                                column
                                                    [ El.width (El.px 178)
                                                    , Background.color theme.greyLight
                                                    , Border.width 1
                                                    , Border.color theme.grey
                                                    , Font.size 12
                                                    , El.htmlAttribute (style "position" "absolute")
                                                    , El.htmlAttribute (style "left" (String.fromInt (coords.col * gridSize) ++ "px"))
                                                    , El.htmlAttribute (style "top" (String.fromInt (coords.row * gridSize) ++ "px"))
                                                    , El.htmlAttribute (class "cio__event_bracket_game")
                                                    ]
                                                    [ el
                                                        [ El.width El.fill
                                                        , El.height (El.px 20)
                                                        , El.paddingXY 4 3
                                                        , Font.color theme.white
                                                        , Background.color
                                                            (if game.state == GameActive then
                                                                theme.primary

                                                             else
                                                                theme.secondary
                                                            )
                                                        ]
                                                        (el [] (text game.name))
                                                    , column [ El.width El.fill ] (List.indexedMap viewSide game.sides)
                                                    ]
                                            }
                                         ]
                                            ++ (case game.winnerToGameId of
                                                    Just winnerGameId ->
                                                        case List.Extra.find (\g -> g.id == winnerGameId) onStage.games of
                                                            Just winnerGame ->
                                                                [ el
                                                                    [ El.htmlAttribute (style "position" "absolute")
                                                                    , El.htmlAttribute (style "left" (String.fromInt (coords.col * gridSize + 178) ++ "px"))
                                                                    , El.htmlAttribute (style "transform" "translateX(-100%)")
                                                                    , El.htmlAttribute (style "top" (String.fromInt (coords.row * gridSize + 71) ++ "px"))
                                                                    , El.height (El.px 17)
                                                                    , El.paddingXY 4 2
                                                                    , Font.size 10
                                                                    , Font.italic
                                                                    , Background.color theme.greyLight
                                                                    , Border.widthEach { left = 1, right = 1, top = 1, bottom = 1 }
                                                                    , Border.color theme.grey
                                                                    , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 4, bottomRight = 0 }
                                                                    ]
                                                                    (text ("W to " ++ winnerGame.name))
                                                                ]

                                                            Nothing ->
                                                                []

                                                    Nothing ->
                                                        []
                                               )
                                        )

                                Nothing ->
                                    El.none

                        viewSvgConnectors : Html Msg
                        viewSvgConnectors =
                            let
                                lineConnectors : List LineConnector
                                lineConnectors =
                                    let
                                        getWinningSideIndex : List Side -> Maybe Int
                                        getWinningSideIndex sides =
                                            sides
                                                |> List.indexedMap (\index side -> ( index, side ))
                                                |> List.filterMap
                                                    (\( index, side ) ->
                                                        if side.result == Just SideResultWon then
                                                            Just index

                                                        else
                                                            Nothing
                                                    )
                                                |> List.head

                                        getLosingSideIndex : List Side -> Maybe Int
                                        getLosingSideIndex sides =
                                            sides
                                                |> List.indexedMap (\index side -> ( index, side ))
                                                |> List.filterMap
                                                    (\( index, side ) ->
                                                        if side.result == Just SideResultLost then
                                                            Just index

                                                        else
                                                            Nothing
                                                    )
                                                |> List.head

                                        connectorsFromGame : Game -> List LineConnector
                                        connectorsFromGame sourceGame =
                                            let
                                                getSourceCoords : GameResult -> Maybe ( Int, Int )
                                                getSourceCoords gameResult =
                                                    case sourceGame.coords of
                                                        Just coords ->
                                                            let
                                                                yOffset =
                                                                    if sourceGame.state == GameComplete then
                                                                        case gameResult of
                                                                            Winner ->
                                                                                case getWinningSideIndex sourceGame.sides of
                                                                                    Just 0 ->
                                                                                        32

                                                                                    Just 1 ->
                                                                                        57

                                                                                    _ ->
                                                                                        45

                                                                            Loser ->
                                                                                case getLosingSideIndex sourceGame.sides of
                                                                                    Just 0 ->
                                                                                        32

                                                                                    Just 1 ->
                                                                                        57

                                                                                    _ ->
                                                                                        45

                                                                    else
                                                                        45
                                                            in
                                                            Just
                                                                ( coords.col * gridSize + 175
                                                                , coords.row * gridSize + yOffset
                                                                )

                                                        _ ->
                                                            Nothing

                                                winnerConnector =
                                                    case ( sourceGame.winnerToGameId, sourceGame.winnerToSide, getSourceCoords Winner ) of
                                                        ( Just targetGameId, Just targetSide, Just fromCoords ) ->
                                                            case List.Extra.find (\g -> g.id == targetGameId) gamesForGroup of
                                                                Just targetGame ->
                                                                    case targetGame.coords of
                                                                        Just coords ->
                                                                            Just
                                                                                (LineConnector Winner
                                                                                    fromCoords
                                                                                    ( coords.col * gridSize + 1
                                                                                    , coords.row
                                                                                        * gridSize
                                                                                        + (if targetSide == 0 then
                                                                                            32

                                                                                           else
                                                                                            57
                                                                                          )
                                                                                    )
                                                                                )

                                                                        Nothing ->
                                                                            Nothing

                                                                Nothing ->
                                                                    Nothing

                                                        _ ->
                                                            Nothing

                                                loserConnector =
                                                    case ( sourceGame.loserToGameId, sourceGame.loserToSide, getSourceCoords Loser ) of
                                                        ( Just targetGameId, Just targetSide, Just fromCoords ) ->
                                                            case List.Extra.find (\g -> g.id == targetGameId) gamesForGroup of
                                                                Just targetGame ->
                                                                    case targetGame.coords of
                                                                        Just coords ->
                                                                            Just
                                                                                (LineConnector Loser
                                                                                    fromCoords
                                                                                    ( coords.col * gridSize + 1
                                                                                    , coords.row
                                                                                        * gridSize
                                                                                        + (if targetSide == 0 then
                                                                                            32

                                                                                           else
                                                                                            57
                                                                                          )
                                                                                    )
                                                                                )

                                                                        Nothing ->
                                                                            Nothing

                                                                Nothing ->
                                                                    Nothing

                                                        _ ->
                                                            Nothing
                                            in
                                            [ winnerConnector, loserConnector ] |> List.filterMap identity
                                    in
                                    gamesForGroup
                                        |> List.map connectorsFromGame
                                        |> List.concat
                            in
                            viewSvgConnector ((colsForGames + 1) * gridSize) ((rowsForGroup - 1) * gridSize) lineConnectors
                    in
                    column
                        [ El.width El.fill, El.spacing 20, El.paddingXY 0 20 ]
                        [ el
                            [ El.width El.fill
                            , El.paddingEach { left = 10, top = 7, right = 0, bottom = 7 }
                            , Background.color theme.greyLight
                            , Font.size 20
                            , Font.medium
                            , El.htmlAttribute (class "cio__event_bracket_group")
                            ]
                            (text ("☷ " ++ group.name))
                        , column [ El.width El.fill, El.htmlAttribute (style "position" "relative") ]
                            [ el [ El.width El.fill ] (El.html viewSvgConnectors)
                            , el [ El.width El.fill, El.height (El.px 12), El.htmlAttribute (style "position" "absolute") ]
                                (column [ El.htmlAttribute (style "position" "relative") ] (List.map viewGroupGame gamesForGroup))
                            ]
                        ]
            in
            case onStage.groups of
                Just groups ->
                    column [ El.width El.fill, El.htmlAttribute (class "cio__event_bracket") ] (List.map viewGroup groups)

                Nothing ->
                    el [] (text "No groups")
    in
    column [ El.width El.fill, El.htmlAttribute (class "cio__event_stages") ]
        [ row [] (List.map viewStageLink event.stages)
        , case onStage.stageType of
            RoundRobin ->
                viewRoundRobin

            Bracket ->
                viewBracket
        ]


viewDraw : Theme -> List Translation -> EventConfig -> Event -> Draw -> Element Msg
viewDraw theme translations eventConfig event draw =
    let
        viewDrawSheet gameId =
            let
                maybeGame =
                    gamesInEvent event
                        |> List.Extra.find (\g -> Just g.id == gameId)

                sheetLabel game =
                    sheetNameForGame event game
            in
            case maybeGame of
                Just game ->
                    viewGame theme translations eventConfig event (sheetLabel game) False draw game

                Nothing ->
                    El.none
    in
    column [ El.width El.fill, El.spacing 20 ]
        [ el
            [ El.width El.fill, El.padding 16, Font.color theme.greyDark, Background.color theme.greyLight ]
            (text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt))
        , column [ El.width El.fill, El.spacing 30 ] (List.map viewDrawSheet draw.drawSheets)
        , case event.timeZone of
            Just timeZone ->
                el [ Font.italic, Font.color theme.greyDark, El.paddingXY 0 10 ] (text ("* " ++ timeZone))

            Nothing ->
                El.none
        ]


viewGame : Theme -> List Translation -> EventConfig -> Event -> String -> Bool -> Draw -> Game -> Element Msg
viewGame theme translations eventConfig event sheetLabel detailed draw game =
    let
        { scoringHilight } =
            eventConfig

        maxNumberOfEnds =
            List.map (\s -> List.length s.endScores) game.sides
                |> List.maximum
                |> Maybe.withDefault 0
                |> max event.numberOfEnds

        teamName side =
            teamForSide event.teams side
                |> Maybe.map .name
                |> Maybe.withDefault "TBD"

        wonOrTied side =
            (side.result == Just SideResultWon)
                || (side.result == Just SideResultTied)

        viewGameCaption =
            let
                label =
                    case game.state of
                        GameComplete ->
                            let
                                winner =
                                    List.Extra.find (\s -> s.result == Just SideResultWon) game.sides

                                loser =
                                    List.Extra.find (\s -> s.result /= Just SideResultWon) game.sides

                                sideResultText result =
                                    sideResultToString translations result
                            in
                            if not detailed && event.endScoresEnabled then
                                text (translate translations "final_game_statistics")

                            else if List.any (\s -> s.result == Just SideResultTied) game.sides then
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
            in
            if List.any (\s -> s.result == Just SideResultUnnecessary) game.sides then
                -- Unnecessary games are never linked.
                el [ Font.italic, Font.color theme.greyDark, El.padding 8 ] (text (translate translations "unnecessary"))

            else if detailed then
                el [ Font.italic, Font.color theme.greyDark, El.padding 8 ] label

            else
                button
                    [ Font.color theme.primary
                    , Font.italic
                    , El.padding 8
                    , El.focused [ Background.color theme.transparent ]
                    ]
                    { onPress = Just (NavigateTo gamePath), label = label }

        viewGameHilight =
            case eventConfig.scoringHilight of
                Just hilight ->
                    button
                        [ El.alignRight
                        , El.padding 8

                        -- , Font.color theme.white -- Commented out in favour of using !important to deal with host site overrides.
                        , El.htmlAttribute (attribute "style" "color: white !important;")
                        , Border.rounded 4
                        , Background.color theme.secondary
                        , El.focused [ Background.color theme.secondary ]
                        ]
                        { onPress = Just (ToggleScoringHilight hilight)
                        , label =
                            text
                                (translate translations
                                    (case hilight of
                                        HilightHammers ->
                                            "hammers"

                                        HilightStolenEnds ->
                                            "stolen_ends"

                                        HilightStolenPoints ->
                                            "stolen_points"

                                        HilightBlankEnds ->
                                            "blank_ends"

                                        Hilight1PointEnds ->
                                            "one_point_ends"

                                        Hilight2PointEnds ->
                                            "two_point_ends"

                                        Hilight3PointEnds ->
                                            "three_point_ends"

                                        Hilight4PointEnds ->
                                            "four_point_ends"

                                        Hilight5PlusPointEnds ->
                                            "five_plus_point_ends"
                                    )
                                )
                        }

                Nothing ->
                    El.none

        gamePath =
            gameUrl event.id game.id

        drawPath =
            drawUrl event.id draw.id

        viewLsd : Side -> Element Msg
        viewLsd side =
            el
                [ El.paddingXY 10 15
                , Border.widthEach { left = 0, top = 0, right = 1, bottom = 1 }
                , Border.color theme.grey
                ]
                (el [ El.centerX ]
                    (text
                        (case side.lsd of
                            Just lsd ->
                                String.fromFloat lsd

                            Nothing ->
                                " "
                        )
                    )
                )

        viewEndScore : Int -> Side -> Element Msg
        viewEndScore endNumber sideFor =
            let
                sideAgainstMaybe =
                    List.Extra.find (\s -> s.firstHammer /= sideFor.firstHammer) game.sides

                hasHammer =
                    case sideAgainstMaybe of
                        Just sideAgainst ->
                            hasHammerInEnd event.mixedDoubles sideFor sideAgainst (endNumber - 1)

                        Nothing ->
                            False

                endScoreStr =
                    case List.Extra.getAt (endNumber - 1) sideFor.endScores of
                        Just es ->
                            String.fromInt es

                        Nothing ->
                            if game.state == GameComplete then
                                "X"

                            else
                                " "

                endScoreInt =
                    List.Extra.getAt (endNumber - 1) sideFor.endScores
                        |> Maybe.withDefault 0

                stolenEnd =
                    (endScoreInt > 0) && not hasHammer

                blankEnd =
                    isBlankEnd (Just sideFor) sideAgainstMaybe (endNumber - 1)

                isHilighted =
                    (scoringHilight == Just HilightHammers && hasHammer)
                        || (scoringHilight == Just HilightStolenEnds && stolenEnd)
                        || (scoringHilight == Just HilightStolenPoints && stolenEnd)
                        || (scoringHilight == Just HilightBlankEnds && blankEnd)
                        || (scoringHilight == Just Hilight1PointEnds && (endScoreInt == 1))
                        || (scoringHilight == Just Hilight2PointEnds && (endScoreInt == 2))
                        || (scoringHilight == Just Hilight3PointEnds && (endScoreInt == 3))
                        || (scoringHilight == Just Hilight4PointEnds && (endScoreInt == 4))
                        || (scoringHilight == Just Hilight5PlusPointEnds && (endScoreInt > 4))
            in
            el
                [ El.paddingXY 10 15
                , Border.widthEach { left = 0, top = 0, right = 1, bottom = 1 }
                , Border.color theme.grey
                , Background.color
                    (if hasHammer && not isHilighted then
                        theme.greyLight

                     else if isHilighted then
                        theme.secondary

                     else
                        theme.transparent
                    )
                , Font.color
                    (if isHilighted then
                        theme.white

                     else
                        theme.defaultText
                    )
                , if isHilighted then
                    Font.bold

                  else
                    Font.regular
                ]
                (el [ El.centerX ] (text endScoreStr))

        teamColumn =
            let
                teamNameElement side =
                    el
                        [ El.paddingXY 10 12
                        , Border.widthEach { left = 1, top = 0, right = 1, bottom = 1 }
                        , Border.color theme.grey
                        , Font.color
                            (if side.firstHammer && scoringHilight == Just HilightHammers then
                                theme.primary

                             else
                                theme.defaultText
                            )
                        , if wonOrTied side then
                            Font.bold

                          else
                            Font.regular
                        ]
                        (el
                            [ El.paddingXY 0 2
                            , Border.widthEach { top = 0, right = 0, bottom = 3, left = 0 }
                            , Border.color
                                (rockColorNameToRGB
                                    (if side.topRock then
                                        event.topRock

                                     else
                                        event.botRock
                                    )
                                )
                            ]
                            (row []
                                [ case teamForSide event.teams side of
                                    Just team ->
                                        button [] { onPress = Just (NavigateTo (teamUrl event.id team.id)), label = text team.name }

                                    Nothing ->
                                        text "TDB"
                                , text
                                    (if side.firstHammer then
                                        " *"

                                     else
                                        ""
                                    )
                                ]
                            )
                        )
            in
            { header =
                row
                    [ El.paddingXY 10 15
                    , El.spacing 10
                    , Border.widthEach { left = 1, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.semiBold
                    ]
                    [ text sheetLabel
                    , if detailed then
                        El.none

                      else
                        button
                            [ Font.color theme.primary
                            , Font.size 12
                            , El.focused [ Background.color theme.transparent ]
                            ]
                            { onPress = Just (NavigateTo gamePath)
                            , label = text game.name
                            }
                    ]
            , width = El.fill
            , view = teamNameElement
            }

        lsdColumn =
            { header =
                el
                    [ El.paddingXY 10 15
                    , Border.widthEach { left = 0, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.bold
                    ]
                    (el [ El.centerX ] (text "LSD"))
            , width = El.px 60
            , view = viewLsd
            }

        endColumn endNumber =
            { header =
                el
                    [ El.paddingXY 10 15
                    , Border.widthEach { left = 0, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.bold
                    ]
                    (el [ El.centerX ] (text (String.fromInt endNumber)))
            , width = El.px 50
            , view = viewEndScore endNumber
            }

        totalColumn =
            { header =
                el
                    [ El.paddingXY 10 15
                    , Border.widthEach { left = 0, top = 1, right = 1, bottom = 2 }
                    , Border.color theme.grey
                    , Font.bold
                    ]
                    (el [ El.centerX ] (text (translate translations "total")))
            , width = El.px 64
            , view =
                \side ->
                    el
                        [ El.paddingXY 10 15
                        , Border.widthEach { left = 0, top = 0, right = 1, bottom = 1 }
                        , Border.color theme.grey
                        , if wonOrTied side then
                            Font.bold

                          else
                            Font.regular
                        ]
                        (el [ El.centerX ] (text (String.fromInt (List.sum side.endScores))))
            }

        tableColumns =
            let
                hasLsd =
                    event.lastStoneDrawEnabled
                        && List.any (\side -> side.lsd /= Nothing) game.sides
            in
            [ teamColumn ]
                ++ (if hasLsd then
                        [ lsdColumn ]

                    else
                        []
                   )
                ++ List.map endColumn (List.range 1 maxNumberOfEnds)
                ++ [ totalColumn ]
    in
    column [ El.width El.fill, El.spacing 10 ]
        -- Breadcrumb
        [ if detailed then
            el [ El.width El.fill, El.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ]
                (row
                    [ El.width El.fill
                    , El.paddingXY 10 15
                    , El.spacing 10
                    , Background.color theme.greyLight
                    , Font.color theme.greyDark
                    ]
                    [ el []
                        (button
                            [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                            { onPress = Just (NavigateTo drawPath)
                            , label = text (translate translations "draw" ++ " " ++ draw.label ++ ": " ++ draw.startsAt)
                            }
                        )
                    , el [] (text "/")
                    , el [] (text game.name)
                    , case game.videoUrl of
                        Just videoUrl ->
                            El.newTabLink
                                [ El.alignRight
                                , El.padding 8
                                , Border.rounded 4
                                , Font.color theme.white
                                , Background.color theme.secondary
                                ]
                                { url = videoUrl
                                , label = text (translate translations "video" ++ " ▶")
                                }

                        Nothing ->
                            El.none
                    ]
                )

          else
            El.none

        -- Table
        , El.table []
            { data = game.sides
            , columns = tableColumns
            }

        -- Won / Lost Caption
        , row [ El.width El.fill ]
            [ viewGameCaption
            , viewGameHilight
            ]
        , if detailed then
            column [ El.width El.fill, El.paddingXY 0 10, El.spacing 10 ]
                [ if event.shotByShotEnabled then
                    -- Shot Percentage
                    el [ El.width El.fill, El.paddingXY 0 20 ]
                        (Results.Reports.ScoringAndPercentages.viewForGame theme translations event game)

                  else
                    El.none
                , button [ El.alignRight, Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                    { onPress = Just (NavigateTo ("/events/" ++ String.fromInt event.id ++ "/reports/scoring_analysis"))
                    , label = el [ Font.size 12 ] (text (translate translations "full_report" ++ " →"))
                    }
                , el [ El.width El.fill, El.spacing 10 ]
                    (List.map (teamForSide event.teams) game.sides
                        |> List.filterMap identity
                        |> Just
                        |> Results.Reports.ScoringAnalysis.view theme translations eventConfig event
                    )
                ]

          else
            El.none
        ]


viewTeam : Theme -> List Translation -> Flags -> Event -> Team -> Element Msg
viewTeam theme translations flags event team =
    let
        viewTeamLineup : Element Msg
        viewTeamLineup =
            let
                hasPosition =
                    List.any (\c -> c.position /= Nothing || c.skip) team.lineup

                hasDelivery =
                    List.any (\c -> c.delivery /= Nothing) team.lineup

                hasPhotoUrl =
                    List.any (\c -> c.photoUrl /= Nothing) team.lineup

                hasLoggedInCurler =
                    List.any (\c -> List.member c.curlerId flags.loggedInCurlerIds) team.lineup

                viewTeamCurler : TeamCurler -> Element Msg
                viewTeamCurler curler =
                    let
                        isLoggedInCurler =
                            List.member curler.curlerId flags.loggedInCurlerIds
                    in
                    -- Card
                    column
                        [ Border.width 1
                        , Border.color theme.grey
                        , El.clip
                        , El.padding 15
                        , El.spacing 15
                        , El.width (El.px 250)
                        ]
                        [ if hasPhotoUrl then
                            el [ El.width El.fill ]
                                (case curler.photoUrl of
                                    Just photoUrl ->
                                        El.image [ El.height (El.px 150), El.centerX ]
                                            { src = photoUrl
                                            , description = curler.name
                                            }

                                    Nothing ->
                                        el [ El.height (El.px 150), El.centerX ] (El.html svgNoImage)
                                )

                          else
                            El.none
                        , column []
                            [ el [ Font.size 18, Font.semiBold ] (text curler.name)
                            , column [ El.spacing 5 ]
                                [ el
                                    [ El.paddingEach { top = 5, right = 0, bottom = 2, left = 0 }
                                    , El.onRight
                                        (if curler.position /= Nothing && curler.skip then
                                            el [ El.paddingXY 0 5, Font.size 12 ] (text (" " ++ translate translations "skip"))

                                         else
                                            El.none
                                        )
                                    ]
                                    (case curler.position of
                                        Just position ->
                                            text (positionNumberToString translations curler.position)

                                        Nothing ->
                                            if curler.skip then
                                                text (translate translations "skip")

                                            else if hasPosition then
                                                text " "

                                            else
                                                El.none
                                    )
                                , if hasDelivery then
                                    -- small
                                    el [ Font.size 12 ] (text (translate translations "delivery" ++ ": " ++ deliveryToString translations curler.delivery))

                                  else
                                    El.none
                                , if flags.showWaiversForTeams then
                                    row [ El.spacing 5 ]
                                        [ el [] (text (translate translations "waiver" ++ ":"))
                                        , if curler.waiver then
                                            el [] (text "✓")

                                          else if hasLoggedInCurler && isLoggedInCurler then
                                            button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/curlers"))
                                                , label = text (translate translations "none")
                                                }

                                          else
                                            el [] (text (translate translations "none"))
                                        ]

                                  else
                                    El.none
                                , if hasLoggedInCurler then
                                    -- small
                                    if isLoggedInCurler then
                                        button [ Font.size 12, Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                            { onPress = Just (NavigateOut (baseClubSubdomainUrl flags ++ "/curlers"))
                                            , label = text (translate translations "edit_curler")
                                            }

                                    else
                                        el [ Font.size 12 ] (text " ")

                                  else
                                    El.none
                                ]
                            ]
                        ]
            in
            El.wrappedRow [ El.spacing 20 ] (List.map viewTeamCurler team.lineup)

        viewTeamInfo : Element Msg
        viewTeamInfo =
            let
                hasTeamContactInfo =
                    [ team.contactName, team.email, team.phone ]
                        |> List.filterMap identity
                        |> List.isEmpty
                        |> not

                hasTeamOtherInfo =
                    [ team.coach, team.affiliation, team.location ]
                        |> List.filterMap identity
                        |> List.isEmpty
                        |> not
            in
            row [ El.width El.fill, El.spacing 20 ]
                [ if hasTeamContactInfo then
                    El.table [ El.width El.fill ]
                        { data =
                            [ { label = "contact_name", data = team.contactName }
                            , { label = "email", data = team.email }
                            , { label = "phone", data = team.phone }
                            ]
                        , columns =
                            [ { header = El.none
                              , width = El.px 160
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Font.semiBold
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (translate translations item.label))
                              }
                            , { header = El.none
                              , width = El.fill
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (Maybe.withDefault "-" item.data))
                              }
                            ]
                        }

                  else
                    El.none
                , if hasTeamOtherInfo then
                    El.table [ El.width El.fill ]
                        { data =
                            [ { label = "coach", data = team.coach }
                            , { label = "affiliation", data = team.affiliation }
                            , { label = "location", data = team.location }
                            ]
                        , columns =
                            [ { header = El.none
                              , width = El.px 160
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Font.semiBold
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (translate translations item.label))
                              }
                            , { header = El.none
                              , width = El.fill
                              , view =
                                    \item ->
                                        el
                                            [ El.padding 15
                                            , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
                                            , Border.color theme.grey
                                            ]
                                            (text (Maybe.withDefault "-" item.data))
                              }
                            ]
                        }

                  else
                    El.none
                ]

        viewTeamSchedule : Element Msg
        viewTeamSchedule =
            let
                hasSideForTeam game =
                    List.any (\s -> s.teamId == Just team.id) game.sides

                drawsAndGames : List { draw : Draw, game : Game }
                drawsAndGames =
                    let
                        gameForDraw draw =
                            List.filterMap identity draw.drawSheets
                                |> List.map (findGameById event)
                                |> List.filterMap identity
                                |> List.filter hasSideForTeam
                                |> List.head
                    in
                    event.draws
                        |> List.map
                            (\draw ->
                                case gameForDraw draw of
                                    Just g ->
                                        Just { draw = draw, game = g }

                                    Nothing ->
                                        Nothing
                            )
                        |> List.filterMap identity

                opponent game =
                    List.Extra.find (\s -> s.teamId /= Just team.id) game.sides
                        |> Maybe.andThen (teamForSide event.teams)

                drawAndSheetName draw game =
                    draw.label ++ " - " ++ sheetNameForGame event game

                viewTeamDrawLabel { draw, game } =
                    if event.endScoresEnabled then
                        tableCell
                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (drawUrl event.id draw.id))
                                , label = text (drawAndSheetName draw game)
                                }
                            )

                    else
                        tableCell (text (drawAndSheetName draw game))

                viewTeamDrawStartsAt { draw, game } =
                    if event.endScoresEnabled then
                        tableCell
                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                { onPress = Just (NavigateTo (drawUrl event.id draw.id))
                                , label = text draw.startsAt
                                }
                            )

                    else
                        tableCell (text draw.startsAt)

                viewTeamDrawResult { draw, game } =
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
                            if event.endScoresEnabled then
                                tableCell
                                    (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                        { onPress = Just (NavigateTo (gameUrl event.id game.id))
                                        , label = text t
                                        }
                                    )

                            else
                                tableCell (text t)

                        Nothing ->
                            tableCell (text "-")

                viewTeamDrawScore { draw, game } =
                    case opponent game of
                        Just oppo ->
                            case gameScore game (Just ( team.id, oppo.id )) of
                                Just score ->
                                    if event.endScoresEnabled then
                                        tableCell
                                            (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                                { onPress = Just (NavigateTo (gameUrl event.id game.id))
                                                , label = text score
                                                }
                                            )

                                    else
                                        tableCell (text score)

                                Nothing ->
                                    tableCell (text "-")

                        Nothing ->
                            tableCell (text "-")

                viewTeamDrawOpponent { draw, game } =
                    case opponent game of
                        Just oppo ->
                            let
                                oppoPath =
                                    teamUrl event.id oppo.id
                            in
                            tableCell
                                (button [ Font.color theme.primary, El.focused [ Background.color theme.transparent ] ]
                                    { onPress = Just (NavigateTo oppoPath)
                                    , label = text oppo.name
                                    }
                                )

                        Nothing ->
                            tableCell (text "-")

                tableHeader content =
                    el
                        [ Font.bold
                        , El.paddingXY 12 16
                        , Border.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        ]
                        (text (translate translations content))

                tableCell content =
                    el
                        [ El.paddingXY 12 16
                        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                        , Border.color theme.grey
                        ]
                        content
            in
            El.table [ El.width El.fill, El.spacingXY 0 15 ]
                { data = drawsAndGames
                , columns =
                    [ { header = tableHeader "draw"
                      , width = El.fill
                      , view = viewTeamDrawLabel
                      }
                    , { header = tableHeader "starts_at"
                      , width = El.fillPortion 2
                      , view = viewTeamDrawStartsAt
                      }
                    , { header = tableHeader "result"
                      , width = El.fill
                      , view = viewTeamDrawResult
                      }
                    , { header = tableHeader "score"
                      , width = El.fill
                      , view = viewTeamDrawScore
                      }
                    , { header = tableHeader "opponent"
                      , width = El.fillPortion 2
                      , view = viewTeamDrawOpponent
                      }
                    ]
                }
    in
    column [ El.spacing 20, El.paddingEach { top = 0, right = 0, bottom = 20, left = 0 }, El.htmlAttribute (class "cio__event_team") ]
        [ el [ Font.size 24, Font.semiBold ] (text team.name)
        , viewTeamLineup
        , viewTeamInfo
        , if not (List.isEmpty event.draws) then
            viewTeamSchedule

          else
            El.none
        ]
