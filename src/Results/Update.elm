module Results.Update exposing (subscriptions, update)

import Browser.Events
import Browser.Navigation as Navigation
import Element
import RemoteData exposing (RemoteData(..))
import Results.Helpers exposing (..)
import Results.Rest exposing (..)
import Results.Types exposing (..)
import Time



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitDevice { viewport } ->
            let
                updatedFlags flags =
                    { flags
                        | device =
                            Element.classifyDevice { width = round viewport.width, height = round viewport.height }
                    }
            in
            ( { model | flags = updatedFlags model.flags }, Cmd.none )

        Tick _ ->
            case model.event of
                Success event ->
                    ( model, reloadEvent model.flags event.id )

                _ ->
                    ( model, Cmd.none )

        SetDevice width height ->
            let
                updatedFlags flags =
                    let
                        device =
                            Element.classifyDevice { width = width, height = height }
                    in
                    { flags | device = device }
            in
            ( { model | flags = updatedFlags model.flags }, Cmd.none )

        NavigateTo newHash ->
            ( model, navigateTo newHash )

        ToggleFullScreen ->
            let
                updatedFlags flags =
                    { flags | fullScreen = not flags.fullScreen }
            in
            ( { model | flags = updatedFlags model.flags }, Cmd.none )

        HashChanged reload hash ->
            let
                ( translations, translationsCmd ) =
                    case model.translations of
                        Success _ ->
                            ( model.translations, Cmd.none )

                        _ ->
                            ( Loading, getTranslations model.flags )

                ( items, itemsCmd ) =
                    getItemsMaybe model hash reload

                ( event, eventCmd ) =
                    getEventMaybe model hash reload

                ( product, productCmd ) =
                    getProductMaybe model hash

                updatedEventConfig eventConfig =
                    { eventConfig
                        | drawSelectionOpen = False
                        , teamSelectionOpen = False
                        , drawSelected = Nothing
                    }
            in
            ( { model
                | hash = hash
                , translations = translations
                , items = items
                , product = product
                , event = event
                , eventConfig = updatedEventConfig model.eventConfig
              }
            , Cmd.batch
                [ translationsCmd
                , itemsCmd
                , productCmd
                , eventCmd
                ]
            )

        Reload ->
            update (HashChanged True model.hash) model

        GotTranslations response ->
            ( { model | translations = response, errorMsg = Nothing }, Cmd.none )

        GotItems response ->
            ( { model | items = response, errorMsg = Nothing }, Cmd.none )

        IncrementPageBy num ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | page = itemFilter.page + num }

                updatedModel =
                    { model | itemFilter = updatedItemFilter model.itemFilter }
            in
            ( updatedModel, Cmd.none )

        UpdateSearch val ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | page = 1, search = String.toLower val }
            in
            ( { model | itemFilter = updatedItemFilter model.itemFilter }, Cmd.none )

        ToggleSeasonSearch ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | seasonSearchOpen = not itemFilter.seasonSearchOpen }
            in
            ( { model | itemFilter = updatedItemFilter model.itemFilter }, Cmd.none )

        UpdateSeasonDelta seasonDelta ->
            let
                updatedItemFilter itemFilter =
                    { itemFilter | page = 1, seasonDelta = seasonDelta }

                updatedModel =
                    { model | itemFilter = updatedItemFilter model.itemFilter }
            in
            ( updatedModel, getItems model.flags updatedModel.itemFilter )

        NavigateOut url ->
            ( model, Navigation.load url )

        GotEvent response ->
            ( { model | event = response }
            , Cmd.none
            )

        ReloadedEvent response ->
            -- The different between this and GetEvent is that we only update the event on the model
            -- when the request succeeds.
            ( case response of
                Success _ ->
                    { model | event = response }

                _ ->
                    model
            , Cmd.none
            )

        GotProduct response ->
            ( { model | product = response }
            , Cmd.none
            )

        ToggleScoringHilight scoringHilight ->
            let
                updatedEventConfig eventConfig =
                    { eventConfig
                        | scoringHilight =
                            if eventConfig.scoringHilight == Just scoringHilight then
                                Nothing

                            else
                                Just scoringHilight
                    }
            in
            ( { model | eventConfig = updatedEventConfig model.eventConfig }
            , Cmd.none
            )

        ToggleDrawSelection ->
            let
                updatedEventConfig eventConfig =
                    { eventConfig
                        | drawSelectionOpen = not eventConfig.drawSelectionOpen
                        , teamSelectionOpen = False
                    }
            in
            ( { model | eventConfig = updatedEventConfig model.eventConfig }
            , Cmd.none
            )

        UpdateDrawSelected drawId ->
            let
                updatedEventConfig eventConfig =
                    { eventConfig | drawSelected = Just drawId, teamSelected = Nothing }
            in
            ( { model | eventConfig = updatedEventConfig model.eventConfig }
            , Cmd.none
            )

        ToggleTeamSelection ->
            let
                updatedEventConfig eventConfig =
                    { eventConfig
                        | teamSelectionOpen = not eventConfig.teamSelectionOpen
                        , drawSelectionOpen = False
                    }
            in
            ( { model | eventConfig = updatedEventConfig model.eventConfig }
            , Cmd.none
            )

        UpdateTeamSelected teamId ->
            let
                updatedEventConfig eventConfig =
                    { eventConfig | teamSelected = Just teamId }
            in
            ( { model | eventConfig = updatedEventConfig model.eventConfig }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        reloadEnabled =
            -- Only if we're on an event, the event is active, end scores are enabled, and we're on a route / screen that is meaningfull to reload.
            case toRoute model.flags.defaultEventSection model.hash of
                EventRoute _ nestedRoute ->
                    case model.event of
                        Success event_ ->
                            (event_.state == EventStateActive)
                                && event_.endScoresEnabled
                                && not (List.member nestedRoute [ DetailsRoute, RegistrationsRoute, SparesRoute, TeamsRoute ])

                        _ ->
                            False

                _ ->
                    False
    in
    Sub.batch
        [ hashChangeReceiver (HashChanged False)
        , Browser.Events.onResize (\values -> SetDevice values)
        , if reloadEnabled then
            Time.every 30000 Tick

          else
            Sub.none
        ]
