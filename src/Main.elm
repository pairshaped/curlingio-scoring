module Main exposing (..)

import Browser
import Helpers exposing (..)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Types exposing (..)
import Views exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags NotAsked NotAsked Nothing, getData flags.url )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData data ->
            let
                selectedGame =
                    case data of
                        Success decodedData ->
                            List.head decodedData.games

                        _ ->
                            Nothing
            in
            ( { model | data = data, selectedGame = selectedGame }, Cmd.none )

        PatchedGame savedGame ->
            ( { model | savedGame = savedGame }, Cmd.none )

        SelectedGame game ->
            ( { model | selectedGame = Just game }, Cmd.none )

        ClosedGame ->
            ( { model | selectedGame = Nothing }, Cmd.none )

        UpdateGameName newName ->
            let
                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | name = newName }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )

        UpdateGamePositionScore gamePosition newScore ->
            ( model, Cmd.none )

        UpdateGamePositionResult gamePosition newResult ->
            ( model, Cmd.none )

        SaveGame id ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
