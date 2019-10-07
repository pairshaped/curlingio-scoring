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
            ( { model | data = data }, Cmd.none )

        PatchedGame savedGame ->
            ( { model | savedGame = savedGame }, Cmd.none )

        SelectedGame game ->
            ( { model | selectedGame = Just game }, Cmd.none )

        UpdateGameName onGame newName ->
            let
                updatedGame game =
                    if game.id == onGame.id then
                        { game | name = newName }

                    else
                        game

                updatedGames games =
                    List.map updatedGame games

                updatedData =
                    case model.data of
                        Success decodedData ->
                            Success { decodedData | games = updatedGames decodedData.games }

                        _ ->
                            model.data
            in
            ( { model | data = updatedData }, Cmd.none )

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
