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

        SelectGame game ->
            ( { model | selectedGame = Just game }, Cmd.none )

        CloseGame ->
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

        UpdateGamePositionScore onGamePosition newScore ->
            let
                updatedGamePosition gamePosition =
                    if gamePosition.id == onGamePosition.id then
                        let
                            updatedScore =
                                case String.toInt newScore of
                                    Just int ->
                                        if int > 99 || int < 0 then
                                            Nothing

                                        else
                                            Just int

                                    Nothing ->
                                        Nothing
                        in
                        { gamePosition | score = updatedScore }

                    else
                        gamePosition

                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | gamePositions = List.map updatedGamePosition game.gamePositions, changed = True }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )

        UpdateGamePositionResult onGamePosition newResult ->
            let
                updatedGamePosition gamePosition =
                    if gamePosition.id == onGamePosition.id then
                        { gamePosition | result = newResult }

                    else
                        case newResult of
                            Won ->
                                { gamePosition | result = Lost }

                            Lost ->
                                { gamePosition | result = Won }

                            Tied ->
                                { gamePosition | result = Tied }

                            NoResult ->
                                { gamePosition | result = NoResult }

                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | gamePositions = List.map updatedGamePosition game.gamePositions, changed = True }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )

        SaveGame ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
