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
    ( Model flags NotAsked NotAsked, getData flags.url )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData fetchedData ->
            ( { model | fetchedData = fetchedData }, Cmd.none )

        PatchedGame savedGame ->
            ( { model | savedGame = savedGame }, Cmd.none )

        UpdateGameName id newName ->
            ( model, Cmd.none )

        UpdateGamePositionScore id newScore ->
            ( model, Cmd.none )

        UpdateGamePositionResult id newResult ->
            ( model, Cmd.none )

        SaveGame id ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
