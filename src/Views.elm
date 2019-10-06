module Views exposing (view)

import Helpers exposing (..)
import Html exposing (Html, button, div, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, max, min, required, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.fetchedData of
        NotAsked ->
            viewNotReady "Initializing..."

        Loading ->
            viewNotReady "Loading..."

        Failure error ->
            let
                errorMessage =
                    case error of
                        Http.BadUrl string ->
                            "Bad URL used to fetch games: " ++ string

                        Http.Timeout ->
                            "Network timeout when trying to fetch games."

                        Http.NetworkError ->
                            "Network error when trying to fetch games."

                        Http.BadStatus int ->
                            "Bad status response from server when trying to fetch games."

                        Http.BadBody string ->
                            "Bad body response from server when trying to fetch games: " ++ string
            in
            viewNotReady errorMessage

        Success data ->
            viewData model data


viewNotReady : String -> Html Msg
viewNotReady message =
    div
        [ class "mt-3" ]
        [ text message ]


viewData : Model -> Data -> Html Msg
viewData model data =
    div [ class "mt-3" ] [ text "TODO" ]
