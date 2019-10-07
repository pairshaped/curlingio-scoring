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
        [ class "m-3" ]
        [ text message ]


viewData : Model -> Data -> Html Msg
viewData model data =
    div [ class "m-3" ]
        [ div
            [ class "table-responsive" ]
            [ table
                [ class "table" ]
                [ viewHeader data.settings
                , viewDraws model data
                ]
            ]
        ]


viewHeader : Settings -> Html Msg
viewHeader settings =
    let
        viewSheet sheet =
            th [ class "text-center" ] [ text sheet ]
    in
    thead
        []
        [ tr
            []
            (th [] [ text "Draw" ]
                :: th [] [ text "Starts at" ]
                :: List.map viewSheet settings.sheets
            )
        ]


viewDraws : Model -> Data -> Html Msg
viewDraws model data =
    tbody []
        (List.map (viewDraw model data) data.draws)


viewDraw : Model -> Data -> Draw -> Html Msg
viewDraw model data draw =
    let
        currentDrawClass =
            if data.settings.currentDrawId == draw.id then
                "table-active"

            else
                ""
    in
    tr [ class "m-2", class currentDrawClass ]
        (td [] [ text draw.label ]
            :: td [] [ text draw.startsAt ]
            :: (List.range 1 (List.length data.settings.sheets)
                    |> List.map (viewDrawSheet model data.games draw.id)
               )
        )


viewDrawSheet : Model -> List Game -> Int -> Int -> Html Msg
viewDrawSheet model games drawId sheet =
    td [ class "text-center" ]
        [ case findGame games drawId sheet of
            Just game ->
                text game.name

            Nothing ->
                text ""
        ]
