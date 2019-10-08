module Views exposing (view)

import Helpers exposing (..)
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, option, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, max, min, required, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import RemoteData exposing (RemoteData(..))
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.data of
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
            case model.selectedGame of
                Just game ->
                    viewSelectedGame model data game

                Nothing ->
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
                viewGame game

            Nothing ->
                text ""
        ]


viewGame : Game -> Html Msg
viewGame game =
    a [ href "#", onClickPreventDefault (SelectedGame game) ] [ text game.name ]


viewSelectedGame : Model -> Data -> Game -> Html Msg
viewSelectedGame model data game =
    div
        [ class "container mt-3" ]
        [ div
            [ class "row justify-content-center" ]
            [ div
                [ class "col-12 col-md-10 col-lg-8 col-xl-6" ]
                [ div
                    [ class "card" ]
                    [ div
                        [ class "card-body" ]
                        (List.append
                            [ h3
                                [ class "card-title" ]
                                [ text game.name ]
                            , h6
                                [ class "card-subtitle mb-2 text-muted" ]
                                [ text
                                    (case findDraw data.draws game.drawId of
                                        Just draw ->
                                            "Draw " ++ draw.label ++ " - " ++ draw.startsAt

                                        Nothing ->
                                            "Unknown Draw"
                                    )
                                ]
                            , hr [] []
                            ]
                            (List.map viewGamePosition game.gamePositions)
                        )
                    , div
                        [ class "d-flex justify-content-between card-footer" ]
                        [ a
                            [ href "#", class "btn btn-secondary", onClickPreventDefault ClosedGame ]
                            [ text "Cancel" ]
                        , a
                            [ href "#", class "btn btn-primary" ]
                            [ text "Save" ]
                        ]
                    ]
                ]
            ]
        ]


viewGamePosition : GamePosition -> Html Msg
viewGamePosition gamePosition =
    p
        []
        [ h5
            [ class "card-text" ]
            [ text gamePosition.teamName ]
        , div
            [ class "btn-group btn-group-sm" ]
            [ button
                [ type_ "button"
                , onClick (UpdateGamePositionResult gamePosition Won)
                , class
                    ("btn btn-info"
                        ++ (case gamePosition.result of
                                Won ->
                                    " active"

                                _ ->
                                    ""
                           )
                    )
                ]
                [ text "Won" ]
            , button
                [ type_ "button"
                , class
                    ("btn btn-info"
                        ++ (case gamePosition.result of
                                Lost ->
                                    " active"

                                _ ->
                                    ""
                           )
                    )
                ]
                [ text "Lost" ]
            , button
                [ type_ "button"
                , class
                    ("btn btn-info"
                        ++ (case gamePosition.result of
                                Tied ->
                                    " active"

                                _ ->
                                    ""
                           )
                    )
                ]
                [ text "Tied" ]
            ]
        ]
