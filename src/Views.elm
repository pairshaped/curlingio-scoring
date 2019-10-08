module Views exposing (view)

import Helpers exposing (..)
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, option, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, href, max, min, required, style, type_, value)
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
                , viewDraws data
                ]
            ]
        ]


viewHeader : Settings -> Html Msg
viewHeader settings =
    let
        viewSheet sheet =
            th [ class "text-center", style "min-width" "160px" ] [ text sheet ]
    in
    thead
        []
        [ tr
            []
            (th [ style "min-width" "60px" ] [ text "Draw" ]
                :: th [ style "min-width" "170px" ] [ text "Starts at" ]
                :: List.map viewSheet settings.sheets
            )
        ]


viewDraws : Data -> Html Msg
viewDraws data =
    tbody []
        (List.map (viewDraw data) data.draws)


viewDraw : Data -> Draw -> Html Msg
viewDraw data draw =
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
                    |> List.map (viewDrawSheet data.games draw.id)
               )
        )


viewDrawSheet : List Game -> Int -> Int -> Html Msg
viewDrawSheet games drawId sheet =
    td [ class "text-center" ]
        [ case findGame games drawId sheet of
            Just game ->
                viewGame game

            Nothing ->
                text ""
        ]


viewGame : Game -> Html Msg
viewGame game =
    a [ href "#", onClickPreventDefault (SelectGame game) ] [ text game.name ]


viewSelectedGame : Model -> Data -> Game -> Html Msg
viewSelectedGame model data game =
    let
        editableGameName =
            input
                [ value game.name
                , onInput UpdateGameName
                ]
                []

        nonEditableGameName =
            span [] [ text game.name ]

        gameName =
            if data.settings.nameChangeAllowed then
                editableGameName

            else
                nonEditableGameName
    in
    div
        [ style "min-height" "100%"
        , style "min-height" "100vh"
        , style "display" "flex"
        , style "align-items" "center"
        , style "background-color" "#444"
        ]
        [ div
            [ class "container" ]
            [ div
                [ class "row justify-content-center align-items-center" ]
                [ div
                    [ class "col-12 col-md-10 col-lg-8 col-xl-6" ]
                    [ div
                        [ class "card" ]
                        [ div
                            [ class "card-body" ]
                            (List.append
                                [ h3
                                    [ class "card-title" ]
                                    [ gameName ]
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
                            [ button
                                [ class "btn btn-secondary"
                                , onClick CloseGame
                                ]
                                [ text "Cancel" ]
                            , button
                                [ class "btn btn-primary"
                                , disabled (not game.changed)
                                , onClick SaveGame
                                ]
                                [ text "Save" ]
                            ]
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
            [ class "d-flex" ]
            [ input
                [ class "form-control mr-3"
                , style "width" "60px"
                , type_ "number"
                , min "0"
                , max "99"
                , value
                    (case gamePosition.score of
                        Just val ->
                            String.fromInt val

                        Nothing ->
                            ""
                    )
                , onInput (UpdateGamePositionScore gamePosition)
                ]
                []
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
                    , onClick (UpdateGamePositionResult gamePosition Lost)
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
                    , onClick (UpdateGamePositionResult gamePosition Tied)
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
                , button
                    [ type_ "button"
                    , onClick (UpdateGamePositionResult gamePosition NoResult)
                    , class
                        ("btn btn-info"
                            ++ (case gamePosition.result of
                                    NoResult ->
                                        " active"

                                    _ ->
                                        ""
                               )
                        )
                    ]
                    [ text "TBD" ]
                ]
            ]
        ]
