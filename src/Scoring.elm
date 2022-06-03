module Scoring exposing (..)

import Browser
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, option, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, href, id, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http



-- MODEL


type alias Model =
    { flags : Flags
    , data : WebData Data
    , savedGame : WebData Game
    , selectedGame : Maybe Game
    , fullScreen : Bool
    }


type alias Flags =
    { fetchUrl : String
    , patchUrl : String
    }


type alias Data =
    { settings : Settings
    , draws : List Draw
    , games : List Game
    }


type alias Settings =
    { eventName : String
    , sheets : List String
    , currentDrawId : Int
    , nameChangeAllowed : Bool
    }


type alias Draw =
    { id : Int
    , label : String
    , startsAt : String
    }


type alias Game =
    { id : String
    , drawId : Int
    , sheet : Int
    , name : String
    , gamePositions : List GamePosition
    , changed : Bool
    , nameTaken : Bool
    }


type alias GamePosition =
    { id : Int
    , teamName : String
    , score : Maybe Int
    , result : GamePositionResult
    }


type GamePositionResult
    = Won
    | Lost
    | Conceded
    | Forfeited
    | Tied
    | NoResult



-- DECODERS


dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> required "settings" settingsDecoder
        |> required "draws" (list drawDecoder)
        |> required "games" (list gameDecoder)


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> required "event_name" string
        |> required "sheets" (list string)
        |> required "current_draw_id" int
        |> required "name_change_allowed" bool


drawDecoder : Decoder Draw
drawDecoder =
    Decode.succeed Draw
        |> required "id" int
        |> required "label" string
        |> required "starts_at" string


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" string
        |> required "draw_id" int
        |> required "sheet" int
        |> required "name" string
        |> required "game_positions" (list gamePositionDecoder)
        |> hardcoded False
        |> hardcoded False


gamePositionDecoder : Decoder GamePosition
gamePositionDecoder =
    Decode.succeed GamePosition
        |> required "id" int
        |> required "team_name" string
        |> optional "score" (nullable int) Nothing
        |> optional "result" gamePositionResultDecoder NoResult


gamePositionResultDecoder : Decoder GamePositionResult
gamePositionResultDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "won" ->
                        Decode.succeed Won

                    "lost" ->
                        Decode.succeed Lost

                    "forfeited" ->
                        Decode.succeed Forfeited

                    "conceded" ->
                        Decode.succeed Conceded

                    "tied" ->
                        Decode.succeed Tied

                    _ ->
                        Decode.succeed NoResult
            )



-- ENCODERS


encodeGame : Game -> Encode.Value
encodeGame game =
    Encode.object
        [ ( "id", Encode.string game.id )
        , ( "draw_id", Encode.int game.drawId )
        , ( "sheet", Encode.int game.sheet )
        , ( "name", Encode.string game.name )
        , ( "game_positions", Encode.list encodeGamePosition game.gamePositions )
        ]


encodeGamePosition : GamePosition -> Encode.Value
encodeGamePosition gamePosition =
    Encode.object
        [ ( "id", Encode.int gamePosition.id )
        , ( "team_name", Encode.string gamePosition.teamName )
        , ( "score", maybe Encode.int gamePosition.score )
        , ( "result", encodeGamePositionResult gamePosition.result )
        ]


encodeGamePositionResult : GamePositionResult -> Encode.Value
encodeGamePositionResult gamePositionResult =
    case gamePositionResult of
        Won ->
            Encode.string "won"

        Lost ->
            Encode.string "lost"

        Forfeited ->
            Encode.string "forfeited"

        Conceded ->
            Encode.string "conceded"

        Tied ->
            Encode.string "tied"

        NoResult ->
            Encode.null



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model flags NotAsked NotAsked Nothing False, getData flags.fetchUrl )


getData : String -> Cmd Msg
getData url =
    RemoteData.Http.get url GotData dataDecoder


patchGame : String -> Game -> Cmd Msg
patchGame url game =
    RemoteData.Http.patch (url ++ "/" ++ game.id) PatchedGame gameDecoder (encodeGame game)


findGame : List Game -> Int -> Int -> Maybe Game
findGame games drawId sheet =
    List.Extra.find (\game -> game.drawId == drawId && game.sheet == sheet) games


findGameByName : List Game -> String -> String -> Maybe Game
findGameByName games excludeId name =
    List.Extra.find (\game -> game.id /= excludeId && game.name == name) games


findDraw : List Draw -> Int -> Maybe Draw
findDraw draws drawId =
    List.Extra.find (\draw -> draw.id == drawId) draws



-- UPDATE


type Msg
    = GotData (WebData Data)
    | ReloadData
    | ToggleFullScreen
    | PatchedGame (WebData Game)
    | SelectGame Game
    | CloseGame
    | UpdateGameName String
    | ValidateGameName
    | UpdateGamePositionScore GamePosition String
    | UpdateGamePositionResult GamePosition GamePositionResult
    | SaveGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData data ->
            let
                selectedGame =
                    case data of
                        Success decodedData ->
                            -- List.head decodedData.games
                            Nothing

                        _ ->
                            Nothing
            in
            ( { model | data = data, selectedGame = selectedGame }, Cmd.none )

        ReloadData ->
            ( { model | data = Loading, savedGame = NotAsked, selectedGame = Nothing }, getData model.flags.fetchUrl )

        SaveGame ->
            let
                sendPatch =
                    case model.selectedGame of
                        Just game ->
                            patchGame model.flags.patchUrl game

                        Nothing ->
                            Cmd.none
            in
            ( { model | savedGame = Loading }, sendPatch )

        PatchedGame gameResponse ->
            let
                selectedGame =
                    case gameResponse of
                        Success decodedGame ->
                            Nothing

                        _ ->
                            model.selectedGame

                savedGame =
                    case gameResponse of
                        Success decodedGame ->
                            NotAsked

                        _ ->
                            gameResponse

                updatedGame gameFromSave game =
                    if game.id == gameFromSave.id then
                        gameFromSave

                    else
                        game

                updatedData =
                    case gameResponse of
                        Success decodedGame ->
                            case model.data of
                                Success decodedData ->
                                    Success { decodedData | games = List.map (updatedGame decodedGame) decodedData.games }

                                _ ->
                                    model.data

                        _ ->
                            model.data
            in
            ( { model | selectedGame = selectedGame, savedGame = savedGame, data = updatedData }, Cmd.none )

        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        SelectGame game ->
            ( { model | selectedGame = Just game, savedGame = NotAsked }, Cmd.none )

        CloseGame ->
            ( { model | selectedGame = Nothing, savedGame = NotAsked }, Cmd.none )

        UpdateGameName newName ->
            let
                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | name = newName, changed = True, nameTaken = False }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )

        ValidateGameName ->
            let
                games =
                    case model.data of
                        Success decodedData ->
                            decodedData.games

                        _ ->
                            []

                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            case findGameByName games game.id game.name of
                                Just existingGame ->
                                    Just { game | nameTaken = True }

                                Nothing ->
                                    Just { game | nameTaken = False }

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

                            Forfeited ->
                                { gamePosition | result = Won }

                            Conceded ->
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



-- VIEWS


view : Model -> Html Msg
view model =
    div
        (List.append
            [ id "scoring" ]
            (if model.fullScreen then
                [ style "width" "100%"
                , style "height" "100%"
                , style "position" "fixed"
                , style "top" "0"
                , style "left" "0"
                , style "z-index" "100"
                , style "overflow-y" "auto"
                , style "backgroup-color" "#fff"
                ]

             else
                []
            )
        )
        [ case model.data of
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
                viewFetchError errorMessage

            Success data ->
                case model.selectedGame of
                    Just game ->
                        viewSelectedGame model data game

                    Nothing ->
                        viewData model data
        ]


viewNotReady : String -> Html Msg
viewNotReady message =
    p [ class "p-3" ] [ text message ]


viewFetchError : String -> Html Msg
viewFetchError message =
    div
        [ class "p-3" ]
        [ p [] [ text message ]
        , button [ class "btn btn-primary", onClick ReloadData ] [ text "Reload" ]
        ]


viewData : Model -> Data -> Html Msg
viewData model data =
    div
        [ class "p-3" ]
        [ div
            [ class "d-flex justify-content-between mb-2" ]
            [ h5 [] [ text data.settings.eventName ]
            , div [ class "text-right" ]
                [ button [ class "btn btn-sm btn-primary mr-2", onClick ReloadData ] [ text "Reload" ]
                , button [ class "btn btn-sm btn-secondary", onClick ToggleFullScreen ]
                    [ text
                        (if model.fullScreen then
                            "Exit"

                         else
                            "Full Screen"
                        )
                    ]
                ]
            ]
        , div
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
    let
        completed =
            List.any (\gp -> gp.result /= NoResult) game.gamePositions

        inProgress =
            List.any (\gp -> gp.score /= Nothing && gp.result == NoResult) game.gamePositions

        resultClass =
            if completed then
                "text-secondary"

            else if inProgress then
                "font-weight-bold"

            else
                ""
    in
    a [ href "#", class resultClass, onClickPreventDefault (SelectGame game) ] [ text game.name ]


viewSelectedGame : Model -> Data -> Game -> Html Msg
viewSelectedGame model data game =
    let
        editableGameName =
            input
                [ value game.name
                , onInput UpdateGameName
                , onBlur ValidateGameName
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
        , style "background-color" "#444"
        , class "pt-5"
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
                                , viewValidationError game
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
                                , viewGameSaveError model
                                ]
                                (List.map viewGamePosition game.gamePositions)
                            )
                        , div
                            [ class "d-flex justify-content-between card-footer" ]
                            [ button
                                [ class "btn btn-secondary"
                                , disabled (model.savedGame == Loading)
                                , onClick CloseGame
                                ]
                                [ text "Cancel" ]
                            , button
                                [ class "btn btn-primary"
                                , disabled (not game.changed || game.nameTaken || model.savedGame == Loading)
                                , onClick SaveGame
                                ]
                                [ text "Save" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewGameSaveError : Model -> Html Msg
viewGameSaveError model =
    case model.savedGame of
        Failure error ->
            let
                errorMessage =
                    case error of
                        Http.BadUrl string ->
                            "Bad URL used to save game: " ++ string

                        Http.Timeout ->
                            "Network timeout when trying to save the game."

                        Http.NetworkError ->
                            "Network error when trying to save the game."

                        Http.BadStatus int ->
                            "Error when trying to save the game."

                        Http.BadBody string ->
                            "Error when trying to save the game."
            in
            div [ class "alert alert-danger" ] [ text errorMessage ]

        _ ->
            span [ style "display" "none" ] []


viewValidationError : Game -> Html Msg
viewValidationError game =
    if game.nameTaken then
        div [ class "text-danger mt-n2 mb-3" ] [ text "Game name has already been taken." ]

    else
        span [ style "display" "none" ] []


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
                , style "margin-top" "-1px"
                , type_ "number"
                , Html.Attributes.min "0"
                , Html.Attributes.max "99"
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
                [ class "btn-group btn-group-sm scoring-result-button-group flex-wrap justify-content-left" ]
                [ button
                    [ type_ "button"
                    , onClick (UpdateGamePositionResult gamePosition Won)
                    , style "margin-top" "-1px"
                    , style "margin-left" "-1px"
                    , class
                        ("btn btn-outline-success"
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
                    , style "margin-top" "-1px"
                    , class
                        ("btn btn-outline-danger"
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
                    , onClick (UpdateGamePositionResult gamePosition Conceded)
                    , style "margin-top" "-1px"
                    , class
                        ("btn btn-outline-danger"
                            ++ (case gamePosition.result of
                                    Conceded ->
                                        " active"

                                    _ ->
                                        ""
                               )
                        )
                    ]
                    [ span [ class "d-none d-md-inline" ] [ text "Conceded" ]
                    , span [ class "d-md-none" ] [ text "Con" ]
                    ]
                , button
                    [ type_ "button"
                    , onClick (UpdateGamePositionResult gamePosition Forfeited)
                    , style "margin-top" "-1px"
                    , class
                        ("btn btn-outline-danger"
                            ++ (case gamePosition.result of
                                    Forfeited ->
                                        " active"

                                    _ ->
                                        ""
                               )
                        )
                    ]
                    [ span [ class "d-none d-md-inline" ] [ text "Forfeited" ]
                    , span [ class "d-md-none" ] [ text "For" ]
                    ]
                , button
                    [ type_ "button"
                    , onClick (UpdateGamePositionResult gamePosition Tied)
                    , style "margin-top" "-1px"
                    , class
                        ("btn btn-outline-info"
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
                    , style "margin-top" "-1px"
                    , class
                        ("btn btn-outline-secondary"
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
