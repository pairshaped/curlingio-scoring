module Scoring exposing (..)

import Browser
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, option, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, href, id, style, tabindex, type_, value)
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
    , settings : WebData Settings
    , draws : WebData (List Draw)
    , games : WebData (List Game)
    , savedGame : WebData Game
    , selectedGame : Maybe Game
    , fullScreen : Bool
    }


type alias Flags =
    { baseUrl : String
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
    , endScoresEnabled : Bool
    , shotByShotEnabled : Bool
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
    , sides : List Side
    , changed : Bool
    }


type alias Side =
    { id : Int
    , teamName : String
    , firstHammer : Bool
    , score : Maybe Int
    , result : SideResult
    , endScores : List (Maybe Int)
    }


type SideResult
    = Won
    | Lost
    | Conceded
    | Forfeited
    | Tied
    | NoResult



-- DECODERS


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> required "event_name" string
        |> required "sheets" (list string)
        |> required "current_draw_id" int
        |> required "end_scores_enabled" bool
        |> required "shot_by_shot_enabled" bool


drawsDecoder : Decoder (List Draw)
drawsDecoder =
    list drawDecoder


drawDecoder : Decoder Draw
drawDecoder =
    Decode.succeed Draw
        |> required "id" int
        |> required "label" string
        |> required "starts_at" string


gamesDecoder : Decoder (List Game)
gamesDecoder =
    list gameDecoder


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" string
        |> required "draw_id" int
        |> required "sheet" int
        |> required "name" string
        |> required "game_positions" (list sideDecoder)
        |> hardcoded False


sideDecoder : Decoder Side
sideDecoder =
    Decode.succeed Side
        |> required "id" int
        |> required "team_name" string
        |> required "first_hammer" bool
        |> optional "score" (nullable int) Nothing
        |> optional "result" sideResultDecoder NoResult
        |> optional "end_scores" (list (nullable int)) []


sideResultDecoder : Decoder SideResult
sideResultDecoder =
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
        [ ( "game_positions", Encode.list encodeSide game.sides )
        ]


encodeSide : Side -> Encode.Value
encodeSide side =
    Encode.object
        [ ( "id", Encode.int side.id )
        , ( "team_name", Encode.string side.teamName )
        , ( "first_hammer", Encode.bool side.firstHammer )
        , ( "score", maybe Encode.int side.score )
        , ( "result", encodeSideResult side.result )
        , ( "end_scores"
          , Encode.list
                (\score ->
                    case score of
                        Just s ->
                            Encode.int s

                        Nothing ->
                            Encode.null
                )
                side.endScores
          )
        ]


encodeSideResult : SideResult -> Encode.Value
encodeSideResult sideResult =
    case sideResult of
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
    ( Model flags NotAsked NotAsked NotAsked NotAsked Nothing False
    , Cmd.batch
        [ getSettings flags.baseUrl
        , getDraws flags.baseUrl
        , getGames flags.baseUrl
        ]
    )


getSettings : String -> Cmd Msg
getSettings baseUrl =
    let
        url =
            baseUrl ++ "/settings"
    in
    RemoteData.Http.get url GotSettings settingsDecoder


getDraws : String -> Cmd Msg
getDraws baseUrl =
    let
        url =
            baseUrl ++ "/draws"
    in
    RemoteData.Http.get url GotDraws drawsDecoder


getGames : String -> Cmd Msg
getGames baseUrl =
    let
        url =
            baseUrl ++ "/games"
    in
    RemoteData.Http.get url GotGames gamesDecoder


patchGame : String -> Game -> Cmd Msg
patchGame baseUrl game =
    let
        url =
            baseUrl ++ "/games/" ++ game.id
    in
    RemoteData.Http.patch url PatchedGame gameDecoder (encodeGame game)


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
    = GotSettings (WebData Settings)
    | GotDraws (WebData (List Draw))
    | GotGames (WebData (List Game))
    | ReloadData
    | ToggleFullScreen
    | PatchedGame (WebData Game)
    | SelectGame Game
    | CloseGame
    | UpdateSideScore Side String
    | UpdateSideResult Side SideResult
    | UpdateSideEndScore Int Int String
    | SaveGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSettings response ->
            ( { model | settings = response }, Cmd.none )

        GotDraws response ->
            ( { model | draws = response }, Cmd.none )

        GotGames response ->
            ( { model | games = response }, Cmd.none )

        ReloadData ->
            ( { model
                | settings = Loading
                , draws = Loading
                , games = Loading
                , savedGame = NotAsked
                , selectedGame = Nothing
              }
            , Cmd.batch
                [ getSettings model.flags.baseUrl
                , getDraws model.flags.baseUrl
                , getGames model.flags.baseUrl
                ]
            )

        SaveGame ->
            let
                sendPatch =
                    case model.selectedGame of
                        Just game ->
                            patchGame model.flags.baseUrl game

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

                updatedGames games =
                    case gameResponse of
                        Success decodedGame ->
                            List.map (updatedGame decodedGame) games

                        _ ->
                            games
            in
            ( { model
                | selectedGame = selectedGame
                , savedGame = savedGame
                , games = RemoteData.map updatedGames model.games
              }
            , Cmd.none
            )

        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        SelectGame game ->
            ( { model | selectedGame = Just game, savedGame = NotAsked }, Cmd.none )

        CloseGame ->
            ( { model | selectedGame = Nothing, savedGame = NotAsked }, Cmd.none )

        UpdateSideScore sideIndex newScore ->
            let
                updatedSide side =
                    if side.id == sideIndex.id then
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
                        { side | score = updatedScore }

                    else
                        side

                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | sides = List.map updatedSide game.sides, changed = True }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )

        UpdateSideResult sideIndex newResult ->
            let
                updatedSide side =
                    if side.id == sideIndex.id then
                        { side | result = newResult }

                    else
                        case newResult of
                            Won ->
                                { side | result = Lost }

                            Lost ->
                                { side | result = Won }

                            Forfeited ->
                                { side | result = Won }

                            Conceded ->
                                { side | result = Won }

                            Tied ->
                                { side | result = Tied }

                            NoResult ->
                                { side | result = NoResult }

                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | sides = List.map updatedSide game.sides, changed = True }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )

        UpdateSideEndScore sideIndex endIndex newScoreStr ->
            let
                newScoreStrFixed =
                    if String.length newScoreStr > 1 then
                        String.right 1 newScoreStr

                    else
                        newScoreStr

                newScore =
                    case String.toInt newScoreStrFixed of
                        Just s ->
                            if s < 0 then
                                Just 0

                            else if s > 8 then
                                Just 8

                            else
                                Just s

                        Nothing ->
                            Nothing

                updatedScore side =
                    { side
                        | score =
                            List.filterMap identity side.endScores
                                |> List.sum
                                |> Just
                    }

                updatedSide idx side =
                    if idx == sideIndex then
                        { side | endScores = List.Extra.updateAt endIndex (\_ -> newScore) side.endScores }
                            |> updatedScore

                    else if Maybe.withDefault 0 newScore > 0 then
                        -- If we have a score greater than 0, then make sure the other team scores 0
                        { side | endScores = List.Extra.updateAt endIndex (\_ -> Just 0) side.endScores }
                            |> updatedScore

                    else
                        side
                            |> updatedScore

                updatedGame =
                    case model.selectedGame of
                        Just game ->
                            Just { game | sides = List.indexedMap updatedSide game.sides, changed = True }

                        Nothing ->
                            Nothing
            in
            ( { model | selectedGame = updatedGame }, Cmd.none )



-- VIEWS


view : Model -> Html Msg
view model =
    let
        mergeResponses settings draws games =
            RemoteData.map (\a b c -> Data a b c) settings
                |> RemoteData.andMap draws
                |> RemoteData.andMap games
    in
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
        [ case mergeResponses model.settings model.draws model.games of
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
            List.any (\gp -> gp.result /= NoResult) game.sides

        inProgress =
            List.any (\gp -> gp.score /= Nothing && gp.result == NoResult) game.sides

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
    div
        [ style "min-height" "100%"
        , style "min-height" "100vh"
        , class "pt-2"
        ]
        [ div
            [ class "container" ]
            [ div
                [ class "row justify-content-center align-items-center" ]
                [ if data.settings.endScoresEnabled then
                    viewSidesWithEndScores model data game

                  else
                    viewSides model data game
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


viewSides : Model -> Data -> Game -> Html Msg
viewSides model data game =
    let
        viewSide : Side -> Html Msg
        viewSide side =
            p
                []
                [ h5
                    [ class "card-text" ]
                    [ text side.teamName ]
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
                            (case side.score of
                                Just val ->
                                    String.fromInt val

                                Nothing ->
                                    ""
                            )
                        , onInput (UpdateSideScore side)
                        ]
                        []
                    , div
                        [ class "btn-group btn-group-sm scoring-result-button-group flex-wrap justify-content-left" ]
                        [ button
                            [ type_ "button"
                            , onClick (UpdateSideResult side Won)
                            , style "margin-top" "-1px"
                            , style "margin-left" "-1px"
                            , class
                                ("btn btn-outline-success"
                                    ++ (case side.result of
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
                            , onClick (UpdateSideResult side Lost)
                            , style "margin-top" "-1px"
                            , class
                                ("btn btn-outline-danger"
                                    ++ (case side.result of
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
                            , onClick (UpdateSideResult side Conceded)
                            , style "margin-top" "-1px"
                            , class
                                ("btn btn-outline-danger"
                                    ++ (case side.result of
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
                            , onClick (UpdateSideResult side Forfeited)
                            , style "margin-top" "-1px"
                            , class
                                ("btn btn-outline-danger"
                                    ++ (case side.result of
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
                            , onClick (UpdateSideResult side Tied)
                            , style "margin-top" "-1px"
                            , class
                                ("btn btn-outline-info"
                                    ++ (case side.result of
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
                            , onClick (UpdateSideResult side NoResult)
                            , style "margin-top" "-1px"
                            , class
                                ("btn btn-outline-secondary"
                                    ++ (case side.result of
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
    in
    div
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
                    , viewGameSaveError model
                    ]
                    (List.map viewSide game.sides)
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
                    , disabled (not game.changed || model.savedGame == Loading)
                    , onClick SaveGame
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


viewSidesWithEndScores : Model -> Data -> Game -> Html Msg
viewSidesWithEndScores model data game =
    let
        numberOfEnds : Int
        numberOfEnds =
            case List.head game.sides of
                Just side ->
                    List.length side.endScores

                Nothing ->
                    10

        viewEndHeader : Int -> Html Msg
        viewEndHeader endNumber =
            th [ class "text-center" ] [ text (String.fromInt endNumber) ]

        viewSideEnds : Int -> Side -> Html Msg
        viewSideEnds sideIndex side =
            let
                viewEndForSide : Int -> Html Msg
                viewEndForSide endNumber =
                    let
                        onTabIndex =
                            ((sideIndex + endNumber) * 2) - sideIndex - 1
                    in
                    td [ class "justify-content-center", style "width" "48px" ]
                        [ input
                            [ class "form-control"
                            , style "width" "36px"
                            , tabindex onTabIndex
                            , value
                                (case List.Extra.getAt (endNumber - 1) side.endScores of
                                    Just val ->
                                        case val of
                                            Just v ->
                                                String.fromInt v

                                            Nothing ->
                                                ""

                                    Nothing ->
                                        ""
                                )
                            , onInput (UpdateSideEndScore sideIndex (endNumber - 1))
                            ]
                            []
                        ]
            in
            tr []
                (td [ class "p-2" ] [ text side.teamName ]
                    :: List.map viewEndForSide (List.range 1 numberOfEnds)
                    ++ [ td [ class "text-center p-2" ] [ text (String.fromInt (Maybe.withDefault 0 side.score)) ] ]
                )
    in
    div
        [ class "col-12 col-xl-10" ]
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
                    , viewGameSaveError model
                    ]
                    [ table [ class "table table-sm table-bordered" ]
                        (tr []
                            (th [] [ text "" ]
                                :: List.map viewEndHeader (List.range 1 numberOfEnds)
                                ++ [ th [ style "width" "50px" ] [ text "Total" ] ]
                            )
                            :: List.indexedMap viewSideEnds game.sides
                        )
                    ]
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
                    , disabled (not game.changed || model.savedGame == Loading)
                    , onClick SaveGame
                    ]
                    [ text "Save" ]
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
