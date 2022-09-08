module Scoring exposing (..)

import Browser
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, label, option, p, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, disabled, href, id, style, tabindex, title, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import Json.Decode as Decode exposing (Decoder, array, bool, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import List.Extra
import Process
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Task
import Time



-- MODEL


type alias Model =
    { flags : Flags
    , data : WebData Data
    , savedGame : WebData Game
    , selectedGame : WebData Game
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
    , numberOfEnds : Int
    , shotByShotEnabled : Bool
    , rockColors : List RockColor
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
    , sides : ( Side, Side )
    , changed : Bool
    , focusedEndNumber : Int
    }


type alias Side =
    { id : Int
    , teamName : String
    , rockColor : String
    , firstHammer : Bool
    , score : Maybe Int
    , result : SideResult
    , endScores : List (Maybe Int)
    , shots : Maybe (List Shot)
    }


type alias Shot =
    { endNumber : Int
    , shotNumber : Int
    , curlerId : Maybe Int
    , turn : Maybe String --TurnType
    , throw : Maybe String --ThrowType
    , rating : Maybe Int
    }


type TurnType
    = Inturn
    | Outturn


type ThrowType
    = Esomthing
    | Fsomething
    | Hsomething


type alias RockColor =
    { key : String
    , value : String
    }


type SideResult
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
        |> required "end_scores_enabled" bool
        |> optional "number_of_ends" int 10
        |> required "shot_by_shot_enabled" bool
        |> required "rock_colors" (list rockColorDecoder)


drawDecoder : Decoder Draw
drawDecoder =
    Decode.succeed Draw
        |> required "id" int
        |> required "label" string
        |> required "starts_at" string


rockColorDecoder : Decoder RockColor
rockColorDecoder =
    Decode.succeed RockColor
        |> required "key" string
        |> required "value" string


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" string
        |> required "draw_id" int
        |> required "sheet" int
        |> required "name" string
        |> required "game_positions" sidesDecoder
        |> hardcoded False
        |> hardcoded 1


sidesDecoder : Decoder ( Side, Side )
sidesDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 sideDecoder)
        (Decode.index 1 sideDecoder)


sideDecoder : Decoder Side
sideDecoder =
    Decode.succeed Side
        |> required "id" int
        |> required "team_name" string
        |> required "rock_color" string
        |> required "first_hammer" bool
        |> optional "score" (nullable int) Nothing
        |> optional "result" sideResultDecoder NoResult
        |> optional "end_scores" (list (nullable int)) []
        |> optional "shots" (nullable (list shotDecoder)) Nothing


shotDecoder : Decoder Shot
shotDecoder =
    Decode.succeed Shot
        |> required "end_number" int
        |> required "shot_number" int
        |> optional "curler_id" (nullable int) Nothing
        |> optional "turn" (nullable string) Nothing
        |> optional "throw" (nullable string) Nothing
        |> optional "rating" (nullable int) Nothing


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
        [ ( "game_positions", Encode.list encodeSide [ Tuple.first game.sides, Tuple.second game.sides ] )
        ]


encodeSide : Side -> Encode.Value
encodeSide side =
    Encode.object
        [ ( "id", Encode.int side.id )
        , ( "team_name", Encode.string side.teamName )
        , ( "rock_color", Encode.string side.rockColor )
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
        , ( "shots"
          , case side.shots of
                Just shots ->
                    Encode.list encodeShot shots

                Nothing ->
                    Encode.null
          )
        ]


encodeShot : Shot -> Encode.Value
encodeShot shot =
    Encode.object
        [ ( "curler_id"
          , case shot.curlerId of
                Just curlerId ->
                    Encode.int curlerId

                Nothing ->
                    Encode.null
          )
        , ( "turn"
          , case shot.turn of
                Just turn ->
                    Encode.string turn

                Nothing ->
                    Encode.null
          )
        , ( "throw"
          , case shot.throw of
                Just throw ->
                    Encode.string throw

                Nothing ->
                    Encode.null
          )
        , ( "rating"
          , case shot.rating of
                Just rating ->
                    Encode.int rating

                Nothing ->
                    Encode.null
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
    ( Model flags NotAsked NotAsked NotAsked False
    , getData flags.baseUrl
    )


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl string ->
            "Bad URL used: " ++ string

        Http.Timeout ->
            "Network timeout. Please check your internet connection."

        Http.NetworkError ->
            "Network error. Please check your internet connection."

        Http.BadStatus int ->
            "Bad status response from server. Please contact Curling I/O support if the issue persists for more than a few minutes."

        Http.BadBody string ->
            "Bad body response from server. Please contact Curling I/O support if the issue persists for more than a few minutes. Details: \"" ++ string ++ "\""


getData : String -> Cmd Msg
getData baseUrl =
    let
        url =
            baseUrl ++ "/games"
    in
    RemoteData.Http.get url GotData dataDecoder


getGame : String -> String -> Cmd Msg
getGame baseUrl id =
    let
        url =
            baseUrl ++ "/games/" ++ id
    in
    RemoteData.Http.get url GotGame gameDecoder


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


sideResultForDisplay : SideResult -> String
sideResultForDisplay result =
    case result of
        Won ->
            "Won"

        Lost ->
            "Lost"

        Conceded ->
            "Conceded"

        Forfeited ->
            "Forfeited"

        Tied ->
            "Tied"

        NoResult ->
            "TBD"


sideResultColor : SideResult -> String
sideResultColor result =
    case result of
        Won ->
            "success"

        Lost ->
            "danger"

        Conceded ->
            "danger"

        Forfeited ->
            "danger"

        Tied ->
            "info"

        NoResult ->
            "secondary"


rockColorForLabel : List RockColor -> String -> Maybe RockColor
rockColorForLabel rockColors key =
    List.Extra.find (\rc -> rc.key == key) rockColors


rockColorValueForLabel : List RockColor -> String -> String
rockColorValueForLabel rockColors key =
    rockColorForLabel rockColors key
        |> Maybe.map (\rc -> rc.value)
        |> Maybe.withDefault ""


sideWithHammerInEnd : ( Side, Side ) -> Int -> Maybe Int
sideWithHammerInEnd ( top, bot ) endIndex =
    -- Figures out which side has hammer for a specific end (index).
    -- For example, you can pass in both sides and an endIndex of 4, and we'll figure out who won the 3rd end, and return which side index gets hammer in the 4th (0 or 1)
    if endIndex == 0 then
        -- First hammer (LSFE)
        if top.firstHammer then
            Just 0

        else
            -- Either bottom has first hammer, or one isn't set and we default to bottom position
            Just 1

    else
        case ( List.Extra.getAt (endIndex - 1) top.endScores, List.Extra.getAt (endIndex - 1) bot.endScores ) of
            ( Just topScore_, Just botScore_ ) ->
                case ( topScore_, botScore_ ) of
                    ( Just topScore, Just botScore ) ->
                        if topScore < botScore then
                            -- top lost previous end, so top has next hammer
                            Just 0

                        else if topScore > botScore then
                            -- top won previous end, so bot has next hammer
                            Just 1

                        else
                            -- Tied, whoever had hammer last time, get's it again, so recurse using previous end as the starting point.
                            sideWithHammerInEnd ( top, bot ) (endIndex - 1)

                    ( Nothing, Just _ ) ->
                        -- top lost previous end, so bot has next hammer
                        Just 0

                    ( Just _, Nothing ) ->
                        -- bot lost previous end, so bot has next hammer
                        Just 1

                    ( Nothing, Nothing ) ->
                        -- Tied, whoever had hammer last time, get's it again, so recurse using previous end as the starting point.
                        -- sideWithHammerInEnd ( top, bot ) (endIndex - 1)
                        Nothing

            ( Nothing, Just botScore_ ) ->
                -- No top score found in index
                case botScore_ of
                    Just botScore ->
                        if botScore > 0 then
                            -- top lost previous end, give top next hammer
                            Just 0

                        else
                            -- tied, recurse
                            sideWithHammerInEnd ( top, bot ) (endIndex - 1)

                    Nothing ->
                        -- tied, recurse
                        -- sideWithHammerInEnd ( top, bot ) (endIndex - 1)
                        Nothing

            ( Just topScore_, Nothing ) ->
                -- No bot score found in index
                case topScore_ of
                    Just topScore ->
                        if topScore > 0 then
                            -- top won previous end, give bot next hammer
                            Just 1

                        else
                            -- tied, recurse
                            sideWithHammerInEnd ( top, bot ) (endIndex - 1)

                    Nothing ->
                        -- tied, recurse
                        -- sideWithHammerInEnd ( top, bot ) (endIndex - 1)
                        Nothing

            ( Nothing, Nothing ) ->
                -- Tied, whoever had hammer last time, get's it again, so recurse using previous end as the starting point.
                -- sideWithHammerInEnd ( top, bot ) (endIndex - 1)
                Nothing


correctEnds : Int -> ( Side, Side ) -> ( Side, Side )
correctEnds minNumberOfEnds ( top, bot ) =
    let
        countEndsScored endScores =
            endScores
                |> List.filterMap identity
                |> List.length

        totalScore endScores =
            endScores
                |> List.filterMap identity
                |> List.sum

        noTieDeclared =
            top.result /= Tied && bot.result /= Tied

        minEndsAreScored =
            (countEndsScored top.endScores >= minNumberOfEnds)
                && (countEndsScored bot.endScores >= minNumberOfEnds)

        scoresAreTied =
            totalScore top.endScores == totalScore bot.endScores

        removeNothingEnds ( t, b ) =
            -- Remove all nothings, then add them back up to the minNumberOfEnds
            let
                filtered side =
                    { side
                        | endScores =
                            List.filterMap identity side.endScores
                                |> List.map Just
                    }
            in
            ( filtered t, filtered b )

        addMissingNothingEnds ( t, b ) =
            let
                addMissing side =
                    if List.length side.endScores < 10 then
                        { side | endScores = side.endScores ++ List.map (\_ -> Nothing) (List.range 1 (minNumberOfEnds - List.length side.endScores)) }

                    else
                        let
                            endsWithAtleastOneScore =
                                max
                                    (countEndsScored top.endScores)
                                    (countEndsScored bot.endScores)
                        in
                        { side | endScores = side.endScores ++ List.map (\_ -> Nothing) (List.range 1 (endsWithAtleastOneScore - List.length side.endScores)) }
            in
            ( addMissing t, addMissing b )

        addNothingEnd ( t, b ) =
            let
                addNothing side =
                    { side | endScores = side.endScores ++ [ Nothing ] }
            in
            ( addNothing t, addNothing b )
    in
    if minEndsAreScored then
        if noTieDeclared && scoresAreTied then
            -- Natural tie
            -- Leave one column of double Nothing values, but remove everything after it.
            removeNothingEnds ( top, bot )
                |> addMissingNothingEnds
                |> addNothingEnd

        else
            -- The game is over. Remove any remaining nothing columns.
            removeNothingEnds ( top, bot )
                |> addMissingNothingEnds

    else
        removeNothingEnds ( top, bot )
            |> addMissingNothingEnds



-- UPDATE


type Msg
    = GotData (WebData Data)
    | ReloadData
    | ToggleFullScreen
    | SelectGame Game
    | GotGame (WebData Game)
    | SaveGame
    | PatchedGame (WebData Game)
    | ResetSavedGame Time.Posix
    | ReloadGame
    | CloseGame
    | SwapFirstHammer
    | UpdateSideColor Side RockColor
    | UpdateSideScore Side String
    | UpdateSideResult Side SideResult
    | UpdateSideEndScore Int Int String
    | UpdateFocusedEndNumber Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData response ->
            ( { model | data = response }, Cmd.none )

        ReloadData ->
            ( { model
                | data = Loading
                , savedGame = NotAsked
                , selectedGame = NotAsked
              }
            , Cmd.batch
                [ getData model.flags.baseUrl
                ]
            )

        ToggleFullScreen ->
            ( { model | fullScreen = not model.fullScreen }, Cmd.none )

        SelectGame game ->
            ( { model | selectedGame = Loading }, getGame model.flags.baseUrl game.id )

        GotGame response ->
            let
                updatedGame game =
                    case model.data of
                        Success data ->
                            { game | sides = correctEnds data.settings.numberOfEnds game.sides }

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame response, savedGame = NotAsked }, Cmd.none )

        SaveGame ->
            let
                sendPatch =
                    case model.selectedGame of
                        Success game ->
                            patchGame model.flags.baseUrl game

                        _ ->
                            Cmd.none
            in
            ( { model | savedGame = Loading }
            , Cmd.batch
                [ sendPatch
                , Task.perform ResetSavedGame (Process.sleep 6000 |> Task.andThen (\_ -> Time.now))
                ]
            )

        PatchedGame response ->
            let
                selectedGame =
                    case response of
                        Success game ->
                            Success
                                (case model.data of
                                    Success data ->
                                        { game | sides = correctEnds data.settings.numberOfEnds game.sides }

                                    _ ->
                                        game
                                )

                        _ ->
                            -- Don't replace the currently selected game data on failure.
                            model.selectedGame

                updatedGame gameFromSave game =
                    if game.id == gameFromSave.id then
                        gameFromSave

                    else
                        game

                updatedGames games =
                    case response of
                        Success decodedGame ->
                            List.map (updatedGame decodedGame) games

                        _ ->
                            games

                updatedData data =
                    { data | games = updatedGames data.games }
            in
            ( { model
                | selectedGame = selectedGame
                , savedGame = response
                , data = RemoteData.map updatedData model.data
              }
            , Cmd.none
            )

        ResetSavedGame t ->
            -- This will clear the save message in our view.
            ( { model | savedGame = NotAsked }, Cmd.none )

        ReloadGame ->
            case model.selectedGame of
                Success game ->
                    ( { model | selectedGame = Loading, savedGame = NotAsked }, getGame model.flags.baseUrl game.id )

                _ ->
                    ( model, Cmd.none )

        CloseGame ->
            ( { model | selectedGame = NotAsked, savedGame = NotAsked }, Cmd.none )

        SwapFirstHammer ->
            let
                updatedSide side =
                    { side | firstHammer = not side.firstHammer }

                updatedGame game =
                    { game | sides = Tuple.mapBoth updatedSide updatedSide game.sides, changed = True }
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , Cmd.none
            )

        UpdateSideColor sideIndex newColor ->
            let
                updatedSide side =
                    if side.id == sideIndex.id then
                        { side | rockColor = newColor.key }

                    else
                        side

                updatedGame game =
                    { game | sides = Tuple.mapBoth updatedSide updatedSide game.sides, changed = True }
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

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

                updatedGame game =
                    { game | sides = Tuple.mapBoth updatedSide updatedSide game.sides, changed = True }
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

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

                updatedGame game =
                    case model.data of
                        Success data ->
                            { game
                                | sides =
                                    Tuple.mapBoth updatedSide updatedSide game.sides
                                        |> correctEnds data.settings.numberOfEnds
                                , changed = True
                            }

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

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
                            Just 0

                updatedScore side =
                    { side
                        | score =
                            List.filterMap identity side.endScores
                                |> List.sum
                                |> Just
                    }

                updatedSide idx side =
                    if idx == sideIndex then
                        { side | endScores = List.Extra.setAt endIndex newScore side.endScores }
                            |> updatedScore

                    else if Maybe.withDefault 0 newScore > 0 then
                        -- If we have a score greater than 0, then make sure the other team scores 0
                        { side | endScores = List.Extra.setAt endIndex (Just 0) side.endScores }
                            |> updatedScore

                    else
                        side
                            |> updatedScore

                updatedGame game =
                    case model.data of
                        Success data ->
                            { game
                                | sides =
                                    ( updatedSide 0 (Tuple.first game.sides), updatedSide 1 (Tuple.second game.sides) )
                                        |> correctEnds data.settings.numberOfEnds
                                , changed = True
                            }

                        _ ->
                            game

                -- TODO: If there is no game result, but all ends have been scored, automatically set / updated the game state.
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateFocusedEndNumber endNumber ->
            let
                updatedGame game =
                    case model.data of
                        Success data ->
                            { game
                                | focusedEndNumber = endNumber
                            }

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )



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
                viewFetchError (errorMessage error)

            Success data ->
                case model.selectedGame of
                    Success game ->
                        viewSelectedGame model data game

                    Loading ->
                        viewNotReady "Loading..."

                    Failure error ->
                        viewFetchError (errorMessage error)

                    _ ->
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
                :: th [ style "min-width" "170px" ] [ text "Starts" ]
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
        completed ( t, b ) =
            let
                sideCompleted side =
                    side.result /= NoResult
            in
            sideCompleted t || sideCompleted b

        inProgress ( t, b ) =
            let
                sideInProgress side =
                    side.score /= Nothing && side.result == NoResult
            in
            sideInProgress t || sideInProgress b

        resultClass =
            if completed game.sides then
                "text-secondary"

            else if inProgress game.sides then
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


viewGameSaveMessage : Model -> Html Msg
viewGameSaveMessage model =
    case model.savedGame of
        Success _ ->
            div [ class "alert alert-success" ] [ text "Game saved." ]

        Failure error ->
            div [ class "alert alert-danger" ] [ text (errorMessage error) ]

        _ ->
            text ""


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
                    , viewGameSaveMessage model
                    ]
                    [ viewSide (Tuple.first game.sides)
                    , viewSide (Tuple.second game.sides)
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


viewSidesWithEndScores : Model -> Data -> Game -> Html Msg
viewSidesWithEndScores model data game =
    let
        numberOfEnds =
            let
                ( top, bot ) =
                    game.sides
            in
            List.length top.endScores
                |> max (List.length bot.endScores)
                |> max data.settings.numberOfEnds

        viewEndHeader : Int -> Html Msg
        viewEndHeader endNumber =
            td
                [ classList
                    [ ( "text-center", True )
                    , ( "font-weight-bold", endNumber == game.focusedEndNumber )
                    , ( "text-primary", endNumber == game.focusedEndNumber )
                    ]
                ]
                [ text (String.fromInt endNumber) ]

        viewSideEnds : Int -> Side -> Html Msg
        viewSideEnds sideIndex side =
            let
                viewEndForSide : Int -> Html Msg
                viewEndForSide endNumber =
                    let
                        onTabIndex =
                            ((sideIndex + endNumber) * 2) - sideIndex - 1

                        hasHammer =
                            sideWithHammerInEnd game.sides (endNumber - 1) == Just sideIndex
                    in
                    td
                        [ classList
                            [ ( "justify-content-center", True )
                            , ( "bg-hammer", hasHammer )
                            ]
                        , style "width" "46px"
                        ]
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
                            , onFocus (UpdateFocusedEndNumber endNumber)
                            ]
                            []
                        ]
            in
            tr []
                (td [ class "p-2" ]
                    [ div [ class "d-flex" ]
                        [ div [ style "border-bottom" ("solid 3px " ++ rockColorValueForLabel data.settings.rockColors side.rockColor) ] [ text side.teamName ]
                        ]
                    ]
                    :: List.map viewEndForSide (List.range 1 numberOfEnds)
                    ++ [ th [ class "text-center px-2", style "padding-top" "12px" ] [ text (String.fromInt (Maybe.withDefault 0 side.score)) ] ]
                )

        viewSideOther : Int -> Side -> Html Msg
        viewSideOther sideIdx side =
            let
                scoreForDisplay =
                    case side.score of
                        Just score ->
                            "(" ++ String.fromInt score ++ ")"

                        Nothing ->
                            ""

                viewSideColor : Html Msg
                viewSideColor =
                    let
                        viewColorButton : RockColor -> Html Msg
                        viewColorButton rockColor =
                            div
                                [ onClick (UpdateSideColor side rockColor)
                                , classList
                                    [ ( "color-btn", True )
                                    , ( "active", side.rockColor == rockColor.key )
                                    ]
                                , style "background-color" rockColor.value
                                ]
                                [ text "" ]
                    in
                    div
                        [ class "mt-2 mb-3 d-flex justify-content-between align-items-center" ]
                        [ button
                            [ classList
                                [ ( "btn", True )
                                , ( "btn-sm", True )
                                , ( "btn-outline-secondary", not side.firstHammer )
                                , ( "btn-success", side.firstHammer )
                                ]
                            , onClick SwapFirstHammer
                            ]
                            [ text "First Hammer" ]
                        , div [ class "d-flex align-items-center" ]
                            (List.map viewColorButton data.settings.rockColors)
                        ]

                viewSideResult : Html Msg
                viewSideResult =
                    let
                        viewResultButton : SideResult -> Html Msg
                        viewResultButton result =
                            button
                                [ type_ "button"
                                , onClick (UpdateSideResult side result)
                                , style "margin-top" "-1px"
                                , style "margin-left" "-1px"
                                , class
                                    (("btn btn-outline-" ++ sideResultColor result)
                                        ++ (if side.result == result then
                                                " active"

                                            else
                                                ""
                                           )
                                    )
                                ]
                                [ text (sideResultForDisplay result) ]
                    in
                    div [ class "mt-2" ]
                        [ h5 [] [ text "Result" ]
                        , div [ class "d-flex " ]
                            [ div
                                [ class "btn-group btn-group-sm scoring-result-button-group flex-wrap justify-content-left mr-2" ]
                                (List.map viewResultButton [ Won, Lost, Conceded, Forfeited, Tied, NoResult ])
                            ]
                        ]
            in
            div
                []
                [ div [ class "d-flex", style "border-bottom" ("solid 3px " ++ rockColorValueForLabel data.settings.rockColors side.rockColor) ]
                    [ h5 [ class "mr-2" ] [ text side.teamName ]
                    , h5 [] [ text scoreForDisplay ]
                    ]
                , viewSideColor
                , viewSideResult
                , if data.settings.shotByShotEnabled then
                    viewShots side game.focusedEndNumber

                  else
                    text ""
                ]
    in
    div
        [ class "col-12 col-xl-10" ]
        [ div
            [ class "card" ]
            [ div
                [ class "card-body" ]
                [ div [ class "d-flex justify-content-between" ]
                    [ div []
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
                        ]
                    , viewGameSaveMessage model
                    ]
                , div [ class "table-responsive" ]
                    [ table [ class "table table-sm table-bordered" ]
                        (tr []
                            (th [] [ text "" ]
                                :: List.map viewEndHeader (List.range 1 numberOfEnds)
                                ++ [ th [ style "width" "50px" ] [ text "Total" ] ]
                            )
                            :: [ viewSideEnds 0 (Tuple.first game.sides)
                               , viewSideEnds 1 (Tuple.second game.sides)
                               ]
                        )
                    ]
                , hr [] []
                , div [ class "d-flex justify-content-between" ]
                    [ viewSideOther 0 (Tuple.first game.sides)
                    , viewSideOther 1 (Tuple.second game.sides)
                    ]
                ]
            , div
                [ class "d-flex justify-content-between card-footer" ]
                [ div []
                    [ button
                        [ class "btn btn-secondary mr-2"
                        , disabled (model.savedGame == Loading)
                        , title "Abandons any changes you've made and takes you back to the draw schedule."
                        , onClick CloseGame
                        ]
                        [ text "Cancel" ]
                    , button
                        [ class "btn btn-info mr-2"
                        , disabled (model.savedGame == Loading)
                        , onClick ReloadGame
                        , title "Abandons any changes you've made and reloads the game from the server."
                        ]
                        [ text "Reload" ]
                    ]
                , button
                    [ class "btn btn-primary"
                    , disabled (not game.changed || model.savedGame == Loading)
                    , title "Saves all changes you've made since opening the game."
                    , onClick SaveGame
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


viewShots : Side -> Int -> Html Msg
viewShots side focusedEndNumber =
    let
        shots : List Shot
        shots =
            let
                missingShot shotNumber =
                    Shot focusedEndNumber shotNumber Nothing Nothing Nothing Nothing
            in
            List.range 1 8
                |> List.map
                    (\onShotNumber ->
                        case side.shots of
                            Just shots_ ->
                                case
                                    List.Extra.find (\s -> s.endNumber == focusedEndNumber && s.shotNumber == onShotNumber) shots_
                                of
                                    Just shot_ ->
                                        shot_

                                    Nothing ->
                                        missingShot onShotNumber

                            Nothing ->
                                missingShot onShotNumber
                    )

        viewShot : Shot -> Html Msg
        viewShot shot =
            div [] [ text ("Shot" ++ String.fromInt shot.shotNumber) ]
    in
    div [] (List.map viewShot shots)



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
