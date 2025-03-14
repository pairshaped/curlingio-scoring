port module Scoring exposing (..)

import Browser
import Html exposing (Html, a, button, div, h3, h5, h6, hr, input, label, option, p, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, property, selected, style, tabindex, title, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Html.Events.Extra exposing (onClickPreventDefault)
import Http
import Json.Decode as Decode exposing (Decoder, array, bool, float, int, list, nullable, string)
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
    , localMode : Bool
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
    , includedSheetNumbers : List Int
    , currentDrawId : Int
    , rockColors : List RockColor
    , endScoresEnabled : Bool
    , numberOfEnds : Int
    , shotByShotEnabled : Bool
    , lastStoneDrawEnabled : Bool
    , mixedDoubles : Bool
    , tiedResultEnabled : Bool
    , unnecessaryResultEnabled : Bool
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
    , state : GameState
    , sides : Maybe ( Side, Side )
    , changed : Bool
    , focusedEndNumber : Int
    }


type GameState
    = GamePending
    | GameActive
    | GameComplete


type alias Side =
    { id : Int
    , position : Int
    , teamName : String
    , firstHammer : Bool
    , timeRemaining : Maybe String
    , lsd : Maybe String
    , lsdCumulative : Maybe String
    , score : Maybe Int
    , result : SideResult
    , endScores : List (Maybe Int)
    , teamCurlers : List TeamCurler
    , shots : Maybe (List Shot)
    }


type alias Shot =
    { endNumber : Int
    , shotNumber : Int
    , curlerId : Maybe Int
    , turn : Maybe String
    , throw : Maybe String
    , rating : Maybe String
    }


type alias TeamCurler =
    { curlerId : Int
    , name : String
    , delivery : Maybe String
    }


type alias RockColor =
    { pos : Int
    , key : String
    , val : String
    }


type SideResult
    = Won
    | Lost
    | Forfeited
      -- We aren't accepting conceded results, since they are just losses.
      -- | Conceded
    | Tied
    | Unnecessary
    | NoResult



-- Define acceptable (valid) values for turns, throws, and ratings.


validShotTurns =
    [ "I", "O", "X" ]


validShotThrows =
    -- A = Takeout
    -- B = Hit and Roll
    -- C = Clear Front
    -- D = Raise Takeout
    -- E = Draw / Raise
    -- F = Front Stone
    -- G = Guard
    -- H = Freeze
    -- J = Tap Back
    [ "A", "B", "C", "D", "E", "F", "G", "H", "J", "X" ]


validShotRatings =
    [ "0", "1", "2", "3", "4", "V", "X" ]



-- DECODERS


isLocalMode : String -> Bool
isLocalMode url =
    String.contains "localhost" url


decodeData : Decoder Data
decodeData =
    Decode.succeed Data
        |> required "settings" decodeSettings
        |> required "draws" (list decodeDraw)
        |> required "games" (list decodeGame)


decodeSettings : Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> required "event_name" string
        |> required "sheets" (list string)
        |> optional "included_sheet_numbers" (list int) [ 1, 2, 3, 4 ]
        |> required "current_draw_id" int
        |> required "rock_colors" (list decodeRockColor)
        |> optional "end_scores_enabled" bool False
        |> optional "number_of_ends" int 10
        |> optional "shot_by_shot_enabled" bool False
        |> optional "last_stone_draw_enabled" bool False
        |> optional "mixed_doubles" bool False
        |> optional "tied_result_enabled" bool False
        |> optional "unnecessary_result_enabled" bool False


decodeDraw : Decoder Draw
decodeDraw =
    Decode.succeed Draw
        |> required "id" int
        |> required "label" string
        |> required "starts_at" string


decodeRockColor : Decoder RockColor
decodeRockColor =
    Decode.succeed RockColor
        |> required "pos" int
        |> required "key" string
        |> required "val" string


decodeGameState : Decoder GameState
decodeGameState =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "active" ->
                        Decode.succeed GameActive

                    "complete" ->
                        Decode.succeed GameComplete

                    _ ->
                        Decode.succeed GamePending
            )


decodeGame : Decoder Game
decodeGame =
    Decode.succeed Game
        |> required "id" string
        |> required "draw_id" int
        |> required "sheet" int
        |> required "name" string
        |> required "state" decodeGameState
        |> hardcoded Nothing
        |> hardcoded False
        |> hardcoded 1


decodeGameDetails : Decoder Game
decodeGameDetails =
    Decode.succeed Game
        |> required "id" string
        |> required "draw_id" int
        |> required "sheet" int
        |> required "name" string
        |> required "state" decodeGameState
        |> optional "game_positions" (Decode.map Just decodeSides) Nothing
        |> hardcoded False
        |> hardcoded 1


decodeSides : Decoder ( Side, Side )
decodeSides =
    Decode.map2 Tuple.pair
        (Decode.index 0 decodeSide)
        (Decode.index 1 decodeSide)


decodeSide : Decoder Side
decodeSide =
    Decode.succeed Side
        |> required "id" int
        |> required "position" int
        |> required "team_name" string
        |> required "first_hammer" bool
        |> optional "time_remaining" (nullable string) Nothing
        |> optional "lsd" (nullable string) Nothing
        |> optional "lsd_cumulative" (nullable string) Nothing
        |> optional "score" (nullable int) Nothing
        |> optional "result" decodeSideResult NoResult
        |> optional "end_scores" (list (nullable int)) []
        |> optional "team_curlers" (list decodeTeamCurler) []
        |> optional "shots" (nullable (list decodeShot)) Nothing


decodeTeamCurler : Decoder TeamCurler
decodeTeamCurler =
    Decode.succeed TeamCurler
        |> required "curler_id" int
        |> required "name" string
        |> optional "delivery" (nullable string) Nothing


decodeShot : Decoder Shot
decodeShot =
    Decode.succeed Shot
        |> required "end_number" int
        |> required "shot_number" int
        |> optional "curler_id" (nullable int) Nothing
        |> optional "turn" (nullable string) Nothing
        |> optional "throw" (nullable string) Nothing
        |> optional "rating" (nullable string) Nothing


decodeSideResult : Decoder SideResult
decodeSideResult =
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
                        -- We aren't accepting conceded results, since they are just losses.
                        -- Decode.succeed Conceded
                        Decode.succeed Lost

                    "tied" ->
                        Decode.succeed Tied

                    "unnecessary" ->
                        Decode.succeed Unnecessary

                    _ ->
                        Decode.succeed NoResult
            )



-- ENCODERS


encodeGame : Game -> Encode.Value
encodeGame game =
    Encode.object
        [ ( "game_positions"
          , case game.sides of
                Just sides ->
                    Encode.list encodeSide [ Tuple.first sides, Tuple.second sides ]

                Nothing ->
                    Encode.null
          )
        ]


encodeSide : Side -> Encode.Value
encodeSide side =
    Encode.object
        [ ( "id", Encode.int side.id )
        , ( "position", Encode.int side.position )
        , ( "team_name", Encode.string side.teamName )
        , ( "first_hammer", Encode.bool side.firstHammer )
        , ( "time_remaining"
          , case side.timeRemaining of
                Just timeRemaining ->
                    Encode.string timeRemaining

                Nothing ->
                    Encode.null
          )
        , ( "lsd"
          , case side.lsd of
                Just lsd ->
                    case String.toFloat lsd of
                        Just lsd_ ->
                            Encode.string lsd

                        Nothing ->
                            Encode.null

                Nothing ->
                    Encode.null
          )
        , ( "lsd_cumulative"
          , case side.lsdCumulative of
                Just lsdCumulative ->
                    case String.toFloat lsdCumulative of
                        Just lsdCumulative_ ->
                            Encode.string lsdCumulative

                        Nothing ->
                            Encode.null

                Nothing ->
                    Encode.null
          )
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
    if shot.curlerId == Nothing && shot.turn == Nothing && shot.throw == Nothing && shot.rating == Nothing then
        -- Shots are stored as JSON in the database, so null values are unecessary (the entire field is overwritten on save).
        -- Therefore, we can save a ton of bandwidth for the public widgets by not saving shots where nothing has been recorded.
        -- The shot data will gradually increase as more games complete, but we'll never store shots for forfeits beyond the ends that were played.
        Encode.null

    else
        Encode.object
            ([ ( "end_number", Encode.int shot.endNumber )
             , ( "shot_number", Encode.int shot.shotNumber )
             , ( "curler_id"
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
                        Encode.string rating

                    Nothing ->
                        Encode.null
               )
             ]
                -- Filter out nulls since we're overwriting the entire shot json payload, there's no need to store nulls.
                |> List.filter (\f -> Tuple.second f /= Encode.null)
            )


encodeSideResult : SideResult -> Encode.Value
encodeSideResult sideResult =
    case sideResult of
        Won ->
            Encode.string "won"

        Lost ->
            Encode.string "lost"

        Forfeited ->
            Encode.string "forfeited"

        -- We aren't accepting conceded results, since they are just losses.
        -- Conceded ->
        --     Encode.string "conceded"
        Tied ->
            Encode.string "tied"

        Unnecessary ->
            Encode.string "unnecessary"

        NoResult ->
            Encode.null



-- HELPERS


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        localMode =
            isLocalMode flags.baseUrl
    in
    ( Model flags NotAsked NotAsked NotAsked False localMode
    , getData localMode flags.baseUrl
    )


run : msg -> Cmd msg
run msg =
    Task.succeed msg
        |> Task.perform identity


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


getData : Bool -> String -> Cmd Msg
getData localMode baseUrl =
    let
        url =
            if localMode then
                baseUrl ++ "/db"

            else
                baseUrl ++ "/games"
    in
    RemoteData.Http.get url GotData decodeData


getGame : String -> String -> Cmd Msg
getGame baseUrl id =
    let
        url =
            baseUrl ++ "/games/" ++ id
    in
    RemoteData.Http.get url GotGame decodeGameDetails


patchGame : String -> Game -> Cmd Msg
patchGame baseUrl game =
    let
        url =
            baseUrl ++ "/games/" ++ game.id
    in
    RemoteData.Http.patch url PatchedGame decodeGameDetails (encodeGame game)


findGame : List Game -> Int -> Int -> Maybe Game
findGame games drawId sheet =
    List.Extra.find (\game -> game.drawId == drawId && game.sheet == sheet) games


findGameByName : List Game -> String -> String -> Maybe Game
findGameByName games excludeId name =
    List.Extra.find (\game -> game.id /= excludeId && game.name == name) games


findDraw : List Draw -> Int -> Maybe Draw
findDraw draws drawId =
    List.Extra.find (\draw -> draw.id == drawId) draws


hasNoScores : ( Side, Side ) -> Bool
hasNoScores sides =
    let
        ( top, bot ) =
            sides
    in
    -- Can't forfeit if there are any non-zero scores.
    -- Forfeits are only for when the teams don't play.
    (Maybe.withDefault 0 top.score == 0)
        && (Maybe.withDefault 0 bot.score == 0)


validGameResultOptions : Settings -> ( Side, Side ) -> List SideResult
validGameResultOptions { tiedResultEnabled, unnecessaryResultEnabled } sides =
    let
        forfeitedState =
            if hasNoScores sides then
                Just Forfeited

            else
                Nothing

        tiedState =
            if tiedResultEnabled then
                Just Tied

            else
                Nothing

        unnecessaryState =
            if unnecessaryResultEnabled && hasNoScores sides then
                Just Unnecessary

            else
                Nothing
    in
    [ Just Won, Just Lost, forfeitedState, tiedState, unnecessaryState, Just NoResult ]
        |> List.filterMap identity


sideResultForDisplay : SideResult -> String
sideResultForDisplay result =
    case result of
        Won ->
            "Won"

        Lost ->
            "Lost"

        Forfeited ->
            "Forfeited"

        -- We aren't accepting conceded results, since they are just losses.
        -- Conceded ->
        --     "Conceded"
        Tied ->
            "Tied"

        Unnecessary ->
            "Unnecessary"

        NoResult ->
            "TBD"


sideResultColor : SideResult -> String
sideResultColor result =
    case result of
        Won ->
            "success"

        Lost ->
            "danger"

        Forfeited ->
            "danger"

        -- We aren't accepting conceded results, since they are just losses.
        -- Conceded ->
        --     "danger"
        Tied ->
            "info"

        Unnecessary ->
            "info"

        NoResult ->
            "secondary"


rockColorForLabel : List RockColor -> Int -> Maybe RockColor
rockColorForLabel rockColors position =
    List.Extra.find (\rc -> rc.pos == position) rockColors


rockColorValueForLabel : List RockColor -> Int -> String
rockColorValueForLabel rockColors position =
    rockColorForLabel rockColors position
        |> Maybe.map (\rc -> rc.val)
        |> Maybe.withDefault ""


hasHammerInEnd : Bool -> Side -> ( Side, Side ) -> Int -> Maybe Bool
hasHammerInEnd mixedDoubles onSide ( top, bot ) endIndex =
    -- Figures out which side has hammer for a specific end (index).
    -- For example, you can pass in both sides and an endIndex of 4, and we'll figure out who won the 3rd end, and return which side index gets hammer in the 4th (0 or 1)
    if endIndex == 0 then
        -- First hammer (LSFE)
        if onSide.firstHammer then
            Just True

        else
            -- Either bottom has first hammer, or one isn't set and we default to bottom position
            Just False

    else
        case ( List.Extra.getAt (endIndex - 1) top.endScores, List.Extra.getAt (endIndex - 1) bot.endScores ) of
            ( Just topScore_, Just botScore_ ) ->
                case ( topScore_, botScore_ ) of
                    ( Just topScore, Just botScore ) ->
                        if topScore < botScore then
                            -- top lost previous end, so top has next hammer
                            Just (top.id == onSide.id)

                        else if topScore > botScore then
                            -- top won previous end, so bot has next hammer
                            Just (bot.id == onSide.id)

                        else if mixedDoubles then
                            -- Blanked ends (ties) in mixed doubles are a special case,
                            -- Instead of keeping the hammer, it's given to the other team,
                            -- So it's the opposite of what we'd normally do.
                            Maybe.map not (hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1))

                        else
                            -- Tied (blanked) previous end
                            -- In normal play, when you blank an end, you keep the hammer (advantage),
                            -- Whoever had hammer last time, get's it again, so recurse using previous end as the starting point.
                            hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)

                    ( Nothing, Just _ ) ->
                        -- top lost previous end, so top has next hammer
                        Just (top.id == onSide.id)

                    ( Just _, Nothing ) ->
                        -- bot lost previous end, so bot has next hammer
                        Just (bot.id == onSide.id)

                    ( Nothing, Nothing ) ->
                        -- Tied, whoever had hammer last time, get's it again, so recurse using previous end as the starting point.
                        -- hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)
                        Nothing

            ( Nothing, Just botScore_ ) ->
                -- No top score found in index
                case botScore_ of
                    Just botScore ->
                        if botScore > 0 then
                            -- top lost previous end, give top next hammer
                            Just (top.id == onSide.id)

                        else
                            -- tied, recurse
                            hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)

                    Nothing ->
                        -- tied, recurse
                        -- hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)
                        Nothing

            ( Just topScore_, Nothing ) ->
                -- No bot score found in index
                case topScore_ of
                    Just topScore ->
                        if topScore > 0 then
                            -- top won previous end, give bot next hammer
                            Just (bot.id == onSide.id)

                        else
                            -- tied, recurse
                            hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)

                    Nothing ->
                        -- tied, recurse
                        -- hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)
                        Nothing

            ( Nothing, Nothing ) ->
                -- Tied, whoever had hammer last time, get's it again, so recurse using previous end as the starting point.
                -- hasHammerInEnd mixedDoubles onSide ( top, bot ) (endIndex - 1)
                Nothing


correctForfeits : ( Side, Side ) -> ( Side, Side )
correctForfeits sides =
    let
        ( top, bot ) =
            sides
    in
    if top.result == Forfeited && hasNoScores sides == False then
        ( { top | result = Lost }, bot )

    else if bot.result == Forfeited && hasNoScores sides == False then
        ( top, { bot | result = Lost } )

    else
        sides


correctEnds : Settings -> ( Side, Side ) -> ( Side, Side )
correctEnds { numberOfEnds, shotByShotEnabled, mixedDoubles } ( top, bot ) =
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
            (countEndsScored top.endScores >= numberOfEnds)
                && (countEndsScored bot.endScores >= numberOfEnds)

        scoresAreTied =
            totalScore top.endScores == totalScore bot.endScores

        removeNothingEnds ( t, b ) =
            -- Remove all nothings, then add them back up to the numberOfEnds
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
                        { side | endScores = side.endScores ++ List.map (\_ -> Nothing) (List.range 1 (numberOfEnds - List.length side.endScores)) }

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
    (if minEndsAreScored then
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
    )
        |> withInitializedShots shotByShotEnabled mixedDoubles


withInitializedShots : Bool -> Bool -> ( Side, Side ) -> ( Side, Side )
withInitializedShots shotByShotEnabled mixedDoubles ( top, bot ) =
    if not shotByShotEnabled then
        ( top, bot )

    else
        let
            updatedSide : Side -> Side
            updatedSide side =
                let
                    updatedShots : List Shot
                    updatedShots =
                        let
                            updatedShotsForEnd : Int -> Maybe Int -> List Shot
                            updatedShotsForEnd endNumber _ =
                                let
                                    curlerIdForShotNumber shotNumber =
                                        let
                                            -- We're turning the shot number into the curler index by doing: round (shotNumber / 2),
                                            -- because there are 2 consecutive shots per curler, so the 7th shot should be the 4th curler (skip) for example.
                                            curlerIndex =
                                                if mixedDoubles then
                                                    -- In mixed doubles, the first curlers throws 1, 5 and the second curler throws 2, 3, 4
                                                    case shotNumber of
                                                        2 ->
                                                            1

                                                        3 ->
                                                            1

                                                        4 ->
                                                            1

                                                        _ ->
                                                            0

                                                else
                                                    round (toFloat shotNumber / 2) - 1
                                        in
                                        List.Extra.getAt curlerIndex side.teamCurlers
                                            |> Maybe.map (\c -> c.curlerId)

                                    missingShot shotNumber =
                                        -- Find the curler for the shot, if there is one
                                        Shot (endNumber + 1) shotNumber (curlerIdForShotNumber shotNumber) Nothing Nothing Nothing

                                    totalShotsPerSide =
                                        if mixedDoubles then
                                            5

                                        else
                                            8
                                in
                                List.range 1 totalShotsPerSide
                                    |> List.map
                                        (\onShotNumber ->
                                            case side.shots of
                                                Just shots_ ->
                                                    case
                                                        List.Extra.find (\s -> s.endNumber == (endNumber + 1) && s.shotNumber == onShotNumber) shots_
                                                    of
                                                        Just shot_ ->
                                                            case shot_.curlerId of
                                                                Nothing ->
                                                                    { shot_ | curlerId = curlerIdForShotNumber onShotNumber }

                                                                _ ->
                                                                    shot_

                                                        Nothing ->
                                                            missingShot onShotNumber

                                                Nothing ->
                                                    missingShot onShotNumber
                                        )
                        in
                        side.endScores
                            |> List.indexedMap updatedShotsForEnd
                            |> List.concat
                in
                { side | shots = Just updatedShots }
        in
        ( updatedSide top, updatedSide bot )



-- UPDATE


type Msg
    = NoOp
    | ForcedTick Time.Posix
    | GotData (WebData Data)
    | ReloadData
    | ToggleFullScreen
    | SelectGame Game
    | GotGame (WebData Game)
    | SaveGame
    | AutoSaveEnd Int
    | PatchedGame (WebData Game)
    | ResetSavedGameMessage Time.Posix
    | ReloadGame
    | CloseGame
    | ToggleFirstHammer Side
    | UpdateSidePosition Side Int
    | UpdateSideScore Side String
    | UpdateSideTimeRemaining Side String
    | UpdateSideLsd Side String
    | UpdateSideCumulativeLsd Side String
    | UpdateSideResult Side SideResult
    | UpdateSideEndScore Side Int String
    | UpdateFocusedEndNumber Int
    | UpdateShotCurlerId Side Shot String
    | UpdateShotTurn Side Shot String
    | UpdateShotThrow Side Shot String
    | UpdateShotRating Side Shot String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ForcedTick t ->
            -- This will clear the save message in our view.
            ( model, Cmd.none )

        GotData response ->
            ( { model | data = response }, Cmd.none )

        ReloadData ->
            ( { model
                | data = Loading
                , savedGame = NotAsked
                , selectedGame = NotAsked
              }
            , Cmd.batch
                [ getData model.localMode model.flags.baseUrl
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
                            case game.sides of
                                Just sides ->
                                    { game
                                        | sides =
                                            Just
                                                (correctEnds data.settings sides)
                                    }

                                Nothing ->
                                    game

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
                , Task.perform ResetSavedGameMessage (Process.sleep 6000 |> Task.andThen (\_ -> Time.now))
                ]
            )

        PatchedGame response ->
            let
                focusedEndNumber =
                    case model.selectedGame of
                        Success game ->
                            game.focusedEndNumber

                        _ ->
                            1

                selectedGame =
                    case response of
                        Success game ->
                            Success
                                (case model.data of
                                    Success data ->
                                        case game.sides of
                                            Just sides ->
                                                { game
                                                    | sides =
                                                        Just
                                                            (correctEnds data.settings sides)
                                                    , focusedEndNumber = focusedEndNumber
                                                }

                                            Nothing ->
                                                { game | focusedEndNumber = focusedEndNumber }

                                    _ ->
                                        { game | focusedEndNumber = focusedEndNumber }
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
            , case model.data of
                Success data ->
                    if data.settings.endScoresEnabled then
                        Cmd.none

                    else
                        run CloseGame

                _ ->
                    Cmd.none
            )

        ResetSavedGameMessage t ->
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

        ToggleFirstHammer side ->
            let
                updatedSide side_ =
                    if side_.id == side.id then
                        { side_ | firstHammer = not side.firstHammer }

                    else if not side.firstHammer then
                        { side_ | firstHammer = False }

                    else
                        side_

                updatedGame game =
                    case game.sides of
                        Just sides ->
                            { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides), changed = True }

                        Nothing ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , Cmd.none
            )

        UpdateSidePosition onSide newPosition ->
            let
                updatedSide side =
                    if side.id == onSide.id then
                        { side | position = newPosition }

                    else
                        -- subtracting 1 and getting the absolute is the same as if 1 then 0 and if 0 then 1.
                        { side | position = abs (newPosition - 1) }

                updatedGame game =
                    case game.sides of
                        Just sides ->
                            { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides), changed = True }

                        Nothing ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateSideScore onSide newScore ->
            let
                updatedSide side =
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
                    if side.id == onSide.id then
                        { side | score = updatedScore }

                    else
                        side

                updatedGame game =
                    case game.sides of
                        Just sides ->
                            { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides |> correctForfeits), changed = True }

                        Nothing ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateSideTimeRemaining onSide newTimeRemaining ->
            let
                updatedSide side =
                    if side.id == onSide.id then
                        let
                            validTime =
                                if newTimeRemaining == "" then
                                    False

                                else
                                    let
                                        isNumber v =
                                            case String.toInt v of
                                                Just v_ ->
                                                    True

                                                Nothing ->
                                                    False
                                    in
                                    if isNumber (String.replace ":" "" newTimeRemaining) then
                                        True

                                    else
                                        False
                        in
                        if validTime then
                            { side | timeRemaining = Just newTimeRemaining }

                        else
                            { side | timeRemaining = Nothing }

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game
                                        | sides =
                                            Just
                                                (Tuple.mapBoth updatedSide updatedSide sides
                                                    |> correctEnds data.settings
                                                )
                                        , changed = True
                                    }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateSideLsd onSide newLsd ->
            let
                updatedSide side =
                    if side.id == onSide.id then
                        { side | lsd = Just newLsd }

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game
                                        | sides =
                                            Just
                                                (Tuple.mapBoth updatedSide updatedSide sides
                                                    |> correctEnds data.settings
                                                )
                                        , changed = True
                                    }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateSideCumulativeLsd onSide newCumulativeLsd ->
            let
                updatedSide side =
                    if side.id == onSide.id then
                        { side | lsdCumulative = Just newCumulativeLsd }

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game
                                        | sides =
                                            Just
                                                (Tuple.mapBoth updatedSide updatedSide sides
                                                    |> correctEnds data.settings
                                                )
                                        , changed = True
                                    }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateSideResult onSide newResult ->
            let
                updatedSide side =
                    if side.id == onSide.id then
                        -- Don't allow a forfeit if the game has end scores
                        { side
                            | result =
                                case ( newResult, onSide.score ) of
                                    ( Forfeited, Nothing ) ->
                                        newResult

                                    ( Forfeited, Just 0 ) ->
                                        newResult

                                    ( Forfeited, _ ) ->
                                        Lost

                                    _ ->
                                        newResult
                        }

                    else
                        case newResult of
                            Won ->
                                { side | result = Lost }

                            Lost ->
                                { side | result = Won }

                            Forfeited ->
                                { side | result = Won }

                            -- We aren't accepting conceded results, since they are just losses.
                            -- Conceded ->
                            --     { side | result = Won }
                            Tied ->
                                { side | result = Tied }

                            Unnecessary ->
                                { side | result = Unnecessary }

                            NoResult ->
                                { side | result = NoResult }

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game
                                        | sides =
                                            Just
                                                (Tuple.mapBoth updatedSide updatedSide sides
                                                    |> correctEnds data.settings
                                                )
                                        , changed = True
                                    }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateSideEndScore onSide endIndex newScoreStr ->
            let
                newScoreStrFixed =
                    if String.length newScoreStr > 1 then
                        String.right 1 newScoreStr

                    else
                        newScoreStr

                newScore =
                    case newScoreStrFixed of
                        "X" ->
                            Nothing

                        "x" ->
                            Nothing

                        _ ->
                            case String.toInt newScoreStrFixed of
                                Just s ->
                                    if s < 0 then
                                        Nothing

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

                updatedSide side =
                    (if side.id == onSide.id then
                        { side | endScores = List.Extra.setAt endIndex newScore side.endScores }

                     else if Maybe.withDefault 0 newScore > 0 then
                        -- If we have a score greater than 0, then make sure the other team scores 0
                        { side | endScores = List.Extra.setAt endIndex (Just 0) side.endScores }

                     else
                        side
                    )
                        |> updatedScore

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game
                                        | sides =
                                            Just
                                                (( updatedSide (Tuple.first sides), updatedSide (Tuple.second sides) )
                                                    |> correctForfeits
                                                    |> correctEnds data.settings
                                                )
                                        , changed = True
                                    }

                                Nothing ->
                                    game

                        _ ->
                            game

                -- TODO: If there is no game result, but all ends have been scored, automatically set / updated the game state.
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , if newScoreStr == "" || String.contains "X" (String.toUpper newScoreStr) then
                -- Don't autosave or autotab
                Cmd.none

              else
                case model.data of
                    Success data ->
                        -- Autosave
                        if data.settings.shotByShotEnabled then
                            run (AutoSaveEnd endIndex)

                        else
                            -- Also, autotab if it's not a shot by shot game
                            Cmd.batch
                                [ run (AutoSaveEnd endIndex)
                                , sendMessage "focusNext"
                                ]

                    _ ->
                        Cmd.none
            )

        AutoSaveEnd endIndex ->
            let
                shouldSave =
                    let
                        hasScoreForEnd game =
                            case game.sides of
                                Just ( sideA, sideB ) ->
                                    case ( List.Extra.getAt endIndex sideA.endScores, List.Extra.getAt endIndex sideB.endScores ) of
                                        ( Just val, Just otherVal ) ->
                                            case ( val, otherVal ) of
                                                ( Just v, Just otherV ) ->
                                                    v >= 0 && otherV >= 0

                                                ( Nothing, Nothing ) ->
                                                    True

                                                _ ->
                                                    False

                                        _ ->
                                            False

                                _ ->
                                    False
                    in
                    case model.selectedGame of
                        Success game ->
                            game.changed && hasScoreForEnd game

                        _ ->
                            False
            in
            if shouldSave then
                update SaveGame model

            else
                ( model, Cmd.none )

        UpdateFocusedEndNumber endNumber ->
            let
                updatedGame game =
                    { game | focusedEndNumber = endNumber }

                -- For some reason, in order to reflect which curler is selected in shots data we need to force a second render on end number focus change.
                forcedTickToUpdateShots =
                    case model.data of
                        Success data ->
                            if data.settings.shotByShotEnabled then
                                Task.perform ForcedTick (Process.sleep 20 |> Task.andThen (\_ -> Time.now))

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , forcedTickToUpdateShots
            )

        UpdateShotCurlerId forSide forShot val ->
            let
                updatedShot shot =
                    if shot.endNumber == forShot.endNumber && shot.shotNumber == forShot.shotNumber then
                        { shot | curlerId = String.toInt val }

                    else if shot.endNumber > forShot.endNumber && shot.shotNumber == forShot.shotNumber && shot.turn == Nothing && shot.throw == Nothing && shot.rating == Nothing then
                        -- We also want to update all future ends with the curler change if turn, throw, rate has not been capture for it yet.
                        { shot | curlerId = String.toInt val }

                    else
                        shot

                updatedSide side =
                    if side.id == forSide.id then
                        case side.shots of
                            Just shots ->
                                { side | shots = Just (List.map updatedShot shots) }

                            Nothing ->
                                side

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides), changed = True }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }, Cmd.none )

        UpdateShotTurn forSide forShot val ->
            let
                formattedVal =
                    String.toUpper val

                validShotTurn =
                    List.member formattedVal validShotTurns

                updatedShot shot =
                    if shot.endNumber == forShot.endNumber && shot.shotNumber == forShot.shotNumber then
                        let
                            validated =
                                if val == "" then
                                    Nothing

                                else if validShotTurn then
                                    Just formattedVal

                                else
                                    shot.turn
                        in
                        { shot | turn = validated }

                    else
                        shot

                updatedSide side =
                    if side.id == forSide.id then
                        case side.shots of
                            Just shots ->
                                { side | shots = Just (List.map updatedShot shots) }

                            Nothing ->
                                side

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides), changed = True }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , if validShotTurn then
                sendMessage "focusNext"

              else
                Cmd.none
            )

        UpdateShotThrow forSide forShot val ->
            let
                formattedVal =
                    String.toUpper val

                validShotThrow =
                    List.member formattedVal validShotThrows

                updatedShot shot =
                    if shot.endNumber == forShot.endNumber && shot.shotNumber == forShot.shotNumber then
                        let
                            validated =
                                if val == "" then
                                    Nothing

                                else if validShotThrow then
                                    Just formattedVal

                                else
                                    shot.throw
                        in
                        { shot | throw = validated }

                    else
                        shot

                updatedSide side =
                    if side.id == forSide.id then
                        case side.shots of
                            Just shots ->
                                { side | shots = Just (List.map updatedShot shots) }

                            Nothing ->
                                side

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides), changed = True }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , if validShotThrow then
                sendMessage "focusNext"

              else
                Cmd.none
            )

        UpdateShotRating forSide forShot val ->
            let
                formattedVal =
                    String.toUpper val

                validShotRating =
                    List.member formattedVal validShotRatings

                updatedShot shot =
                    if shot.endNumber == forShot.endNumber && shot.shotNumber == forShot.shotNumber then
                        let
                            validated =
                                if val == "" then
                                    Nothing

                                else if validShotRating then
                                    Just formattedVal

                                else
                                    shot.rating
                        in
                        { shot | rating = validated }

                    else
                        shot

                updatedSide side =
                    if side.id == forSide.id then
                        case side.shots of
                            Just shots ->
                                { side | shots = Just (List.map updatedShot shots) }

                            Nothing ->
                                side

                    else
                        side

                updatedGame game =
                    case model.data of
                        Success data ->
                            case game.sides of
                                Just sides ->
                                    { game | sides = Just (Tuple.mapBoth updatedSide updatedSide sides), changed = True }

                                Nothing ->
                                    game

                        _ ->
                            game
            in
            ( { model | selectedGame = RemoteData.map updatedGame model.selectedGame }
            , if validShotRating then
                sendMessage "focusNext"

              else
                Cmd.none
            )



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
            :: (data.settings.includedSheetNumbers
                    |> List.map (viewDrawSheet data.games draw.id)
               )
        )


viewDrawSheet : List Game -> Int -> Int -> Html Msg
viewDrawSheet games drawId sheet =
    let
        viewDrawSheetGame game =
            a
                [ href "#"
                , classList
                    [ ( "text-secondary", game.state == GameComplete )
                    , ( "font-weight-bold", game.state == GameActive )
                    ]
                , onClickPreventDefault (SelectGame game)
                ]
                [ text game.name ]
    in
    td [ class "text-center" ]
        [ case findGame games drawId sheet of
            Just game ->
                viewDrawSheetGame game

            Nothing ->
                text ""
        ]


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
                [ case ( data.settings.endScoresEnabled, game.sides ) of
                    ( True, Just sides ) ->
                        viewSidesWithEndScores model data game sides

                    ( False, Just sides ) ->
                        viewSides model data game sides

                    _ ->
                        text ""
                ]
            ]
        ]


viewGameMessage : Model -> Game -> Html Msg
viewGameMessage model game =
    if game.changed then
        case model.data of
            Success data ->
                case model.savedGame of
                    Failure error ->
                        div [ class "alert alert-danger" ] [ text (errorMessage error) ]

                    _ ->
                        if data.settings.endScoresEnabled then
                            div [ class "alert alert-warning" ] [ text "There are unsaved changes." ]

                        else
                            text ""

            _ ->
                text ""

    else
        case model.savedGame of
            Success _ ->
                div [ class "alert alert-success" ] [ text "Game saved." ]

            Failure error ->
                div [ class "alert alert-danger" ] [ text (errorMessage error) ]

            _ ->
                text ""


viewSides : Model -> Data -> Game -> ( Side, Side ) -> Html Msg
viewSides model data game sides =
    let
        viewSide : Side -> Html Msg
        viewSide side =
            let
                viewSideScore =
                    input
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
                    div
                        [ class "btn-group btn-group-sm scoring-result-button-group flex-wrap justify-content-left mr-2" ]
                        (List.map viewResultButton (validGameResultOptions data.settings sides))
            in
            p
                []
                [ h5
                    [ class "card-text" ]
                    [ text side.teamName ]
                , div [ class "d-flex " ]
                    [ viewSideScore
                    , viewSideResult
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
                                    let
                                        sheetName =
                                            case List.Extra.getAt (game.sheet - 1) data.settings.sheets of
                                                Just n ->
                                                    " - " ++ n

                                                Nothing ->
                                                    ""
                                    in
                                    "Draw " ++ draw.label ++ " - " ++ draw.startsAt ++ sheetName

                                Nothing ->
                                    "Unknown Draw"
                            )
                        ]
                    , hr [] []
                    , viewGameMessage model game
                    ]
                    [ viewSide (Tuple.first sides)
                    , viewSide (Tuple.second sides)
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


viewSidesWithEndScores : Model -> Data -> Game -> ( Side, Side ) -> Html Msg
viewSidesWithEndScores model data game sides =
    let
        { lastStoneDrawEnabled, shotByShotEnabled, mixedDoubles, rockColors, sheets } =
            data.settings

        numberOfEnds =
            let
                ( top, bot ) =
                    sides
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
                    , ( "mark", endNumber == game.focusedEndNumber )
                    ]
                , style "color" "#007bff"
                ]
                [ text (String.fromInt endNumber) ]

        viewSideEnds : Int -> Side -> Html Msg
        viewSideEnds sideIndex side =
            let
                viewEndForSide : Int -> Html Msg
                viewEndForSide endNumber =
                    let
                        onTabIndex =
                            -- If shot by shot is enabled, ends past the currently focused
                            -- end will need to account for all of the shot by shot tabs (16 * 4)
                            if shotByShotEnabled && game.focusedEndNumber < endNumber then
                                ((sideIndex + endNumber) * 2) - sideIndex - 1 + (16 * 4)

                            else
                                ((sideIndex + endNumber) * 2) - sideIndex - 1

                        hasHammer =
                            Maybe.withDefault False (hasHammerInEnd mixedDoubles side sides (endNumber - 1))
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
                            , id ("tab" ++ String.fromInt onTabIndex)
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
                            , onInput (UpdateSideEndScore side (endNumber - 1))
                            , onFocus (UpdateFocusedEndNumber endNumber)
                            ]
                            []
                        ]
            in
            tr []
                (td [ class "p-2" ]
                    [ div [ class "d-flex" ]
                        [ div
                            [ style "border-bottom" ("solid 3px " ++ rockColorValueForLabel rockColors side.position) ]
                            [ text side.teamName ]
                        , if side.firstHammer then
                            span [ class "ml-2" ] [ text "*" ]

                          else
                            text ""
                        ]
                    ]
                    :: List.map viewEndForSide (List.range 1 numberOfEnds)
                    ++ [ th [ class "text-center px-2", style "padding-top" "12px" ] [ text (String.fromInt (Maybe.withDefault 0 side.score)) ] ]
                )

        viewSideOther : Int -> Side -> Html Msg
        viewSideOther sideIndex side =
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
                        viewColorButton : Int -> RockColor -> Html Msg
                        viewColorButton idx rockColor =
                            div
                                [ onClick (UpdateSidePosition side idx)
                                , classList
                                    [ ( "color-btn", True )
                                    , ( "mr-1", True )
                                    , ( "active", side.position == rockColor.pos )
                                    ]
                                , style "background-color" rockColor.val
                                , style "border" "1px solid black"
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
                            , onClick (ToggleFirstHammer side)
                            ]
                            [ text "First Hammer" ]
                        , div [ class "d-flex align-items-center" ]
                            (List.indexedMap viewColorButton rockColors)
                        ]

                viewSideTimeRemaining : Html Msg
                viewSideTimeRemaining =
                    div [ class "d-flex mt-3 form-group mr-3" ]
                        [ label [ class "label mr-2 mt-2" ] [ text "Time Remaining:" ]
                        , input
                            [ class "form-control"
                            , style "width" "80px"
                            , value (Maybe.withDefault "" side.timeRemaining)
                            , placeholder "MM:SS"
                            , onInput (UpdateSideTimeRemaining side)
                            ]
                            []
                        ]

                viewSideLsd : Html Msg
                viewSideLsd =
                    div [ class "d-flex form-group mr-3" ]
                        [ label [ class "label mr-2 mt-2" ] [ text "Game LSD:" ]
                        , input
                            [ class "form-control"
                            , style "width" "70px"
                            , value (Maybe.withDefault "" side.lsd)
                            , onInput (UpdateSideLsd side)
                            ]
                            []
                        ]

                viewSideCumulativeLsd : Html Msg
                viewSideCumulativeLsd =
                    div [ class "d-flex form-group" ]
                        [ label [ class "label mr-2 mt-2" ] [ text "Cumulative LSD:" ]
                        , input
                            [ class "form-control"
                            , style "width" "70px"
                            , value (Maybe.withDefault "" side.lsdCumulative)
                            , onInput (UpdateSideCumulativeLsd side)
                            ]
                            []
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
                                (List.map viewResultButton (validGameResultOptions data.settings sides))
                            ]
                        ]
            in
            div
                []
                [ div [ class "d-flex", style "border-bottom" ("solid 3px " ++ rockColorValueForLabel rockColors side.position) ]
                    [ h5 [ class "mr-2" ] [ text side.teamName ]
                    , h5 [] [ text scoreForDisplay ]
                    ]
                , if shotByShotEnabled then
                    viewShots sideIndex side game.focusedEndNumber game

                  else
                    text ""
                , viewSideColor
                , viewSideResult
                , viewSideTimeRemaining
                , if lastStoneDrawEnabled then
                    div [ class "d-flex mt-2" ]
                        [ viewSideLsd, viewSideCumulativeLsd ]

                  else
                    text ""
                ]

        sidesOrderedForEnds =
            -- Order sides by their rock color for the end scores (top / bottom)
            let
                ( top, bot ) =
                    sides
            in
            if top.position > 0 then
                ( bot, top )

            else
                sides

        sidesOrderedForShots =
            -- If shot by shot is enabled, the side with the hammer should always show up last (on the right).
            if shotByShotEnabled then
                let
                    ( top, bot ) =
                        sides
                in
                if Maybe.withDefault False (hasHammerInEnd mixedDoubles top sides (game.focusedEndNumber - 1)) then
                    ( bot, top )

                else
                    sides

            else
                sidesOrderedForEnds
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
                                        let
                                            sheetName =
                                                case List.Extra.getAt (game.sheet - 1) sheets of
                                                    Just n ->
                                                        " - " ++ n

                                                    Nothing ->
                                                        ""
                                        in
                                        "Draw " ++ draw.label ++ " - " ++ draw.startsAt ++ sheetName

                                    Nothing ->
                                        "Unknown Draw"
                                )
                            ]
                        ]
                    , viewGameMessage model game
                    ]
                , div [ class "table-responsive" ]
                    [ table [ class "table table-sm table-bordered" ]
                        (tr []
                            (th [] [ text "" ]
                                :: List.map viewEndHeader (List.range 1 numberOfEnds)
                                ++ [ th [ style "width" "50px" ] [ text "Total" ] ]
                            )
                            :: [ viewSideEnds 0 (Tuple.first sidesOrderedForEnds)
                               , viewSideEnds 1 (Tuple.second sidesOrderedForEnds)
                               ]
                        )
                    ]
                , hr [] []
                , div [ class "d-flex justify-content-between" ]
                    [ viewSideOther 0 (Tuple.first sidesOrderedForShots)
                    , viewSideOther 1 (Tuple.second sidesOrderedForShots)
                    ]
                ]
            , div
                [ class "d-flex justify-content-between card-footer" ]
                [ button
                    [ class "btn btn-secondary mr-2"
                    , disabled (game.changed || model.savedGame == Loading)
                    , title
                        (if game.changed then
                            "There are unsaved changes."

                         else
                            "Takes you back to the draw schedule."
                        )
                    , onClick CloseGame
                    ]
                    [ text "Back" ]
                , button
                    [ class "btn btn-primary"
                    , disabled (not game.changed || model.savedGame == Loading)
                    , title
                        (if game.changed then
                            "Saves changes made since opening the game."

                         else
                            "No changes have been made."
                        )
                    , onClick SaveGame
                    ]
                    [ text "Save" ]
                ]
            ]
        ]


viewShots : Int -> Side -> Int -> Game -> Html Msg
viewShots sideIndex side focusedEndNumber game =
    let
        viewShot : Shot -> Html Msg
        viewShot shot =
            let
                startingTabIndex =
                    let
                        tabsUsedByEndScores =
                            -- 2 tabs for each end.
                            -- If we're on the first end, that's (1 * 2) = 0
                            -- If we're on the second end, that's (2 * 2) = 4
                            -- etc.
                            shot.endNumber * 2
                    in
                    tabsUsedByEndScores
                        -- Each row
                        + ((shot.shotNumber - 1) * 6)
                        -- Which side
                        + (sideIndex * 3)
                        -- 1 based instead of 0 based
                        + 1

                viewCurlerOptions selectedCurlerId =
                    let
                        viewCurlerOption curlerIndex teamCurler =
                            let
                                isSelected =
                                    selectedCurlerId == teamCurler.curlerId
                            in
                            -- For some reason, in order to reflect which curler is selected in
                            -- shots data we need to force a second render on end number focus change.
                            option
                                [ selected isSelected
                                , value (String.fromInt teamCurler.curlerId)
                                ]
                                [ text teamCurler.name ]
                    in
                    side.teamCurlers
                        |> List.indexedMap viewCurlerOption

                saveableShot =
                    game.changed
                        && shot.curlerId
                        /= Nothing
                        && shot.turn
                        /= Nothing
                        && shot.throw
                        /= Nothing
                        && shot.rating
                        /= Nothing
            in
            tr
                [ class
                    (case List.Extra.find (\tc -> Just tc.curlerId == shot.curlerId) side.teamCurlers of
                        Just teamCurler ->
                            if teamCurler.delivery == Just "left" then
                                "border-left border-3 border-secondary"

                            else
                                ""

                        Nothing ->
                            ""
                    )
                ]
                [ td []
                    [ select
                        [ class "shot-curler mr-1 form-control"
                        , onInput (UpdateShotCurlerId side shot)
                        , id ("tab" ++ String.fromInt (startingTabIndex + 9000))
                        , tabindex (startingTabIndex + 9000)
                        ]
                        (viewCurlerOptions (Maybe.withDefault -1 shot.curlerId))
                    ]
                , td []
                    [ input
                        [ class "shot-turn mr-1 text-center form-control"
                        , value (Maybe.withDefault "" shot.turn)
                        , onInput (UpdateShotTurn side shot)
                        , id ("tab" ++ String.fromInt startingTabIndex)
                        , tabindex startingTabIndex
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ class "shot-throw mr-1 text-center form-control"
                        , value (Maybe.withDefault "" shot.throw)
                        , onInput (UpdateShotThrow side shot)
                        , id ("tab" ++ String.fromInt (startingTabIndex + 1))
                        , tabindex (startingTabIndex + 1)
                        ]
                        []
                    ]
                , td []
                    [ input
                        [ class "shot-rating mr-1 text-center form-control"
                        , value (Maybe.withDefault "" shot.rating)
                        , onInput (UpdateShotRating side shot)
                        , onBlur
                            (if saveableShot then
                                SaveGame

                             else
                                NoOp
                            )
                        , id ("tab" ++ String.fromInt (startingTabIndex + 2))
                        , tabindex (startingTabIndex + 2)
                        ]
                        []
                    ]
                ]

        shots =
            case side.shots of
                Just shots_ ->
                    shots_
                        |> List.filter (\s -> s.endNumber == focusedEndNumber)

                Nothing ->
                    []
    in
    div [ class "mt-4 mb-2" ]
        [ h5 [] [ text ("Shots - End " ++ String.fromInt focusedEndNumber) ]
        , if List.isEmpty side.teamCurlers then
            div [ class "alert alert-danger m-1" ] [ text "No curlers have been assigned. You must first assign curlers to the team before you can record shot by shot." ]

          else
            table [ class "mt-2 table table-sm" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Curler" ]
                        , th [ class "text-center" ] [ text "Turn" ]
                        , th [ class "text-center" ] [ text "Throw" ]
                        , th [ class "text-center" ] [ text "Rate" ]
                        ]
                    ]
                , tbody [] (List.map viewShot shots)
                ]
        , hr [] []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- PORTS


port sendMessage : String -> Cmd msg



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
