module Types exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import RemoteData exposing (WebData)


type Msg
    = GotData (WebData Data)
    | PatchedGame (WebData Game)
    | SelectedGame Game
    | UpdateGameName Game String
    | UpdateGamePositionScore GamePosition Int
    | UpdateGamePositionResult GamePosition String
    | SaveGame Int


type alias Model =
    { flags : Flags
    , data : WebData Data
    , savedGame : WebData Game
    , selectedGame : Maybe Game
    }


type alias Flags =
    { url : String
    }


type alias Data =
    { settings : Settings
    , draws : List Draw
    , games : List Game
    }


type alias Settings =
    { sheets : List String
    , currentDrawId : Int
    }


type alias Draw =
    { id : Int
    , label : String
    , startsAt : String
    }


type alias Game =
    { id : Int
    , drawId : Int
    , sheet : Int
    , name : String
    , gamePositions : List GamePosition
    }


type alias GamePosition =
    { id : Int
    , teamName : String
    , score : Maybe Int
    , result : Maybe String
    }


dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> required "settings" settingsDecoder
        |> required "draws" (list drawDecoder)
        |> required "games" (list gameDecoder)


settingsDecoder : Decoder Settings
settingsDecoder =
    Decode.succeed Settings
        |> required "sheets" (list string)
        |> required "current_draw_id" int


drawDecoder : Decoder Draw
drawDecoder =
    Decode.succeed Draw
        |> required "id" int
        |> required "label" string
        |> required "starts_at" string


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "id" int
        |> required "draw_id" int
        |> required "sheet" int
        |> required "name" string
        |> required "game_positions" (list gamePositionDecoder)


gamePositionDecoder : Decoder GamePosition
gamePositionDecoder =
    Decode.succeed GamePosition
        |> required "id" int
        |> required "team_name" string
        |> required "score" (nullable int)
        |> required "result" (nullable string)


encodeGame : Game -> Encode.Value
encodeGame game =
    Encode.object
        [ ( "id", Encode.int game.id )
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
        , ( "result", maybe Encode.string gamePosition.result )
        ]
