module Helpers exposing (..)

import List.Extra
import RemoteData exposing (RemoteData(..))
import RemoteData.Http
import Types exposing (..)


getData : String -> Cmd Msg
getData url =
    RemoteData.Http.get url GotData dataDecoder


patchGame : String -> Game -> Cmd Msg
patchGame url game =
    RemoteData.Http.patch (url ++ "/" ++ String.fromInt game.id) PatchedGame gameDecoder (encodeGame game)


findGame : List Game -> Int -> Int -> Maybe Game
findGame games drawId sheet =
    List.Extra.find (\game -> game.drawId == drawId && game.sheet == sheet) games


findGameByName : List Game -> Int -> String -> Maybe Game
findGameByName games excludeId name =
    List.Extra.find (\game -> game.id /= excludeId && game.name == name) games


findDraw : List Draw -> Int -> Maybe Draw
findDraw draws drawId =
    List.Extra.find (\draw -> draw.id == drawId) draws
