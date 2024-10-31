module Scripts.DownloadScripts exposing (..)

{-|


# Downloading scripts

This module allows you to download a script from the folder with scripts.

-}

import Http
import Parser
import Scripts.ParseScripts exposing (DialogState)


type alias Msg =
    Result Error DialogState


type Error
    = HttpFailure Http.Error
    | ParserFailure (List Parser.DeadEnd)


downloadScript : { name : String, toMsg : Msg -> msg } -> Cmd msg
downloadScript { name, toMsg } =
    Http.get
        { url = "/TFL-G12/scripts/" ++ name
        , expect =
            Result.mapError HttpFailure
                >> Result.andThen (Scripts.ParseScripts.fromString >> Result.mapError ParserFailure)
                >> Result.map Scripts.ParseScripts.fromDialog
                >> toMsg
                |> Http.expectString
        }
