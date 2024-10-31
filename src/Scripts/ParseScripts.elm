module Scripts.ParseScripts exposing (..)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)
import Recursion
import Set


type alias DialogState =
    { current : Dialog
    , flags : Dict Flag Dialog
    }


type alias Dialog =
    List DialogPiece


type DialogPiece
    = Choice Flag String
    | EndOfDialogue
    | EndOfSentence
    | FlagSpot Flag
    | GoTo Flag
    | Message String
    | SetTown String


type alias Flag =
    String


dialogParser : Parser Dialog
dialogParser =
    P.succeed (::)
        |= P.map Message messageParser
        |= P.loop []
            (\items ->
                P.oneOf
                    [ P.succeed
                        (\newItems -> P.Loop (List.append items newItems))
                        |= dialogSectionParser
                    , P.succeed (P.Done items)
                        |. P.end
                    ]
            )


dialogSectionParser : Parser (List DialogPiece)
dialogSectionParser =
    P.succeed identity
        |. P.symbol "["
        |. P.spaces
        |= P.oneOf
            [ P.succeed (\f m -> [ Choice f m ])
                |. P.keyword "choice"
                |. P.spaces
                |= flagParser
                |. P.spaces
                |. P.symbol "]"
                |. P.spaces
                |= messageParser
            , P.succeed (\m -> [ EndOfDialogue, Message m ])
                |. P.keyword "end"
                |. P.spaces
                |. P.symbol "]"
                |. P.spaces
                |= messageParser
            , P.succeed (\f -> [ GoTo f ])
                |. P.keyword "goto"
                |. P.spaces
                |= flagParser
                |. P.spaces
                |. P.symbol "]"
                |. P.spaces
                |. messageParser

            -- , P.succeed (\t m -> [ SetTown t, Message m ])
            --     |. P.keyword "set-town"
            --     |. P.spaces
            --     |= P.variable
            --         { start = Char.isAlpha
            --         , inner = Char.isAlphaNum
            --         , reserved = Set.empty
            --         }
            --     |. P.spaces
            --     |. P.symbol "]"
            --     |. P.spaces
            --     |= messageParser
            -- NOTE: Ignored because nothing happens after a goto
            , P.succeed (\m -> [ EndOfSentence, Message m ])
                |. P.symbol "]"
                |. P.spaces
                |= messageParser
            , P.succeed (\f m -> [ FlagSpot f, Message m ])
                |= flagParser
                |. P.spaces
                |. P.symbol "]"
                |. P.spaces
                |= messageParser
            ]


flagParser : Parser Flag
flagParser =
    P.variable
        { start = Char.isUpper
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


findFlags : Dialog -> Dict Flag Dialog
findFlags =
    Recursion.runRecursion
        (\dialog ->
            case dialog of
                [] ->
                    Recursion.base Dict.empty

                (FlagSpot flag) :: tail ->
                    Recursion.recurseThen tail (Dict.insert flag tail >> Recursion.base)

                _ :: tail ->
                    Recursion.recurse tail
        )


fromDialog : Dialog -> DialogState
fromDialog dialog =
    { current = dialog
    , flags = findFlags dialog
    }


fromString : String -> Result (List P.DeadEnd) Dialog
fromString =
    P.run dialogParser


goto : Flag -> DialogState -> Maybe DialogState
goto flag dialog =
    dialog.flags
        |> Dict.get flag
        |> Maybe.map (\d -> { dialog | current = d })


messageParser : Parser String
messageParser =
    P.chompUntilEndOr "[" |> P.getChompedString


nextPiece : DialogState -> Maybe DialogState
nextPiece =
    Recursion.runRecursion
        (\dialog ->
            case dialog.current of
                EndOfDialogue :: _ ->
                    Recursion.base Nothing

                EndOfSentence :: tail ->
                    Recursion.base (Just { dialog | current = tail })

                (GoTo flag) :: tail ->
                    case goto flag dialog of
                        Just d ->
                            Recursion.recurse d

                        Nothing ->
                            Recursion.recurse { dialog | current = tail }

                _ :: tail ->
                    Recursion.recurse { dialog | current = tail }

                [] ->
                    Recursion.base Nothing
        )


view : DialogState -> { text : String, options : List ( Flag, String ), setTown : Maybe String }
view dialog =
    Recursion.runRecursion
        (\d ->
            case d of
                [] ->
                    Recursion.base { text = "", options = [], setTown = Nothing }

                (Choice flag message) :: tail ->
                    Recursion.recurseThen tail
                        (\data ->
                            Recursion.base { data | options = ( flag, message ) :: data.options }
                        )

                EndOfDialogue :: _ ->
                    Recursion.base { text = "", options = [], setTown = Nothing }

                EndOfSentence :: _ ->
                    Recursion.base { text = "", options = [], setTown = Nothing }

                (FlagSpot _) :: tail ->
                    Recursion.recurse tail

                (GoTo flag) :: tail ->
                    case Dict.get flag dialog.flags of
                        Just newD ->
                            Recursion.recurse newD

                        Nothing ->
                            Recursion.recurse tail

                (Message m) :: tail ->
                    Recursion.recurseThen tail
                        (\data ->
                            Recursion.base { data | text = m ++ "\n" ++ data.text }
                        )

                (SetTown name) :: tail ->
                    Recursion.recurseThen tail
                        (\data ->
                            Recursion.base { data | setTown = Just name }
                        )
        )
        dialog.current
