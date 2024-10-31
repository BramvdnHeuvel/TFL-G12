module Screens.Dialog exposing (..)

import Color exposing (Color)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events
import Layout
import Parser as P
import Scripts.DownloadScripts as D
import Scripts.ParseScripts as S
import Theme



-- MODEL


type alias Model =
    { face : String, dialog : Maybe S.DialogState, text : String }


type Msg
    = ChooseOption String
    | ClickDialog
    | OnDownloadScript D.Msg
    | SetText String


init : String -> String -> ( Model, Cmd Msg )
init face scriptName =
    ( { face = face, dialog = Nothing, text = "" }
    , D.downloadScript { name = scriptName, toMsg = OnDownloadScript }
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        ChooseOption f ->
            case model.dialog of
                Just d ->
                    if List.any (\( flag, _ ) -> flag == f) (S.view d).options then
                        { model | dialog = S.goto f d }

                    else
                        model

                Nothing ->
                    model

        ClickDialog ->
            case model.dialog of
                Just d ->
                    case (S.view d).options of
                        _ :: _ ->
                            model

                        [] ->
                            { model | dialog = S.nextPiece d }

                Nothing ->
                    model

        OnDownloadScript (Err _) ->
            { model | dialog = Nothing }

        OnDownloadScript (Ok state) ->
            { model | dialog = Just state }

        SetText text ->
            { model
                | dialog =
                    S.fromString text
                        |> Result.toMaybe
                        |> Maybe.map S.fromDialog
                , text = text
            }
    , Cmd.none
    )



-- VIEW


flavor : Theme.Flavor
flavor =
    Theme.Latte



-- or Theme.Latte, Theme.Macchiato, etc.


view :
    { buttonColor : Color
    , errorBoxColor : Color
    , height : Int
    , model : Model
    , textBoxColor : Color
    , toMsg : Msg -> msg
    , width : Int
    }
    -> Element msg
view data =
    let
        topSectionHeight =
            (data.height * 4) // 5

        middleSectionHeight =
            50

        -- Adjust this as needed
        bottomSectionHeight =
            data.height - topSectionHeight - middleSectionHeight

        -- Extract the dialog information
        dialogState =
            case data.model.dialog of
                Just ds ->
                    S.view ds

                Nothing ->
                    { text = "When was columbus born?"
                    , options = [ ( "option1", "1451" ), ( "option2", "1492" ), ( "option3", "1506" ), ( "option4", "1510" ) ]
                    , setTown = Nothing
                    }

        dialogText =
            dialogState.text

        options =
            dialogState.options
    in
    Element.column []
        [ Element.row []
            [ viewNPC { face = data.model.face, height = topSectionHeight, onClick = data.toMsg ClickDialog, width = data.width }
            ]
        , Element.el
            [ Element.height (Element.px middleSectionHeight)
            , Element.width (Element.px data.width)
            , Element.Background.color (Theme.yellowUI flavor)
            ]
            (Element.paragraph [] [(Element.text dialogText)])

        -- Show dialog text here
        , Element.row
            []
            [ viewOptions
                { buttonColor = data.buttonColor
                , model = Just { options = options, text = dialogText } -- Pass options and text
                , toMsg = data.toMsg
                , width = data.width
                }
            ]
        ]


viewOptions : { buttonColor : Color, model : Maybe { options : List ( S.Flag, String ), text : String }, toMsg : Msg -> msg, width : Int } -> Element msg
viewOptions { buttonColor, model, toMsg, width } =
    case model of
        Just dialogState ->
            let
                options =
                    dialogState.options

                -- Access options directly from dialogState
            in
            Element.row []
                (List.map
                    (\( flag, name ) ->
                        Element.el
                            [ Element.Events.onClick (toMsg (ChooseOption flag)) -- Set the click event
                            , Element.height (Element.px 110)
                            , Element.width (Element.px (width // List.length options))
                            , Element.Background.color (Theme.toElmUiColor buttonColor)
                            , Element.Border.rounded 5 -- Optional: rounded corners
                            , Element.Border.color (Theme.yellowUI flavor) -- Set the border color to black
                            , Element.Border.width 2 -- Set the border width
                            , Element.padding 10 -- Optional: padding for the button
                            ]
                            (Element.text name)
                     -- Show the option text on the button
                    )
                    options
                )

        Nothing ->
            Element.none


viewDebugScreen : { height : Int, model : String, textBoxColor : Color, width : Int } -> Element msg
viewDebugScreen data =
    case S.fromString data.model of
        Ok _ ->
            Element.none

        Err e ->
            e
                |> List.map
                    (\deadEnd ->
                        [ Element.text "Line "
                        , Element.el [ Element.Font.bold ] (Element.text (String.fromInt deadEnd.row))
                        , Element.text ", column "
                        , Element.el [ Element.Font.bold ] (Element.text (String.fromInt deadEnd.col))
                        , Element.text ": "
                        , case deadEnd.problem of
                            P.Expecting name ->
                                Element.text ("Expected value \"" ++ name ++ "\"")

                            P.ExpectingInt ->
                                Element.text "Expected a number"

                            P.ExpectingHex ->
                                Element.text "Expected a hex value"

                            P.ExpectingOctal ->
                                Element.text "Expected an octal value"

                            P.ExpectingBinary ->
                                Element.text "Expected a binary value"

                            P.ExpectingFloat ->
                                Element.text "Expected a floating point number"

                            P.ExpectingNumber ->
                                Element.text "Expected a number"

                            P.ExpectingVariable ->
                                Element.text "Expected a variable"

                            P.ExpectingSymbol symbol ->
                                Element.text ("Expected the symbol \"" ++ symbol ++ "\" here")

                            P.ExpectingKeyword keyword ->
                                Element.text ("Expected the keyword \"" ++ keyword ++ "\" here")

                            P.ExpectingEnd ->
                                Element.text "Expected the end of the string there"

                            P.UnexpectedChar ->
                                Element.text "Encountered an unexpected character"

                            P.Problem name ->
                                Element.text ("Encountered a custom problem: " ++ name)

                            P.BadRepeat ->
                                Element.text "Encountered a BadRepeat"
                        ]
                            |> Element.paragraph [ Element.centerX, Element.centerY ]
                    )
                |> Element.column
                    [ Element.Background.color (Theme.toElmUiColor data.textBoxColor)
                    , Element.height (Element.px data.height)
                    , Element.width (Element.px data.width)
                    ]


viewNPC : { face : String, height : Int, onClick : msg, width : Int } -> Element msg
viewNPC data =
    let
        characterImage =
            data.face

        -- Update with the correct path to your uploaded image
        -- Use Element.Background.image to create a background image style
        backgroundStyle =
            Element.Background.image characterImage

        -- Define some padding or margin to space out the dialogue bubble
        padding =
            10
    in
    Element.row [ Element.Events.onClick data.onClick ]
        [ Element.el
            [ Element.height (Element.px data.height)
            , Element.width (Element.px data.width)
            , backgroundStyle
            , Element.Border.rounded 15 -- Optional: rounded corners
            , Element.padding padding
            ]
            (Element.text "")

        -- Pass a single Element here
        ]
