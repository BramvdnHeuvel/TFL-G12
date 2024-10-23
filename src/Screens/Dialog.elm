module Screens.Dialog exposing (..)

import Color exposing (Color)
import Element exposing (Element)
import Layout
import Scripts.ParseScripts as S
import Html exposing (Html)
import Html.Attributes exposing (style)
import Element.Border
import Element.Background
import Theme
import Element.Events
import Html.Events
import Parser as P
import Element.Font

-- MODEL

type alias Model =
    { dialog : Maybe S.DialogState, text : String }

type Msg
    = ChooseOption String
    | ClickDialog
    | SetText String

init : Model
init = { dialog = Nothing, text = "" }

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        ChooseOption f ->
            case model.dialog of
                Just d ->
                    if List.any (\(flag, _) -> flag == f) (S.view d).options then
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
flavor = Theme.Latte  -- or Theme.Latte, Theme.Macchiato, etc.

view :
    { buttonColor : Color
    , errorBoxColor : Color
    , height : Int
    , model : Model
    , textBoxColor : Color
    , toMsg : Msg -> msg
    , width : Int
    } -> Element msg
view data =
    let
        topSectionHeight = (data.height * 4) // 5
        middleSectionHeight = 50  -- Adjust this as needed
        bottomSectionHeight = data.height - topSectionHeight - middleSectionHeight

        -- Extract the dialog information
        dialogState =
            case data.model.dialog of
                Just ds -> S.view ds
                Nothing -> { text = "When was columbus born?", options = [ ( "option1", "1451" ), ( "option2", "1492" ), ( "option3", "1506" ), ( "option4", "1510" )] }

        dialogText = dialogState.text
        options = dialogState.options
    in
    Element.column []
        [ Element.row []
            [ viewNPC { height = topSectionHeight, width = data.width }
            ]
        , Element.el
            [ Element.height (Element.px middleSectionHeight)
            , Element.width (Element.px data.width)
            , Element.Background.color (Theme.yellowUI flavor)
            ]
            (Element.text dialogText)  -- Show dialog text here
        , Element.row []
            [ viewOptions
                { buttonColor = data.buttonColor
                , model = Just { options = options, text = dialogText }  -- Pass options and text
                , toMsg = data.toMsg
                , width = data.width
                }
            ]
        ]

viewOptions : { buttonColor : Color, model : Maybe { options : List (S.Flag, String), text : String }, toMsg : Msg -> msg, width : Int } -> Element msg
viewOptions { buttonColor, model, toMsg, width } =
    case model of
        Just dialogState ->
            let
                options = dialogState.options  -- Access options directly from dialogState
            in
            Element.row []
                (List.map
                    (\(flag, name) ->
                        Element.el
                            [ Element.Events.onClick (toMsg (ChooseOption flag))  -- Set the click event
                            , Element.height (Element.px 110)
                            , Element.width (Element.px (width // List.length options))
                            , Element.Background.color (Theme.toElmUiColor buttonColor)
                            , Element.Border.rounded 5  -- Optional: rounded corners
                            , Element.Border.color (Theme.yellowUI flavor)  -- Set the border color to black
                            , Element.Border.width 2  -- Set the border width
                            , Element.padding 10  -- Optional: padding for the button
                            ]
                            (Element.text name)  -- Show the option text on the button
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

viewNPC : { height : Int, width : Int } -> Element msg
viewNPC data =
    let
        characterImage = "/Users/ivansladonja/Documents/TFL-G12/src/Images/villager_image.png"  -- Update with the correct path to your uploaded image

        -- Use Element.Background.image to create a background image style
        backgroundStyle =
            Element.Background.image characterImage

        -- Define some padding or margin to space out the dialogue bubble
        padding = 10
    in
    Element.row []
        [ Element.el
            [ Element.height (Element.px data.height)
            , Element.width (Element.px data.width)
            , backgroundStyle
            , Element.Border.rounded 15  -- Optional: rounded corners
            , Element.padding padding
            ]
            (Element.text "")  -- Replace with any text or element
        , Element.el
            [ Element.height (Element.px (data.height - 20))
            , Element.width (Element.px 150)
            , Element.padding padding
            , Element.Border.rounded 10  -- Optional: rounded corners for this element as well
            ]
            (Element.text "This is the character's dialogue.")  -- Pass a single Element here
        ]
