module Sprites.SelectionBox exposing (..)

import Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events



-- MODEL


type alias Model =
    Bool


type Msg
    = OnHover
    | OnHoverOut


init : Model
init =
    False



-- UPDATE


update : Msg -> Model -> Model
update msg _ =
    case msg of
        OnHover ->
            True

        OnHoverOut ->
            False



-- VIEW


selectionBox :
    { color : Color
    , colorHover : Color
    , height : Float
    , model : Model
    , onClick : msg
    , toMsg : Msg -> msg
    , width : Float
    , x : Float
    , y : Float
    }
    -> Svg msg
selectionBox data =
    rect
        [ height (String.fromFloat data.height)
        , width (String.fromFloat data.width)
        , x (String.fromFloat data.x)
        , y (String.fromFloat data.y)
        , (if data.model then
            data.colorHover

           else
            data.color
          )
            |> Color.toCssString
            |> fill
        , Svg.Events.onClick data.onClick
        , Svg.Events.onMouseOver (data.toMsg OnHover)
        , Svg.Events.onMouseOut (data.toMsg OnHoverOut)
        ]
        []


transformStandard : { a | height : Float, width : Float, x : Float, y : Float } -> Svg.Attribute msg
transformStandard data =
    [ data.width, 0, 0, data.height, data.x, data.y ]
        |> List.map String.fromFloat
        |> String.join " "
        |> (\coords -> "matrix(" ++ coords ++ ")")
        |> transform
