module Sprites.Town exposing (..)

{-| This module hosts the town.
-}

import Color exposing (Color)
import Sprites.Castle as Castle
import Sprites.Farm as Farm
import Sprites.House as House
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MODEL


type alias Model =
    { colorCastleDoor : Color
    , colorCastleWall : Color
    , farm1 : Farm.Model
    , farm2 : Farm.Model
    }


type Msg
    = OnFarm1 Farm.Msg
    | OnFarm2 Farm.Msg


init : Model
init =
    { colorCastleDoor = Castle.defaultDoorColor
    , colorCastleWall = Castle.defaultGrayStoneColor
    , farm1 = Farm.init Farm.defaultInitInput
    , farm2 = Farm.init Farm.defaultInitInput
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFarm1 m ->
            case Farm.update m model.farm1 of
                ( mdl, cmd ) ->
                    ( { model | farm1 = mdl }
                    , Cmd.map OnFarm1 cmd
                    )

        OnFarm2 m ->
            case Farm.update m model.farm2 of
                ( mdl, cmd ) ->
                    ( { model | farm2 = mdl }
                    , Cmd.map OnFarm2 cmd
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Farm.subscriptions model.farm1 |> Sub.map OnFarm1
        , Farm.subscriptions model.farm2 |> Sub.map OnFarm2
        ]



-- VIEW


paddingSize : Int
paddingSize =
    30


view :
    { height : Float
    , model : Model
    , width : Float
    , x : Float
    , y : Float
    }
    -> Svg svg
view data =
    g
        [ transformStandard data ]
        [ house1 { x = 0.8, y = 0.15 }
        , Castle.castle
            { colorDoor = data.model.colorCastleDoor
            , colorStone = data.model.colorCastleWall
            , height = 0.75 * 0.3
            , width = 0.3
            , x = 0.35
            , y = 0.1
            }
        , house1 { x = 0.25, y = 0.225 }
        , house1 { x = 0.05, y = 0.25 }
        , house1 { x = 0.65, y = 0.35 }
        , house1 { x = 0.8, y = 0.4 }
        , house1 { x = 0.3, y = 0.425 }
        , house1 { x = 0.15, y = 0.5 }
        , house1 { x = 0.25, y = 0.55 }
        , Farm.view
            { height = 0.15
            , model = data.model.farm1
            , width = 0.3
            , x = 0.1
            , y = 0.8
            }
        , Farm.view
            { height = 0.15
            , model = data.model.farm2
            , width = 0.3
            , x = 0.65
            , y = 0.7
            }
        , house1 { x = 0.8, y = 0.8 }
        ]


house1 : { x : Float, y : Float } -> Svg svg
house1 { x, y } =
    House.house
        { colorDoor = House.defaultDoorColor
        , colorDoorKnob = House.defaultKnobColor
        , colorRoof = House.defaultRoofColor
        , colorWall = House.defaultWallColor
        , colorWindow = House.defaultWindowColor
        , height = 0.15
        , width = 0.75 * 0.15
        , x = x
        , y = y
        }


transformStandard : { a | height : Float, width : Float, x : Float, y : Float } -> Svg.Attribute msg
transformStandard data =
    [ data.width, 0, 0, data.height, data.x, data.y ]
        |> List.map String.fromFloat
        |> String.join " "
        |> (\coords -> "matrix(" ++ coords ++ ")")
        |> transform
