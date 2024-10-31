module Screens.Town exposing (..)

import Element exposing (Element)
import Layout
import Sprites.Town as Town



-- MODEL


type alias Model =
    Town.Model


type alias Msg =
    Town.Msg


init : Model
init =
    Town.init



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Town.update



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    Town.subscriptions



-- VIEW


paddingSize : Int
paddingSize =
    30


view :
    { height : Int
    , model : Model
    , onClickCastle : msg
    , toMsg : Msg -> msg
    , width : Int
    }
    -> Element msg
view data =
    Layout.svg
        { aspectRatio = 1 / 1
        , height = data.height - 2 * paddingSize
        , svg =
            Town.view
                { height = 1
                , model = data.model
                , onClickCastle = data.onClickCastle
                , toMsg = data.toMsg
                , width = 1
                , x = 0
                , y = 0
                }
        , viewMinX = 0
        , viewMaxX = 1
        , viewMinY = 0
        , viewMaxY = 1
        , width = data.width - 2 * paddingSize
        }
        |> Element.el [ Element.padding paddingSize ]
