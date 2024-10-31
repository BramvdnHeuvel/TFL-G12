module Main exposing (main)

{-|


# Main module

This module is the main module that gets compiled to the single HTML file.

It contains a complete web application.

@docs main

-}

import Browser
import Browser.Dom
import Browser.Events
import Element
import Element.Background
import Screens.Dialog as Dialog
import Screens.Farm as Farm
import Screens.Town as Town
import Screens.Welcome as Welcome
import Task
import Theme


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { flavor : Theme.Flavor
    , height : Int
    , screen : Screen
    , width : Int
    }


type Msg
    = OnClickCastle
    | OnClickHouses
    | OnClickPope
    | OnClickSeedSeller
    | OnDialogScreen Dialog.Msg
    | OnPigScreen Farm.Msg
    | OnTownScreen Town.Msg
    | ScreenSize { height : Int, width : Int }
    | StartGame


type Screen
    = DialogScreen Dialog.Model
    | EmptyScreen
    | PigScreen Farm.Model
    | TownScreen Town.Model
    | WelcomeScreen


init : () -> ( Model, Cmd Msg )
init () =
    ( { flavor = Theme.Latte
      , height = 480
      , screen = TownScreen Town.init
      , width = 720
      }
    , Browser.Dom.getViewport
        |> Task.perform
            (\viewport ->
                ScreenSize
                    { height = floor viewport.viewport.height
                    , width = floor viewport.viewport.width
                    }
            )
        |> List.singleton
        |> Cmd.batch
    )



-- UPDATE


loadDialog : String -> String -> ( Model, Cmd Msg )
loadDialog face source =
    case Dialog.init face source of
        ( mdl, cmd ) ->
            ( { flavor = Theme.Latte
              , height = 480
              , screen = DialogScreen mdl
              , width = 720
              }
            , Browser.Dom.getViewport
                |> Task.perform
                    (\viewport ->
                        ScreenSize
                            { height = floor viewport.viewport.height
                            , width = floor viewport.viewport.width
                            }
                    )
                |> List.singleton
                |> List.append [ Cmd.map OnDialogScreen cmd ]
                |> Cmd.batch
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnClickSeedSeller ->
            loadDialog "/static/villager_image.png" "seed_seller.txt"

        OnClickCastle ->
            loadDialog "/static/king.png" "test.txt"

        OnClickPope ->
            loadDialog "/static/villager_image.png" "seed_seller.txt"

        OnClickHouses ->
            loadDialog "/static/villager_image.png" "seed_seller.txt"

        OnDialogScreen m ->
            case model.screen of
                DialogScreen mdl ->
                    case Dialog.update m mdl of
                        ( newMdl, cmd ) ->
                            case newMdl.dialog of
                                Just _ ->
                                    ( { model | screen = DialogScreen newMdl }
                                    , Cmd.map OnDialogScreen cmd
                                    )

                                Nothing ->
                                    ( { model | screen = TownScreen Town.init }
                                    , Cmd.none
                                    )

                _ ->
                    ( model, Cmd.none )

        OnPigScreen m ->
            case model.screen of
                PigScreen mdl ->
                    case Farm.update m mdl of
                        ( newMdl, cmd ) ->
                            ( { model | screen = PigScreen newMdl }
                            , Cmd.map OnPigScreen cmd
                            )

                _ ->
                    ( model, Cmd.none )

        OnTownScreen m ->
            case model.screen of
                TownScreen mdl ->
                    case Town.update m mdl of
                        ( newMdl, cmd ) ->
                            ( { model | screen = TownScreen newMdl }
                            , Cmd.map OnTownScreen cmd
                            )

                _ ->
                    ( model, Cmd.none )

        ScreenSize { height, width } ->
            ( { model | height = height, width = width }
            , Cmd.none
            )

        StartGame ->
            ( { model | screen = PigScreen Farm.init }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> ScreenSize { width = w, height = h })
        , case model.screen of
            PigScreen mdl ->
                Farm.subscriptions mdl |> Sub.map OnPigScreen

            TownScreen mdl ->
                Town.subscriptions mdl |> Sub.map OnTownScreen

            _ ->
                Sub.none
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "TFL - G12"
    , body =
        (case model.screen of
            DialogScreen mdl ->
                Dialog.view
                    { buttonColor = Theme.surface0 model.flavor
                    , errorBoxColor = Theme.red model.flavor
                    , height = model.height
                    , model = mdl
                    , textBoxColor = Theme.mantle model.flavor
                    , toMsg = OnDialogScreen
                    , width = model.width
                    }

            EmptyScreen ->
                Element.none

            PigScreen mdl ->
                Farm.view
                    { height = model.height
                    , model = mdl
                    , toMsg = OnPigScreen
                    , width = model.width
                    }

            TownScreen mdl ->
                Town.view
                    { height = model.height
                    , model = mdl
                    , onClickSeedSeller = OnClickSeedSeller
                    , onClickCastle = OnClickCastle
                    , onClickPope = OnClickPope
                    , onClickHouses = OnClickHouses
                    , toMsg = OnTownScreen
                    , width = model.width
                    }

            WelcomeScreen ->
                Welcome.view
                    { backgroundColor = Theme.base model.flavor
                    , boxColor = Theme.mantle model.flavor
                    , buttonColor = Theme.mauve model.flavor
                    , height = model.height
                    , onStart = StartGame
                    , width = model.width
                    }
        )
            |> Element.layout
                [ Element.Background.color (Theme.greenUI model.flavor)
                ]
            |> List.singleton
    }
