module Main exposing (main)

import Browser
import Element
import Element.Font as Font
import Html exposing (Html)
import PuzzleScript exposing (PuzzleScript)
import PuzzleScript.Rule as Rule


type alias Model =
    ()


type alias Msg =
    ()


script : PuzzleScript
script =
    { title = "Simple Block Pushing Game"
    , author = "David Skinner"
    , homepage = "www.puzzlescript.net"
    , objects = """Background
LIGHTGREEN GREEN
11111
01111
11101
11111
10111


Target
DarkBlue
.....
.000.
.0.0.
.000.
.....

Wall
BROWN DARKBROWN
00010
11111
01000
11111
00010

Player
Black Orange White Blue
.000.
.111.
22222
.333.
.3.3.

Crate
Orange
00000
0...0
0...0
0...0
00000"""
    , legend = """. = Background
# = Wall
P = Player
* = Crate
@ = Crate and Target
O = Target"""
    , sounds = """Crate MOVE 36772507"""
    , collisionlayers = """Background
Target
Player, Wall, Crate"""
    , rules =
        [ Rule.constant "Player"
            |> Rule.whileMoving Rule.forwards
        , Rule.constant "Crate"
            |> Rule.thenMoving Rule.forwards
        ]
            |> Rule.touching
            |> Rule.fromPattern
            |> List.singleton
    , winconditions = """All Target on Crate"""
    , levels = """####..
#.O#..
#..###
#@P..#
#..*.#
#..###
####..


######
#....#
#.#P.#
#.*@.#
#.O@.#
#....#
######"""
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( ()
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    script
        |> PuzzleScript.toString
        |> Element.text
        |> List.singleton
        |> Element.textColumn [ Font.family [ Font.monospace ], Font.size 12, Element.padding 20 ]
        |> Element.layout []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
