module PuzzleScript.Rule exposing
    ( into, spawn, kill, constant
    , Direction(..), whileMoving, thenMoving, thenStopping
    , Pattern, touching, onLine, multiLine, singleton
    , Rule, fromPattern, fromTransition, toString
    , Transition, Element, Touching, Line, MultiLine, LineOr
    )

{-| Module for constructing PuzzleScript rules


## Transition

@docs into, spawn, kill, constant


## Moving Transition

@docs Direction, whileMoving, thenMoving, thenStopping


## Pattern

@docs Pattern, touching, onLine, multiLine, singleton


## Rule

@docs Rule, fromPattern, fromTransition, toString


## Internal

@docs Transition, Element, Touching, Line, MultiLine, LineOr

-}


type Direction
    = Up
    | Down
    | Left
    | Right


{-| Internal type.
-}
type alias Element =
    { object : String, direction : Maybe Direction }


{-| Internal type.

Transition from one object to another.

-}
type alias Transition =
    ( Maybe Element, Maybe Element )


{-| PuzzleScript Rule.

Use this module to construct a rule.

-}
type alias Rule =
    List (List (List Transition))


{-| Internal type representing a touching pattern
-}
type alias Touching =
    Tag
        { noLine : ()
        , noMultiLine : ()
        }


{-| Internal type representing a line pattern
-}
type alias Line =
    Tag { noMultiLine : () }


{-| Internal type representing a multiline pattern
-}
type alias MultiLine =
    Tag
        { isMultiline : ()
        }


{-| Internal type representing either a `Line` or `Touching`
-}
type alias LineOr touching =
    Tag { touching | noMultiLine : () }


{-| Internal type.
-}
type Tag a
    = Tag a


{-| Pattern for a PuzzleScript Result.

A rule can be composed of multiple patterns

-}
type Pattern a
    = Pattern
        { touchingPattern : List Transition
        , linePattern : List (List Transition)
        , multiPattern : List (List (List Transition))
        }


{-| constructs a "transition" from a stationary object.

That object will not move.

    "player"
      |> constant
      |> fromTransition
      |> toString
      --> "[ player ] -> [ player ]"

-}
constant : String -> Transition
constant object =
    let
        side =
            { object = object, direction = Nothing }
    in
    ( Just side, Just side )


{-| constructs a transition.

    "player"
      |> into "star"
      |> fromTransition
      |> toString
      --> "[ player ] -> [ star ]"

-}
into : String -> String -> Transition
into to from =
    ( Just { object = from, direction = Nothing }
    , Just { object = to, direction = Nothing }
    )


{-| adds a constant movement to a transition

    "player"
    |> constant
    |> whileMoving Right
    |> fromTransition
    |> toString
    --> "[ > player ] -> [ > player ]"

-}
whileMoving : Direction -> Transition -> Transition
whileMoving direction =
    let
        addMovement part =
            { part | direction = Just direction }
    in
    Tuple.mapBoth (Maybe.map addMovement) (Maybe.map addMovement)


{-| transitions into a new movement.

    "player"
    |> constant
    |> whileMoving Right
    |> thenMoving Left
    |> fromTransition
    |> toString
    --> "[ > player ] -> [ < player ]"

if there was no movement to begin with, then it will start moving

    "player"
    |> constant
    |> thenMoving Left
    |> fromTransition
    |> toString
    --> "[ player ] -> [ < player ]"

-}
thenMoving : Direction -> Transition -> Transition
thenMoving direction =
    let
        addMovement part =
            { part | direction = Just direction }
    in
    Tuple.mapSecond (Maybe.map addMovement)


{-| stops the movement.

    "player"
    |> constant
    |> whileMoving Right
    |> thenStopping
    |> fromTransition
    |> toString
    --> "[ > player ] -> [ player ]"

-}
thenStopping : Transition -> Transition
thenStopping =
    let
        noMovement part =
            { part | direction = Nothing }
    in
    Tuple.mapSecond (Maybe.map noMovement)


{-| constructs a transition into nothing

    "player"
      |> kill
      |> fromTransition
      |> toString
      --> "[ player ] -> []"

-}
kill : String -> Transition
kill from =
    ( Just { object = from, direction = Nothing }, Nothing )


{-| constructs a transition from nothing

    "monster"
      |> spawn
      |> fromTransition
      |> toString
      --> "[] -> [ monster ]"

-}
spawn : String -> Transition
spawn to =
    ( Nothing, Just { object = to, direction = Nothing } )


{-| constructs a pattern of a single object. This is useful in combination with `onLine`.

    constant "player" |> singleton
    --> constant "player" |> List.singleton |> touching

-}
singleton : Transition -> Pattern Touching
singleton =
    List.singleton >> touching


{-| pattern of touching objects

    [ constant "fire", "tree" |> into "fire" ]
      |> touching
      |> fromPattern
      |> toString
      --> "[ fire | tree ] -> [ fire | fire ]"

-}
touching : List Transition -> Pattern Touching
touching touchingPattern =
    Pattern
        { touchingPattern = touchingPattern
        , linePattern = []
        , multiPattern = []
        }


{-| pattern of objects on a straight line

    [ touching [ "player" |> kill, constant "portal"]
    , touching [ constant "portal", "player" |> spawn ]
    ]
      |> onLine
      |> fromPattern
      |> toString
      --> "[ player | portal | ... | portal |  ] -> [  | portal | ... | portal | player ]"

-}
onLine : List (Pattern Touching) -> Pattern Line
onLine list =
    Pattern
        { touchingPattern = []
        , linePattern =
            list
                |> List.map (\(Pattern { touchingPattern }) -> touchingPattern)
        , multiPattern = []
        }


{-| pattern of objects on different lines. These lines do not need to be next to each other.

    [ [ "player" |> kill, constant "portal" ] |> touching
    , [ constant "portal", "player" |> spawn ] |> touching
    ]
      |> multiLine
      |> fromPattern
      |> toString
      --> "[ player | portal ][ portal |  ] -> [  | portal ][ portal | player ]"

-}
multiLine : List (Pattern (LineOr touching)) -> Pattern MultiLine
multiLine list =
    Pattern
        { touchingPattern = []
        , linePattern = []
        , multiPattern = list |> List.map toLine
        }


{-| convert any pattern into a rule.

    constant "player"
      |> fromTransition
      |> toString
      --> "[ player ] -> [ player ]"

-}
fromPattern : Pattern pattern -> Rule
fromPattern (Pattern pattern) =
    if pattern.multiPattern /= [] then
        pattern.multiPattern

    else
        Pattern pattern
            |> toLine
            |> List.singleton


{-| convert a transition into a rule.

    constant "player"
        |> fromTransition
        --> constant "player" |> singleton |> fromPattern

-}
fromTransition : Transition -> Rule
fromTransition =
    singleton
        >> fromPattern


{-| converts a rule into a string.

    []
      |> toString
      --> "[] -> []"

-}
toString : Rule -> String
toString pattern =
    let
        ( first, second ) =
            pattern
                |> List.map (List.map List.unzip >> List.unzip)
                |> List.unzip
                |> Tuple.mapBoth singlePatternToString singlePatternToString
    in
    first ++ " -> " ++ second



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


directionToString : Direction -> String
directionToString direction =
    case direction of
        Up ->
            "^"

        Down ->
            "v"

        Left ->
            "<"

        Right ->
            ">"


elementToString : Element -> String
elementToString element =
    [ element.direction
        |> Maybe.map (directionToString >> List.singleton)
        |> Maybe.withDefault []
    , element.object |> List.singleton
    ]
        |> List.concat
        |> String.join " "


singlePatternToString : List (List (List (Maybe Element))) -> String
singlePatternToString list =
    case list of
        [] ->
            "[]"

        _ ->
            list
                |> List.map
                    (\l ->
                        case l of
                            [] ->
                                "[]"

                            [ [] ] ->
                                "[]"

                            [ [ Nothing ] ] ->
                                "[]"

                            _ ->
                                [ "[ "
                                , l
                                    |> List.map
                                        (List.map
                                            (Maybe.map elementToString
                                                >> Maybe.withDefault ""
                                            )
                                            >> String.join " | "
                                        )
                                    |> String.join " | ... | "
                                , " ]"
                                ]
                                    |> String.concat
                    )
                |> String.join ""


toLine : Pattern pattern -> List (List Transition)
toLine (Pattern pattern) =
    if pattern.linePattern /= [] then
        pattern.linePattern

    else
        pattern.touchingPattern |> List.singleton
