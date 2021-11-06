module PuzzleScript.Rule exposing
    ( into, spawn, kill, constant
    , Direction, turningLeft, turningRight, backwards, forwards, horizontal, vertical, up, down, left, right, stationary
    , whileMoving, thenMoving, thenStopping
    , Pattern, layered, touching, onLine, multiLine
    , Rule, fromPattern, withEval, toString
    , Transition, Element, Tag, Singleton
    , Normalized, Explicit, Relative, Grouped, NonRelative
    , Layered, Touching, Line, MultiLine, LayeredOr, TouchingOrLayeredOr, LineOrTouchingOrLayeredOr
    )

{-| Module for constructing PuzzleScript rules


## Transition

@docs into, spawn, kill, constant


## Direction

@docs Direction, turningLeft, turningRight, backwards, forwards, horizontal, vertical, up, down, left, right, stationary


## Moving Transition

@docs whileMoving, thenMoving, thenStopping


## Pattern

@docs Pattern, layered, touching, onLine, multiLine


## Rule

@docs Rule, fromPattern, withEval, toString


## Internal

@docs Transition, Element, Tag, Singleton
@docs Normalized, Explicit, Relative, Grouped, NonRelative
@docs Layered, Touching, Line, MultiLine, LayeredOr, TouchingOrLayeredOr, LineOrTouchingOrLayeredOr

-}

import Internal.Direction
import Internal.Rule
import Internal.Tag


{-| Directions
-}
type alias Direction direction =
    Internal.Direction.Direction direction


{-| Internal type.
-}
type alias Normalized =
    Direction Internal.Direction.Normalized


{-| Internal type.
-}
type alias Explicit =
    Direction Internal.Direction.Explicit


{-| Internal type.
-}
type alias Relative =
    Direction Internal.Direction.Relative


{-| Internal type.
-}
type alias Grouped =
    Direction Internal.Direction.Grouped


{-| Internal type.
-}
type alias NonRelative dir =
    Direction (Internal.Direction.NonRelative dir)


{-| Internal type.
-}
type alias Element =
    Internal.Rule.Element {}


{-| Internal type.

Transition from one object to another.

-}
type alias Transition =
    Internal.Rule.Transition {}


{-| PuzzleScript Rule.

Use this module to construct a rule.

-}
type alias Rule =
    Internal.Rule.Rule {} (Maybe (Direction (Internal.Direction.NonRelative {})))


{-| Internal type representing a singleton pattern
-}
type alias Singleton =
    Tag
        { noLayers : ()
        , noTouching : ()
        , noLine : ()
        , noMultiLine : ()
        }


{-| Internal type representing a layered pattern
-}
type alias Layered =
    Tag
        { noTouching : ()
        , noLine : ()
        , noMultiLine : ()
        }


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


{-| Internal type representing either a `Line`, `Touching`, `Layered` or a `singleton`
-}
type alias LineOrTouchingOrLayeredOr singleton =
    Tag { singleton | noMultiLine : () }


{-| Internal type representing either a `Touching`, `Layered` or a `singleton`
-}
type alias TouchingOrLayeredOr singleton =
    Tag { singleton | noLine : () }


{-| Internal type representing either a `Layered` or a `singleton`
-}
type alias LayeredOr singleton =
    Tag { singleton | noTouching : () }


{-| Internal type.
-}
type alias Tag a =
    Internal.Tag.Tag a


{-| Pattern for a PuzzleScript Result.

A rule can be composed of multiple patterns

-}
type Pattern a
    = Pattern
        { singletonPattern : Transition
        , layeredPattern : List Transition
        , touchingPattern : List (List Transition)
        , linePattern : List (List (List Transition))
        , multiPattern : List (List (List (List Transition)))
        , hasMovement : Bool
        }


{-| constructs a "transition" from a stationary object.

That object will not move.

    "player"
      |> constant
      |> fromPattern
      |> toString
      --> "LATE [ player ] -> [ player ]"

-}
constant : String -> Pattern Singleton
constant object =
    let
        side =
            { object = object, direction = Nothing }
    in
    ( Just side, Just side )
        |> fromTransition


{-| constructs a transition.

    "player"
      |> into "star"
      |> fromPattern
      |> toString
      --> "LATE [ player ] -> [ star ]"

-}
into : String -> String -> Pattern Singleton
into to from =
    ( Just { object = from, direction = Nothing }
    , Just { object = to, direction = Nothing }
    )
        |> fromTransition


{-| adds a constant movement to a transition

    "player"
    |> constant
    |> whileMoving forwards
    |> fromPattern
    |> toString
    --> "[ > player ] -> [ > player ]"

-}
whileMoving : Direction a -> Pattern Singleton -> Pattern Singleton
whileMoving direction =
    let
        addDirection part =
            { part | direction = Just <| Internal.Direction.unsafe direction }
    in
    mapSingleton (Tuple.mapBoth (Maybe.map addDirection) (Maybe.map addDirection))
        >> addMovement


{-| transitions into a new movement.

    "player"
    |> constant
    |> whileMoving forwards
    |> thenMoving backwards
    |> fromPattern
    |> toString
    --> "[ > player ] -> [ < player ]"

if there was no movement to begin with, then it will start moving

    "player"
    |> constant
    |> thenMoving backwards
    |> fromPattern
    |> toString
    --> "[ player ] -> [ < player ]"

-}
thenMoving : Direction a -> Pattern Singleton -> Pattern Singleton
thenMoving direction =
    let
        addDirection part =
            { part | direction = Just <| Internal.Direction.unsafe direction }
    in
    mapSingleton (Tuple.mapSecond (Maybe.map addDirection))
        >> addMovement


{-| stops the movement.

    "player"
    |> constant
    |> whileMoving forwards
    |> thenStopping
    |> fromPattern
    |> toString
    --> "[ > player ] -> [ player ]"

-}
thenStopping : Pattern Singleton -> Pattern Singleton
thenStopping =
    let
        noMovement part =
            { part | direction = Nothing }
    in
    Tuple.mapSecond (Maybe.map noMovement)
        |> mapSingleton


{-| constructs a transition into nothing

    "player"
      |> kill
      |> fromPattern
      |> toString
      --> "LATE [ player ] -> []"

-}
kill : String -> Pattern Singleton
kill from =
    ( Just { object = from, direction = Nothing }
    , Nothing
    )
        |> fromTransition


{-| constructs a transition from nothing

    "monster"
      |> spawn
      |> fromPattern
      |> toString
      --> "LATE [] -> [ monster ]"

-}
spawn : String -> Pattern Singleton
spawn to =
    ( Nothing, Just { object = to, direction = Nothing } )
        |> fromTransition


{-| pattern of touching objects

    [ kill "player" , kill "mine" ]
      |> layered
      |> fromPattern
      |> toString
      --> "LATE [ player mine ] -> []"

-}
layered : List (Pattern Singleton) -> Pattern Layered
layered list =
    Pattern
        { emptyPattern
            | layeredPattern = list |> List.map toSingleton
            , hasMovement = list |> List.any hasMovement
        }


{-| pattern of touching objects

    [ constant "fire" , "tree" |> into "fire" ]
      |> touching
      |> fromPattern
      |> toString
      --> "LATE [ fire | tree ] -> [ fire | fire ]"

-}
touching : List (Pattern (LayeredOr singleton)) -> Pattern Touching
touching list =
    Pattern
        { emptyPattern
            | touchingPattern = list |> List.map toLayered
            , hasMovement = list |> List.any hasMovement
        }


{-| pattern of objects on a straight line

    [ [ "player" |> kill |> whileMoving forwards
      , constant "portal"
      ]
        |> touching
    , touching [ constant "portal", "player" |> spawn ]
    ]
      |> onLine
      |> fromPattern
      |> toString
      --> "[ > player | portal | ... | portal |  ] -> [  | portal | ... | portal | player ]"

-}
onLine : List (Pattern (TouchingOrLayeredOr singleton)) -> Pattern Line
onLine list =
    Pattern
        { emptyPattern
            | linePattern = list |> List.map toTouching
            , hasMovement = list |> List.any hasMovement
        }


{-| pattern of objects on different lines. These lines do not need to be next to each other.

    [ [ "player" |> kill |> whileMoving forwards
      , constant "portal"
      ]
        |> touching
    , [ constant "portal", "player" |> spawn ] |> touching
    ]
      |> multiLine
      |> fromPattern
      |> toString
      --> "[ > player | portal ][ portal |  ] -> [  | portal ][ portal | player ]"

-}
multiLine : List (Pattern (LineOrTouchingOrLayeredOr singleton)) -> Pattern MultiLine
multiLine list =
    Pattern
        { emptyPattern
            | multiPattern = list |> List.map toLine
            , hasMovement = list |> List.any hasMovement
        }


{-| convert any pattern into a rule.

    constant "player"
      |> whileMoving left
      |> thenStopping
      |> fromPattern
      |> toString
      --> "[ LEFT player ] -> [ player ]"

-}
fromPattern : Pattern pattern -> Rule
fromPattern (Pattern pattern) =
    { pattern =
        if pattern.multiPattern /= [] then
            pattern.multiPattern

        else
            Pattern pattern
                |> toLine
                |> List.singleton
    , lateEvaluation = not pattern.hasMovement
    , directionalEvaluation = Nothing
    }


{-| converts a rule into a string.

    []
      |> multiLine
      |> fromPattern
      |> toString
      --> "LATE [] -> []"

-}
toString : Rule -> String
toString rule =
    let
        mapUnzip : (a -> ( b, c )) -> List a -> ( List b, List c )
        mapUnzip f =
            List.map f
                >> List.unzip

        ( first, second ) =
            rule.pattern
                |> mapUnzip (mapUnzip (mapUnzip List.unzip))
                |> Tuple.mapBoth singlePatternToString singlePatternToString
    in
    (if rule.lateEvaluation then
        "LATE "

     else
        ""
    )
        ++ (rule.directionalEvaluation
                |> Maybe.map (\movement -> Internal.Direction.toString movement ++ " ")
                |> Maybe.withDefault ""
           )
        ++ first
        ++ " -> "
        ++ second


{-| Restrict the evaluation to a specific set of directions

    constant "player"
    |> whileMoving forwards
    |> fromPattern
    |> withEval vertical
    |> toString
    --> "VERTICAL [ > player ] -> [ > player ]"

-}
withEval : Internal.Direction.Direction (Internal.Direction.NonRelative dir) -> Rule -> Rule
withEval dir rule =
    { rule
        | directionalEvaluation =
            dir
                |> Internal.Direction.fromNonRelative
                |> Internal.Direction.unsafe
                |> Just
    }



--------------------------------------------------------------------------------
-- Direction
--------------------------------------------------------------------------------


{-| -}
turningLeft : Internal.Direction.Direction Internal.Direction.Relative
turningLeft =
    Internal.Direction.fromRelative Internal.Direction.TurningLeft


{-| -}
turningRight : Internal.Direction.Direction Internal.Direction.Relative
turningRight =
    Internal.Direction.fromRelative Internal.Direction.TurningRight


{-| -}
backwards : Internal.Direction.Direction Internal.Direction.Relative
backwards =
    Internal.Direction.fromRelative Internal.Direction.Backwards


{-| -}
forwards : Internal.Direction.Direction Internal.Direction.Relative
forwards =
    Internal.Direction.fromRelative Internal.Direction.Forwards


{-| -}
stationary : Internal.Direction.Direction Internal.Direction.Idle
stationary =
    Internal.Direction.fromIdle Internal.Direction.Stationary


{-| -}
horizontal : Internal.Direction.Direction Internal.Direction.Grouped
horizontal =
    Internal.Direction.fromGroup Internal.Direction.Horizontal


{-| -}
vertical : Internal.Direction.Direction Internal.Direction.Grouped
vertical =
    Internal.Direction.fromGroup Internal.Direction.Vertical


{-| -}
up : Internal.Direction.Direction Internal.Direction.Explicit
up =
    Internal.Direction.fromExplicit Internal.Direction.Up


{-| -}
down : Internal.Direction.Direction Internal.Direction.Normalized
down =
    Internal.Direction.fromNormal Internal.Direction.Down


{-| -}
left : Internal.Direction.Direction Internal.Direction.Explicit
left =
    Internal.Direction.fromExplicit Internal.Direction.Left


{-| -}
right : Internal.Direction.Direction Internal.Direction.Normalized
right =
    Internal.Direction.fromNormal Internal.Direction.Right



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


elementToString : Element -> String
elementToString element =
    [ element.direction
        |> Maybe.map (Internal.Direction.toString >> List.singleton)
        |> Maybe.withDefault []
    , element.object |> List.singleton
    ]
        |> List.concat
        |> String.join " "


mapJoin : String -> (a -> String) -> List a -> String
mapJoin sep fun =
    List.map fun
        >> String.join sep


singlePatternToString : List (List (List (List (Maybe Element)))) -> String
singlePatternToString list =
    case list of
        [] ->
            "[]"

        _ ->
            list
                |> List.map
                    (mapJoin " | ... | "
                        (mapJoin " | "
                            (List.filterMap identity
                                >> mapJoin " "
                                    elementToString
                            )
                        )
                        >> (\l ->
                                if l == "" then
                                    "[]"

                                else
                                    [ "[ "
                                    , l
                                    , " ]"
                                    ]
                                        |> String.concat
                           )
                    )
                |> String.join ""


toLine : Pattern pattern -> List (List (List Transition))
toLine (Pattern pattern) =
    if pattern.linePattern /= [] then
        pattern.linePattern

    else
        Pattern pattern
            |> toTouching
            |> List.singleton


toTouching : Pattern pattern -> List (List Transition)
toTouching (Pattern pattern) =
    if pattern.touchingPattern /= [] then
        pattern.touchingPattern

    else
        Pattern pattern
            |> toLayered
            |> List.singleton


toLayered : Pattern pattern -> List Transition
toLayered (Pattern pattern) =
    if pattern.layeredPattern /= [] then
        pattern.layeredPattern

    else
        Pattern pattern
            |> toSingleton
            |> List.singleton


toSingleton : Pattern pattern -> Transition
toSingleton (Pattern pattern) =
    pattern.singletonPattern


fromTransition : Transition -> Pattern Singleton
fromTransition transition =
    Pattern { emptyPattern | singletonPattern = transition }


mapSingleton : (Transition -> Transition) -> Pattern Singleton -> Pattern Singleton
mapSingleton fun (Pattern pattern) =
    Pattern { pattern | singletonPattern = fun pattern.singletonPattern }


addMovement : Pattern pattern -> Pattern pattern
addMovement (Pattern pattern) =
    Pattern
        { pattern | hasMovement = True }


hasMovement : Pattern pattern -> Bool
hasMovement (Pattern pattern) =
    pattern.hasMovement


emptyPattern :
    { singletonPattern : Transition
    , layeredPattern : List Transition
    , touchingPattern : List (List Transition)
    , linePattern : List (List (List Transition))
    , multiPattern : List (List (List (List Transition)))
    , hasMovement : Bool
    }
emptyPattern =
    { singletonPattern = ( Nothing, Nothing )
    , layeredPattern = []
    , touchingPattern = []
    , linePattern = []
    , multiPattern = []
    , hasMovement = False
    }
