module PuzzleScript.Rule exposing
    ( into, spawn, kill, constant
    , Pattern, touching, onLine, multiLine, fromPattern
    , singleton, toString
    , Rule
    , Transition, Touching, Line, MultiLine, LineOr
    )

{-| Module for constructing PuzzleScript rules


## Transition

@docs into, spawn, kill, constant


## Pattern

@docs Pattern, touching, onLine, multiLine, fromPattern


## Advanced

@docs singleton, toString


## Rule

@docs Rule


## Internal

@docs Transition, Touching, Line, MultiLine, LineOr

-}


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Element =
    { object : String, direction : Direction }


{-| Internal type.

Transition from one object to another.

-}
type alias Transition =
    ( Maybe String, Maybe String )


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

-}
constant : String -> Transition
constant c =
    ( Just c, Just c )


{-| constructs a transition.

    "player" |> into "star" --> ( Just "player", Just "star" )

-}
into : String -> String -> Transition
into to from =
    ( Just from, Just to )


{-| constructs a transition into nothing

    "player" |> kill --> (Just "player", Nothing )

-}
kill : String -> Transition
kill from =
    ( Just from, Nothing )


{-| constructs a transition from nothing

    "monster" |> spawn --> (Nothing, Just "monster")

-}
spawn : String -> Transition
spawn to =
    ( Nothing, Just to )


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
      |> singleton
      |> fromPattern
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


singlePatternToString : List (List (List (Maybe String))) -> String
singlePatternToString list =
    if list == [] then
        "[]"

    else
        list
            |> List.map
                (\l ->
                    [ "[ "
                    , l
                        |> List.map
                            (List.map (Maybe.withDefault "")
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
