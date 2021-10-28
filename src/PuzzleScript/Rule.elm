module PuzzleScript.Rule exposing
    ( into, constant
    , Pattern
    , Rule
    , Transition, Touching, Line, MultiLine
    , fromPattern, multiLine, onLine, toString, touching
    )

{-| Module for constructing PuzzleScript rules


## Transition

@docs into, constant


## Pattern

@docs Pattern


## Rule

@docs Rule


## Internal

@docs Transition, Touching, Line, MultiLine

-}


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
type Touching
    = Touching


{-| Internal type representing a line pattern
-}
type Line
    = Line


{-| Internal type representing a multiline pattern
-}
type MultiLine
    = MultiLine


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


onLine : List (Pattern Touching) -> Pattern Line
onLine list =
    Pattern
        { touchingPattern = []
        , linePattern =
            list
                |> List.map (\(Pattern { touchingPattern }) -> touchingPattern)
        , multiPattern = []
        }


multiLine : List (Pattern Line) -> Pattern MultiLine
multiLine list =
    Pattern
        { touchingPattern = []
        , linePattern = []
        , multiPattern =
            list
                |> List.map (\(Pattern { linePattern }) -> linePattern)
        }


fromPattern : Pattern pattern -> Rule
fromPattern (Pattern pattern) =
    if pattern.multiPattern |> List.isEmpty then
        if pattern.linePattern |> List.isEmpty then
            pattern.touchingPattern |> List.singleton |> List.singleton

        else
            pattern.linePattern |> List.singleton

    else
        pattern.multiPattern


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
    list
        |> List.map
            (\l ->
                [ "[ "
                , l
                    |> List.map
                        (List.map (Maybe.withDefault " ")
                            >> String.join " | "
                        )
                    |> String.join " | ... | "
                , " ]"
                ]
                    |> String.concat
            )
        |> String.join " "
