module Internal.Rule exposing (Element, Rule, Transition)

import Internal.Direction as Direction exposing (DirSort(..), Direction(..), ExplicitDirection(..), NormalDirection(..), RelativeDirection(..))


type alias Element a =
    { object : String, direction : Maybe (Direction a) }


type alias Transition a =
    ( Maybe (Element a), Maybe (Element a) )


type alias Rule a b =
    { pattern : List (List (List (List (Transition a))))
    , lateEvaluation : Bool
    , directionalEvaluation : b
    }


mapDirection : (Direction a -> Direction b) -> Element a -> Element b
mapDirection fun element =
    { object = element.object
    , direction = element.direction |> Maybe.map fun
    }


instantiateTransition : NormalDirection -> Transition {} -> Transition (Direction.NonRelative {})
instantiateTransition direction =
    let
        f =
            Maybe.map
                (mapDirection
                    (\(Direction dir) ->
                        case dir.sort of
                            RelativeDirSort ->
                                dir.relative |> Direction.instantiate direction

                            _ ->
                                Direction dir
                    )
                )
    in
    Tuple.mapBoth f f


applyRight : Rule {} (Maybe (Direction.NonRelative {})) -> Rule (Direction.NonRelative {}) NormalDirection
applyRight =
    applyNormal Right


applyDown : Rule {} (Maybe (Direction.NonRelative {})) -> Rule (Direction.NonRelative {}) NormalDirection
applyDown =
    applyNormal Down


reverse : Rule {} (Maybe (Direction (Direction.NonRelative {}))) -> Rule {} (Maybe (Direction (Direction.NonRelative {})))
reverse rule =
    let
        fun dir =
            { pattern = rule.pattern |> Debug.todo "reverse"
            , directionalEvaluation = dir |> Direction.fromNormal |> Direction.unsafe |> Just
            , lateEvaluation = rule.lateEvaluation
            }
    in
    case rule.directionalEvaluation |> Maybe.map (\(Direction d) -> ( d.sort, d.explicit )) of
        Just ( Direction.ExplicitDirSort, Left ) ->
            fun Right

        Just ( Direction.ExplicitDirSort, Up ) ->
            fun Down

        _ ->
            rule


applyNormal : NormalDirection -> Rule {} (Maybe (Direction.NonRelative {})) -> Rule (Direction.NonRelative {}) NormalDirection
applyNormal directionalEvaluation rule =
    { pattern =
        rule.pattern
            |> List.map
                (List.map
                    (List.map
                        (List.map (instantiateTransition directionalEvaluation))
                    )
                )
    , directionalEvaluation = directionalEvaluation
    , lateEvaluation = rule.lateEvaluation
    }


normalizeRule : Rule {} (Maybe (Direction (Direction.NonRelative {}))) -> Rule (Direction.NonRelative {}) NormalDirection
normalizeRule rule =
    case rule.directionalEvaluation of
        _ ->
            Debug.todo "implement remaining direction"
