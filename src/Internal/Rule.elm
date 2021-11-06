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


instantiateTransition : Transition {} -> Transition (Direction.NonRelative {})
instantiateTransition =
    let
        f =
            Maybe.map
                (mapDirection
                    (\(Direction dir) ->
                        case dir.sort of
                            RelativeDirSort ->
                                dir.relative |> Direction.instantiate

                            _ ->
                                Direction dir
                    )
                )
    in
    Tuple.mapBoth f f


applyRight : Rule {} (Maybe ExplicitDirection) -> Rule (Direction.NonRelative {}) NormalDirection
applyRight rule =
    { pattern =
        rule.pattern
            |> List.map
                (List.map
                    (List.map
                        (List.map instantiateTransition)
                    )
                )
    , directionalEvaluation = Right
    , lateEvaluation = rule.lateEvaluation
    }


normalizeRule : Rule {} (Maybe ExplicitDirection) -> Rule (Direction.NonRelative {}) NormalDirection
normalizeRule rule =
    case rule.directionalEvaluation of
        _ ->
            Debug.todo "implement remaining direction"
