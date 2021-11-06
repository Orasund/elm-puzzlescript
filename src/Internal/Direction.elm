module Internal.Direction exposing (DirSort(..), Direction(..), Explicit, ExplicitDirection(..), GroupDirection(..), Grouped, Idle, IdleDirection(..), NonRelative, NormalDirection(..), Normalized, Relative, RelativeDirection(..), explicitDirectionToString, fromExplicit, fromGroup, fromIdle, fromNonRelative, fromNormal, fromRelative, instantiate, toExplicit, toGroup, toIdle, toNormal, toRelative, toString, unsafe)

import Internal.Tag exposing (Tag(..))


type DirSort
    = NormalDirSort
    | RelativeDirSort
    | ExplicitDirSort
    | GroupDirSort
    | IdleDirSort


type alias Idle =
    Tag
        { isNotRelative : ()
        , contains : IdleDirection
        }


type alias Normalized =
    Tag
        { isNotRelative : ()
        , contains : NormalDirection
        }


type alias Relative =
    Tag
        { contains : RelativeDirection }


type alias Explicit =
    Tag
        { isNotRelative : ()
        , contains : ExplicitDirection
        }


type alias Grouped =
    Tag
        { isNotRelative : ()
        , contains : GroupDirection
        }


type alias NonRelative dir =
    Tag
        { dir | isNotRelative : () }


type NormalDirection
    = Down
    | Right


type IdleDirection
    = Stationary


type RelativeDirection
    = TurningLeft
    | TurningRight
    | Backwards
    | Forwards


type ExplicitDirection
    = Up
    | Left


type GroupDirection
    = Horizontal
    | Vertical


type Direction direction
    = Direction
        { group : GroupDirection
        , relative : RelativeDirection
        , explicit : ExplicitDirection
        , normal : NormalDirection
        , idle : IdleDirection
        , sort : DirSort
        }


instantiate : RelativeDirection -> Direction (NonRelative {})
instantiate dir =
    case dir of
        TurningLeft ->
            Up |> fromExplicit |> unsafe

        TurningRight ->
            Down |> fromNormal |> unsafe

        Backwards ->
            Left |> fromExplicit |> unsafe

        Forwards ->
            Right |> fromNormal |> unsafe


unsafe : Direction a -> Direction b
unsafe (Direction dir) =
    Direction dir


fromNonRelative : Direction (NonRelative dir) -> Direction (NonRelative {})
fromNonRelative (Direction dir) =
    Direction dir


fromRelative : RelativeDirection -> Direction Relative
fromRelative dir =
    Direction
        { internalDefault
            | relative = dir
            , sort = RelativeDirSort
        }


toIdle : Direction Idle -> IdleDirection
toIdle (Direction dir) =
    dir.idle


fromIdle : IdleDirection -> Direction Idle
fromIdle dir =
    Direction
        { internalDefault
            | idle = dir
            , sort = IdleDirSort
        }


toRelative : Direction Relative -> RelativeDirection
toRelative (Direction dir) =
    dir.relative


fromExplicit : ExplicitDirection -> Direction Explicit
fromExplicit dir =
    Direction
        { internalDefault
            | explicit = dir
            , sort = ExplicitDirSort
        }


toExplicit : Direction Explicit -> ExplicitDirection
toExplicit (Direction dir) =
    dir.explicit


fromNormal : NormalDirection -> Direction Normalized
fromNormal dir =
    Direction
        { internalDefault
            | normal = dir
            , sort = NormalDirSort
        }


toNormal : Direction Normalized -> NormalDirection
toNormal (Direction dir) =
    dir.normal


fromGroup : GroupDirection -> Direction Grouped
fromGroup dir =
    Direction
        { internalDefault
            | group = dir
            , sort = GroupDirSort
        }


toGroup : Direction Grouped -> GroupDirection
toGroup (Direction dir) =
    dir.group


internalDefault :
    { relative : RelativeDirection
    , explicit : ExplicitDirection
    , normal : NormalDirection
    , group : GroupDirection
    , idle : IdleDirection
    , sort : DirSort
    }
internalDefault =
    { relative = Forwards
    , explicit = Left
    , normal = Right
    , group = Horizontal
    , idle = Stationary
    , sort = NormalDirSort
    }


normalDirectionToString : NormalDirection -> String
normalDirectionToString dir =
    case dir of
        Down ->
            "DOWN"

        Right ->
            "RIGHT"


explicitDirectionToString : ExplicitDirection -> String
explicitDirectionToString dir =
    case dir of
        Up ->
            "UP"

        Left ->
            "LEFT"


groupDirectionToString : GroupDirection -> String
groupDirectionToString dir =
    case dir of
        Horizontal ->
            "HORIZONTAL"

        Vertical ->
            "VERTICAL"


relativeDirectionToString : RelativeDirection -> String
relativeDirectionToString dir =
    case dir of
        TurningLeft ->
            "^"

        TurningRight ->
            "v"

        Backwards ->
            "<"

        Forwards ->
            ">"


idleDirectionToString : IdleDirection -> String
idleDirectionToString dir =
    case dir of
        Stationary ->
            "STATIONARY"


toString : Direction a -> String
toString (Direction direction) =
    case direction.sort of
        NormalDirSort ->
            normalDirectionToString direction.normal

        ExplicitDirSort ->
            explicitDirectionToString direction.explicit

        RelativeDirSort ->
            relativeDirectionToString direction.relative

        GroupDirSort ->
            groupDirectionToString direction.group

        IdleDirSort ->
            idleDirectionToString direction.idle
