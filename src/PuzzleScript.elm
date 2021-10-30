module PuzzleScript exposing (..)

import PuzzleScript.Rule as Rule exposing (Rule)


type alias PuzzleScript =
    { title : String
    , author : String
    , homepage : String
    , objects : String
    , legend : String
    , sounds : String
    , collisionlayers : String
    , rules : List Rule
    , winconditions : String
    , levels : String
    }


toString : PuzzleScript -> String
toString puzzleScript =
    [ "title" ++ puzzleScript.title
    , "author" ++ puzzleScript.author
    , "homepage" ++ puzzleScript.homepage
    , ""
    , ""
    , "========"
    , "OBJECTS"
    , "========"
    , ""
    , puzzleScript.objects
    , ""
    , ""
    , "======="
    , "LEGEND"
    , "======="
    , ""
    , puzzleScript.legend
    , ""
    , ""
    , "========"
    , "SOUNDS"
    , "========"
    , ""
    , puzzleScript.sounds
    , ""
    , ""
    , "================"
    , "COLLISIONLAYERS"
    , "================"
    , ""
    , puzzleScript.collisionlayers
    , ""
    , ""
    , "======"
    , "RULES"
    , "======"
    , ""
    , puzzleScript.rules
        |> List.map Rule.toString
        |> String.join "\n"
    , ""
    , ""
    , "=============="
    , "WINCONDITIONS"
    , "=============="
    , ""
    , puzzleScript.winconditions
    , ""
    , ""
    , "======="
    , "LEVELS"
    , "======="
    , ""
    , puzzleScript.levels
    ]
        |> String.join "\n"
