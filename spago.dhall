{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "gadts"
, dependencies =
    [ "arrays"
    , "console"
    , "effect"
    , "leibniz"
    , "matryoshka"
    , "maybe"
    , "prelude"
    , "psci-support"
    , "record"
    , "refs"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
