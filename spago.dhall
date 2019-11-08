{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "gadts"
, dependencies =
    [ "console"
    , "effect"
    , "leibniz"
    , "maybe"
    , "prelude"
    , "psci-support"
    , "refs"
    , "tuples"
    , "arrays"
    , "record"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
