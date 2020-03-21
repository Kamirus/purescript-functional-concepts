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
    , "postgresql-client"
    , "prelude"
    , "prettier-printer"
    , "prettyprinter"
    , "psci-support"
    , "record"
    , "refs"
    , "selda"
    , "tuples"
    , "typelevel-eval"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
