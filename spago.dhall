{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "nix-purescript-concur-frontend-starter"
, dependencies =
    [ "b64"
    , "behaviors"
    , "canvas"
    , "concur-react"
    , "console"
    , "css"
    , "datetime"
    , "drawing"
    , "effect"
    , "foreign-object"
    , "formatters"
    , "random"
    , "record"
    , "spec"
    , "spec-quickcheck"
    , "unordered-collections"
    ]
, packages =
    ./packages.dhall
}
