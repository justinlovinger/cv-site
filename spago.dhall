{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "nix-purescript-concur-frontend-starter"
, dependencies =
    [ "behaviors"
    , "canvas"
    , "concur-react"
    , "console"
    , "css"
    , "datetime"
    , "drawing"
    , "effect"
    , "foreign-object"
    , "random"
    , "record"
    , "spec"
    ]
, packages =
    ./packages.dhall
}
