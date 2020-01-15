{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "nix-purescript-concur-frontend-starter"
, dependencies =
    [ "concur-react"
    , "console"
    , "css"
    , "datetime"
    , "effect"
    , "foreign-object"
    , "record"
    , "spec"
    ]
, packages =
    ./packages.dhall
}
