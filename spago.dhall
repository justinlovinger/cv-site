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
    , "concur-react"
    , "console"
    , "css"
    , "datetime"
    , "effect"
    , "foreign-object"
    , "formatters"
    , "spec"
    , "spec-quickcheck"
    , "unordered-collections"
    ]
, packages =
    ./packages.dhall
}
