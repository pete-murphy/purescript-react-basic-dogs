{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "affjax"
    , "argonaut-codecs"
    , "console"
    , "css"
    , "effect"
    , "psci-support"
    , "react-basic-hooks"
    , "remotedata"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
