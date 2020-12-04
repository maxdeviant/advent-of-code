{ name = "day-n"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "integers"
  , "node-fs"
  , "ordered-collections"
  , "psci-support"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
