{ name = "day-6"
, dependencies =
  [ "console"
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
