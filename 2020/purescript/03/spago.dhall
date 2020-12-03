{ name = "day-3"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "integers"
  , "node-fs"
  , "psci-support"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
