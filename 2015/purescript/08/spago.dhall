{ name = "day-8"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "node-fs"
  , "psci-support"
  , "spec"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
