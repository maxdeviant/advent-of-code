{ name = "day-7"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "integers"
  , "node-fs"
  , "ordered-collections"
  , "psci-support"
  , "spec"
  , "stringutils"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
