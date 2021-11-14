{ name = "advent-of-code-2017"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
