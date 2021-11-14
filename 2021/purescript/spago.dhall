{ name = "advent-of-code-2021"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
