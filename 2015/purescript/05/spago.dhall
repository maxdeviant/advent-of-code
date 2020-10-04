{ name = "day-5"
, dependencies =
  [ "console", "effect", "node-fs", "psci-support", "stringutils" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
