{ name = "day-2"
, dependencies =
  [ "console", "effect", "integers", "node-fs", "psci-support", "stringutils" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
