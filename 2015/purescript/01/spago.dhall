{ name = "my-project"
, dependencies =
  [ "console", "effect", "node-fs", "psci-support", "stringutils" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
