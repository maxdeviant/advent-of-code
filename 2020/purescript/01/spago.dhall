{ name = "day-1"
, dependencies = [ "console", "effect", "node-fs", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
