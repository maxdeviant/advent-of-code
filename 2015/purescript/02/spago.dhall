{ name = "day-2"
, dependencies = [ "console", "effect", "node-fs", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
