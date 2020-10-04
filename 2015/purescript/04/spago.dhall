{ name = "day-n"
, dependencies =
  [ "console", "crypto", "effect", "node-fs", "psci-support", "stringutils" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
