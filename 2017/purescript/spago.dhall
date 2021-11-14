{ name = "advent-of-code-2017"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "stringutils"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
