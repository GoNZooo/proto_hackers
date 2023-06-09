{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "proto-hackers"
, dependencies =
  [ "arrays"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "erl-atom"
  , "erl-binary"
  , "erl-kernel"
  , "erl-lists"
  , "erl-logger"
  , "erl-maps"
  , "erl-pinto"
  , "erl-process"
  , "erl-quickcheck-helpers"
  , "erl-sets"
  , "erl-simplebus"
  , "pg-bus"
  , "erl-tuples"
  , "erl-untagged-union"
  , "foldable-traversable"
  , "foreign"
  , "integers"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "purerl-test"
  , "quickcheck"
  , "simple-json"
  , "simple-server"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
