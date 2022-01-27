{ name = "var"
, dependencies = [ "console", "effect", "prelude", "psci-support", "contravariant", "invariant", "either", "tuples" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
