let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20241124/packages.dhall
        sha256:418bbef5e4ef20c8a82b331919e27b55233f884223100a1b82977ee5736bc730

in  upstream

  with variant = {
    dependencies =
      [ "aff"
      , "arrays"
      , "assert"
      , "bifunctors"
      , "console"
      , "const"
      , "control"
      , "distributive"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "foreign-object"
      , "functors"
      , "identity"
      , "lists"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "profunctor"
      , "record"
      , "safe-coerce"
      , "transformers"
      , "tuples"
      ]
    , repo = "https://github.com/sigma-andex/purescript-variant"
    , version = "switch-to-visible-type-applications"
  }
