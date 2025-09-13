module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoConfusingPrefixOperator
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoPrematureLetComputation
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoExposingEverything.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Results/Types.elm"
            , "src/Results/Rest.elm"
            , "src/Results/Reports/Helpers.elm"
            , "src/Results/Helpers.elm"
            ]
    , NoImportingEverything.rule
        [ "Results.Types"
        , "Results.Helpers"
        , "Results.Rest"
        , "Results.Reports.Helpers"
        , "Results.CustomSvg"
        ]
    , NoMissingTypeAnnotation.rule
    , NoPrematureLetComputation.rule
    , NoConfusingPrefixOperator.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]
