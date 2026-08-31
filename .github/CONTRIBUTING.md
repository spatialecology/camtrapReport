# Contributing to camtrapReport

Thank you for your interest in contributing to `camtrapReport`.
Bug reports, feature suggestions, documentation improvements, and new
report modules are all welcome.

## Reporting problems

Please report bugs through the
[GitHub issue tracker](https://github.com/spatialecology/camtrapReport/issues).

To help us investigate the problem, please include:

- a clear description of what happened;
- the result you expected;
- a minimal reproducible example;
- the output of `sessionInfo()`;
- a small synthetic or openly shareable Camtrap DP dataset, where possible.

If sharing a dataset is not possible because of data privacy, confidentiality,
or sensitivity concerns, please contact the maintainer
[by email](mailto:eebrahimi.bio@gmail.com) to discuss an appropriate
alternative for reproducing the issue.

Please do not upload confidential camera-trap data, sensitive species
locations, restricted images, personal information, or other protected
material to a public GitHub issue.

## Suggesting features

Feature requests and ideas for new ecological-report modules are welcome.
For larger changes, please open an issue first so that we can discuss how
the proposal fits the package structure and existing workflow.

## Pull requests

Before submitting a pull request, please:

1. Keep the proposed change focused.
2. Add or update tests when behaviour changes.
3. Update the roxygen2 documentation when needed.
4. Add a short entry to
   [`NEWS.md`](https://github.com/spatialecology/camtrapReport/blob/main/NEWS.md)
   for user-facing changes.
5. Run:

```r
devtools::document()
devtools::test()
devtools::check()
```

Please make sure that all checks pass before submitting the pull request.

## Coding conventions

Please follow the structure and coding style already used in the package.
Large formatting changes should be kept separate from functional changes
so that contributions are easier to review.

New public functions should use `snake_case`. Some existing public functions
retain older names for backward compatibility; do not rename them without a
documented deprecation plan and replacement aliases.

## Dependencies and testing

The package deliberately uses both `data.table` and `dplyr` for different
parts of the workflow. `data.table::fread()` provides efficient input of
Camtrap DP tables, while `dplyr` is used for data transformation, grouping,
and joins. `tidyr` is an optional dependency used only by report modules that
need data reshaping, including species-accumulation output.

Other packages in `Suggests` support optional functionality such as report
rendering, the graphical interface, figures, maps, tables, specialised
ecological analyses, spatial and solar-time operations, and taxonomic
enrichment. They should be loaded only when the corresponding functionality
is requested. New optional modules should declare their packages in module
metadata and fail with a clear installation message when a package is absent.

The standard test suite does not require API tokens. Tests should use bundled
or synthetic fixtures, temporary files managed with `withr`, and mocked or
otherwise isolated network behaviour. Never commit credentials, private
camera-trap data, sensitive species locations, or other restricted material.

## Package architecture

The central `camReport` object is implemented using an R Reference Class.
This is an intentional design choice because report generation is a stateful
workflow: the same object stores the imported camera-trap data, metadata,
settings, intermediate analytical results, and report configuration as the
workflow progresses.

Reference semantics allow these components to remain associated with the same
`camReport` object while users modify settings, select analyses, update report
sections, and generate outputs. This avoids requiring each operation to rebuild
and return a new report object.

Contributors adding functionality to the `camReport` object should therefore
preserve this stateful workflow and avoid changes to the class architecture
unless there is a clear reason to do so.

### Module rendering environment

Report-module code is evaluated in an environment created by the internal
`.make_render_env()` helper. This environment provides access to the current
`camReport` object, selected object fields, report-numbering functions, and
internal helpers required by report modules.

This gives modules a consistent execution context during report generation.
Contributors developing new modules should normally use the objects and helpers
made available through this environment rather than modifying the rendering
environment itself.

## Code of Conduct

Participation in this project is governed by the
[Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md).
