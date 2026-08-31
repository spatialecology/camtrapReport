# Contributing to camtrapReport

Thank you for your interest in contributing to `camtrapReport`.
Bug reports, feature suggestions, documentation improvements and new
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
If this is not possible because of data privacy, confidentiality or sensitivity
concerns, please contact the maintainer [by email](mailto:eebrahimi.bio@gmail.com)
to discuss an appropriate alternative for reproducing the issue.

Please do not upload confidential camera-trap data, sensitive species
locations, restricted images, personal information or other protected
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
4. Add a short entry to [`NEWS.md`](../NEWS.md) for user-facing changes.
5. Run:

```r
devtools::document()
devtools::test()
devtools::check()
```

Please make sure that all checks pass before submitting the pull request.

## Architecture and report-module execution

The central `camReport` object is implemented as a Reference Class. This is an
intentional design choice because report generation is stateful: the same object
stores imported data, metadata, settings, selected sections, intermediate
analytical results, and report configuration as the workflow progresses. It also
avoids repeatedly copying potentially large camera-trap datasets. Changes to
this class can affect the whole workflow, so discuss proposed class or public-API
changes in an issue before implementing them.

Report modules are YAML files that can store R code as text. The internal
`.eval()` helper is the bridge between that representation and execution: it
parses module code and evaluates it in the explicit environment supplied by the
caller. This keeps package requirements module-specific and allows modules to be
extended without hard-coding every analytical package in the core functions.
Do not replace this mechanism casually, and never evaluate modules from an
untrusted source.

During rendering, `.make_render_env()` constructs the module environment. It
exposes the central object under the historical names used by bundled modules,
copies the required object fields and formatting helpers, and keeps assignments
made by a module outside the user's global environment. New module code should
use only the data and helpers it needs, qualify package calls where practical,
and declare all required packages in the YAML metadata.

Internal utilities are split by responsibility: rendering and module execution
helpers are in `R/utils-render.R`, taxonomy helpers are in
`R/utils-taxonomy.R`, spatial and correlation-plot helpers are in
`R/utils-spatial.R`, and general string, date, file, and formatting helpers
remain in `R/utils.R`.

## Dependency policy

Core dependencies in `Imports` are required for data input, object setup, the
interactive interface, or report rendering. Both `data.table` and `dplyr` are
used deliberately: the former supports keyed sequence aggregation and efficient
table operations, while the latter provides the joins and column transformations
used throughout the data-preparation and summary workflow.

Packages needed only for particular analytical or visual report sections should
remain optional and must be declared in that module's YAML `packages` field.
`install_All()` discovers those declarations from all currently available YAML
modules, including modules added or modified by users, and passes the resulting
references to `pak`. It is an explicit opt-in operation and is never called at
package load or report-render time. When adding or changing a module dependency,
update its documentation and tests, and verify that a missing optional package
produces a clear message rather than breaking unrelated sections.

## Coding conventions

Please follow the structure and coding style already used in the package.
Large formatting changes should be kept separate from functional changes
so that contributions are easier to review.

Some exported function names use camel case or underscores because they are part
of the established public API. Keep those names for backward compatibility.
Use clear, consistent names for new internal helpers and avoid renaming existing
functions as part of an unrelated change.

## Code of Conduct

Participation in this project is governed by the
[Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md)
