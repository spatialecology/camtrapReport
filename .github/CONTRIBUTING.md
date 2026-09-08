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

The central `camReport` object is implemented as a Reference Class because
report generation is stateful: the same object stores imported data, metadata,
settings, selected sections, intermediate results, and report configuration as
the workflow progresses. R6 could provide similar reference semantics, but
changing the established implementation would require substantial refactoring
without a clear user-facing benefit.

The term **module** is used here in the general sense of an independently
defined report component, rather than in the specific sense of a Shiny module.
Shiny is used separately for the optional graphical interface and is not part
of the report-module execution mechanism. The module system was developed
specifically for the `camtrapReport` reporting workflow and was not derived
from Shiny modules.

A fixed R Markdown template would have been simpler internally, but modifying
or adding analyses would then require changes to the central template. The
module-based design instead keeps analytical and reporting components
independently configurable and extensible without changing the package core.

Report modules are defined in YAML files and may include explanatory text,
R code, rendering options, and declarations of optional package dependencies.
During report generation, the module code is inserted into the generated
R Markdown document and evaluated by `knitr`/`rmarkdown` in a dedicated
rendering environment created by `.make_render_env()`.

Separately, `.eval()` is a small internal wrapper around base R `parse()` and
`eval()` used in some dynamic code paths where an R expression is intentionally
represented as character text and must be evaluated in a specified environment.
It is not the mechanism by which YAML module code is rendered. These uses do
not require quosures, data masks, or other tidy-evaluation semantics, so adding
`rlang` would not simplify the evaluation model. Where dynamic evaluation is
not needed, direct function calls are preferred.

For guidance on creating and managing modules, including a worked example, see
the [Module Management guide](../vignettes/articles/modules.Rmd).

## Dependency policy

Core dependencies in `Imports` are required for data input, object setup, the
interactive interface, or report rendering. Both `data.table` and `dplyr` are
used deliberately: the former supports keyed sequence aggregation and efficient
table operations, while the latter provides the joins and column transformations
used throughout the data-preparation and summary workflow.

Packages needed only for particular analytical or visual report sections should
remain optional and must be declared within the module using `#| packages:`.
`install_all()` discovers these declarations from the available YAML modules
and passes the resulting package references to `pak`. It is an explicit opt-in
operation and is never called at package load or report-render time. When adding
or changing a module dependency, update its documentation and tests, and verify
that a missing optional package produces a clear message rather than breaking
unrelated sections.

## Coding conventions

Please follow the structure and coding style already used in the package.
Large formatting changes should be kept separate from functional changes
so that contributions are easier to review.

Some exported function names use camel case or underscores because they are part
of the established public API. Keep those names for backward compatibility.
Use clear, consistent names for new internal helpers and avoid renaming existing
functions as part of an unrelated change.

## Use of coding-assistance tools

The package architecture, scientific methodology, modular reporting framework,
and core functionality of `camtrapReport` were designed and developed by the maintainer.
Coding-assistance tools were used during later stages of development for specific supporting tasks,
including code review, debugging, checking for inconsistencies,
and converting manually written documentation into roxygen2 format.
These tools were not used to determine the scientific methods,
analytical choices, package architecture, or overall design of the software.
All suggested code changes were reviewed, adapted where necessary,
and tested by the maintainer before being incorporated into the package.
Responsibility for the package design, implementation, scientific content,
and released code remains with the maintainer.


## Code of Conduct

Participation in this project is governed by the
[Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md) 