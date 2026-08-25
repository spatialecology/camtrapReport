# Contributing to camtrapReport

Thank you for your interest in contributing to `camtrapReport`. Bug reports, feature suggestions, documentation improvements, and new report modules are welcome.

## Reporting problems

Please report bugs through the [GitHub issue tracker](https://github.com/spatialecology/camtrapReport/issues).

To help us investigate, please include:

* a clear description of the problem;
* the result you expected;
* a minimal reproducible example;
* the output of `sessionInfo()`; and
* a small synthetic or openly shareable Camtrap DP dataset, where possible.

If you cannot share the data because of privacy, confidentiality, or sensitivity concerns, please contact the maintainer [by email](mailto:eebrahimi.bio@gmail.com) to discuss an appropriate alternative.

Do not upload confidential camera-trap data, sensitive species locations, restricted images, personal information, credentials, or other protected material to a public GitHub issue.

## Suggesting features

Feature requests and proposals for new ecological-report modules are welcome.

For substantial changes, please open an issue first so that we can discuss how the proposal fits the package structure and existing workflow.

## Pull requests

Before submitting a pull request:

1. Keep the proposed change focused.
2. Add or update tests when behaviour changes.
3. Update the roxygen2 documentation when necessary.
4. Add an entry to [`NEWS.md`](https://github.com/spatialecology/camtrapReport/blob/main/NEWS.md) for user-facing changes.
5. Install the package dependencies and run the development checks:

```r
pak::pak("deps::.")

devtools::document()
devtools::test()
devtools::check()
```

Please make sure that the checks pass before submitting the pull request.

## Coding conventions

Follow the structure and coding style already used in the package. Keep large formatting changes separate from functional changes so that pull requests remain easy to review.

New public functions should use `snake_case`.

Some existing public functions retain older names for backward compatibility. Do not remove or rename them without providing replacement aliases and a documented deprecation plan.

## Dependencies

The package uses `data.table` and `dplyr` for different purposes:

* `data.table::fread()` provides efficient input of Camtrap DP tables.
* `dplyr` supports data transformation, grouping, filtering, and joins.
* `tidyr` is an optional dependency used only by report modules that require data reshaping, including species-accumulation output.

Other packages listed in `Suggests` support optional functionality, including:

* report and vignette rendering;
* the graphical interface;
* figures and interactive visualisations;
* maps and spatial processing;
* tables;
* specialised ecological analyses;
* solar-time calculations; and
* taxonomic enrichment.

Optional dependencies should be checked with `requireNamespace()` and used only when the corresponding functionality is requested. Functions should provide a clear installation message when an optional package is unavailable.

New optional modules should declare their package requirements in the module metadata.

## Testing

The standard test suite must not require API tokens, private datasets, or an active network connection.

Tests should use:

* bundled or synthetic fixtures;
* temporary files managed with `withr`;
* mocked or otherwise isolated network behaviour; and
* focused expectations that clearly describe the intended behaviour.

Use testthat expectations such as `expect_no_match()` instead of indirect expressions such as `expect_false(grepl(...))` where an appropriate specialised expectation exists.

Never commit credentials, private camera-trap data, sensitive species locations, or other restricted material.

## Package architecture

### The `camReport` object

The central `camReport` object is implemented using an R Reference Class. This is an intentional design decision because report generation is a stateful workflow.

The same object stores:

* imported camera-trap data and metadata;
* user settings;
* intermediate analytical results;
* selected report modules; and
* report configuration.

Reference semantics allow these components to remain associated with the same `camReport` object while users update settings, select analyses, modify report sections, and generate outputs. This avoids requiring every operation to reconstruct and return a new report object.

Contributors should preserve this stateful workflow. Changes to the class architecture should be proposed and discussed in an issue before implementation.

### Module rendering environment

Report-module code is evaluated in an environment created by the internal `.make_render_env()` helper.

This environment provides controlled access to:

* the current `camReport` object;
* selected object fields;
* report-numbering functions; and
* internal helpers required by report modules.

This gives report modules a consistent execution context. Contributors developing new modules should normally use the objects and helpers provided through this environment instead of modifying the rendering environment itself.

### Installation of module dependencies

The package provides `install_all()` to discover optional dependencies declared by bundled and registered user modules. It then delegates package installation to `pak`.

This function is specific to the package’s module system; it is not intended to replace `pak` as a general dependency installer.

## Code of Conduct

Participation in this project is governed by the [Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md).
