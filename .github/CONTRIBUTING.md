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

## Optional development environment and local checks

These tools are conveniences for contributors, not requirements for using
`camtrapReport` or replacements for the cross-platform GitHub checks. They do
not change package dependencies, `.eval()`, analytical settings or module code.
No Git hook is installed by the package or merely by cloning the repository.

### Developer container

If you already use a Docker-compatible engine and an editor supporting
[Dev Containers](https://containers.dev/), the optional `.devcontainer/`
configuration provides a Linux development environment. Open a trusted
checkout and choose **Reopen in Container** in your editor. This explicitly
builds/downloads a large image; it does not install software on the host or
change the package's runtime dependency declarations. The mounted checkout
remains writable, so the container is not a sandbox for untrusted modules.

The Dockerfile pins the official
[Rocker geospatial R 4.6.1 image](https://hub.docker.com/r/rocker/geospatial/tags?name=4.6.1)
by manifest digest, with Linux amd64 and arm64 variants verified in the
registry on 27 October 2026. Its
[official build source](https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/geospatial_4.6.1.Dockerfile)
includes GDAL, GEOS, PROJ, Pandoc and developer tools; the small extension
ensures command-line ZIP support and a writable container-user R library.
Automatic formatting on save, paste and typing is disabled in the supplied
editor settings. No post-create command installs package or module
dependencies automatically.

After opening the container, verify its tools before using it:

```sh
Rscript --vanilla -e 'print(R.version.string); print(sf::sf_extSoftVersion()); print(rmarkdown::pandoc_version()); print(Sys.which("zip")); stopifnot(requireNamespace("devtools", quietly = TRUE))'
```

Install the package's declared development dependencies only when you choose
to do so, then run the tests:

```r
devtools::install_deps(dependencies = TRUE)
testthat::test_local(stop_on_failure = TRUE)
```

Installing all optional analytical-module packages is a separate, deliberate
choice; it is not part of container startup. PDF rendering additionally needs
a supported Chrome/Edge installation; the container configuration does not
claim to provide or test that browser. Preserve your data, package versions
and analysis settings when reproducing a scientific result: an image pin
alone does not lock later development-package installations or online services.

The configuration and image metadata can be checked without Docker, but the
image must still be built and exercised on a Docker-enabled system. Do not
report a successful container build solely from JSON or Dockerfile validation.

### Optional pre-commit checks

The supplied `.pre-commit-config.yaml` uses local commands, not downloaded
formatting hooks. Install the external
[pre-commit tool](https://pre-commit.com/#install) separately if desired.
Enabling it is an explicit action from the repository root:

```sh
pre-commit install
pre-commit run --all-files
```

Only base-R syntax parsing of `R/` and `tests/` files runs at commit time.
It does not evaluate those files, install R packages or rewrite source. The
existing CI remains authoritative even if contributors do not install hooks.
Two extra checks are available only when deliberately invoked:

```sh
# Requires the separately installed Jarl command on PATH; no --fix is used.
pre-commit run --hook-stage manual --all-files jarl-check

# Executes the test suite; requires the declared test dependencies.
pre-commit run --hook-stage manual --all-files r-tests
```

Jarl's [official integration guide](https://jarl.etiennebacher.com/howto/precommit)
also describes its upstream hook. This repository deliberately uses a local,
manual-only `jarl check` command so that enabling hooks does not introduce
automatic stylistic changes or silently fetch another linter version.
Record `jarl --version` when reporting lint results. Tests execute R code,
so run them only on a trusted checkout. No formatter, spelling checker,
`.eval()` rewrite or automatic fix is enabled by this configuration.

## Architecture and report-module execution

`camtrapReport` uses a modular, stateful architecture. The central `camReport`
object is implemented as a Reference Class and acts as a mutable container for
the imported data, metadata, settings, selected report sections, intermediate
results, and report configuration used throughout the workflow. Reference
semantics are useful here because the same object is progressively updated as
the analysis and reporting workflow proceeds. R6 could provide similar
semantics, but the current Reference Class implementation is already integrated
throughout the package.

The term **module** is used in the general sense of an independently defined
report component. It does not refer to a Shiny module. Shiny is used separately
for the optional graphical interface and was not part of the original
report-module design.

A fixed R Markdown template would provide a simpler internal structure, but it
would make the report less flexible: adding or modifying an analysis would
require changes to the central template. Instead, `camtrapReport` keeps
analytical and reporting components as separate modules that can be selected,
reordered, modified, or extended without changing the package core. This also
allows contributors to develop additional analytical components for different
camera-trap datasets and reporting needs.

Report modules are defined in YAML files and can contain metadata, explanatory
text, R code, rendering options, and declarations of optional package
dependencies. During report generation, the selected module content is
assembled into the generated R Markdown document and evaluated by
`knitr`/`rmarkdown` in a dedicated rendering environment created by
`.make_render_env()`.

The internal `.eval()` helper serves a separate purpose. It is a small wrapper
around base R `parse()` and `eval()` used in a few dynamic code paths where an R
expression is intentionally stored as character text and must be evaluated in a
specified environment. It is not used to render YAML module code. These uses do
not require quosures, data masks, or other tidy-evaluation semantics. Where
dynamic evaluation is unnecessary, direct function calls should be preferred.

For guidance on creating, registering, testing, and managing report modules,
including a worked YAML example, see the
[Module Management guide](../vignettes/articles/modules.Rmd).

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

The current `.require()` helper also attaches a package after checking its
namespace, and selected report modules are loaded with `library()` during
rendering. Existing modules may call unqualified functions, so these operations
are part of the current execution contract and can affect `search()`. Replacing
them with namespace checks alone requires a separate compatibility review;
do not make that change as a lint fix. `.eval()` and the dedicated render
environment do not isolate process-wide effects such as package attachment,
network requests, options, or file writes.

Only set minimum dependency versions when a required API or known
incompatibility establishes the lower bound. Installed developer versions are
not evidence of a minimum supported version. Describe and test any new
version constraints separately from documentation changes.

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
