# camtrapReport 1.0.47 (2026-08-26)

## rOpenSci review preparation

- Prepared the package for rOpenSci software peer review.
- Improved package structure, namespace management, and internal code quality.
- Strengthened automated testing and package-check workflows.
- Improved robustness of report generation and supporting utilities.
- Improved handling of optional report-module dependencies.
- Replaced the former `installAll()` implementation with the simpler snake-case `install_all()` function.
- Updated `install_all()` so that it discovers dependencies declared by bundled and registered user-defined report modules, installs only missing optional dependencies, and avoids updating packages that are already installed.
- Updated dependency installation to use `pak` with `upgrade = FALSE`.

## Documentation

- Expanded and revised the README and package overview to clarify the package scope, workflow, target users, and differences from existing camera-trap tools.
- Improved the pkgdown website, reference documentation, vignettes, and reporting resources.
- Updated installation instructions to use `pak`.
- Clarified the role of optional dependencies used by report modules, interactive outputs, maps, activity analyses, density estimation, species accumulation, taxonomic enrichment, and the Shiny interface.
- Added contributor-facing documentation for internal design choices and report-generation structure.

## Maintenance

- Removed obsolete and unused package assets and temporary files.
- Removed the unused `camtraptor` dependency declaration because it is discussed only as a complementary package and is not called by package code.
- Resolved non-ASCII source-code issues and other package-check findings.
- Updated CRAN package links to their canonical form.
- Regenerated documentation and package website assets.

This release primarily prepares `camtrapReport` for rOpenSci software peer review and improves package robustness, documentation, reproducibility, dependency handling, and maintainability.

# camtrapReport 1.0.0

## Initial release

- Initial public release of `camtrapReport`.
- Added support for processing camera-trap data in Camtrap DP format.
- Added functionality for generating Data Status Reports.
- Added functionality for generating Ecological Reports.
- Added a modular reporting structure for extending report content.
- Added summaries, tables, figures, and maps for camera-trap monitoring datasets.
- Added package documentation, examples, tests, and a pkgdown website.
