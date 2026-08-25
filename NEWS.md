# camtrapReport 1.0.47

## rOpenSci review preparation

This release prepares `camtrapReport` for rOpenSci software review and for a possible Methods in Ecology and Evolution software-article route.

`camtrapReport` provides a reproducible workflow for processing camera-trap data in Camtrap DP format and generating data-status and ecological reports for wildlife monitoring, biodiversity assessment, and conservation decision support.

## Package quality and review readiness

- Prepared the package for rOpenSci review using `pkgcheck`.
- Added a `ropensci-pkgcheck-report.md` file documenting the package-review status.
- Confirmed that `R CMD check` reports no errors and no warnings.
- Confirmed that GitHub Actions continuous integration is available and passing.
- Confirmed package test coverage of approximately 78%.
- Confirmed that all exported functions have examples.
- Confirmed that the package website is available through `pkgdown`.
- Added or revised GitHub issue templates to support clearer bug reports, feature requests, and user feedback.
- Added or revised community files, including contributing guidance and code-of-conduct information.

## Documentation and website

- Revised the README to clarify the package scope, intended users, main workflows, and relationship to Camtrap DP.
- Expanded the package overview to better explain how `camtrapReport` supports reproducible camera-trap data reporting.
- Improved the pkgdown website structure, including the Get started page, Articles, Reference, Resources, GitHub links, discussion links, and issue-reporting links.
- Added and revised article pages for:
  - Data Status Report
  - Ecological Report
  - Package Overview
  - Module Management
  - Resources
- Added example Data Status Report and Ecological Report galleries to the website.
- Added links to full example HTML reports through the website.
- Added a Resources page listing publications, workshops, and training activities related to `camtrapReport`.
- Improved image paths and static assets used by pkgdown articles.
- Regenerated the pkgdown website for the review version.

## Example reports and example data

- Added example HTML reports to demonstrate package outputs.
- Added example Data Status Reports for multiple camera-trap monitoring datasets.
- Added example Ecological Reports for multiple monitoring projects.
- Improved documentation around the bundled Leuven example dataset.
- Documented the provenance, intended use, licence, and limitations of the example dataset.
- Added guidance that example reports are intended to demonstrate reporting structure and functionality, not to replace dataset-specific ecological interpretation.

## Data Status Report

- Improved documentation of the Data Status Report workflow.
- Clarified how the report can be used to assess dataset completeness, possible data issues, and readiness for ecological reporting.
- Improved presentation of example data-status outputs on the package website.
- Added clearer guidance on metadata completeness, spatial coverage, temporal coverage, species information, and media availability.
- Improved wording in report examples and website descriptions.

## Ecological Report

- Improved documentation of the Ecological Report workflow.
- Added website examples showing how ecological reports are generated from camera-trap data.
- Clarified that ecological reports combine summaries, tables, maps, figures, and modular report sections.
- Improved description of how users can configure reports for different study aims.
- Improved handling and documentation of optional ecological modules.
- Added clearer guidance on interpretation, reproducibility, and sensitive-species information.
- Clarified that users should interpret ecological summaries in relation to their study design, sampling effort, and data limitations.

## Module system

- Improved documentation of the modular report architecture.
- Clarified how modules are selected, ordered, configured, and added to reports.
- Improved documentation of module dependencies.
- Clarified the role of optional packages required by specific modules.
- Improved the Module Management article.
- Improved examples for registering, testing, and using report modules.
- Retained the modular design so users can extend `camtrapReport` with their own report sections.

## Dependencies and optional functionality

- Reviewed package dependencies for rOpenSci preparation.
- Clarified the distinction between required imports and optional suggested packages.
- Improved documentation of optional functionality that depends on packages such as `activity`, `camtrapDensity`, `Distance`, `iNEXT`, `leaflet`, `plotly`, `sf`, `suncalc`, `taxize`, and related reporting packages.
- Improved guidance for users when optional dependencies are needed for selected report modules.
- Retained support for dynamic installation of optional module dependencies where appropriate.

## Testing and continuous integration

- Expanded the test suite beyond placeholder tests.
- Added or improved tests for core package functionality.
- Added tests for report-section handling, module behaviour, package utilities, and report configuration.
- Confirmed that the package test suite runs successfully under GitHub Actions.
- Confirmed that GitHub Actions includes checks across multiple operating systems.
- Added or maintained a test-coverage workflow.

## Internal structure and maintainability

- Reorganised utility code into more focused files.
- Reduced reliance on large monolithic utility scripts.
- Improved temporary-file handling during report generation.
- Improved separation between data processing, report configuration, report rendering, and module management.
- Improved internal consistency of object handling and report-section logic.
- Made structural changes with no intended breaking changes to the public API.

## User-facing improvements

- Improved report metadata handling, including title, subtitle, authors, institution, site name, and logo paths.
- Improved wording in generated reports and examples.
- Improved handling of selected report sections.
- Improved support for focusing reports on selected species groups.
- Added clearer examples for configuring reports.
- Improved guidance for users working with incomplete or heterogeneous camera-trap datasets.
- Improved documentation of limitations related to sampling design, metadata completeness, spatial coverage, temporal coverage, and data sensitivity.

## Website assets and static files

- Added static figures used in article pages.
- Added outreach figures for the Resources page.
- Added gallery images for Data Status Report and Ecological Report examples.
- Added example HTML reports under the pkgdown website structure.
- Corrected case-sensitive file paths for GitHub Pages deployment.
- Regenerated the website after correcting static-asset paths.

## Notes

- This release focuses on review readiness, documentation, testing, website stability, and reproducibility.
- No major breaking changes to the public user interface are intended.
- Users are encouraged to report issues through the GitHub issue templates and to consult the package website for examples and guidance.

# camtrapReport 1.0.0

## Initial release

- Initial public release of `camtrapReport`.
- Added support for reading and processing camera-trap data in Camtrap DP format.
- Added functionality for generating Data Status Reports.
- Added functionality for generating Ecological Reports.
- Added a modular reporting structure for extending report content.
- Added summaries, tables, figures, and maps for camera-trap monitoring datasets.
- Added support for assessing metadata completeness, sampling effort, species records, spatial coverage, and temporal coverage.
- Added package documentation, examples, and a pkgdown website.
- Added initial tests and continuous integration.