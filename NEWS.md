# camtrapReport 1.0.56 (development version)

## Internal improvements

* Addressed selected static-analysis and good-practice findings in package code
  without changing the public API or intended outputs.
* Made function arguments explicit and improved portable path and string
  construction.
* Made character-column handling explicit in selected data frames.
* Retained the dynamic module-evaluation and optional dependency-discovery
  mechanisms used by bundled and user-provided report modules.

## Testing

* Replaced general test assertions with more specific `testthat` expectations
  for values, types, comparisons, and object names.
* Confirmed that the complete test suite passes after the internal changes.

# camtrapReport 1.0.55

* Revised unit-test expectations in response to findings reported by `jarl`.

# camtrapReport 1.0.54

* Addressed additional static-analysis findings in package code and tests.

# camtrapReport 1.0.53

* Improved internal code and tests following static-analysis review.

# camtrapReport 1.0.52

* Applied minor internal corrections identified by `jarl`.

# camtrapReport 1.0.51

* Made the coverage-test fixture independent of optional report-module
  packages and made Pandoc availability explicit in the coverage workflow.
* Added network-independent tests for taxonomy-lookup failure handling.
* Revised the documentation topic for `install_All()` to avoid a
  case-insensitive filename collision on Windows.

# camtrapReport 1.0.50

* Updated the package manuals and rebuilt the pkgdown website.

# camtrapReport 1.0.49

* Revised package code and tests in response to findings reported by
  `pkgcheck`.

# camtrapReport 1.0.48

* Revised the package in response to the initial rOpenSci editor assessment.
* Updated `install_All()` to delegate dependency resolution and installation
  to `pak`.
* Documented the opt-in role of `install_All()` in discovering dependencies
  from bundled and user-provided report modules.
* Split internal rendering, taxonomy, and spatial utilities into focused files
  without changing the public API.
* Replaced the earlier example fixture with a documented,
  relationship-preserving subset of the GMU8_LEUVEN Camtrap DP dataset.
* Removed contact email addresses from the example data because they are not
  required by examples or automated tests.

# camtrapReport 1.0.47

* Improved the worked example, contributor documentation,
  optional-dependency tests, website navigation, and temporary-file cleanup
  in response to rOpenSci editor feedback.
* Corrected citation metadata for the package and its associated conference
  paper.

# camtrapReport 1.0.46 (2026-08-09)

## Improvements

* Prepared the package for rOpenSci software peer review.
* Improved package structure, namespace management, and internal code quality.
* Strengthened automated testing and package-check workflows.
* Improved the robustness of report generation and supporting utilities.

## Documentation

* Expanded the documentation to clarify the package scope, workflow, and
  differences from existing camera-trap tools.
* Improved the pkgdown website, reference documentation, vignettes, and
  reporting resources.
* Standardised package metadata, citations, links, and release information.

## Maintenance

* Removed obsolete package assets and temporary files.
* Resolved non-ASCII source-code issues and other package-check findings.
* Updated CRAN package links to their canonical forms.

# camtrapReport 1.0.45 (2026-08-05)

* Initial public release.
