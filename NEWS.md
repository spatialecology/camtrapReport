# camtrapReport 1.0.55

* Fixing testunit to pass the jarl test

# camtrapReport 1.0.54

* Additional updates to pass the jarl test

# camtrapReport 1.0.53

* More updates to pass the jarl test

# camtrapReport 1.0.52

* Minor update to pass jarl test

# camtrapReport 1.0.51

* Made the coverage test fixture independent of optional report-module
  packages and made Pandoc availability explicit in the coverage workflow.
* Added network-free tests for taxonomy lookup failure handling.
* Avoided a case-insensitive roxygen2 filename collision for the
  `install_All()` help topic.
  
# camtrapReport 1.0.50

* Updated manuals and website.

# camtrapReport 1.0.49

* Improve multiple R scripts to pass pkgcheck test.

# camtrapReport 1.0.48

* Further improvement to address rOpenSci editor feedbacks.
* install_All is adopted to use pak.
* Documented the opt-in role of `install_All()` in discovering dependencies
  from bundled and user-provided report modules before delegating to `pak`.
* Split internal rendering, taxonomy, and spatial utilities into focused files
  without changing their implementations or the public API.
* Replaced the earlier toy fixture with a documented, relationship-preserving
  subset of the GMU8_LEUVEN Camtrap DP dataset. Contact email addresses are
  omitted because they are not needed for examples or automated tests.

# camtrapReport 1.0.47

* Responded to rOpenSci editor feedback by improving the worked example,
  contributor documentation, optional-dependency tests, website navigation,
  and temporary-file cleanup.
* Corrected citation metadata for the package and its related conference paper.

# camtrapReport 1.0.46 (2026-08-09)

## Improvements

* Prepared the package for rOpenSci software peer review.
* Improved package structure, namespace management, and internal code quality.
* Strengthened automated testing and package-check workflows.
* Improved robustness of report generation and supporting utilities.

## Documentation

* Expanded and revised the package documentation to clarify the scope, workflow, and differences from existing camera-trap tools.
* Improved the pkgdown website, reference documentation, vignettes, and reporting resources.
* Standardised package metadata, citations, links, and release information.

## Maintenance

* Removed obsolete and unused package assets and temporary files.
* Resolved non-ASCII source-code issues and other package-check findings.
* Updated CRAN package links to their canonical form.

This release primarily prepares camtrapReport for rOpenSci software peer review and improves package robustness, documentation, reproducibility, and maintainability.

# camtrapReport 1.0.45 (2026-08-05)

* Initial official release.
