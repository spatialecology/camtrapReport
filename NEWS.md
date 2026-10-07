# camtrapReport 1.0.61 (development version)

- Added separate pooled and annual REM report modules. The EOW profile now
  fits detection, movement-speed, activity, trap-rate, and density parameters
  independently for each species and sampling year; the standard profile
  retains the pooled multi-year method for backwards compatibility.
- Corrected REM trap-rate input to sum the Camtrap DP event-level `count`
  field as numbers of individuals, with a backwards-compatible row-count
  fallback when no usable counts are supplied.
- Applied deployment inclusion flags consistently to detection and speed
  calibration data, added reproducible REM repetition and seed metadata, and
  separated pooled and annual caches.
- Added an REM analysis signature so obsolete cached REM estimates are cleared
  when an existing `__camReport_Object.rds` is loaded.
- Replaced live OpenStreetMap tiles in bundled report modules with a
  self-contained offline background. Optional CartoDB, Esri, and OpenStreetMap
  backgrounds remain available through `cm$setting$map_basemap`.
- Restored the package species palette for multi-species trend figures instead
  of switching palettes when more than six species are displayed.

# camtrapReport 1.0.60

- Added `html`, `pdf`, and `both` compatibility metadata to the report and
  data-status module registries. Module management now preserves this metadata,
  and `add_Module()` and `move_Module()` can set it explicitly. Module-management
  functions now also accept the documented `dir` argument for writable
  project-level registries.
- Report generation now excludes format-incompatible modules and their orphaned
  children while leaving the user's attached section selection unchanged.
- Added a non-tabbed `appendix_eow` module so the EOW profile's data-status
  sections render as ordinary appendix subsections in HTML and PDF output.
- PDF reports no longer include a table of contents. Print styling also
  suppresses Bootstrap's appended hyperlink URLs and expands printable tabset
  content so that non-interactive PDF readers can access every panel.
- Expanded tests and documentation for module-format metadata, EOW appendix
  structure, PDF routing, print styling, and selection restoration.

# camtrapReport 1.0.59

- Added reusable `reportProfile` objects and bundled `default` and `EOW`
  profiles for selecting ordered ecological and data-status module sets.
- Added qualified `report::module` and `status::module` references, including
  profile-level parent overrides that allow selected status modules to appear
  below the ecological report appendix without modifying module YAML files.
- Extended `section_names()` and `sections()` to inspect and edit profile and
  data-status selections while retaining their previous default behaviour.
- Added `read_profile()`, `write_profile()`, `add_profile()`, and
  `profile_names()` for sharing and registering profile YAML files.
- Added optional static PDF output to `report()` and `status()`. PDF generation
  prints the existing HTML output with `pagedown` and Chrome or Edge, preserving
  compatibility with HTML-oriented report modules.
- Added the EOW module variants to the module registry and corrected the
  malformed text scalar in `location_EOW.yml`.
- Expanded profile, cross-pool selection, and PDF-routing tests and updated the
  README, vignettes, reference documentation, and pkgdown configuration.

# camtrapReport 1.0.58

- Renamed `install_All()` to `install_all()` for consistency with R naming
  conventions. `install_All()` remains available as a deprecated compatibility
  alias for existing code.
- Clarified the report-module architecture and contributor documentation,
  including the distinction from Shiny modules, the rationale for the
  Reference Class implementation, and the role of `.eval()` relative to
  `rlang`-style evaluation.
- Improved the README and package website with clearer descriptions and direct
  example outputs for the Data Status Check and Ecological Report.
- Added the rOpenSci Software Peer Review status badge to the README and
  package website.
- Expanded deterministic unit coverage for sampling and project metadata,
  taxonomy lookup fallbacks, module-management dispatch, and nested report
  object insertion.
- The new tests use in-memory fixtures and mocked external lookups. They do not
  contact remote services, install packages, or require optional module
  dependencies.
- Increased locally measured `covr::package_coverage()` from 73.50% to 77.76%.

# camtrapReport 1.0.56

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
