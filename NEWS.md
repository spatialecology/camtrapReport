# camtrapReport 1.0.47 (2026-08-09)

## Improvements

- Prepared the package for rOpenSci software peer review.
- Improved package structure, namespace management, and internal code quality.
- Strengthened automated testing and package-check workflows.
- Improved robustness of report generation and supporting utilities.
- Replaced the former S4 `install_All()` implementation with the simpler
  snake-case `install_all()` function. It automatically discovers dependencies
  declared by bundled and registered user-managed report modules, and delegates
  dependency resolution and installation to `pak`.

## Documentation

- Expanded and revised the package documentation to clarify the scope, workflow, and differences from existing camera-trap tools.
- Improved the pkgdown website, reference documentation, vignettes, and reporting resources.
- Standardised package metadata, citations, links, and release information.

## Maintenance

- Removed obsolete and unused package assets and temporary files.
- Resolved non-ASCII source-code issues and other package-check findings.
- Updated CRAN package links to their canonical form.

This release primarily prepares camtrapReport for rOpenSci software peer review and improves package robustness, documentation, reproducibility, and maintainability.

# camtrapReport 1.0.45 (2026-08-05)

- Initial official release.