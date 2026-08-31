<p align="center">
  <img src="inst/report-assets/camtrapReport-logo.png" width="200" alt="camtrapReport"/>
</p>

<h1 align="center">camtrapReport</h1>

<p align="center">
  <em>Reproducible reporting for camera-trap monitoring data.</em>
</p>

<p align="center">
  <a href="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>
  <a href="https://app.codecov.io/github/spatialecology/camtrapReport"><img src="https://codecov.io/github/spatialecology/camtrapReport/graph/badge.svg?token=9VBXAR9XOD" alt="Codecov test coverage"></a>
  <a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active"></a>
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white" alt="R ≥ 4.1.0"></a>
  <a href="https://github.com/spatialecology/camtrapReport/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-yellow.svg" alt="MIT"></a>
  <a href="https://doi.org/10.5281/zenodo.18405441"><img src="https://img.shields.io/badge/DOI-10.5281%2Fzenodo.18405441-blue.svg" alt="DOI"></a>
  <a href="https://spatialecology.github.io/camtrapReport/"><img src="https://img.shields.io/badge/docs-pkgdown-orange.svg" alt="pkgdown"></a>
</p>

---

`camtrapReport` converts camera-trap data in [Camtrap DP](https://camtrap-dp.tdwg.org/) format into two coordinated HTML outputs:

- a **Data Status Check**, which identifies issues in data completeness, consistency, annotation, and spatiotemporal coverage; and
- an **Ecological Report**, which combines selected ecological summaries, analyses, figures, tables, maps, metadata, and explanatory text in a configurable report.

The package focuses on the reporting workflow. It complements tools for camera-trap data management and specialised ecological modelling rather than replacing them.

## Use in practice

Although `camtrapReport` has not yet been applied in a published ecological study, it has been developed and evaluated using real-world camera-trap monitoring datasets from multiple study sites across North America, Asia, Europe, Africa, and Australia. Evaluation involved both developer-led testing and independent users who applied the package to their own datasets and provided feedback. In Europe, this evaluation has drawn particularly on datasets available through camera-trap research networks, including the European Observatory of Wildlife network, which spans diverse geographic settings and monitoring designs.

Experience across these heterogeneous datasets has informed checks for metadata completeness and spatiotemporal coverage, improvements to report generation, and revisions based on user feedback. Related publications, presentations, and training activities are listed on the [Resources](https://spatialecology.github.io/camtrapReport/articles/resources.html) page.

## Installation

`camtrapReport` requires R 4.1.0 or later. Install it from GitHub with `pak`:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install("spatialecology/camtrapReport")
library(camtrapReport)
```

Most report modules need only the packages imported by `camtrapReport`. Some analytical modules declare additional packages in their YAML definitions. These packages are loaded only when the corresponding modules run. To install the packages declared by all currently available modules, including modules added or modified by users, use:

```r
install_All()
```

`install_All()` is an explicit, opt-in helper. It discovers module declarations and delegates dependency resolution and installation to `pak`; it is never called when the package is loaded or when a report is rendered.

## A reproducible first report

The package includes a small Camtrap DP dataset for examples and tests. The code below copies it to a temporary directory because `camData()` may store a processed object beside writable input data, creates a `camReport` object, selects the lightweight introduction module, and renders a report.

```r
library(camtrapReport)

source_dataset <- system.file(
  "external",
  "dataset",
  package = "camtrapReport",
  mustWork = TRUE
)

example_root <- tempfile("camtrapReport-example-")
dir.create(example_root)
file.copy(source_dataset, example_root, recursive = TRUE)
example_dataset <- file.path(example_root, basename(source_dataset))

cm <- camData(example_dataset)
cm <- sections(cm, "introduction")

report_file <- report(
  cm,
  filename = file.path(example_root, "introduction-report"),
  view = FALSE
)

file.exists(report_file)
#> [1] TRUE

# Remove the temporary input and report when finished.
unlink(example_root, recursive = TRUE, force = TRUE)
```

Rendering requires Pandoc, which is included with RStudio and Quarto. To inspect data quality or create a report you intend to retain, provide an explicit output path:

```r
# Use a directory that already exists.
status(
  cm,
  filename = file.path("path", "to", "reports", "data-status"),
  view = TRUE
)

report(
  cm,
  filename = file.path("path", "to", "reports", "ecological-report"),
  view = TRUE
)
```

## Configure a report

The `camReport` object keeps the imported data, report metadata, settings, selected sections, and intermediate results together. Common operations include:

```r
# Inspect or update report metadata.
info(cm)
info(cm, "title") <- "Camera-trap monitoring report"

# Inspect available and selected sections.
section_names()
sections(cm)
listReportSections(cm)

# Keep a selected set of report sections.
cm <- sections(
  cm,
  section_names(keep = c("introduction", "methods", "study_area"))
)

# Open the interactive interface.
gui(cm)
```

Module YAML files can contain executable R code. Only add modules from trusted sources, and inspect their code, package requirements, network access, and file operations before running them.

## Interpretation and reproducibility

A successful Data Status Check confirms that the implemented checks completed; it does not establish that a survey design or dataset is suitable for a particular ecological analysis. Interpret results in relation to sampling effort, detectability, season, habitat, and method-specific assumptions.

For reproducibility, retain the input data, optional inputs, analytical settings, selected report sections, and relevant package versions. General guidance is available from the [Reproducible Research CRAN Task View](https://cran.r-project.org/view=ReproducibleResearch).

Reports may contain maps or derived results that reveal sensitive species locations. Review all content before sharing a report.

## Example-data provenance

The bundled data are a compact, relationship-preserving subset of the EOW
Veluwe Camtrap DP export produced through [Agouti](https://www.agouti.eu/).
The included [dataset metadata](inst/external/dataset/datapackage.json) retain
the source project and institutional rights holder, while personal contributor
names and contact details are replaced with clearly fictional `example.org`
records. The subset is intended for demonstration and software testing, not
ecological inference.

## Relationship to other camera-trap packages

Several R packages support related parts of camera-trap workflows. For example, `camtrapdp` reads, validates, and transforms Camtrap DP datasets; `camtraptor` supports exploration and visualisation; and `camtrapR` and `ct` provide broader data-management and analysis workflows.

`camtrapReport` complements these tools by integrating data-status checks, selected ecological analyses, figures, maps, tables, metadata, and explanatory text within a configurable reporting workflow. Its module framework allows additional analyses and report components to be incorporated without modifying the package core.

## Documentation and support

- [Get started](https://spatialecology.github.io/camtrapReport/)
- [Package overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html)
- [Data Status Check](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html)
- [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html)
- [Module management](https://spatialecology.github.io/camtrapReport/articles/modules.html)
- [Function reference](https://spatialecology.github.io/camtrapReport/reference/index.html)
- [Issue tracker](https://github.com/spatialecology/camtrapReport/issues)
- [GitHub repository](https://github.com/spatialecology/camtrapReport)

## Citation

Obtain the package citation with:

```r
citation("camtrapReport")
```

## Contributing

Bug reports, feature requests, documentation improvements, and new modules are welcome. Please read the [contributing guidelines](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md) and [Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md) before contributing.
