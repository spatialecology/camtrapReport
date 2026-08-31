---
title: "Get started"
---

# camtrapReport

*Reproducible reporting for camera-trap monitoring data*

`camtrapReport` converts camera-trap data in [Camtrap DP](https://camtrap-dp.tdwg.org/) format into a Data Status Check and a configurable Ecological Report. It coordinates data checks, selected analyses, figures, maps, tables, metadata, and explanatory text while keeping the reporting workflow reproducible.

## Use in practice

Although `camtrapReport` has not yet been applied in a published ecological study, it has been developed and evaluated with real-world camera-trap monitoring datasets from study sites in North America, Asia, Europe, Africa, and Australia. Both developers and independent users have tested the package with their own datasets and provided feedback. Related publications, presentations, and training activities are listed on the [Resources](articles/resources.html) page.

## Installation

`camtrapReport` requires R 4.1.0 or later. Install the development version with `pak`:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install("spatialecology/camtrapReport")
library(camtrapReport)
```

Some analytical modules declare additional packages in their YAML definitions. To install the packages declared by all currently available modules, including modules added or modified by users, run `install_All()`. This explicit, opt-in helper discovers the module declarations and delegates installation to `pak`; it is never called when the package is loaded or when a report is rendered.

## A reproducible first report

The package includes a small Camtrap DP dataset. This example copies it to a temporary writable directory, creates the central report object, retains only the lightweight introduction section, and renders an HTML report.

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

unlink(example_root, recursive = TRUE, force = TRUE)
```

Rendering requires Pandoc, which is included with RStudio and Quarto. Supply an explicit path to keep an output:

```r
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

`camData()` returns a `camReport` object containing the processed data, report metadata, settings, selected sections, and intermediate results. Users normally interact with it through package functions:

```r
info(cm)
info(cm, "title") <- "Camera-trap monitoring report"

section_names()
sections(cm)
listReportSections(cm)

cm <- sections(
  cm,
  section_names(keep = c("introduction", "methods", "study_area"))
)

# Launch the interactive interface when desired.
gui(cm)
```

Module YAML files can contain executable R code. Only add modules from trusted sources, and inspect their code, package requirements, network access, and file operations before running them.

## Interpretation and reproducibility

A successful Data Status Check confirms that the implemented checks completed; it does not establish that a survey design or dataset is suitable for a particular ecological analysis. Interpret outputs in relation to sampling effort, detectability, season, habitat, and method-specific assumptions.

Retain the input data, optional inputs, settings, selected sections, and relevant package versions. See the [Reproducible Research CRAN Task View](https://cran.r-project.org/view=ReproducibleResearch) for general guidance.

## Example-data provenance

The bundled data are a compact, relationship-preserving subset of the EOW Veluwe Camtrap DP export produced through [Agouti](https://www.agouti.eu/). The included dataset metadata retain the source project and institutional rights holder, while personal contributor names and contact details are replaced with clearly fictional `example.org` records. The subset is intended for demonstration and software testing, not ecological inference.

## Relationship to other packages

`camtrapReport` complements related packages rather than replacing them. `camtrapdp` reads, validates, and transforms Camtrap DP datasets; `camtraptor` supports exploration and visualisation; and `camtrapR` and `ct` provide broader data-management and analysis workflows. `camtrapReport` contributes a configurable report-centred layer that connects data-status checks, analyses, visual outputs, metadata, and narrative text.

## Documentation and support

- [Package overview](articles/Package-Overview.html)
- [Data Status Check](articles/data-status-report.html)
- [Ecological Report](articles/ecological-report.html)
- [Module management](articles/modules.html)
- [Function reference](reference/index.html)
- [Resources](articles/resources.html)
- [Issue tracker](https://github.com/spatialecology/camtrapReport/issues)
- [GitHub repository](https://github.com/spatialecology/camtrapReport)

## Citation

```r
citation("camtrapReport")
```

## Author and contributing

`camtrapReport` is developed and maintained by [Elham Ebrahimi](https://orcid.org/0000-0001-5191-9832). Bug reports and contributions are welcome through the [GitHub repository](https://github.com/spatialecology/camtrapReport); please read the [contributing guidelines](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md) and [Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md).
