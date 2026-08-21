
<!-- README.md is generated from README.Rmd. Edit README.Rmd, not README.md. -->

<p align="center">

<img src="inst/report-assets/camtrapReport-logo.png" width="200" alt="camtrapReport logo"/>
</p>

<h1 align="center">

camtrapReport
</h1>

<p align="center">

<em>Reproducible reporting for camera-trap monitoring data</em>
</p>

<p align="center">

<a href="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R CMD check status"></a>
<a href="https://github.com/spatialecology/camtrapReport/actions/workflows/pkgcheck.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/pkgcheck.yaml/badge.svg" alt="rOpenSci package check status"></a>
<a href="https://app.codecov.io/github/spatialecology/camtrapReport"><img src="https://codecov.io/github/spatialecology/camtrapReport/graph/badge.svg?token=9VBXAR9XOD" alt="Test coverage"></a>
<a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project status: active"></a>
<a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white" alt="R version 4.1.0 or later"></a>
<a href="https://github.com/spatialecology/camtrapReport/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-yellow.svg" alt="MIT license"></a>
<a href="https://spatialecology.github.io/camtrapReport/"><img src="https://img.shields.io/badge/docs-pkgdown-orange.svg" alt="pkgdown documentation"></a>
</p>

------------------------------------------------------------------------

`camtrapReport` turns camera-trap data in [Camtrap
DP](https://camtrap-dp.tdwg.org/) format into two coordinated HTML
outputs:

- a **Data Status Check**, which identifies issues in data completeness,
  consistency, annotation, and spatiotemporal coverage; and
- an **Ecological Report**, which brings selected ecological summaries,
  analyses, figures, tables, maps, metadata, and explanatory text into a
  configurable report.

The package focuses on the reporting workflow itself. It complements
tools for camera-trap data management and specialised ecological
modelling rather than replacing them.

<p align="center">

<img src="vignettes/figures/package-architecture.png" width="900" alt="Workflow from Camtrap DP input through the camReport object to the Data Status Check and Ecological Report"/>
<br> <em>Overview of the camtrapReport workflow.</em>
</p>

## Use in practice

`camtrapReport` has been developed and evaluated with real camera-trap
monitoring datasets that differ in geography, survey design, duration,
and data structure. These include datasets from the European Observatory
of Wildlife network and datasets used by external teams in Spain,
Slovenia, and Germany. The package has also been tested in workshops at
Utrecht University and the Research Institute for Nature and Forest in
Belgium.

These applications have primarily supported software validation, report
generation, and user feedback. Experience with heterogeneous datasets
has informed checks for metadata completeness, temporal and spatial
coverage, and robust report generation. Publications, presentations, and
training activities are listed on the [Resources
page](https://spatialecology.github.io/camtrapReport/articles/resources.html).

## Installation

`camtrapReport` requires R 4.1.0 or later. Install the development
version from GitHub with [`pak`](https://pak.r-lib.org/):

``` r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install(
  "spatialecology/camtrapReport",
  dependencies = TRUE
)
```

`dependencies = TRUE` installs optional packages used by particular
report modules, visualisations, and the graphical interface. The package
checks these dependencies when the relevant functionality is requested;
users do not need every optional package for every workflow.

## Quick start

The following example is executed when `README.md` is built. It uses a
compact Camtrap DP dataset derived from **GMU8_LEUVEN**, together with
bundled habitat data and a study-area boundary. The files are copied to
a temporary directory because `camData()` may store processed files
beside a writable input dataset.

``` r
library(camtrapReport)

example_root <- tempfile("camtrapReport-example-")
dir.create(example_root)

file.copy(
  system.file("external", "dataset", package = "camtrapReport"),
  example_root,
  recursive = TRUE
)
#> [1] TRUE

cm <- camData(
  file.path(example_root, "dataset")
)

cm
#> Camera trap Object for the site : GMU8 LEUVEN
#> =====================================================
#> Total number of sequences       :  10182
#> Total number of observations    :  10798
#> Total number of animals         :  4693
#> Total number of detected species:  41
#> Date/time (years) with data     :  2018, 2019, 2020, 2021, 2022, and 2023
#> -----------------------------------------------------
```

`camData()` also accepts a Camtrap DP ZIP archive or an extracted
Camtrap DP directory. Habitat data and a study-area boundary are
optional.

### Generate a Data Status Check

``` r
status(cm, view = FALSE)
#> Rendering R Markdown data_status report ...
#> Data_Status Report generated at: C:\Users\ebrah010\AppData\Local\Temp\RtmpS261EZ\data_status.html
```

### Generate an Ecological Report

``` r
report(cm, view = FALSE)
#> Rendering R Markdown report ...
#>
#> Quitting from report.Rmd:510-654 [location_camera_locations_leaflet]
#> Report generation is stopped because of an error; add `test = TRUE` to exclude the modules that cause error!
#> [1] "Error in gzfile(file, \"rb\") : cannot open the connection\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <error/rlang_error>
#> Error in `gzfile()`:
#> ! cannot open the connection
#> ---
#> Backtrace:
#>     ▆
#>  1. └─camtrapReport (local) plot_locations(...)
#>  2.   ├─terra::readRDS(object$study_area$path)
#>  3.   └─terra::readRDS(object$study_area$path)
#>  4.     └─base::readRDS(file = file, refhook = refhook)
#>  5.       └─base::gzfile(file, "rb")
```

The examples use `view = FALSE` so that rendering `README.Rmd` does not
open browser windows. In an interactive R session, use `view = TRUE` to
open the generated report in the default RStudio viewer or web browser.

## Configure a report

Report metadata and available report sections can be inspected and
modified before generating the report:

``` r
# Inspect selected metadata
info(cm, name = c("title", "authors"))
#> $title
#> [1] "Camera-Trap Monitoring Report for GMU8 LEUVEN, Belgium"
#>
#> $authors
#> [1] "Martijn Bollen, Niko Boone, Jim Casaer, Peter Desmet, Sander Devisscher, Sanne Govaert, Lynn Pallemaerts, Anneleen Rutten, and Jan Vercammen"
#>
#> attr(,"class")
#> [1] "camInfo"

# Inspect all available report sections
section_names()
#>  [1] "introduction"         "methods"              "study_area"
#>  [4] "sampling"             "location"             "effort"
#>  [7] "image_processing"     "data_processing"      "results"
#> [10] "captures"             "abundance_trends"     "population_density"
#> [13] "model_parameters"     "population_densities" "activity_patterns"
#> [16] "richness"             "co_occurrence"        "spatial_density"
#> [19] "habitat_preferences"  "species_accumulation" "acknowledgements"
#> [22] "appendix"             "References"

# Select report sections
selected_sections <- section_names(
  keep = c(
    "introduction",
    "methods",
    "study_area",
    "sampling",
    "effort"
  )
)

sections(cm, selected_sections)
#>
#> The report sections are updated.
```

Reports can also be configured for selected taxonomic groups, survey
years, and observation-count thresholds. Existing report sections can be
updated or extended. See the [Ecological
Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html)
and [Module
management](https://spatialecology.github.io/camtrapReport/articles/modules.html)
guides for detailed examples.

An interactive graphical interface is available through `gui(cm)` for
users who prefer to configure and generate reports without working
entirely from the console.

## Interpretation and reproducibility

`camtrapReport` standardises data checks, calculations, and report
assembly. A successful Data Status Check does not establish that the
sampling design is suitable for a particular ecological question or that
the data meet the assumptions of a specific analysis. Results should be
interpreted in relation to sampling effort, detectability, season,
habitat, and the assumptions of each method.

For reproducibility, retain the input data, optional inputs, analytical
settings, and selected report sections, and record the versions of R and
relevant packages. See the [Reproducible Research CRAN Task
View](https://cran.r-project.org/view=ReproducibleResearch) for general
guidance.

Reports may contain maps, metadata, or derived results that reveal
sensitive species locations. Review all report content before sharing
it.

## Example-data provenance

The bundled example dataset is derived from the [**GMU8_LEUVEN** Camtrap
DP dataset](https://doi.org/10.15468/4u3wm4), published by the Research
Institute for Nature and Forest (INBO) and available through GBIF. The
bundled subset is intended for demonstrating and testing the package,
not for ecological inference. Its selection and preparation are
documented in
[`inst/external/README-Leuven-dataset.md`](https://github.com/spatialecology/camtrapReport/blob/main/inst/external/README-Leuven-dataset.md).

## Relationship to other camera-trap packages

Several R packages cover related parts of camera-trap workflows:

- [`camtrapdp`](https://inbo.github.io/camtrapdp/) reads, validates, and
  transforms Camtrap DP datasets;
- [`camtraptor`](https://inbo.github.io/camtraptor/) explores and
  visualises Camtrap DP datasets; and
- [`camtrapR`](https://jniedballa.github.io/camtrapR/) and
  [`ct`](https://cran.r-project.org/package=ct) provide broader
  camera-trap data-management and analysis workflows.

`camtrapReport` is complementary to these packages. Its distinct purpose
is to turn Camtrap DP input into a configurable ecological report
combining data checks, selected analyses, maps, tables, metadata, and
explanatory text. It does not replace specialised analysis or modelling
packages; their methods can be incorporated through report modules where
appropriate.

## Documentation and support

- [Get started](https://spatialecology.github.io/camtrapReport/)
- [Package
  overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html)
- [Data Status
  Check](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html)
- [Ecological
  Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html)
- [Module
  management](https://spatialecology.github.io/camtrapReport/articles/modules.html)
- [Function
  reference](https://spatialecology.github.io/camtrapReport/reference/index.html)
- [Issue
  tracker](https://github.com/spatialecology/camtrapReport/issues)
- [GitHub repository](https://github.com/spatialecology/camtrapReport)

## Citation

To obtain the current citation for the installed package, run:

``` r
citation("camtrapReport")
#> To cite camtrapReport in publications, use:
#>
#>   Ebrahimi E (2026). _camtrapReport: Camera-Trap Report Generator_. R
#>   package version 1.0.47,
#>   <https://spatialecology.github.io/camtrapReport/>.
#>
#> A BibTeX entry for LaTeX users is
#>
#>   @Manual{,
#>     title = {camtrapReport: Camera-Trap Report Generator},
#>     author = {Elham Ebrahimi},
#>     year = {2026},
#>     note = {R package version 1.0.47},
#>     url = {https://spatialecology.github.io/camtrapReport/},
#>   }
```

## Author

`camtrapReport` is developed and maintained by [Elham
Ebrahimi](https://orcid.org/0000-0001-5191-9832).

## Contributing

Bug reports, feature requests, documentation improvements, code
contributions, and proposals for new report modules are welcome. Before
contributing, please read the [contributing
guidelines](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md)
and the [Code of
Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md).
