
<!-- README.md is generated from README.Rmd. Edit this source, not README.md. -->

# camtrapReport

<p class="lead">

From camera-trap data to reproducible ecological reports.
</p>

<p class="status-badges">

<a href="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R CMD check status"></a>
<a href="https://github.com/spatialecology/camtrapReport/actions/workflows/pkgcheck.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/pkgcheck.yaml/badge.svg" alt="rOpenSci package check status"></a>
<a href="https://github.com/ropensci/software-review/issues/799"><img src="https://badges.ropensci.org/799_status.svg" alt="Under rOpenSci Software Peer Review"></a>
<a href="https://app.codecov.io/github/spatialecology/camtrapReport"><img src="https://codecov.io/github/spatialecology/camtrapReport/graph/badge.svg?token=9VBXAR9XOD" alt="Test coverage"></a>
<a href="https://github.com/spatialecology/camtrapReport/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-yellow.svg" alt="MIT license"></a>
</p>

`camtrapReport` turns [Camtrap DP](https://camtrap-dp.tdwg.org/)
datasets into two complementary outputs:

- **Data Status Check:** inspect completeness, consistency, annotation,
  and spatial and temporal coverage before interpreting your data.
- **Ecological Report:** combine selected analyses, maps, figures,
  tables, and explanatory text in a reproducible document.

Work in R or through the optional graphical interface. HTML is the
default; static PDF output is also available with compatible modules,
the optional `pagedown` package, and Chrome or Edge.

<p class="workflow-figure">

<img src="vignettes/figures/package-architecture.png" width="820"
       alt="Camtrap DP data pass through camtrapReport to a Data Status Check and an Ecological Report">
</p>

## Available analyses

Choose analyses to match your monitoring question and available
measurements.

| Area | Included analyses |
|:---|:---|
| Sampling | Study area, camera locations, sampling effort, image and data processing |
| Species records | Captures, capture-based trends, richness, species accumulation |
| Community and behaviour | Co-occurrence and activity patterns |
| Space and habitat | Spatial point-pattern density and habitat preferences |
| Population density | Random Encounter Model (REM) parameters and estimates |

Capture rates and spatial point-pattern density are **not**
population-density estimates. REM requires detection, movement,
activity, effort, and count data, as well as an appropriate survey
design. A module’s availability does not establish its suitability for
your dataset. The [Ecological Report
guide](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html)
explains the workflow and method-specific requirements.

## Install and create your first report

Requires R 4.1.0 or later. Install from GitHub using
[`pak`](https://pak.r-lib.org/):

``` r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pkg_install("spatialecology/camtrapReport")
```

Some analytical modules require optional packages. Review their
`#| packages:` declarations before installation; explicitly calling
`camtrapReport::install_all()` discovers and installs missing module
dependencies, which may be numerous. It is not needed for the small
example below. See [dependency inspection without
installation](https://spatialecology.github.io/camtrapReport/articles/modules.html#inspect-dependencies-without-installing).

### Prepare the input

Use a Camtrap DP directory or ZIP containing `datapackage.json`,
`deployments.csv`, `media.csv`, and `observations.csv`. Keep the tables’
linked identifiers intact. The primary input is a path, not an in-memory
list of tables. Use your platform’s Camtrap DP export or upstream
[Camtrap DP preparation and validation
guidance](https://camtrap-dp.tdwg.org/#validation); `camtrapReport` is
not a general arbitrary-table converter or complete schema validator.
The [package
overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html#input-data)
explains table relationships, derived sequences, and optional study
areas.

The example copies bundled data to a temporary directory because
`camData()` can save a processed-object cache alongside its inputs.

``` r
library(camtrapReport)

example_root <- tempfile("camtrapReport-example-")
dir.create(example_root)
stopifnot(file.copy(
  system.file("external", "dataset", package = "camtrapReport"),
  example_root, recursive = TRUE
))
cm <- camData(file.path(example_root, "dataset"), update = TRUE)
#> Warning in cm$setup(): The "suncalc" package is not installed; it is required
#> for the analysis of activity patterns...
```

### Check data, then generate a small report

This deliberately small profile needs no additional analytical packages:
three status sections and an introduction-only ecological report.

``` r
first_report <- reportProfile(
  name = "first-report",
  report = "introduction",
  status = c("spatial", "temporal", "availability")
)

status_file <- status(
  cm, profile = first_report,
  filename = file.path(example_root, "first-status.html"),
  view = FALSE
)
report_file <- report(
  cm, profile = first_report,
  filename = file.path(example_root, "first-report.html"),
  view = FALSE
)
stopifnot(file.exists(status_file), file.exists(report_file))
```

Open the returned files, or use `view = TRUE` when generating them. For
fuller illustrations, view the [Data Status
Check](https://spatialecology.github.io/camtrapReport/reports/DataStatusReport_example.html)
and [Ecological
Report](https://spatialecology.github.io/camtrapReport/reports/EcologicalReport_example.html).
Those separate reports may use different data, inputs, profiles, and
optional packages; they are not the expected output of this minimal
example.

Correct source records when needed, rebuild with
`cm <- camData(data_path, update = TRUE)`, then run `status()` again.
Rerunning a report on the existing object does not reread edited
CSV/JSON files. Checks identify possible problems; they do not repair
records, assign an overall quality grade, or guarantee analytical
suitability.

## GUI and customisation

Inspect modules, select sections, and update narrative or metadata:

``` r
section_names(object = cm, profile = "default")
sections(cm, "introduction")
info(cm, name = c("title", "authors"))

report(cm, filename = file.path(tempdir(), "custom-report.html"))
gui(cm)
```

Shiny supports `gui()`; the code workflow does not start an application.
Report narrative comes from YAML modules’ text and R code, together with
object metadata. Use `updateReportSection()` or `info()` for
object-level edits, or copy the registry into a writable project folder
for reusable module changes. Only load trusted modules: their R code and
embedded expressions execute with your session’s permissions. The
[module
guide](https://spatialecology.github.io/camtrapReport/articles/modules.html)
explains copying, editing, reloading, testing, and sharing modules.

## Reusable analytical profiles

Profiles are context-specific, reusable selections of modules and their
organisation. They let a project or monitoring network agree on a
reporting workflow without imposing one universal analysis on every
survey.

- **Default:** the standard ecological selection; REM parameters are
  pooled across years within species, while trap rate remains
  year-specific.
- **EOW:** customised sections for the European Observatory of Wildlife
  workflow, annual species-by-year REM parameters, and selected status
  checks in the ecological appendix. Inclusion is not a claim of formal
  network endorsement.

A profile stores module references and optional parent overrides,
**not** data, fitted models, package versions, or independent parameter
settings. Analytical choices live in the referenced modules and object
settings; retain those alongside the profile when sharing a reproducible
workflow. Pooled and annual REM answer different parameter-sharing
questions and can give different estimates. Choose and document the
approach; neither is automatically correct for every dataset.

``` r
profile_names(cm)
section_names(object = cm, profile = "EOW")
report(
  cm, profile = "EOW",
  filename = file.path(tempdir(), "EOW-report.html")
)

write_profile("EOW", file.path(tempdir(), "EOW-profile.yml"), object = cm)
```

For a static PDF, use a `.pdf` filename or `format = "pdf"`. PDF output
has no table of contents; printable tab panels are expanded. The
registry’s `formats` column controls module compatibility. See the
[Profiles
guide](https://spatialecology.github.io/camtrapReport/articles/profiles.html)
for creating, editing, importing, and exporting profiles.

A community knowledge hub of reviewed analytical workflows is a **future
direction**, not a currently implemented service. Projects can already
share profiles with their required module files and documented settings
through normal version-controlled collaboration.

## Technical guidance and interpretation

Continue with the guide that matches your next task:

- [Package
  overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html):
  input relationships, cache files, object structure, scope, and
  reproducibility.
- [Data Status
  Check](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html):
  review checks and repeat them after corrections.
- [Ecological
  Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html):
  filters, years, methods, report timing, PDF output, and map
  backgrounds.
- [Profiles](https://spatialecology.github.io/camtrapReport/articles/profiles.html)
  and [module
  management](https://spatialecology.github.io/camtrapReport/articles/modules.html):
  reuse workflows or develop trusted extensions.
- [Function
  reference](https://spatialecology.github.io/camtrapReport/reference/index.html):
  exact arguments and return values.

Retain input data, optional inputs, analytical settings, module/profile
versions, and software versions. Interpret results in light of sampling
effort, detectability, season, habitat, and method-specific assumptions.
Diagnostic `test = TRUE` can omit failed sections; it is not a
substitute for successful full analyses. The default offline map
background needs no tile service; online backgrounds depend on provider
permissions and availability.

When `taxize` is installed, import may query GBIF automatically; the
current API has no per-call opt-out. This can affect privacy, offline
use, and reproducibility. Reports can reveal sensitive species
locations; review them before sharing. See the overview for details on
these limits and caches.

The bundled example is a relationship-preserving subset of
[GMU8_LEUVEN](https://doi.org/10.15468/4u3wm4), intended for software
demonstration and testing, **not ecological inference** ([provenance and
preparation](https://github.com/spatialecology/camtrapReport/blob/main/inst/external/README-Leuven-dataset.md)).

## Citation, development, and support

Developed and maintained by [Elham
Ebrahimi](https://orcid.org/0000-0001-5191-9832). Use
`citation("camtrapReport")` for the software and associated paper
entries.

Development has drawn on real monitoring datasets and independent user
feedback across regions, including European monitoring networks; see
[publications and training
resources](https://spatialecology.github.io/camtrapReport/articles/resources.html).
The package complements data-management and analysis tools such as
[`camtrapdp`](https://inbo.github.io/camtrapdp/),
[`camtraptor`](https://inbo.github.io/camtraptor/),
[`camtrapR`](https://jniedballa.github.io/camtrapR/), and
[`ct`](https://cran.r-project.org/package=ct).

[Report an
issue](https://github.com/spatialecology/camtrapReport/issues) or read
the [contributing
guide](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md)
and [code of
conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md)
before contributing a module, profile, documentation improvement, or
fix.
