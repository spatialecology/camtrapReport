<!-- README.md is generated from README.Rmd. Edit README.Rmd, not README.md. -->

<h1 align="center">camtrapReport</h1>
<p align="center"><strong>From camera-trap data to reproducible ecological reports</strong></p>
<p align="center">
  <a href="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R CMD check"></a>
  <a href="https://github.com/ropensci/software-review/issues/799"><img src="https://badges.ropensci.org/799_status.svg" alt="rOpenSci review"></a>
  <a href="https://codecov.io/github/spatialecology/camtrapReport"><img src="https://codecov.io/github/spatialecology/camtrapReport/graph/badge.svg?token=9VBXAR9XOD" alt="Test coverage"></a>
  <a href="https://spatialecology.github.io/camtrapReport/"><img src="https://img.shields.io/badge/Documentation-Website-1976D2" alt="Documentation"></a>
  <a href="https://github.com/spatialecology/camtrapReport/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/License-MIT-2E7D32" alt="MIT License"></a>
</p>

`camtrapReport` turns [Camtrap DP](https://camtrap-dp.tdwg.org/) datasets into two configurable outputs:

- **Data Status Check** — checks completeness, consistency, classifications and spatial/temporal coverage.
- **Ecological Report** — combines analyses, maps, figures, tables and explanatory text.

It supports reproducible workflows in R or through a graphical interface. Reports are HTML by default; PDF output is also available where supported by the chosen modules and local setup.

<p align="center"><img src="vignettes/figures/package-architecture.png" width="820" alt="camtrapReport workflow from Camtrap DP data to reports"></p>

## What can it analyse?

| Area | Included analyses |
|:--|:--|
| **Sampling** | Study area, camera locations, sampling effort, data and image processing |
| **Species records** | Captures, capture-based trends, richness, species accumulation |
| **Community & behaviour** | Co-occurrence, activity patterns |
| **Space & habitat** | Spatial point-pattern density, habitat preferences |
| **Abundance & density** | Random Encounter Model (REM) parameters and density estimates |

Available analyses depend on the dataset, selected modules/profile and method-specific requirements. In particular, capture rates are **not** population-density estimates, and REM analyses need additional inputs and assumptions. The [Ecological Report guide](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html) describes modules and requirements; the [Module management guide](https://spatialecology.github.io/camtrapReport/articles/modules.html) covers extensions.

## Install

Requires **R 4.1.0 or later**. Install the development version:

```r
install.packages("pak") # if needed
pak::pkg_install("spatialecology/camtrapReport")
```

Some modules use optional packages. Install missing dependencies as needed, or use `camtrapReport::install_all()` to check and install module dependencies (which may be numerous).

## Quick start

Input must be a Camtrap DP ZIP file or extracted directory containing `datapackage.json`, `deployments.csv`, `media.csv` and `observations.csv`. If necessary, prepare your data with upstream tools such as [`camtrapdp`](https://inbo.github.io/camtrapdp/). See [`?camData`](https://spatialecology.github.io/camtrapReport/reference/camData.html) for input tables and their relationships, observations/sequences, and study-area limitations. The primary data input is currently a **path**, not an in-memory list of tables.

The following example uses bundled data. It copies the dataset to a writable temporary folder because `camData()` may save processed objects alongside its input.

```r
library(camtrapReport)

example_root <- tempfile("camtrapReport-")
dir.create(example_root)
file.copy(system.file("external", "dataset", package = "camtrapReport"),
          example_root, recursive = TRUE)

habitat <- read.csv(system.file("external", "habitat", "habitat.csv",
                                package = "camtrapReport"))
study_area <- system.file("external", "study_area", "study_area.shp",
                          package = "camtrapReport")

cm <- camData(file.path(example_root, "dataset"),
              habitat = habitat, study_area = study_area, update = TRUE)
cm
```

**1. Check the data**

```r
status(cm, view = FALSE)
```

[View example Data Status Check](https://spatialecology.github.io/camtrapReport/reports/DataStatusReport_example.html). If you correct the source files, rebuild the object with `camData(..., update = TRUE)` before rerunning `status()`. Checks identify possible problems; they do not automatically repair data or guarantee suitability for an ecological analysis.

**2. Generate a report**

```r
report(cm, view = FALSE)
```

[View example Ecological Report](https://spatialecology.github.io/camtrapReport/reports/EcologicalReport_example.html). Outputs vary with the dataset, module selection and optional inputs. Use `view = TRUE` to open a rendered report.

**3. Select sections or use the GUI**

```r
section_names()
sections(cm, section_names(keep = c("introduction", "methods",
                                    "study_area", "sampling", "effort")))
report(cm, view = FALSE)
gui(cm)
```

Report text comes from YAML-based modules and may incorporate metadata and analysis results. Create or adapt modules in a **project-level registry** rather than editing installed package files. See [Module management](https://spatialecology.github.io/camtrapReport/articles/modules.html).

## Reusable profiles

A **profile** specifies a reusable selection and order of ecological and data-status modules, allowing monitoring projects to follow consistent reporting conventions. The bundled **default** and **EOW** profiles use different reporting configurations; for example, the EOW REM workflow estimates parameters by species and year, whereas the default profile pools years.

```r
profile_names(cm)
section_names(object = cm, profile = "EOW")
report(cm, profile = "EOW", view = FALSE)
```

You can create, save and share custom profiles; see the [Profiles guide](https://spatialecology.github.io/camtrapReport/articles/profiles.html). A profile standardises settings but does not establish their scientific suitability for every survey.

## Learn more

**Guides:** [Data Status Check](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html) · [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html) · [Profiles](https://spatialecology.github.io/camtrapReport/articles/profiles.html) · [Module management](https://spatialecology.github.io/camtrapReport/articles/modules.html) · [Function reference](https://spatialecology.github.io/camtrapReport/reference/index.html)

`camtrapReport` complements data tools such as [`camtrapdp`](https://inbo.github.io/camtrapdp/), [`camtrapR`](https://jniedballa.github.io/camtrapR/), [`camtraptor`](https://inbo.github.io/camtraptor/) and [`ct`](https://cran.r-project.org/package=ct) by integrating data checks and ecological results into configurable reports. Development has drawn on real camera-trap datasets and user feedback; see [Resources](https://spatialecology.github.io/camtrapReport/articles/resources.html).

The bundled example is a reduced version of [GMU8_LEUVEN](https://doi.org/10.15468/4u3wm4), intended for software demonstration, **not ecological inference** ([provenance](https://github.com/spatialecology/camtrapReport/blob/main/inst/external/README-Leuven-dataset.md)). Retain original data, settings and package versions, interpret analyses in light of detectability and sampling design, and check reports for sensitive species locations before sharing.

**Maintainer:** [Elham Ebrahimi](https://orcid.org/0000-0001-5191-9832) · **Citation:** `citation("camtrapReport")` · [Contributing](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md) · [Code of conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md) · [Report an issue](https://github.com/spatialecology/camtrapReport/issues)

