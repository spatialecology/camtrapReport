<p align="center">
  <img src="man/figures/logo.png" width="230" alt="camtrapReport logo"/>
</p>

<h1 align="center">camtrapReport</h1>

<p align="center">
  <a href="https://www.r-project.org/">
    <img src="https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg" alt="R >= 4.1.0">
  </a>
  <a href="LICENSE">
    <img src="https://img.shields.io/badge/licence-GPL--3-green.svg" alt="GPL-3 licence">
  </a>
  <a href="https://doi.org/10.5281/zenodo.18405441">
    <img src="https://img.shields.io/badge/DOI-10.5281%2Fzenodo.18405441-blue.svg" alt="DOI">
  </a>
</p>

<p align="center">
  <strong>From standardised camera-trap data to reproducible ecological reports.</strong>
</p>

---

## Overview

`camtrapReport` is a modular and extensible R package for automating camera-trap data reporting in wildlife monitoring. It is designed for datasets provided in, or converted to, the [Camtrap-DP standard format](https://camtrap-dp.tdwg.org/), and provides a reproducible workflow for transforming standardised camera-trap records into structured ecological outputs. The package addresses a common bottleneck in camera-trap projects: although data-management platforms and standards have improved the organisation, exchange and publication of camera-trap data, ecological interpretation often still requires fragmented scripts, repeated manual checks and ad hoc report preparation.

`camtrapReport` is designed to automate the camera-trap reporting workflow as much as possible. Its two main outputs are:

1. **Data Status Check Report**: evaluates dataset completeness, consistency, annotation quality, spatial and temporal coverage, and readiness for ecological analysis.

2. **Ecological Insight Report**: generates modular ecological outputs, including maps, tables, figures and narrative text, with commonly used camera-trap analyses such as sampling effort, species richness, activity patterns, co-occurrence, habitat use, abundance trends and spatial density.

The modular design allows users to update, extend or replace report sections according to their research aims, monitoring questions and data availability, while also supporting privacy-aware sharing when raw images or precise locations cannot be openly released.

<p align="center">
  <img src="man/figures/camtrapReport.png" width="900" alt="Conceptual workflow of camtrapReport"/>
</p>

<p align="center">
  <em>Schematic overview of the `camtrapReport` workflow. Camera-trap data are imported and harmonised during pre-processing, then stored in a reusable `camReport` object. Dedicated methods perform data-quality checks and ecological analyses, populate modular report sections and compile the selected outputs into Data Status Check and Ecological Reports.</em>
</p>

---

## Installation

Install the development version from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("spatialecology/camtrapReport")
```

Load the package:

```r
library(camtrapReport)
```

Some report modules require additional packages. These can be installed with:

```r
install_All()
```

---

## Input data

The required input is a single `.zip` file containing a dataset in [Camtrap-DP](https://camtrap-dp.tdwg.org/) format. Optional inputs, including a **habitat table** and a **study-area polygon**, can be added to enrich maps, summaries and ecological outputs.

An example `habitat.csv` template can be downloaded [here](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing), and an example input-folder structure is shown [here](https://drive.google.com/file/d/1ykJA_aKsWuNsyiiTrUUyTo-7CdXA_UIy/view?usp=sharing). The Camtrap-DP ZIP file is the only mandatory input; all other files are optional.

---

## Example data

If you do not have access to your own Camtrap-DP dataset, you can test `camtrapReport` using the Leuven example dataset, an open-source camera-trap dataset available through [GBIF](https://www.gbif.org/composition/4fZGV2vrXjo3rNxySz41sj/exploring-camera-trap-data).

| Dataset | Optional inputs | Quick test |
|:---:|:---:|:---:|
| [Leuven Camtrap-DP dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Habitat table](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) · [Study-area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) | [Smaller Leuven subset](https://drive.google.com/file/d/1N_dAABTlJVP1Fj655RqS3RsOclBTzOBw/view?usp=sharing) |

**Note:** The habitat table and study-area boundary were prepared only for testing `camtrapReport` and are not official publications from the original dataset owners. The full Leuven dataset is relatively large because it covers several years and many camera-trap locations, so processing may take some time. For a quick first test, use the smaller subset.

---
---
## Get started

### 1. Create a `camReport` object

`camReport` is the central object used in the `camtrapReport` workflow. It stores the input data, metadata, settings, derived summaries and report components used to generate the outputs.

The only required input is a single `.zip` file containing the dataset in Camtrap-DP format.

```r
cm <- camData("path/to/camtrap-dp-dataset.zip")
```

With optional habitat and study-area inputs:

```r
library(terra)

habitat <- read.csv("path/to/habitat.csv")
study_area <- terra::vect("path/to/study_area.shp")

cm <- camData(
  data = "path/to/camtrap-dp-dataset.zip",
  habitat = habitat,
  study_area = study_area
)

cm
```

### 2. Generate the Data Status Check report

```r
status(cm, view = TRUE)
```

### 3. Generate the Ecological Report

```r
report(cm, view = TRUE)
```

Reports are saved in the output directory associated with the `camReport` object. With `view = TRUE`, the generated HTML report opens automatically.

---

## Typical workflow

```r
library(camtrapReport)

cm <- camData("path/to/camtrap-dp-dataset.zip")

status(cm, view = TRUE)

report(cm, view = TRUE)
```

After the `camReport` object has been created, users can inspect and update report metadata.

```r
info(cm)

info(cm, name = "authors") <- c("First Author", "Second Author")
info(cm, name = "title") <- "Camera-Trap Report for Example Site"
```

Users can also adjust the focus group, year range and filters.

```r
names(cm$group_definition)

cm$set_focus_group("large_mammals")

cm$years <- 2021:2023

cm$filterCount <- 25

cm$setup()
```

---

## Focus groups

The Ecological Report can be generated for selected taxonomic or observation groups. The selected focus group determines which records are used in the analyses.

```r
names(cm$group_definition)

cm$set_focus_group("large_mammals")
cm$setup()
```

Users can also define custom groups.

```r
cm$add_group(
  name = "example_group",
  x = list(
    scientificName = c("Vulpes vulpes", "Meles meles"),
    class = "Mammalia"
  )
)

cm$set_focus_group("example_group")
cm$setup()
```

---

## Report sections

`camtrapReport` is built around modular report sections. Sections can include text, code, tables, figures, maps, captions and package dependencies. This design allows the workflow to be adapted to different taxa, monitoring objectives, study designs and reporting needs.

List available or attached report sections.

```r
listReportSections(cm)

sections(cm)

section_names()
```

Update the text of an existing report section.

```r
updateReportSection(
  cm,
  section = "introduction",
  text = "This section was updated by the user.",
  append_text = FALSE
)
```

Generate the report after editing.

```r
report(cm, view = TRUE)
```

---

## Graphical user interface

A graphical user interface is available for interactive exploration and reporting.

```r
gui(cm)
```

---

## Learn more

This README introduces the basic workflow for setting up input data, reviewing data quality and generating reports with `camtrapReport`.

- For the package structure and main concepts, see the [Package Overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html).
- For data-quality checks, see the [Data Status Report](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html).
- For ecological reporting and customisation, see the [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html).
- For the modular and extensible design, including how to add new report sections, see [Module Management](https://spatialecology.github.io/camtrapReport/articles/modules.html).

---

## Contribute

Questions, suggestions and ideas for improvement are welcome. You can contribute by [opening an issue](https://github.com/spatialecology/camtrapReport/issues) or joining the conversation in [GitHub Discussions](https://github.com/spatialecology/camtrapReport/discussions).

---
