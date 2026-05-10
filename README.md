<p align="center">
  <img src="man/figures/logo.png" width="260" alt="camtrapReport logo"/>
</p>

# camtrapReport

<!-- badges: start -->
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-green.svg)](LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18405441.svg)](https://doi.org/10.5281/zenodo.18405441)
<!-- badges: end -->

`camtrapReport` is a modular R package for processing standardised camera-trap datasets and automatically generating reproducible ecological reports. It is designed for datasets provided in, or converted to, the [Camtrap-DP standard format](https://camtrap-dp.tdwg.org/), and integrates data-quality diagnostics, preprocessing, ecological analysis, visualisation and report assembly within a single workflow.

The package produces two main outputs:

1. a **Data Status Check report**, which evaluates data completeness, consistency, annotation quality and readiness for ecological analysis; and
2. an **Ecological Insight report**, which assembles selected analytical modules into a structured, reproducible, article-style report.

`camtrapReport` aims to reduce the gap between camera-trap data collection and ecological interpretation by providing a transparent, extensible and reproducible workflow for wildlife monitoring, biodiversity assessment and conservation reporting.

## Why camtrapReport?

Camera traps are widely used in wildlife monitoring and biodiversity research because they provide continuous, non-invasive information on species occurrence, behaviour, abundance and community composition. However, transforming camera-trap records into interpretable ecological outputs often requires many fragmented steps: checking metadata, identifying missing or inconsistent values, summarising deployments and observations, calculating sampling effort, producing maps and figures, and assembling outputs into a report.

`camtrapReport` brings these steps together in one workflow. It helps users:

- import and process Camtrap-DP datasets;
- diagnose spatial, temporal, taxonomic, annotation and validation issues;
- summarise deployments, observations, species records and sampling effort;
- generate a Data Status Check before ecological analysis;
- produce an Ecological Insight report with modular analytical sections;
- customise metadata, focus groups, years, filters and report sections;
- create reproducible outputs that can be shared even when raw images or sensitive locations cannot be released.

## Main outputs

### 1. Data Status Check report

The Data Status Check report is a pre-analysis quality-control report. It evaluates whether the dataset is suitable for automated ecological reporting and highlights issues that may need correction.

Typical checks include:

- dataset setup and metadata summary;
- spatial checks, including coordinate range, duplicate coordinates, missing spatial metadata, spatial outliers and study-area extent;
- temporal checks, including year coverage, first and last records, missing deployment intervals, invalid timestamps and temporal inconsistencies;
- availability of essential fields in locations, deployments, media, observations, sequences and taxonomy;
- species records retained at species level and their event-level captures;
- validation and annotation summaries;
- observation types by capture method;
- an overall data-quality classification.

The Data Status Check classifies dataset readiness into three broad categories:

- **Perfect**: all key checks passed and no major issues were detected;
- **Acceptable**: minor issues were found, but the dataset remains usable; corrections are recommended;
- **Needs Improvement**: major issues were identified and should be corrected before generating the final Ecological Insight report.

### 2. Ecological Insight report

The Ecological Insight report generates standardised ecological summaries and visualisations from the prepared `camReport` object. Depending on data availability and selected modules, the report can include:

- study-site and sampling summaries;
- sampling effort;
- species accumulation curves;
- species richness maps;
- species co-occurrence patterns;
- spatial density maps;
- capture-based abundance trends;
- capture rates per 100 trap-days;
- number of occupied camera-trap locations;
- activity patterns;
- habitat-use summaries;
- REM-based density estimates and model parameters, where sufficient information is available.

The report is modular: users can include, exclude, reorder or modify report sections according to the objectives of their study.

## Installation

Install `camtrapReport` from GitHub using the `remotes` package:

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

Some report modules require optional packages. To install all optional/reporting dependencies used by the full workflow, run:

```r
install_All()
```

Check the installed version:

```r
packageVersion("camtrapReport")
```

## Input data

The main input is a single `.zip` file containing a dataset in [Camtrap-DP](https://camtrap-dp.tdwg.org/) format. Camtrap-DP is a standard for exchanging and archiving camera-trap data.

The expected core files are:

- `datapackage.json`
- `deployments.csv`
- `media.csv`
- `observations.csv`

Optional files can improve maps and ecological interpretation:

- a habitat table, usually a CSV file containing camera-trap location names and habitat classes;
- a study-area polygon, such as a shapefile or another spatial vector object.

A simple habitat table should contain at least:

| locationName | Habitat |
|---|---|
| VEL-01 | Forest |
| VEL-02 | Grassland |
| VEL-03 | Wetland |

## Example data

If you do not have your own Camtrap-DP dataset, you can test `camtrapReport` using the Leuven example dataset and optional spatial inputs.

| Data | Habitat | Study area |
|:---:|:---:|:---:|
| [Leuven dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Leuven habitat](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) | [Leuven study area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) |

**Note 1:** The [habitat] (https://doi.org/10.1038/s41597-025-06235-7) and study-area files were prepared for testing `camtrapReport` and are not official publications from the original dataset owners.
**Note 2:** The original Leuven dataset is large, covering several years and more than 300 unique camera-trap locations, so processing may take some time. For a quicker first test, use [this smaller subset] (https://drive.google.com/file/d/1N_dAABTlJVP1Fj655RqS3RsOclBTzOBw/view?usp=sharing).

The original Leuven dataset is large and covers several years and many camera-trap locations, so processing may take some time depending on your computer.

## Quick start

### 1. Create a `camReport` object

The `camReport` object stores the imported data, settings, summaries, analyses and report components.

```r
cm <- camData("path/to/camtrap-dp-dataset.zip")
```

With optional habitat data and a study-area boundary:

```r

habitat <- read.csv("path/to/Leuven_habitat.csv")
study_area <- terra::vect("path/to/Leuven_study_area.shp")

cm <- camData(
  data = "path/to/Leuven_dataset.zip",
  habitat = habitat,
  study_area = study_area
)

cm
```

### 2. Generate the Data Status Check report

```r
status(cm, view = TRUE)
```

The report is saved to the working directory. With `view = TRUE`, it opens automatically after rendering.

### 3. Generate the Ecological Insight report

```r
report(cm, view = TRUE)
```

The Ecological Insight report is also saved to the working directory and opens automatically when `view = TRUE`.

## Metadata and report information

The package extracts metadata from the dataset where possible. Users can inspect or overwrite report metadata before rendering.

```r
# Show available metadata
info(cm)

# Retrieve selected fields
info(cm, name = "title")
info(cm, name = "subtitle")
info(cm, name = "authors")
info(cm, name = "institute")

# Update fields
info(cm, name = "authors") <- c("First Author", "Second Author")
info(cm, name = "institute") <- "Institution name"

# Check updates
info(cm)
```

## Focus groups

Reports can be generated for selected taxonomic or observation groups. The selected focus group determines which species and records are used in the Ecological Insight report.

```r
# Current focus group
cm$setting$focus_groups

# Available group definitions
names(cm$group_definition)

# Change the focus group
cm$set_focus_group("large_mammals")
cm$setup()
```

Predefined groups may include groups such as `large_mammals`, `wild_animals`, `birds`, `amphibians`, `domestic` and `human_observation`, depending on the package version and dataset.

Users can also define or modify groups using fields such as `scientificName`, `class`, `order` or `observationType`.

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

## Selecting years and filters

Reports can be restricted to selected years or to species with sufficient records.

```r
# Check years detected in the dataset
cm$extractYears()

# Select years for reporting
cm$years <- 2021:2023
cm$setup()

# Check and change the minimum capture threshold
cm$filterCount
cm$filterCount <- 25
cm$setup()
```

## Report sections and modular workflow

`camtrapReport` is modular. Report sections can be inspected, updated, included or excluded.

```r
# List available sections
listReportSections(cm)
sections(cm)
section_names(cm)

# Update text in a section
updateReportSection(
  cm,
  section = "introduction",
  text = "This section was updated by the user.",
  append_text = FALSE
)

# Use all sections except selected ones
sections(cm) <- section_names(exclude = c("richness", "co_occurrence"))
report(cm, view = TRUE)

# Restore all available sections
sections(cm) <- section_names()
```

## Adding or customising modules

Analytical components in `camtrapReport` are implemented as modules. Users can modify existing modules or add new modules using R code or YAML templates. This makes it possible to adapt the workflow to different monitoring objectives, taxonomic groups, indicators or reporting requirements without changing the package core.

A module can include:

- explanatory text;
- code for analysis or visualisation;
- tables, figures or maps;
- captions and narrative summaries;
- dependencies and rendering settings.

This modular structure allows the workflow to evolve as users add new analytical sections.


## Graphical user interface

In addition to the command-line workflow, `camtrapReport` includes a graphical user interface for interactively exploring the `camReport` object, adjusting settings and running selected analyses.

```r
gui(cm)
```
