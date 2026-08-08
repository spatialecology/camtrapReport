---
title: "Get started"
---

# Get started

## Package aims

[`camtrapReport`](https://github.com/spatialecology/camtrapReport) is an R package for processing camera-trap data and producing standardised, reproducible reports for wildlife monitoring. It brings data-quality checks, summaries, visualisations, and ecological analyses together in a modular reporting workflow.

The package is intended to reduce the repeated technical work involved in preparing camera-trap reports. Using a consistent workflow also makes outputs easier to compare among monitoring sites and across time. The two principal outputs are:

- **Data Status Check.** This report checks the quality and completeness of the input data. It summarises spatial and temporal coverage, the availability of important fields, annotation and validation status, and observation types by capture method. These checks help identify missing information, inconsistencies, and values that may require further inspection.

- **Ecological Report.** This report produces standardised ecological summaries through a set of modules. Depending on the selected modules and the available data, it can include descriptive information, tables, visualisations, and ecological analyses.

For a more detailed description of the package structure and its relationship to other camera-trap tools, see the [Package Overview](articles/Package-Overview.html).

## Data format

`camtrapReport` uses datasets in the [Camtrap DP format](https://camtrap-dp.tdwg.org/), a community-developed data exchange format for camera-trap data. Camera-trap data management systems including [Agouti](https://agouti.eu/) and [TRAPPER](https://os-conservation.org/trapper/) can export Camtrap DP datasets.

Data from other sources, including manually managed datasets and Wildlife Insights exports, can also be used after conversion to Camtrap DP.

### Example datasets

The following open-access datasets follow the Camtrap DP standard and can be used to test the package. They are part of a collection of camera-trap datasets available through [GBIF](https://www.gbif.org/composition/4fZGV2vrXjo3rNxySz41sj/exploring-camera-trap-data).

| Dataset | Habitat data | Study area |
| :--- | :--- | :--- |
| [Leuven dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Leuven habitat](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) | [Leuven study-area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) |
| [Antwerp dataset](https://album.wildlabs.net/dataset/a209cef2-cfad-460b-8ed4-0ccf211a8240/download) | [Antwerp habitat](https://drive.google.com/file/d/1ByUVZXc4w6JNFnMbgXEUu9ihJreIp7UJ/view?usp=sharing) | [Antwerp study-area boundary](https://drive.google.com/file/d/1Avb-SRqYsL59mrBrcmNdIkS8f582UVkR/view?usp=sharing) |
| [MICA dataset](https://album.wildlabs.net/dataset/8a5cbaec-2839-4471-9e1d-98df301095dd/download) | [MICA habitat](https://drive.google.com/file/d/1-1i8Kw8AUPYpedme8e8t6GKUgqatR8ji/view?usp=sharing) | [MICA study-area boundary](https://drive.google.com/file/d/1xskwg3H1vZw4gu-VDaiCHgXPXeoktDvH/view?usp=sharing) |


## Installation

`camtrapReport` requires R 4.1.0 or later. Install the current development version from GitHub, load the package, and optionally install all additional dependencies:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("spatialecology/camtrapReport")

library(camtrapReport)

# Optional: install packages required by additional report sections
install_All()
```

**Note:** Installation may require approval prompts, source compilation, and an internet connection.

### Troubleshooting installation

If `remotes::install_github()` fails, one possible cause is a missing GitHub personal access token. A token can be created and stored from R as follows:

```r
install.packages(c("remotes", "usethis", "gitcreds"))

usethis::create_github_token()
gitcreds::gitcreds_set()

remotes::install_github("spatialecology/camtrapReport")
```

## Create a `camReport` object

The main object used by the package is a mutable Reference Class object named `camReport`. It stores the processed input data, package settings, selected report sections, and intermediate results used during reporting.

Use `camData()` to read and pre-process a camera-trap dataset and create this object.

### Required input

The only required input is a `.zip` file containing a dataset in Camtrap DP format:

```r
cm <- camData("path/to/your/dataset.zip")
```

### Optional input

Optional habitat data and a study-area boundary can be supplied to add spatial context to maps, summaries, and analyses.

Habitat information can be provided as a two-column CSV file containing `locationName` and `Habitat`. An example [`habitat.csv` template](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing) is available for download. A study-area boundary can be supplied as a polygon spatial object.

```r
# Habitat data
habitat <- read.csv("path/to/habitat.csv")

# Check the expected structure
head(habitat)

#   locationName      Habitat
# 1       VEL-01     Sandhill
# 2       VEL-02       Forest
# 3       VEL-03 Dry_heathland

# Study-area boundary
study_area <- terra::vect("path/to/study_area.shp")

# Create the camReport object with optional inputs
cm <- camData(
  "path/to/your/dataset.zip",
  habitat = habitat,
  study_area = study_area
)
```

## Generate a Data Status Check

Use `status()` to inspect the quality and completeness of the input data:

```r
status(cm, view = TRUE)
```

## Generate an Ecological Report

After checking the input data quality, generate the Ecological Report with:

```r
report(cm, view = TRUE)
```

## Further documentation

This page introduced the basic workflow for preparing input data, reviewing data quality, and generating reports with `camtrapReport`.

For more information, see the [Package Overview](articles/Package-Overview.html) for the package scope and architecture, the [Data Status Check](articles/data-status-report.html) and [Ecological Report](articles/ecological-report.html) articles for the two reporting workflows, and [Module Management](articles/modules.html) for extending or reorganising report sections.

Publications, workshops, and training materials are listed on the [Resources](articles/resources.html) page, and individual functions and methods are documented in the [function reference](reference/index.html).

## Contribute

Questions, suggestions, and ideas for improvement are welcome. You can contribute to the development of the [`camtrapReport` R package](https://github.com/spatialecology/camtrapReport) by [opening an issue](https://github.com/spatialecology/camtrapReport/issues) or joining the conversation in [GitHub Discussions](https://github.com/spatialecology/camtrapReport/discussions).

## Citation

To cite `camtrapReport` in publications, run:

```r
citation("camtrapReport")
```

## Acknowledgements

The development of `camtrapReport` was supported by [Biodiversa+](https://www.biodiversa.eu/2022/10/07/2022-2023-joint-call/) through the [Big Picture project](https://wildlifecamera.eu/).
