---
title: "Get started"
---

# Get started

## Package aims

[`camtrapReport`](https://github.com/spatialecology/camtrapReport) provides a
modular workflow for turning standardised camera-trap data into reproducible
wildlife-monitoring reports. It combines data-quality assessment, ecological
analysis, visualisation, and report generation within a single workflow.

The package produces two main outputs:

- **Data Status Check**, which evaluates data quality, completeness, and
  spatiotemporal structure before ecological analysis; and
- **Ecological Report**, which assembles selected ecological analyses,
  summaries, tables, figures, and maps into a structured report.

For more detail on the package scope and its relationship to other
camera-trap tools, see the
[Package Overview](articles/Package-Overview.html).

## Data format

`camtrapReport` uses datasets in the [Camtrap DP format](https://camtrap-dp.tdwg.org/), a community-developed data exchange format for camera-trap data. Camera-trap data management systems including [Agouti](https://agouti.eu/) and [TRAPPER](https://os-conservation.org/trapper/) can export Camtrap DP datasets.

Data from other sources, including manually managed datasets and Wildlife Insights exports, can also be used after conversion to Camtrap DP.

## Installation

`camtrapReport` requires R 4.1.0 or later. Install the development version from GitHub with [`pak`](https://pak.r-lib.org/):

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::pkg_install(
  "spatialecology/camtrapReport",
  dependencies = TRUE
)

library(camtrapReport)
```

Using `dependencies = TRUE` also installs optional packages required by some report modules and examples.

## Quick start with the bundled example data

`camtrapReport` includes a compact example dataset derived from the **GMU8_LEUVEN** Camtrap DP dataset, together with example habitat data and a study-area boundary. These files are bundled with the package for examples, automated tests, and software demonstrations.

Because `camData()` stores a processed `camReport` object beside the input dataset, the bundled dataset is first copied to a temporary working directory so that the installed package files remain unchanged.

```r
library(camtrapReport)

# Copy the bundled example dataset to a temporary working directory
example_dir <- tempfile("camtrapReport-")
dir.create(example_dir)

file.copy(
  system.file("external", "dataset", package = "camtrapReport"),
  example_dir,
  recursive = TRUE
)

# Create a camReport object
cm <- camData(file.path(example_dir, "dataset"))

cm
```

The bundled Leuven fixture is intended for demonstrating and testing the
software rather than for ecological inference. Its provenance and the
transformations used to prepare it are documented in the
[bundled dataset documentation](https://github.com/spatialecology/camtrapReport/blob/main/inst/external/README-Leuven-dataset.md).

## Using your own data

The only required input to `camData()` is a Camtrap DP dataset, supplied either as a ZIP archive or as an extracted Camtrap DP directory.

Optional habitat data and a study-area boundary can also be supplied to add spatial context to maps, summaries, and analyses. Habitat data should include `locationName` and `Habitat`; the study area can be provided as a spatial file path, `SpatVector`, or `sf` object.

```r
# Required input
dataset <- "path/to/your/dataset.zip"

# Create a camReport object using only the required input
cm <- camData(dataset)

# Optional inputs
habitat <- read.csv("path/to/habitat.csv")
study_area <- "path/to/study_area.shp"

# Create a camReport object with optional spatial information
cm <- camData(
  data = dataset,
  habitat = habitat,
  study_area = study_area
)
```

### Additional example datasets

Several open-access Camtrap DP datasets can also be used to explore the package:

| Dataset | Habitat data | Study area |
| :--- | :--- | :--- |
| [Leuven dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Leuven habitat](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) | [Leuven study-area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) |
| [Antwerp dataset](https://album.wildlabs.net/dataset/a209cef2-cfad-460b-8ed4-0ccf211a8240/download) | [Antwerp habitat](https://drive.google.com/file/d/1ByUVZXc4w6JNFnMbgXEUu9ihJreIp7UJ/view?usp=sharing) | [Antwerp study-area boundary](https://drive.google.com/file/d/1Avb-SRqYsL59mrBrcmNdIkS8f582UVkR/view?usp=sharing) |
| [MICA dataset](https://album.wildlabs.net/dataset/8a5cbaec-2839-4471-9e1d-98df301095dd/download) | [MICA habitat](https://drive.google.com/file/d/1-1i8Kw8AUPYpedme8e8t6GKUgqatR8ji/view?usp=sharing) | [MICA study-area boundary](https://drive.google.com/file/d/1xskwg3H1vZw4gu-VDaiCHgXPXeoktDvH/view?usp=sharing) |

These larger datasets are useful for more realistic exploration, while the bundled Leuven fixture is preferable for a quick and reproducible first run.

## Generate a Data Status Check

Use `status()` to inspect the quality and completeness of the input data:

```r
status(cm, view = TRUE)
```

The generated report summarises key aspects of the dataset, including spatial and temporal coverage, field availability, annotation and validation information, and other checks relevant to downstream analysis.

## Generate an Ecological Report

After reviewing the input data, generate the Ecological Report with:

```r
report(cm, view = TRUE)
```

The report is assembled from configurable modules. The available content depends on the data supplied and on the report sections selected for inclusion.

## Customise the report

The contents of a `camReport` object can be inspected and adjusted before generating the final report.

For example, report metadata can be viewed with:

```r
info(cm)
```

Available report sections can be inspected with:

```r
listReportSections(cm)
section_names()
```

Sections can then be selected or excluded before generating the report. See the [Module Management](articles/modules.html) article for more advanced customisation and extension.

## Further documentation

For more information, see:

- the [Package Overview](articles/Package-Overview.html) for the package scope and design;
- the [Data Status Check](articles/data-status-report.html) article for data-quality reporting;
- the [Ecological Report](articles/ecological-report.html) article for ecological summaries and analyses;
- [Module Management](articles/modules.html) for configuring and extending report sections;
- the [Resources](articles/resources.html) page for publications, workshops, and training materials; and
- the [function reference](reference/index.html) for individual functions and methods.

## Contribute

Questions, suggestions, bug reports, documentation improvements, and ideas for new report modules are welcome.

You can contribute by [opening an issue](https://github.com/spatialecology/camtrapReport/issues), joining a discussion in [GitHub Discussions](https://github.com/spatialecology/camtrapReport/discussions), or reading the [contributing guidelines](CONTRIBUTING.html).

## Citation

To cite `camtrapReport` in publications, run:

```r
citation("camtrapReport")
```

## Acknowledgements

The development of `camtrapReport` was supported by [Biodiversa+](https://www.biodiversa.eu/2022/10/07/2022-2023-joint-call/) through the [Big Picture project](https://wildlifecamera.eu/).
