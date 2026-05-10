<p align="center">
  <img src="man/figures/logo.png" width="230" alt="camtrapReport logo"/>
</p>

# camtrapReport

[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-green.svg)](LICENSE)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.18405441-blue.svg)](https://doi.org/10.5281/zenodo.18405441)

`camtrapReport` is a modular and extensible R package for automating camera-trap data reporting in wildlife monitoring. The package is designed for datasets provided in, or converted to, the [Camtrap-DP standard format](https://camtrap-dp.tdwg.org/), and provides a reproducible workflow for transforming standardised camera-trap records into structured ecological outputs.

The package addresses a common bottleneck in camera-trap projects: although data-management platforms and standards have improved the organisation and exchange of camera-trap data, ecological interpretation often still requires fragmented scripts, repeated manual checks and ad hoc report preparation. `camtrapReport` helps bridge this gap by connecting standardised inputs, data-quality diagnostics, ecological analysis, visualisation and automated report assembly within a single workflow.

## Main outputs

`camtrapReport` currently produces two main outputs.

| Output | Purpose |
|---|---|
| **Data Status Check report** | Evaluates whether a Camtrap-DP dataset is complete, internally consistent and ready for ecological analysis. |
| **Ecological Report** | Generates modular ecological summaries, figures, maps and report-ready outputs from a prepared `camReport` object. |

The **Data Status Check report** summarises key aspects of dataset quality, including spatial and temporal coverage, essential field availability, species records, validation status, annotation quality and observation types.

The **Ecological Report** assembles selected analytical modules into a structured report. Outputs can include sampling effort, species records, richness, activity patterns, co-occurrence, habitat-use summaries, abundance trends, spatial summaries and density-related analyses, depending on the available data and selected modules.

## Why use camtrapReport?

Camera-trap datasets are increasingly large, standardised and reusable, but converting them into interpretable ecological outputs can still require substantial manual effort. `camtrapReport` was developed to make this step more reproducible, transparent and efficient.

The workflow supports users by:

- reading camera-trap datasets in Camtrap-DP format;
- checking data completeness and consistency before analysis;
- preparing a reusable `camReport` object;
- generating automated data-status and ecological reports;
- producing maps, tables, figures and narrative report text;
- supporting modular report sections that can be updated, extended or replaced;
- supporting privacy-aware reporting by enabling structured ecological reports to be shared even when raw images, precise locations or other sensitive data cannot be openly released.

In this way, `camtrapReport` functions as a reporting layer within the broader camera-trap data ecosystem. It complements existing data-management, publication and analytical tools by focusing on the reproducible transformation of harmonised camera-trap records into shareable ecological knowledge.

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


## Input data

The required input is a single `.zip` file containing a dataset in [Camtrap-DP](https://camtrap-dp.tdwg.org/) format. Optional inputs can be added to enrich the report:

| Optional input | Purpose |
|---|---|
| Habitat table | Adds habitat information for each camera-trap location |
| Study-area polygon | Supports mapping and spatial summaries |

An example `habitat.csv` template can be downloaded [here](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing). For your convenience, your input folder should look like [this](https://drive.google.com/file/d/1ykJA_aKsWuNsyiiTrUUyTo-7CdXA_UIy/view?usp=sharing). The Camtrap-DP ZIP file is the only mandatory input; all other files are optional.

## Example data

If you do not have access to a Camtrap-DP dataset, you can use Leuven dataset which is a open-source camera-trap datasets available through [GBIF](https://www.gbif.org/composition/4fZGV2vrXjo3rNxySz41sj/exploring-camera-trap-data) to test camtrapReport. 

| Data | Habitat | Study area | 
|---|---|---|---|
| [Leuven dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Leuven habitat](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) | [Leuven study-area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) |

**Note:** The habitat and study-area files were prepared for testing `camtrapReport` and are not official publications from the original dataset owners.

**Note:** The original Leuven dataset is relatively large because it covers several years and many camera-trap locations. Processing the full dataset may take some time. For a quick first test, use the [smaller subset](https://drive.google.com/file/d/1N_dAABTlJVP1Fj655RqS3RsOclBTzOBw/view?usp=sharing).

## Get started

### 1. Create a `camReport` object

`camReport` is the main object used in the `camtrapReport` workflow. It stores the input data, settings, and results used to generate reports.

Create it with `camData()`, which reads and prepares your camera-trap dataset.


The only required input is a single `.zip` file containing the dataset in Camtrap-DP format.
```r
cm <- camData("path/to/camtrap-dp-dataset.zip")
```

With optional habitat and study-area inputs:

```r

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

## Typical workflow

```r
library(camtrapReport)

cm <- camData("path/to/camtrap-dp-dataset.zip")

status(cm, view = TRUE)

report(cm, view = TRUE)
```

After the `camReport` object has been created, users can inspect and update report metadata:

```r
info(cm)

info(cm, name = "authors") <- c("First Author", "Second Author")
info(cm, name = "title") <- "Camera-Trap Report for Example Site"
```

Users can also adjust the focus group, year range and filters:

```r
names(cm$group_definition)

cm$set_focus_group("large_mammals")

cm$years <- 2021:2023

cm$filterCount <- 25

cm$setup()
```

## Focus groups

The Ecological Report can be generated for selected taxonomic or observation groups. The selected focus group determines which records are used in the analyses.

```r
names(cm$group_definition)

cm$set_focus_group("large_mammals")
cm$setup()
```

Users can also define custom groups:

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

## Report sections

`camtrapReport` is built around modular report sections. Sections can include text, code, tables, figures, maps, captions and package dependencies. This design allows the workflow to be adapted to different taxa, monitoring objectives, study designs and reporting needs.

List available or attached report sections:

```r
listReportSections(cm)

sections(cm)

section_names()
```

Update the text of an existing report section:

```r
updateReportSection(
  cm,
  section = "introduction",
  text = "This section was updated by the user.",
  append_text = FALSE
)
```

Generate the report after editing:

```r
cm$setup()
report(cm, view = TRUE)
```

## Graphical user interface

A graphical user interface is available for interactive exploration and reporting.

```r
gui(cm)
```

## Conclusion

This page introduced the basic workflow for setting up input data, 
reviewing data quality, and generating reports with `camtrapReport`.
To understand how the package is organised, see the [Package Overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html). 
To explore data quality checks in detail, see the [Data Status Report](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html). 
To customise the ecological report, see the [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html). 
For more detail on the package's modular and extensible design, including how to add new report sections, see [Module Management](https://spatialecology.github.io/camtrapReport/articles/modules.html).

## Contribute

Questions, suggestions, and ideas for improvement are always welcome. You can contribute to the development of the [camtrapReport R package](https://github.com/spatialecology/camtrapReport) by [opening an issue](https://github.com/spatialecology/camtrapReport/issues) or joining the conversation in [GitHub Discussions](https://github.com/spatialecology/camtrapReport/discussions).

## Citation

As the official scientific publication for `camtrapReport` is still in preparation, please cite the package as:

[Ebrahimi, E., & Jansen, P. A. (2026). <em>CamtrapReport: An R Package for Automating Camera-Trap Data Reporting for Wildlife Monitoring</em>. p. 65. Abstract from <em>The International Biogeography Society – 12th Biennial Conference</em>, Aarhus, Denmark. <https://zenodo.org/records/18405441>]{.citation-text}

For package citation details, please refer to the citation information available in the [GitHub repository](https://github.com/spatialecology/camtrapReport).
