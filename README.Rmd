<p align="center">
  <img src="inst/report-assets/camtrapReport-logo.png" width="200" alt="camtrapReport"/>
</p>

<h1 align="center">camtrapReport</h1>

<p align="center">
  <em>A modular R package for automating camera-trap data reporting in wildlife monitoring.</em>
</p>

<p align="center">
  <a href="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R-CMD-check"></a>
  <a href="https://github.com/spatialecology/camtrapReport/actions/workflows/pkgcheck.yaml"><img src="https://github.com/spatialecology/camtrapReport/actions/workflows/pkgcheck.yaml/badge.svg" alt="pkgcheck"></a>
  <a href="https://app.codecov.io/github/spatialecology/camtrapReport"><img src="https://codecov.io/github/spatialecology/camtrapReport/graph/badge.svg?token=9VBXAR9XOD" alt="Codecov test coverage"></a>
  <a href="https://www.repostatus.org/#active"><img src="https://www.repostatus.org/badges/latest/active.svg" alt="Project Status: Active"></a>
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white" alt="R ≥ 4.1.0"></a>
  <a href="https://github.com/spatialecology/camtrapReport/blob/main/LICENSE.md"><img src="https://img.shields.io/badge/license-MIT-yellow.svg" alt="MIT"></a>
  <a href="https://spatialecology.github.io/camtrapReport/"><img src="https://img.shields.io/badge/docs-pkgdown-orange.svg" alt="pkgdown"></a>
</p>

---

`camtrapReport` turns standardised camera-trap datasets into structured, reproducible ecological reports through an automated workflow. Drop in a [Camtrap DP](https://camtrap-dp.tdwg.org/) ZIP file, and the package will diagnose data quality, run a suite of ecological analyses, and compile narrative text, figures, maps, and tables into a single article-style HTML document.

<p align="center">
  <img src="vignettes/articles/figures/workflow.png" width="900" alt="Schematic overview of the camtrapReport workflow"/>
  <br>
  <em>Schematic overview of the automated <code>camtrapReport</code> workflow.</em>
</p>

## Use in practice

`camtrapReport` has been developed and evaluated using multiple real-world
camera-trap datasets from different parts of the world, spanning a wide range of
geographic regions, monitoring contexts, survey designs, and data structures.
A substantial part of this evaluation has involved datasets from across Europe,
particularly through the European Observatory of Wildlife (EOW) network. The
package has also been presented at conferences, workshops, and training events,
and is increasingly being used and tested by external users.

Testing the package across these heterogeneous datasets has helped identify
practical issues related to data structure, metadata completeness, spatial and
temporal coverage, and report generation. Feedback from users working with these
datasets has been incorporated into the continued development and refinement of
the package.

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

Using `dependencies = TRUE` also installs packages listed as optional dependencies, which are needed by some report modules and examples.

## Quick start with the bundled example data

The package includes a compact Camtrap DP dataset derived from the **GMU8_LEUVEN** dataset, together with example habitat data and a study-area boundary. These files are bundled specifically for examples, automated tests, and software demonstrations.

The example below is fully reproducible after installing the package. Because `camData()` stores a processed `camReport` object beside the input dataset, the bundled dataset is first copied to a temporary working directory so that the installed package files remain unchanged.

```r
library(camtrapReport)

# Locate the bundled Camtrap DP example dataset
bundled_dataset <- system.file(
  "external",
  "dataset",
  package = "camtrapReport"
)

# Work on a copy because camData() stores processed output beside the input data
example_root <- file.path(tempdir(), "camtrapReport-example")
if (dir.exists(example_root)) {
  unlink(example_root, recursive = TRUE)
}
dir.create(example_root)

file.copy(
  bundled_dataset,
  example_root,
  recursive = TRUE
)

example_dataset <- file.path(
  example_root,
  basename(bundled_dataset)
)

# Locate the bundled optional inputs
habitat_path <- system.file(
  "external",
  "habitat",
  "habitat.csv",
  package = "camtrapReport"
)

study_area_path <- system.file(
  "external",
  "study_area",
  "study_area.shp",
  package = "camtrapReport"
)

habitat <- read.csv(
  habitat_path,
  stringsAsFactors = FALSE
)

# Create the camReport object
cm <- camData(
  data = example_dataset,
  habitat = habitat,
  study_area = study_area_path
)

cm
```

`camData()` also accepts a user's own Camtrap DP dataset as either a ZIP archive or an extracted Camtrap DP directory. Habitat data and a study-area boundary are optional.

Once the `camReport` object is created, the same object can be used to generate the **Data Status Check** and the **Ecological Report**.

## Data Status Check

To review the quality and completeness of the input data, generate a Data Status Check with `status()`:

```r
status(cm, view = TRUE)  # With view = TRUE, the generated report opens automatically
```

## Ecological Report

After reviewing the input data, generate the Ecological Report with `report()`:

```r
report(cm, view = TRUE)  # Opens the report automatically after creation
```

## Customising the report

The contents of the `camReport` object are extracted or inferred from the main dataset. These metadata can be viewed and modified using the `info()` function:

```r
info(cm)  # Shows metadata for available fields

# You can also retrieve information for specific fields:
info(cm, name = c("title", "subtitle"))
info(cm, name = "authors")

# You can override the information:
info(cm, name = "authors") <- c("Elham Ebrahimi", "Patrick Jansen")

# View the updated metadata:
info(cm)
```

## Adjusting the focus group

The Ecological Report can be generated for a selected taxonomic group, referred to as the `focus_group`. By default, this is set to `"large_mammals"`.

Several predefined groups are already included in the package, such as `"large_mammals"`, `"wild_animals"`, `"birds"`, `"amphibians"`, `"domestic"`, and `"human_observation"`.

Users can also define a new group by assigning records based on one or more of the following fields:

- `scientificName`
- `class`
- `order`
- `observationType`

```r
# Check which group is currently set as the focus group
cm$setting$focus_groups

# Check which groups are available
names(cm$group_definition)

# Change the focus group
cm$set_focus_group(x = "wild_animals")

# Check the rule used to define specific groups
cm$get_group("large_mammals")
cm$get_group("wild_mammals")

# Add a new group or modify an existing group definition
cm$add_group(
  name = "wild_animals",
  x = list(
    scientificName = c(
      "Mustela putorius",
      "Myodes glareolus",
      "Procyon lotor"
    )
  )
)

cm$setup()
```

## Selecting years and adjusting filters

The report can be generated for all years in the dataset or for a selected time period.

```r
# Check which years are covered by the records
cm$extractYears()

# Select a specific time window for report generation
cm$years <- 2023:2024

cm$setup()
```

The count filter defines the minimum number of observations required for a species to be included in the report.

```r
# Check the current count filter
cm$filterCount

# The default value is usually 25

# Modify the count filter
cm$filterCount <- 10
```

## Viewing and updating report sections

`camtrapReport` is a modular and extensible package, which means each report is built from independent sections.

You can inspect the available sections before editing the report. This helps you identify the exact section names that can be modified, updated, kept, or excluded.

The function `updateReportSection()` can be used to edit the content of an existing report section. For example, if the text in the introduction section needs to be changed, it can be updated as shown below.

```r
# View available report sections
listReportSections(cm)
sections(cm)
section_names()

# Update the text of a specific report section
updateReportSection(
  cm,
  section = "introduction",
  text = "This is new text to replace the existing section content.",
  append_text = FALSE
)

# If append_text = TRUE, the new text is added to the existing text.
# If append_text = FALSE, the existing text is replaced.

# Check the report sections again after making changes
listReportSections(cm)
```

## Selecting which sections to include

Before generating the report, you can check which sections will be included.

If you want to exclude or keep certain sections, you can use the `sections()` function. In `sections()`, you specify the names of the sections that should be included in the report.

To make it easier to access section names, use the `section_names()` function.

```r
# Check what sections will be included in the report
listReportSections(cm)

# Show the names of all existing sections in the package
section_names()

# Exclude one section
section_names(exclude = "introduction")

# Exclude more than one section
section_names(exclude = c("acknowledgements", "appendix"))

# Keep only selected sections
section_names(
  keep = c("introduction", "methods", "study_area")
)

# Example:
# Include all sections except "richness" and "co_occurrence"
n <- section_names(
  exclude = c("richness", "co_occurrence")
)

# Check the names of the sections that will be used
n

# Update the sections in the cm object
sections(cm, n)

# Generate the report with the selected sections
report(cm, view = TRUE)

# Restore all available sections
sections(cm, n = section_names())
```

## Graphical user interface

A graphical user interface is available for exploring outputs, adjusting settings and generating reports interactively.

```r
gui(cm)
```

## Privacy-aware by design

Reports can be shared even when raw images or precise locations cannot — broadening the range of monitoring programmes that can contribute results to research, management and policy.

## Example data

The Quick Start above uses a compact derived subset of the **GMU8_LEUVEN** Camtrap DP dataset that is distributed with `camtrapReport`. The fixture contains repeated deployments across eight example camera locations and multiple years, together with bundled habitat and study-area inputs. It is intended for software demonstration and testing rather than ecological inference.

The provenance and transformations applied to the example fixture are documented in [`inst/external/README-Leuven-dataset.md`](https://github.com/spatialecology/camtrapReport/blob/main/inst/external/README-Leuven-dataset.md). For scientific analyses, users should refer to the complete source dataset and its original metadata.

The package has also been demonstrated and tested in hands-on workshops using camera-trap data; related publications, workshops, and training activities are listed on the [Resources](https://spatialecology.github.io/camtrapReport/articles/resources.html) page.

## Learn more

[Package overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html) · [Data Status Report](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html) · [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html) · [Module management](https://spatialecology.github.io/camtrapReport/articles/modules.html)

## Relationship to other camera-trap packages

R packages provide complementary tools for different parts of camera-trap
workflows, including standardised data handling (`camtrapdp`), data management
and exploration (`camtrapR`, `ct`, `camtraptor`), specialised ecological
analyses (`activity`, `overlap`, `iNEXT`, `camtrapDensity`, `Distance`), and
hierarchical or spatial modelling (`unmarked`, `secr`).

`camtrapReport` complements these tools by focusing on automated reporting.
Starting from a standardised Camtrap DP dataset, it coordinates data-quality
assessment, selected ecological analyses, figures, tables, maps, metadata and
narrative text within a configurable and reproducible report. It is not intended
to replace specialised analytical packages; their methods and outputs can be
incorporated into the modular reporting workflow where appropriate.

## Citation

To cite `camtrapReport` in publications, run:

```r
citation("camtrapReport")
```

## Contributing

Contributions to `camtrapReport` are very welcome. These may include bug
reports, feature requests, documentation improvements, code contributions,
or proposals for new report modules. You can contribute by opening a
[GitHub issue](https://github.com/spatialecology/camtrapReport/issues)
or starting a [discussion](https://github.com/spatialecology/camtrapReport/discussions). Before contributing, please read the
[contributing guidelines](https://github.com/spatialecology/camtrapReport/blob/main/.github/CONTRIBUTING.md)
and the [Code of Conduct](https://github.com/spatialecology/camtrapReport/blob/main/.github/CODE_OF_CONDUCT.md).
