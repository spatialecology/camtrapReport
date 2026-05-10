<p align="center">
  <img src="man/figures/logo.png" width="200" alt="camtrapReport"/>
</p>

<h1 align="center">camtrapReport</h1>

<p align="center">
  <em>From a Camtrap-DP archive to a publication-ready ecological report — in three lines of R.</em>
</p>

<p align="center">
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%E2%89%A5%204.1.0-276DC3?logo=r&logoColor=white" alt="R ≥ 4.1.0"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-GPL--3-green.svg" alt="GPL-3"></a>
  <a href="https://doi.org/10.5281/zenodo.18405441"><img src="https://img.shields.io/badge/DOI-10.5281%2Fzenodo.18405441-blue.svg" alt="DOI"></a>
  <a href="https://spatialecology.github.io/camtrapReport/"><img src="https://img.shields.io/badge/docs-pkgdown-orange.svg" alt="pkgdown"></a>
</p>

---

`camtrapReport` turns standardised camera-trap datasets into structured, reproducible ecological reports. Drop in a [Camtrap-DP](https://camtrap-dp.tdwg.org/) `.zip`, and the package will diagnose data quality, run a suite of ecological analyses, and compile narrative, figures, maps and tables into a single article-style HTML document.

## Install

```r
remotes::install_github("spatialecology/camtrapReport")
library(camtrapReport)
install_All()   # report modules pull in extra packages
```

## Create the camReport object

The only required input is a single `.zip` file containing the dataset in Camtrap-DP format.

```r
cm <- camData("Leuven-data.zip")   # build the camReport object
```

## Optional input

Optional input data can be used to improve maps, add spatial context, and support richer summaries and analyses. The two supported optional inputs are **habitat data** and a **study area polygon**.

Habitat information can be provided as a two-column CSV file containing `locationName` and `Habitat`. An example `habitat.csv` template can be downloaded [here](https://drive.google.com/file/d/1lo_CwpLQmuxOVB5193tIAsEq7WF9v0t-/view?usp=sharing).

A polygon shapefile representing the study area boundary can also be provided.

When available, these optional inputs can be passed directly to `camData()`.


```r
habitat <- read.csv("C:/Users/Data/habitat.csv")

head(habitat) # check if the data.frame follows the required structure:
#   locationName      Habitat
# 1       VEL-01     Sandhill
# 2       VEL-02       Forest
# 3       VEL-03 Dry_heathland

# Spatial polygon of the study area:
bnd <- vect("C:/Users/Data/polygon.shp")

# Read the camera-trap data together with habitat data and the study area boundary:
cm <- camData(
  data = "C:/Users/Data/EOW-Veluwe.zip",
  habitat = habitat,
  study_area = bnd
)

cm # shows brief information about the camReport object
```

After reading the dataset and creating the `camReport` object in the previous step, you can generate two extensive reports, including **Data Status**, and **Ecological Insights**:

## Data status report

To generate a Data Status Report and review the quality and completeness of the input data, use `status()`:

```r
status(cm, view = TRUE)  # With view = TRUE, the generated report opens automatically
```

## Ecological report

Once the input data have been prepared, a full ecological report can be generated using the `report()` function:

```r
report(cm, view = TRUE)  # Opens the report automatically after creation

```
**Tip**: *Reports are saved in your current working directory.*


## Customising report

```r
# Metadata
info(cm, name = "authors") <- c("First Author", "Second Author")
info(cm, name = "title")   <- "Camera-Trap Report for Example Site"

# Focus on a taxonomic group (or define your own)
cm$set_focus_group("large_mammals")

cm$add_group(
  name = "mustelids",
  x = list(scientificName = c("Mustela putorius", "Meles meles"))
)

# Restrict time window and minimum detections
cm$years       <- 2021:2023
cm$filterCount <- 25
cm$setup()

# Add, drop or rewrite report sections
listReportSections(cm)
updateReportSection(cm, section = "introduction", text = "Custom intro…")
sections(cm, section_names(exclude = c("richness", "co_occurrence")))

# Interactive UI
gui(cm)
```

## Privacy-aware by design

Reports can be shared even when raw images or precise locations cannot — broadening the range of monitoring programmes that can contribute results to research, management and policy.

## Example data

Try the workflow on the open [Leuven Camtrap-DP dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download), with optional [habitat table](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) and [study-area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing). For a quick first run, use the [smaller subset](https://drive.google.com/file/d/1N_dAABTlJVP1Fj655RqS3RsOclBTzOBw/view?usp=sharing).

## Learn more

[Package overview](https://spatialecology.github.io/camtrapReport/articles/Package-Overview.html) · [Data Status Report](https://spatialecology.github.io/camtrapReport/articles/data-status-report.html) · [Ecological Report](https://spatialecology.github.io/camtrapReport/articles/ecological-report.html) · [Module management](https://spatialecology.github.io/camtrapReport/articles/modules.html)

## Contribute

Open an [issue](https://github.com/spatialecology/camtrapReport/issues), start a [discussion](https://github.com/spatialecology/camtrapReport/discussions), or contribute a module. All welcome.
