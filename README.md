<p align="center">
  <img src="man/figures/logo.png" width="240" alt="camtrapReport logo"/>
</p>

<h1 align="center">camtrapReport</h1>

<p align="center">
  <a href="https://www.r-project.org/"><img src="https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg" alt="R >= 4.1.0"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-GPL--3-green.svg" alt="GPL-3 license"></a>
  <a href="https://doi.org/10.5281/zenodo.18405441"><img src="https://img.shields.io/badge/DOI-10.5281%2Fzenodo.18405441-blue.svg" alt="DOI"></a>
</p>

<p align="center">
  <strong>Automated, reproducible camera-trap reporting from Camtrap-DP data.</strong>
</p>

`camtrapReport` is a modular R package for processing standardised camera-trap datasets and generating reproducible ecological reports. It is designed for datasets provided in, or converted to, the [Camtrap-DP standard format](https://camtrap-dp.tdwg.org/) and combines data-quality diagnostics, preprocessing, ecological analysis, visualisation and report assembly in a single workflow.

<div align="center">

<table>
<tr>
<td align="center"><strong>1. Data Status Check</strong></td>
<td align="center"><strong>2. Ecological Insight Report</strong></td>
</tr>
<tr>
<td align="center">Checks completeness, consistency, annotation quality and readiness for analysis.</td>
<td align="center">Generates modular ecological summaries, maps, figures and report-ready outputs.</td>
</tr>
</table>

</div>

---

## Why use camtrapReport?

Camera-trap projects often require many repeated steps before ecological interpretation: checking metadata, identifying missing or inconsistent values, summarising deployments and observations, calculating sampling effort, producing maps and figures, and compiling outputs into a report. These steps are often scattered across separate scripts.

`camtrapReport` brings them together in one reproducible workflow:

<div align="center">

<table>
<tr>
<td align="center">📦<br><strong>Import</strong><br>Camtrap-DP datasets</td>
<td align="center">🔍<br><strong>Check</strong><br>spatial, temporal, taxonomic and annotation quality</td>
<td align="center">📊<br><strong>Summarise</strong><br>deployments, observations, effort and species records</td>
<td align="center">🗺️<br><strong>Visualise</strong><br>maps, trends, activity and habitat-use patterns</td>
<td align="center">📄<br><strong>Report</strong><br>reproducible ecological outputs</td>
</tr>
</table>

</div>

---

## Main outputs

<div align="center">

<table>
<tr>
<th align="center">Report</th>
<th align="center">Purpose</th>
<th align="center">Typical content</th>
</tr>
<tr>
<td><strong>Data Status Check</strong></td>
<td>Pre-analysis quality control and dataset readiness assessment.</td>
<td>Setup, spatial checks, temporal checks, essential field availability, species records, validation, annotation and observation types.</td>
</tr>
<tr>
<td><strong>Ecological Insight Report</strong></td>
<td>Ecological summaries and visualisations from the prepared <code>camReport</code> object.</td>
<td>Sampling effort, species accumulation, richness maps, co-occurrence, spatial density, activity patterns, habitat use, abundance trends and REM-based density estimates.</td>
</tr>
</table>

</div>

The Data Status Check classifies dataset readiness as:

<div align="center">

<table>
<tr>
<td align="center">🟢 <strong>Perfect</strong><br>All key checks passed.</td>
<td align="center">🟠 <strong>Acceptable</strong><br>Minor issues found; dataset remains usable.</td>
<td align="center">🔴 <strong>Needs Improvement</strong><br>Major issues should be corrected before reporting.</td>
</tr>
</table>

</div>

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

Optional/reporting dependencies can be installed with:

```r
install_All()
```

Check the installed version:

```r
packageVersion("camtrapReport")
```

---

## Input data

The main input is a single `.zip` file containing a dataset in [Camtrap-DP](https://camtrap-dp.tdwg.org/) format.

<div align="center">

<table>
<tr>
<th align="center">Required files</th>
<th align="center">Optional files</th>
</tr>
<tr>
<td>
<code>datapackage.json</code><br>
<code>deployments.csv</code><br>
<code>media.csv</code><br>
<code>observations.csv</code>
</td>
<td>
Habitat table<br>
Study-area polygon<br>
Additional spatial or metadata layers
</td>
</tr>
</table>

</div>

A simple habitat table should contain at least:

<div align="center">

| locationName | Habitat |
|:---:|:---:|
| VEL-01 | Forest |
| VEL-02 | Grassland |
| VEL-03 | Wetland |

</div>

---

## Example data

You can test `camtrapReport` using the Leuven example dataset and optional spatial inputs.

<div align="center">

| Data | Habitat | Study area | Smaller test subset |
|:---:|:---:|:---:|:---:|
| [Leuven dataset](https://album.wildlabs.net/dataset/c9cbc586-660e-4d89-ba14-0000c5770de1/download) | [Leuven habitat](https://drive.google.com/file/d/1kVO3SztP4aeW53KIMJNQi5DDcGK3Wgsk/view?usp=sharing) | [Leuven study-area boundary](https://drive.google.com/file/d/1frZsAFzxHtrXU98_5XFsBhSlbyf7quAe/view?usp=sharing) | [Small subset](https://drive.google.com/file/d/1N_dAABTlJVP1Fj655RqS3RsOclBTzOBw/view?usp=sharing) |

</div>

**Notes:** The habitat and study-area files were prepared for testing `camtrapReport` and are not official publications from the original dataset owners. The full Leuven dataset is large, covers several years and includes many camera-trap locations, so processing may take some time.

---

## Quick start

### 1. Create a `camReport` object

```r
library(camtrapReport)

cm <- camData("path/to/camtrap-dp-dataset.zip")
```

With optional habitat and study-area files:

```r
library(camtrapReport)
library(terra)

habitat <- read.csv("path/to/Leuven_habitat.csv")
study_area <- terra::vect("path/to/Leuven_study_area.shp")

cm <- camData(
  data = "path/to/Leuven_dataset.zip",
  habitat = habitat,
  study_area = study_area
)

cm
```

### 2. Generate the Data Status Check

```r
status(cm, view = TRUE)
```

### 3. Generate the Ecological Insight Report

```r
report(cm, view = TRUE)
```

Both reports are saved to the working directory. With `view = TRUE`, the report opens automatically after rendering.

---

## Common workflow

<div align="center">

<table>
<tr>
<th align="center">Task</th>
<th align="center">Code</th>
</tr>
<tr>
<td>Inspect metadata</td>
<td><code>info(cm)</code></td>
</tr>
<tr>
<td>Update authors</td>
<td><code>info(cm, name = "authors") &lt;- c("First Author", "Second Author")</code></td>
</tr>
<tr>
<td>Check focus group</td>
<td><code>cm$setting$focus_groups</code></td>
</tr>
<tr>
<td>Change focus group</td>
<td><code>cm$set_focus_group("large_mammals"); cm$setup()</code></td>
</tr>
<tr>
<td>Select years</td>
<td><code>cm$years &lt;- 2021:2023; cm$setup()</code></td>
</tr>
<tr>
<td>Change capture threshold</td>
<td><code>cm$filterCount &lt;- 25; cm$setup()</code></td>
</tr>
<tr>
<td>List report sections</td>
<td><code>section_names(cm)</code></td>
</tr>
<tr>
<td>Exclude sections</td>
<td><code>sections(cm) &lt;- section_names(exclude = c("richness", "co_occurrence"))</code></td>
</tr>
</table>

</div>

---

## Focus groups

Reports can be generated for selected taxonomic or observation groups. The selected focus group determines which species and records are used in the Ecological Insight Report.

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

---

## Modular reports

Report sections can be inspected, updated, included or excluded.

```r
listReportSections(cm)
sections(cm)
section_names(cm)

updateReportSection(
  cm,
  section = "introduction",
  text = "This section was updated by the user.",
  append_text = FALSE
)

sections(cm) <- section_names(exclude = c("richness", "co_occurrence"))
report(cm, view = TRUE)

sections(cm) <- section_names()
```

Modules can include text, code, tables, figures, maps, captions and dependencies. This makes the workflow adaptable to different monitoring objectives, study systems and reporting needs.

---

## Graphical user interface

A graphical interface is available for interactive exploration and reporting.

```r
gui(cm)
```

---

## Event-level captures

For ecological summaries, `camtrapReport` uses event-level observations where possible. In Camtrap-DP, `observationLevel = "event"` refers to observations summarising an event or sequence, while `observationLevel = "media"` refers to classifications directly associated with media files.

In the species records table, **captures** represent the number of unique event-level sequences in which each species was recorded. They should not be interpreted as the number of photos or the number of individual animals unless the dataset explicitly supports that interpretation.

---

## Troubleshooting

<div align="center">

<table>
<tr>
<th align="center">Problem</th>
<th align="center">Suggested solution</th>
</tr>
<tr>
<td>GitHub installation fails</td>
<td>Create a GitHub token with <code>usethis::create_github_token()</code> and set it with <code>gitcreds::gitcreds_set()</code>.</td>
</tr>
<tr>
<td>Optional package missing</td>
<td>Run <code>install_All()</code> or install the missing package manually.</td>
</tr>
<tr>
<td>Report rendering stops</td>
<td>Run <code>status(cm, view = TRUE, test = TRUE)</code> or <code>report(cm, view = TRUE, test = TRUE)</code>.</td>
</tr>
<tr>
<td>Functions changed after update</td>
<td>Restart R, reinstall the package and recreate the <code>camReport</code> object.</td>
</tr>
<tr>
<td>Lazy-load database is corrupt</td>
<td>Restart R, remove and reinstall the package.</td>
</tr>
</table>

</div>

---

## Citation

If you use `camtrapReport`, please cite the package and the associated software note.

```r
citation("camtrapReport")
```

Suggested citation:

> Ebrahimi, E., & Jansen, P. (2026). CamtrapReport: An R Package for Automating Camera-Trap Data Reporting for Wildlife Monitoring. The International Biogeography Society - 12th Biennial Conference, Aarhus, Denmark. https://doi.org/10.5281/zenodo.18405441

---

## Contributing

Bug reports, feature requests and suggestions are welcome through the [issue tracker](https://github.com/spatialecology/camtrapReport/issues).

When reporting a problem, please include the package version, R version, operating system, a minimal reproducible example, the error message and whether the issue occurred in `camData()`, `status()`, `report()` or `gui()`.

---

## License

`camtrapReport` is released under the GPL-3 licence to strongly support open science, transparency and reproducible research. Third-party assets, if included, retain their own licences and copyright notices. See `LICENSE` for details.