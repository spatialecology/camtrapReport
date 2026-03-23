---
title: camtrapReport
---

<p align="center">
  <img src="man/figures/logo.png" width="260" alt="camtrapReport logo"/>
</p>

# camtrapReport

`camtrapReport` is a modular and extensible R package for processing camera-trap data and generating reproducible ecological reports.

## Overview

Camera traps have become an essential tool in wildlife monitoring and biodiversity research, but turning raw camera-trap records into standardised, reproducible outputs often requires multiple disconnected processing and reporting steps. `camtrapReport` was developed to streamline this workflow by integrating data processing, summarisation, ecological analysis, visualisation, and report generation within a single framework.

The package is designed for datasets formatted in, or converted to, the Camtrap-DP standard and supports the creation of maps, tables, figures, and narrative report content for wildlife monitoring applications.

## Installation

```r
install.packages("remotes")
remotes::install_github("spatialecology/camtrapReport")