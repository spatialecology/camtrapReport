# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  March 2026
# Version 0.2.22
# Licence MIT
#--------

# 
# 
# .testReportSection <- function(x,object,view=TRUE) {
#   if (missing(object)) object <- NULL
#   if (missing(view)) view <- TRUE # view=FALSE is used only to test if error is generated!
#   rmd_file <- tempfile(fileext = ".Rmd")
#   output_file <- tempfile(fileext = ".html")
#   .env = new.env()
#   .env$title <- paste0('Testing the text section named: ',x@name)
#   
#   rmd_template <- glue::glue("
# ---
# title: \"{title}\"
# date: \"`r format(Sys.Date(), '%B %d, %Y')`\"
# output:
#   html_document:
#     theme: flatly
#     highlight: tango
#     df_print: paged
#     number_sections: true
#     self_contained: true
# ---
# 
# ```{{r load library, echo=FALSE, results='hide', message=FALSE, warning=FALSE}}
# library(knitr)
# library(ggplot2)
# library(ggrepel)
# library(dplyr)
# library(RColorBrewer)
# library(rmarkdown)
# library(ctdp)
# library(camtraptor)
# library(htmltools)
# library(stringr)
# library(xts)
# library(camtrapDensity)
# library(lubridate)
# library(gridExtra)
# library(leaflet)
# library(magick)
# library(spatstat)
# library(terra)
# library(tidyverse)
# library(DT)
# library(data.table)
# library(gt)
# library(htmlwidgets)
# library(ggthemes)
# library(camtrapdp)
# library(devtools)
# library(htmltools)
# 
# .paste_comma_and <- camtrapReport:::.paste_comma_and
# .require <- camtrapReport:::.require
# .get_projected_vect <- camtrapReport:::.get_projected_vect
# 
# ```
#       ",.envir = .env)
#   
#   if (length(x@headLevel) == 0) x@headLevel <- 1
#   
#   rmd_template <- paste0(rmd_template,'\n\n',.glueTextSection(x,.envir = object))
#   
#   # Write out the R Markdown file
#   cat(rmd_template, file = rmd_file, sep = "\n")
#   
#   # Render the R Markdown file
#   # We can pass an environment so that the Rmd sees the object fields directly
#   # One approach: pass the entire object as 'object' in the environment
#   render_env <- new.env(parent = globalenv())
#   render_env$object <- object
#   
#   if (view) message("Rendering R Markdown report ...")
#   out <- rmarkdown::render(
#     input       = rmd_file, 
#     output_file = output_file,
#     envir       = render_env
#   )
#   
#   if (view) {
#     message("Report generated at: ", normalizePath(out))
#     viewer <- getOption("viewer")
#     if (!is.null(viewer)) {
#       # Launch the rendered HTML in the Viewer pane
#       viewer(out)
#     } else {
#       # Fallback: open in default browser
#       .eval('utils::browseURL(out)',env = environment())
#     }
#   }
#   
# }
# #-------------

.testReportSection <- function(x, object = NULL, view = TRUE) {
  if (!inherits(x, ".textSection")) {
    stop("'x' should be a '.textSection' object.")
  }
  
  rmd_file    <- tempfile(fileext = ".Rmd")
  output_file <- tempfile(fileext = ".html")
  
  # Title environment for glue
  .env <- new.env(parent = emptyenv())
  .env$title <- paste0("Testing the text section named: ", x@name)
  
  # Collect packages only from this section/chunk tree
  # Keep 'knitr' as a tiny core dependency for rendering.
  module_pkgs <- .collect_module_packages(x)
  
  # Reuse the same package-loader helper used by generateReport()
  .env$pkg_chunk <- .make_package_loader_chunk(
    pkgs = module_pkgs,
    core = c("knitr")
  )
  
  rmd_template <- glue::glue(
    "---
title: \"{title}\"
date: \"`r format(Sys.Date(), '%B %d, %Y')`\"
output:
  html_document:
    theme: flatly
    highlight: tango
    df_print: paged
    number_sections: true
    self_contained: true
---

{pkg_chunk}"
,
    .envir = .env
  )
  
  if (length(x@headLevel) == 0 || is.null(x@headLevel) || is.na(x@headLevel)) {
    x@headLevel <- 1
  }
  
  rmd_template <- paste0(
    rmd_template,
    "\n\n",
    .glueTextSection(x, .envir = object)
  )
  
  writeLines(rmd_template, con = rmd_file, useBytes = TRUE)
  
  render_env <- .make_render_env(object)
  
  if (isTRUE(view)) message("Rendering R Markdown report ...")
  
  out <- rmarkdown::render(
    input       = rmd_file,
    output_file = output_file,
    envir       = render_env,
    quiet       = !isTRUE(view)
  )
  
  if (isTRUE(view)) {
    message("Report generated at: ", normalizePath(out, winslash = "/", mustWork = FALSE))
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(out)
    } else {
      utils::browseURL(out)
    }
  }
  
  invisible(out)
}

if (!isGeneric("testSection")) {
  setGeneric("testSection", function(x,object,view)
    standardGeneric("testSection"))
}


setMethod('testSection', signature(x='.textSection'), 
          function(x,object,view) {
            if (missing(view)) view <- TRUE
            .testReportSection(x,object,view)
          }
)

