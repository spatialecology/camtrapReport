# Functions for testing camtrapReport report sections
# Licence: MIT
#--------

.QuickTestReportSection <- function(x, object = NULL, path = NULL) {
  
  if (is.null(path)) {
    rmd_file <- tempfile(fileext = ".Rmd")
    output_file <- tempfile(fileext = ".html")
  } else {
    rmd_file <- paste0(path, "/test.Rmd")
    output_file <- paste0(path, "/test.html")
  }
  
  # Title environment for glue
  .env <- new.env(parent = emptyenv())
  .env$title <- "Quick Test"
  
  # Collect packages only from this section/chunk tree.
  # Keep 'knitr' as a small core dependency for rendering.
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

{pkg_chunk}",
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
  
  out <- try(
    rmarkdown::render(
      input = rmd_file,
      output_file = output_file,
      envir = render_env,
      quiet = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(out, "try-error")) {
    return(FALSE)
  }
  
  TRUE
}

#---------


.testReportSection <- function(x, object = NULL, view = TRUE) {
  
  if (!inherits(x, ".textSection")) {
    stop("'x' should be a '.textSection' object.")
  }
  
  rmd_file <- tempfile(fileext = ".Rmd")
  output_file <- tempfile(fileext = ".html")
  
  # Title environment for glue
  .env <- new.env(parent = emptyenv())
  .env$title <- paste0("Testing the text section named: ", x@name)
  
  # Collect packages only from this section/chunk tree.
  # Keep 'knitr' as a small core dependency for rendering.
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

{pkg_chunk}",
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
  
  if (isTRUE(view)) {
    message("Rendering R Markdown report ...")
  }
  
  out <- rmarkdown::render(
    input = rmd_file,
    output_file = output_file,
    envir = render_env,
    quiet = !isTRUE(view)
  )
  
  if (isTRUE(view)) {
    message(
      "Report generated at: ",
      normalizePath(out, winslash = "/", mustWork = FALSE)
    )
    
    viewer <- getOption("viewer")
    
    if (!is.null(viewer)) {
      viewer(out)
    } else {
      utils::browseURL(out)
    }
  }
  
  invisible(out)
}

#---------

setGeneric(
  "testSection",
  function(x, object, view) {
    methods::standardGeneric("testSection")
  }
)

#' Test a report section
#'
#' Render a `.textSection` object as a temporary HTML report to check whether
#' the section text and code can be rendered successfully.
#'
#' This function is mainly used to test custom report sections before they are
#' added to a full [`camReport`][camReport-classes] report. It checks whether the
#' section text, R Markdown chunk settings, required packages, and R code can be
#' rendered successfully.
#'
#' The `object` argument is only needed when the R code in the section refers to
#' a `camReport` object.
#'
#' @param x A `.textSection` object, usually created with [reportSection()].
#' @param object An optional [`camReport`][camReport-classes] object created by
#'   [camData()]. The default is `NULL`.
#' @param view A logical value (default `TRUE`) specifying whether the rendered
#'   temporary HTML file is opened after rendering.
#'
#' @return Invisibly returns the path to the rendered temporary HTML file.
#'
#' @seealso [camData()], [reportSection()], [updateReportSection()], [report()]
#' @family report sections
#'
#' @usage testSection(x, object, view)
#' @rdname testSection
#' @aliases testSection
#'
#' @examplesIf rmarkdown::pandoc_available()
#' tx <- reportSection(
#'   name = "introduction",
#'   title = "Introduction",
#'   parent = NULL,
#'   txt = "This is an introduction section.",
#'   code = {
#'     plot(1:10)
#'   }
#' )
#'
#' # Render the section without opening a browser
#' test_file <- testSection(
#'   tx,
#'   view = FALSE
#' )
#'
#' file.exists(test_file)
#'
#' # Remove the generated HTML file
#' unlink(test_file)
setMethod("testSection",signature(x = ".textSection"),
  function(x, object, view) {
    
    if (missing(object)) {
      object <- NULL
    }
    
    if (missing(view)) {
      view <- TRUE
    }
    
    .testReportSection(x, object, view)
  }
)

#---------
