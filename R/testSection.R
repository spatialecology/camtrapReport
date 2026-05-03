# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  May 2026
# Version 1.3
# Licence GPL v3
#--------



.QuickTestReportSection <- function(x, object = NULL,path=NULL) {
  if (is.null(path)) {
    rmd_file    <- tempfile(fileext = ".Rmd")
    output_file <- tempfile(fileext = ".html")
  } else {
    rmd_file    <- paste0(path,"/test.Rmd")
    output_file <- paste0(path,"/test.html")
  }
  
  
  # Title environment for glue
  .env <- new.env(parent = emptyenv())
  .env$title <- "Quick Test"
  
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
  
  out <- try(rmarkdown::render(
    input = rmd_file,
    output_file = output_file,
    envir = render_env,
    quiet = TRUE
  ),silent = TRUE)
  
  if (inherits(out,'try-error')) return(FALSE)
  
  return(TRUE)
}
#---------
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

