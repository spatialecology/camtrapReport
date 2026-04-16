# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  April 2026
# Version 1.0
# Licence GPL v3
#--------


if (!isGeneric("status")) {
  setGeneric("status", function(object, filename, view)
    standardGeneric("status"))
}

setMethod("status", signature(object = "camReport"),
          function(object, filename = "data_status", view) {
            if (missing(view)) view <- FALSE
            
            # Resolve requested filename
            if (missing(filename) || is.null(filename) || !nzchar(filename)) {
              filename <- "data_status"
              fi <- NULL
            } else {
              fi <- .file_info(filename)
              filename <- fi$filename
              if (identical(fi$path, ".")) fi <- NULL
            }
            #-----
            # Resolve base output directory
            base_dir <- object$info$directory
            
            # Turn into absolute path and check it exists
            base_dir <- tryCatch(
              normalizePath(base_dir, winslash = "/", mustWork = TRUE),
              error = function(e) {
                getwd()
              }
            )
            
            # Decide final output stem
            if (is.null(fi)) {
              out_stem <- file.path(base_dir, filename)
            } else {
              out_dir <- tryCatch(
                normalizePath(fi$path, winslash = "/", mustWork = TRUE),
                error = function(e) {
                  warning(
                    'The directory specified in "filename" ("', fi$path,
                    '") does not exist; The default path is used instead.'
                  )
                  base_dir
                }
              )
              out_stem <- file.path(out_dir, filename)
            }
            
            # Generate report
            object$generateStatusReport(
              output_file = paste0(out_stem, ".html"),
              rmd_file    = paste0(out_stem, ".Rmd")
            )
            
            if (isTRUE(view)) {
              out <- paste0(out_stem, ".html")
              message("Report generated at: ", normalizePath(out, winslash = "/", mustWork = FALSE))
              
              viewer <- getOption("viewer")
              if (!is.null(viewer)) {
                viewer(out)
              } else {
                utils::browseURL(out)
              }
            }
            
            invisible(paste0(out_stem, ".html"))
          }
)
