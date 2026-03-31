# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  March 2026
# Version 1.2
# Licence GPL v3
#--------


if (!isGeneric("report")) {
  setGeneric("report", function(x, filename, view)
    standardGeneric("report"))
}

setMethod("report", signature(x = "camReport"),
          function(x, filename = "report", view) {
            if (missing(view)) view <- FALSE
            # Resolve base output directory
            base_dir <- x$info$directory
            
            if (is.null(base_dir) || !nzchar(base_dir)) {
              stop("x$info$directory is empty or NULL.")
            }
            
            # Turn into absolute path and check it exists
            base_dir <- tryCatch(
              normalizePath(base_dir, winslash = "/", mustWork = TRUE),
              error = function(e) {
                stop("The report directory does not exist: ", base_dir, call. = FALSE)
              }
            )
            
            # Resolve requested filename
            if (missing(filename) || is.null(filename) || !nzchar(filename)) {
              filename <- "report"
              fi <- NULL
            } else {
              fi <- .file_info(filename)
              filename <- fi$filename
              if (identical(fi$path, ".")) fi <- NULL
            }
            
            # Decide final output stem
            if (is.null(fi)) {
              out_stem <- file.path(base_dir, filename)
            } else {
              out_dir <- tryCatch(
                normalizePath(fi$path, winslash = "/", mustWork = TRUE),
                error = function(e) {
                  warning(
                    'The directory specified in "filename" ("', fi$path,
                    '") does not exist; using x$info$directory instead.'
                  )
                  base_dir
                }
              )
              out_stem <- file.path(out_dir, filename)
            }
            
            # Generate report
            x$generateReport(
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
# 
# if (!isGeneric("report")) {
#   setGeneric("report", function(x,filename,view)
#     standardGeneric("report"))
# }
# 
# #object$info$directory
# 
# setMethod('report', signature(x='camReport'), 
#           function(x,filename,view=FALSE) {
#             if (missing(view)) view <- FALSE
#             if (missing(filename)) {
#               filename <- 'report'
#               .fi <- NULL
#             } else {
#               .fi <- .file_info(filename)
#               filename <- .fi$filename
#               if (.fi$path == '.') .fi <- NULL
#             }
#             #-----------
#             if (is.null(.fi)) {
#               filename <- path.expand(paste0(x$info$directory,'/',filename))
#             } else {
#               if (dir.exists(.fi$path)) filename <- paste0(.fi$path,'/',filename)
#               else {
#                 warning(paste0('The directory specified in the filename ("',.fi$path,'") does not exist, therefore, the default location is used to write the report file...!'))
#                 filename <- path.expand(paste0(x$info$directory,'/',filename))
#               }
#             }
#             #---------
#             x$generateReport(output_file = paste0(filename,'.html'),rmd_file=paste0(filename,'.Rmd'))
#             
#             if (view) {
#               out <-  paste0(filename,'.html')
#               message("Report generated at: ", normalizePath(out))
#               viewer <- getOption("viewer")
#               if (!is.null(viewer)) {
#                 # Launch the rendered HTML in the Viewer pane
#                 viewer(out)
#               } else {
#                 # Fallback: open in default browser
#                 .eval('utils::browseURL(out)',env = environment())
#               }
#             }
#             
#           }
# )
# 
