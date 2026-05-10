# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : May 2026
# Version 1.5
# Licence GPL v3
#--------

#' Generate an Ecological Report
#'
#' Generates an automated ecological report from a `camReport` object.
#'
#' @param object A `camReport` object.
#' @param filename Output filename or file path without extension. Defaults to `"report"`.
#' @param view Logical. If `TRUE`, the generated HTML report is opened after rendering.
#' @param test Logical. If `TRUE`, modules are tested if report generation fails.
#'
#' @return Invisibly returns the path to the generated HTML report.
#'
#' @export
setGeneric(
  "report",
  function(object, filename, view, test)
    standardGeneric("report")
)

#' @rdname report
#' @export
setMethod(
  "report",
  signature(object = "camReport"),
  function(object, filename = "report", view, test) {
    
    if (missing(view)) view <- FALSE
    if (missing(test)) test <- FALSE
    
    # Resolve requested filename
    if (missing(filename) || is.null(filename) || !nzchar(filename)) {
      filename <- "report"
      fi <- NULL
    } else {
      fi <- .file_info(filename)
      filename <- fi$filename
      if (identical(fi$path, ".")) fi <- NULL
    }
    
    # Resolve base output directory
    base_dir <- object$info$directory
    
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
            '") does not exist; the default path is used instead.'
          )
          base_dir
        }
      )
      out_stem <- file.path(out_dir, filename)
    }
    
    # Generate report
    w <- try(
      object$generateReport(
        output_file = paste0(out_stem, ".html"),
        rmd_file    = paste0(out_stem, ".Rmd")
      ),
      silent = TRUE
    )
    
    if (inherits(w, "try-error")) {
      
      if (test) {
        
        message("\nTesting of modules is started....")
        
        ww <- which(is.na(object$reportObjectElements$Modules_info$tested))
        
        if (length(ww) > 0) {
          
          dir.create(
            paste0(object$info$directory, "/_temp"),
            showWarnings = FALSE
          )
          
          if (dir.exists(paste0(object$info$directory, "/_temp"))) {
            .path <- paste0(object$info$directory, "/_temp")
          } else {
            .path <- NULL
          }
          
          n <- object$reportObjectElements$Modules_info$name[ww]
          
          for (nn in n) {
            .w <- .QuickTestReportSection(
              object$reportObjectElements$Modules[[nn]],
              object,
              path = .path
            )
            
            object$reportObjectElements$Modules_info$tested[
              object$reportObjectElements$Modules_info$name == nn
            ] <- .w
          }
          
          .attach_modules(
            object,
            n = object$reportObjectElements$Modules_info$name[
              which(object$reportObjectElements$Modules_info$tested)
            ]
          )
          
          message(
            "\nTesting is done; the modules are attached, and the report generation is started...!"
          )
          
          return(report(object, filename = filename, view = view, test = FALSE))
          
        } else {
          
          if (!all(object$reportObjectElements$Modules_info$tested)) {
            
            .attach_modules(
              object,
              n = object$reportObjectElements$Modules_info$name[
                which(object$reportObjectElements$Modules_info$tested)
              ]
            )
            
            return(report(object, filename = filename, view = view, test = FALSE))
            
          } else {
            stop("Although all sections are tested, the report cannot be generated...!")
          }
        }
        
      } else {
        
        message(
          "Report generation is stopped because of an error; add `test = TRUE` to exclude the modules that cause error!"
        )
        
        return(w)
      }
    }
    
    if (isTRUE(view)) {
      
      out <- paste0(out_stem, ".html")
      
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
    
    invisible(paste0(out_stem, ".html"))
  }
)