# Functions for selecting and updating camtrapReport report sections
# Licence: MIT
#--------

setGeneric(
  "section_names",
  function(keep, exclude) {
    methods::standardGeneric("section_names")
  }
)


#' Select report sections
#'
#' Get the names of available report sections or update which sections are
#' included in a [`camReport`][camReport-classes] report.
#'
#' `section_names()` returns the names of available report sections. It can be
#' used to identify valid section names before selecting sections with
#' `sections()`.
#'
#' `sections()` updates the report-section selection stored in a `camReport`
#' object. Only the selected sections are included when the report is generated.
#' When `n` is omitted, it returns the names of the object's currently available
#' sections without changing the object.
#'
#' @param keep An optional character vector of section names to keep. When
#'   omitted, all available sections are returned.
#' @param exclude An optional character vector of section names to exclude. The
#'   default is `NULL`.
#' @param x A [`camReport`][camReport-classes] object created by [camData()].
#' @param n An optional character vector giving the names of report sections to
#'   include. When omitted, the current section names are returned.
#'
#' @return `section_names()` returns a character vector of section names.
#'   `sections()` returns a character vector when `n` is omitted; otherwise, it
#'   updates the supplied `camReport` object and returns it invisibly.
#'
#' @seealso [camData()], [report()], [updateReportSection()],
#'   [listReportSections()]
#' @family report sections
#'
#' @usage
#' section_names(keep, exclude)
#'
#' sections(x, n)
#' @name section_names
#' @aliases section_names sections section_names,ANY-method
#' @aliases sections,camReport-method
#'
#' @examples
#' # List all available report-section names
#' available_sections <- section_names()
#' head(available_sections)
#'
#' \donttest{
#' # Load the packaged example dataset
#' source_dataset <- system.file(
#'   "external",
#'   "dataset",
#'   package = "camtrapReport"
#' )
#' example_dataset <- tempfile("camtrapReport-example-")
#' dir.create(example_dataset)
#' invisible(file.copy(
#'   list.files(source_dataset, full.names = TRUE),
#'   example_dataset,
#'   recursive = TRUE
#' ))
#'
#' cm <- camData(example_dataset)
#'
#' # Inspect the sections currently available for this object
#' current_sections <- sections(cm)
#' head(current_sections)
#'
#' # Select a small subset of available sections
#' selected_sections <- head(current_sections, 4)
#' cm <- sections(cm, selected_sections)
#'
#' # Show the selected section names
#' selected_sections
#'
#' unlink(example_dataset, recursive = TRUE, force = TRUE)
#' }
setMethod("section_names",signature(keep = "ANY"),
  function(keep, exclude) {
    
    if (missing(keep)) keep <- NULL
    if (missing(exclude)) exclude <- NULL
    
    # Get all available module names
    n <- .get_module_names()
    
    if (is.character(keep) && length(keep) > 0) {
      
      w <- keep %in% n
      
      if (!all(w)) {
        if (!any(w)) {
          stop(
            "None of the specified section/module names in 'keep' are available; use section_names() to get a list of existing modules."
          )
        }
        
        warning(
          "Several section/module names specified in 'keep' are not available: ",
          .paste_comma_and(keep[!w])
        )
      }
      
      n <- keep[w]
      
      w <- .check_parent(n)
      
      if (!is.null(w)) {
        n <- n[!n %in% w]
      }
      
      return(n)
    }
    
    if (is.character(exclude) && length(exclude) > 0) {
      
      w <- exclude %in% n
      
      if (!all(w)) {
        if (!any(w)) {
          stop(
            "None of the specified section/module names in 'exclude' are available; use section_names() to get a list of existing modules."
          )
        }
        
        warning(
          "Several section/module names specified in 'exclude' are not available: ",
          .paste_comma_and(exclude[!w])
        )
      }
      
      exclude <- exclude[w]
      n <- n[!n %in% exclude]
      
      w <- .check_parent(n)
      
      if (!is.null(w)) {
        n <- n[!n %in% w]
      }
      
      return(n)
    }
    
    n
  }
)

#-------
setGeneric(
  "sections",
  function(x, n) {
    methods::standardGeneric("sections")
  }
)

setMethod("sections",signature(x = "camReport"),
  function(x, n) {
    
    if (missing(n)) {
      n <- NULL
    } else if (!is.character(n)) {
      n <- NULL
      warning("`n` should be character; it is ignored.")
    }
    
    # Sections with successful or untested status, ordered by test result
    w <- sort(
      c(
        which(is.na(x$reportObjectElements$Modules_info$tested)),
        which(x$reportObjectElements$Modules_info$tested)
      )
    )
    
    nn <- x$reportObjectElements$Modules_info$name[w]
    
    if (is.null(n)) {
      return(nn)
    }
    
    if (!all(n %in% nn)) {
      
      if (all(n %in% x$reportObjectElements$Modules_info$name)) {
        
        message(
          "\nSome of the specified sections are excluded because their test results were problematic."
        )
        
      } else {
        
        if (!any(n %in% nn)) {
          stop(
            "None of the specified section names are known. Use section_names() to get the correct names of available sections."
          )
        } else {
          message(
            "\nSome of the specified section names are unknown and ignored. Use section_names() to get the correct names of available sections."
          )
        }
      }
    }
    
    n <- n[n %in% nn]
    
    .attach_modules(x, n = n)
    
    message("\nThe report sections are updated.")
    
    invisible(x)
  }
)

#-------
