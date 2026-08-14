# Functions for accessing and updating camtrapReport report information
# Licence: MIT
#--------

.default_cam_info_names <- c(
  "title",
  "subtitle",
  "authors",
  "institute",
  "siteName",
  "logoPath"
)



setGeneric(
  "info",
  function(x, name) {
    methods::standardGeneric("info")
  }
)

#' Get or set information in a camReport object
#'
#' Get selected information fields from a [`camReport`][camReport-classes]
#' object or update the value of a field used in report generation.
#'
#' A `camReport` object contains metadata and report information extracted from
#' the camera-trap dataset, such as title, subtitle, authors, institute, site
#' name, description, and acknowledgement text.
#'
#' The `info` function can be used to inspect selected fields. The replacement
#' form, `info(x, name) <- value`, can be used to update report information
#' before generating the report.
#'
#' @param x A [`camReport`][camReport-classes] object created by [camData()].
#' @param name A character vector naming fields to retrieve, or a single
#'   character string naming the field to update. When omitted from `info()`,
#'   the default report-information fields are returned.
#' @param value The new value to assign to the specified field.
#'
#' @return `info()` returns an object of class `camInfo`
#'   containing the requested fields. The replacement method returns the
#'   updated `camReport` object invisibly.
#'
#' @seealso [camData()], [report()], [status()], [updateReportSection()]
#' @family report metadata
#'
#' @rdname info
#' @aliases info info<-
#'
#' @examples
#' example_dataset <- system.file(
#'   "external",
#'   "dataset",
#'   package = "camtrapReport"
#' )
#'
#' cm <- camData(example_dataset)
#'
#' # Retrieve all report information
#' info(cm)
#'
#' # Retrieve selected fields
#' selected_info <- info(
#'   cm,
#'   name = c("title", "authors", "institute")
#' )
#'
#' selected_info
#'
#' # Update individual fields
#' info(cm, "title") <- "Camera-trap monitoring report"
#' info(cm, "institute") <- "Example Wildlife Research Institute"
#'
#' # Inspect the updated fields
#' info(
#'   cm,
#'   name = c("title", "institute")
#' )
setMethod(
  "info",
  signature(x = "camReport"),
  function(x, name) {
    
    if (missing(name)) {
      name <- NULL
    }
    
    .inf <- list()
    
    if (is.null(name)) {
      
      name <- .default_cam_info_names
      
    } else {
      
      name <- name[name %in% names(camR$fields())]
      
      if (length(name) == 0) {
        warning(
          paste0(
            "The specified name(s) are not identified or available ",
            "in the camReport object; the default fields are used."
          )
        )
        name <- .default_cam_info_names
      }
    }
    
    for (n in name) {
      .inf[[n]] <- x[[n]]
    }
    
    class(.inf) <- "camInfo"
    .inf
  }
)

setGeneric(
  "info<-",
  function(x, name, value) {
    methods::standardGeneric("info<-")
  }
)

#' @rdname info
setReplaceMethod(
  "info",
  signature(x = "camReport"),
  function(x, name, value) {
    
    if (
      missing(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(trimws(name))
    ) {
      stop(
        "'name' must be one non-empty character string.",
        call. = FALSE
      )
    }
    
    name <- trimws(as.character(name))
    name_lower <- tolower(name)
    
    if (name_lower %in% c(
      "introduction",
      "study area",
      "image processing",
      "sampling efforts",
      "sampling effort",
      "acknowledgements",
      "acknowledgement"
    )) {
      
      if (name_lower == "study area") {
        x$description <- value
        return(invisible(x))
      }
      
      if (name_lower == "introduction") {
        return(updateReportSection(x, "introduction", text = value))
      }
      
      if (name_lower == "image processing") {
        return(updateReportSection(x, "image processing", text = value))
      }
      
      if (name_lower %in% c("sampling efforts", "sampling effort")) {
        return(updateReportSection(x, "sampling efforts", text = value))
      }
      
      if (name_lower %in% c("acknowledgements", "acknowledgement")) {
        return(updateReportSection(x, "acknowledgements", text = value))
      }
      
    } else {
      
      .f <- camR$fields()
      name <- name[name %in% names(.f)]
      
      if (length(name) == 0) {
        stop(
          paste0(
            "The specified name is not identified or available ",
            "in the camReport object."
          )
        )
      }
      
      x[[name]] <- value
      return(invisible(x))
    }
  }
)
#--------
