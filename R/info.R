# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : May 2026
# Version 1.1
# Licence  MIT
#--------

.default_cam_info_names <- c(
  "title",
  "subtitle",
  "authors",
  "institute",
  "siteName",
  "logoPath"
)

#' View information stored in a camReport object
#'
#' This function returns selected metadata fields stored in a `camReport`
#' object, such as the report title, subtitle, authors, institute, site name
#' and logo path.
#'
#' @param x A `camReport` object.
#' @param name Optional character vector with the names of fields to retrieve.
#'   If missing, default report information fields are returned.
#'
#' @return A list of class `camInfo` containing the selected fields.
#'
#' @export
setGeneric(
  "info",
  function(x, name)
    standardGeneric("info")
)

#' @rdname info
#' @export
setMethod(
  "info",
  signature(x = "camReport"),
  function(x, name) {
    
    if (missing(name)) name <- NULL
    
    .inf <- list()
    
    if (is.null(name)) {
      
      name <- .default_cam_info_names
      
    } else {
      
      name <- name[name %in% names(camR$fields())]
      
      if (length(name) == 0) {
        warning(
          "The specified name(s) are not identified or available in the camReport object; the default fields are used."
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

#' Update information stored in a camReport object
#'
#' This replacement method updates selected metadata fields or selected report
#' text sections in a `camReport` object.
#'
#' @param value Replacement value.
#'
#' @rdname info
#' @export
setGeneric(
  "info<-",
  function(x, name, value)
    standardGeneric("info<-")
)

#' @rdname info
#' @export
setReplaceMethod(
  "info",
  signature(x = "camReport", name = "character"),
  function(x, name, value) {
    
    if (length(name) > 1) {
      stop("Only one field can be updated at a time.")
    }
    
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
        return(updateReportSection(x, "introduction", text = value))
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
        stop("The specified name is not identified or available in the camReport object.")
      }
      
      x[[name]] <- value
      return(invisible(x))
    }
  }
)

#--------
