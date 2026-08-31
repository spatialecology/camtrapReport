# Internal utility functions for camtrapReport
# Licence: MIT
#--------

.paste_comma_and <- function(x) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return("")
  }
  
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[nzchar(x)]
  x <- unique(x)
  
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  
  paste0(
    toString(x[-length(x)]),
    ", and ",
    x[length(x)]
  )
}

#--------

.trim <- function(x, squish = TRUE) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return("")
  }
  
  x <- as.character(x[1])
  
  if (is.na(x)) {
    return("")
  }
  
  x <- trimws(x)
  
  if (isTRUE(squish)) {
    x <- gsub("\\s+", " ", x)
  }
  
  x
}

#--------

.trim_chr <- function(x) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return(character())
  }
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

#--------

.require <- function(x) {
  x <- as.character(x)[1]
  
  if (is.na(x) || !nzchar(x)) {
    return(FALSE)
  }
  
  requireNamespace(
    x,
    quietly = TRUE
  )
}

#--------

.suppress_startup <- function(expr) {
  suppressPackageStartupMessages(
    suppressMessages(
      force(expr)
    )
  )
}

#--------

.loadPKG <- function(pkgs) {
  pkgs <- as.character(pkgs)
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  
  suppressWarnings(
    all(unlist(lapply(pkgs, .require)))
  )
}

#--------

