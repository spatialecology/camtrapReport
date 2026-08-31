.isZip <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(FALSE)
  }
  
  grepl("\\.[Zz][Ii][Pp]$", basename(x[1]))
}

#--------

.isJson <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(FALSE)
  }
  
  grepl("\\.[Jj][Ss][Oo][Nn]$", basename(x[1]))
}

#--------

.getFormat <- function(x) {
  .dtFormats <- c(
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d %H:%M:%OS",
    "%Y/%m/%dT%H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )
  
  x <- x[!is.na(x)]
  x <- x[nzchar(as.character(x))]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  o <- logical(length(.dtFormats))
  
  for (i in seq_along(.dtFormats)) {
    parsed <- suppressWarnings(
      as.POSIXct(
        x,
        format = .dtFormats[i],
        tz = "UTC"
      )
    )
    o[i] <- !anyNA(parsed)
  }
  
  if (any(o)) {
    .dtFormats[which(o)[1]]
  } else {
    NA_character_
  }
}

#--------

.file_info <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(list(
      path = ".",
      filename = NA_character_,
      extension = NA_character_
    ))
  }
  
  x <- as.character(x[1])
  
  if (basename(x) == x || dirname(x) == ".") {
    .dir <- "."
  } else {
    .dir <- dirname(x)
    
    if (.dir == getwd()) {
      .dir <- "."
    }
  }
  
  w <- strsplit(basename(x), ".", fixed = TRUE)[[1]]
  
  if (length(w) > 1) {
    .filename <- paste(w[-length(w)], collapse = "_")
    .extension <- w[length(w)]
  } else {
    .filename <- basename(x)
    .extension <- NA_character_
  }
  
  list(path = .dir, filename = .filename, extension = .extension)
}

#--------

