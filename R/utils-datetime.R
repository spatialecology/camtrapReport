.getYear <- function(x, .interval = FALSE) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    if (isTRUE(.interval)) {
      return(list())
    }
    
    return(numeric())
  }
  
  if (isTRUE(.interval)) {
    x <- as.character(x)
    
    lapply(x, function(z) {
      if (is.na(z) || !nzchar(z)) {
        return(numeric())
      }
      
      yrs <- regmatches(z, gregexpr("\\b[0-9]{4}\\b", z))[[1]]
      unique(suppressWarnings(as.numeric(yrs)))
    })
  } else {
    suppressWarnings(as.numeric(substr(as.character(x), 1, 4)))
  }
}

#--------

.get_hour <- function(x, tz = "UTC") {
  if (is.null(x) || length(x) == 0) {
    return(numeric())
  }
  
  if (inherits(x, "POSIXct")) {
    pxct <- x
  } else if (inherits(x, "POSIXlt")) {
    pxct <- as.POSIXct(x, tz = tz)
  } else {
    x <- as.character(x)
    x[!nzchar(trimws(x))] <- NA_character_
    
    formats <- c(
      "%Y-%m-%d %H:%M:%OS",
      "%Y-%m-%dT%H:%M:%OS",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%dT%H:%M:%S",
      "%Y/%m/%d %H:%M:%OS",
      "%Y/%m/%dT%H:%M:%OS",
      "%Y/%m/%d %H:%M:%S",
      "%Y/%m/%dT%H:%M:%S",
      "%Y-%m-%d",
      "%Y/%m/%d"
    )
    
    pxct <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = tz)
    
    for (fmt in formats) {
      missing_i <- is.na(pxct) & !is.na(x)
      
      if (!any(missing_i)) {
        break
      }
      
      parsed <- suppressWarnings(
        as.POSIXct(x[missing_i], format = fmt, tz = tz)
      )
      
      ok <- !is.na(parsed)
      pxct[which(missing_i)[ok]] <- parsed[ok]
    }
  }
  
  px <- as.POSIXlt(pxct, tz = tz)
  out <- px$hour + px$min / 60 + px$sec / 3600
  out[is.na(pxct)] <- NA_real_
  out
}

#--------

.get_Time_length <- function(x, y = NULL, unit = "days") {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return(numeric())
  }
  
  if (is.null(y)) {
    x <- as.character(x)
    
    out <- vapply(x, function(z) {
      if (is.na(z) || !nzchar(z) || !grepl("--", z, fixed = TRUE)) {
        return(NA_real_)
      }
      
      parts <- strsplit(z, "--", fixed = TRUE)[[1]]
      
      if (length(parts) < 2) {
        return(NA_real_)
      }
      
      start <- suppressWarnings(as.POSIXct(parts[1]))
      end <- suppressWarnings(as.POSIXct(parts[2]))
      
      if (is.na(start) || is.na(end)) {
        return(NA_real_)
      }
      
      as.numeric(difftime(end, start, units = unit))
    }, numeric(1))
    
    names(out) <- NULL
    out
  } else {
    start <- suppressWarnings(as.POSIXct(x))
    end <- suppressWarnings(as.POSIXct(y))
    
    as.numeric(difftime(start, end, units = unit))
  }
}

#--------

.is.POSIXct <- function(x) {
  inherits(x, "POSIXct")
}

#--------

