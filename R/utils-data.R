.bind_rows <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(data.frame())
  }
  
  x <- x[!vapply(x, is.null, logical(1))]
  
  if (length(x) == 0) {
    return(data.frame())
  }
  
  if (requireNamespace("dplyr", quietly = TRUE)) {
    return(as.data.frame(dplyr::bind_rows(x)))
  }
  
  all_cols <- unique(unlist(lapply(x, names)))
  
  x <- lapply(x, function(df) {
    df <- as.data.frame(df)
    missing_cols <- setdiff(all_cols, names(df))
    
    for (cc in missing_cols) {
      df[[cc]] <- NA
    }
    
    df[, all_cols, drop = FALSE]
  })
  
  do.call(rbind, x)
}

#--------

.get_match <- function(x, y, several = TRUE, case_sensitive = FALSE) {
  if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
    return(NA)
  }
  
  if (case_sensitive) {
    xx <- try(match.arg(x, y, several.ok = several), silent = TRUE)

    if (inherits(xx, "try-error")) {
      NA
    } else {
      xx
    }
  } else {
    .x <- tolower(x)
    .y <- tolower(y)

    .yy <- try(match.arg(.x, .y, several.ok = several), silent = TRUE)

    if (inherits(.yy, "try-error")) {
      NA
    } else {
      o <- character()

      for (n in .yy) {
        w <- which(.y == n)
        o <- c(o, y[w])
      }

      o
    }
  }
}

#--------

.pick_col <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df)) {
    return(NA_character_)
  }
  
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit)) {
    hit[1]
  } else {
    NA_character_
  }
}

#--------

