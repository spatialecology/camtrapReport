.rmChar <- function(x, rm, rmLast = FALSE) {
  x <- strsplit(as.character(x), "", fixed = TRUE)[[1]]
  
  if (length(x) == 0) {
    return("")
  }
  
  rm <- rm[rm >= 1 & rm <= length(x)]
  
  if (length(rm) > 0) {
    x <- x[-rm]
  }
  
  if (isTRUE(rmLast) && length(x) > 0) {
    x <- x[-length(x)]
  }
  
  paste(x, collapse = "")
}

#--------

.findParent <- function(x, n) {
  if (length(x) == 0) {
    return(NA)
  }
  
  for (i in seq_along(x)) {
    if (is.list(x[[i]])) {
      out <- .findParent(x[[i]], n)
      
      if (!all(is.na(out))) {
        return(out)
      }
    } else {
      if (inherits(x[[i]], ".textSection") && identical(x[[i]]@parent, n)) {
        return(c(index = i, name = x[[i]]@name, parent = x[[i]]@parent))
      }
    }
  }
  
  NA
}

#--------

.firstUpper <- function(x) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return(character())
  }
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

#--------

.charN <- function(x, space = TRUE) {
  if (missing(x) || is.null(x)) {
    return(NULL)
  }
  
  x <- as.character(x)
  
  if (length(x) > 1) {
    return(vapply(x, .charN, numeric(1), space = space))
  }
  
  if (is.na(x) || !nzchar(trimws(x))) {
    return(0)
  }
  
  x <- .trim(x)
  x <- strsplit(x, "", fixed = TRUE)[[1]]
  
  if (space) {
    length(x)
  } else {
    length(x[x != " "])
  }
}

#--------

.wordN <- function(x) {
  if (missing(x) || is.null(x)) {
    return(NULL)
  }
  
  x <- as.character(x)
  
  if (length(x) > 1) {
    return(vapply(x, .wordN, numeric(1)))
  }
  
  if (is.na(x) || !nzchar(trimws(x))) {
    return(0)
  }
  
  length(strsplit(.trim(x), "\\s+")[[1]])
}

#--------

.word <- function(x, start = NULL, end = NULL) {
  if (missing(x) || is.null(x)) {
    return(NULL)
  }
  
  x <- as.character(x)
  
  if (length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(NULL)
  }
  
  x <- .trim(x[1])
  .w <- unlist(strsplit(x, "[ ,;:.]+"))
  .w <- .w[nzchar(.w)]
  
  if (length(.w) == 0) {
    return(.w)
  }
  
  if (!is.null(start) && is.numeric(start) && start != 0) {
    if (start < 0) {
      start <- abs(as.integer(start))
      
      if (start > length(.w)) {
        start <- length(.w)
      }
      
      end <- length(.w)
      start <- length(.w) - start + 1
    } else {
      if (start <= length(.w)) {
        if (is.null(end)) {
          end <- start
        } else if (!is.numeric(end) || end > length(.w)) {
          end <- start
        } else if (end < start) {
          warning("The 'end' argument cannot be lower than 'start'.")
          end <- start
        }
      } else {
        start <- 1
        end <- length(.w)
      }
    }
  } else {
    start <- 1
    end <- length(.w)
  }
  
  .w[start:end]
}

#--------

.pretty_label <- function(x) {
  if (missing(x) || is.null(x) || length(x) == 0) {
    return("")
  }
  
  x <- as.character(x)
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  
  paste0(toString(x[-length(x)]), ", and ", x[length(x)])
}
