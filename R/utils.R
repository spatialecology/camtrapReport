# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : May 2026
# Version 2.3
# Licence GPL v3
#--------

.paste_comma_and <- function(x) {
  x <- x[!is.na(x)]
  x <- as.character(x)
  
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}

#-----------

.trim <- function(x, squish = TRUE) {
  if (missing(x) || is.null(x) || length(x) == 0) return("")
  
  x <- as.character(x[1])
  if (is.na(x)) return("")
  
  x <- trimws(x)
  
  if (isTRUE(squish)) {
    x <- gsub("\\s+", " ", x)
  }
  
  x
}

#------------

# Quiet package loader.
# Returns TRUE if the package is available and attached; FALSE otherwise.
# It hides startup/conflict messages such as:
# "Loading required package...", "Attaching package...", and "masked from ...".
.require <- function(x) {
  x <- as.character(x)[1]
  
  if (is.na(x) || !nzchar(x)) return(FALSE)
  
  if (!requireNamespace(x, quietly = TRUE)) {
    return(FALSE)
  }
  
  ok <- suppressWarnings(
    suppressMessages(
      suppressPackageStartupMessages(
        require(
          x,
          character.only = TRUE,
          quietly = TRUE,
          warn.conflicts = FALSE
        )
      )
    )
  )
  
  isTRUE(ok)
}

# Run an expression while hiding package startup chatter.
.suppress_startup <- function(expr) {
  suppressPackageStartupMessages(
    suppressMessages(
      force(expr)
    )
  )
}

#------------

.format_duration <- function(seconds) {
  seconds <- suppressWarnings(as.numeric(seconds))
  
  if (length(seconds) == 0 || is.na(seconds) || !is.finite(seconds) || seconds < 0) {
    return("unknown time")
  }
  
  seconds <- round(seconds)
  
  if (seconds < 60) {
    return(paste0(seconds, " sec"))
  }
  
  if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    sec <- seconds %% 60
    return(paste0(minutes, " min ", sprintf("%02d", sec), " sec"))
  }
  
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  paste0(hours, " h ", minutes, " min")
}

#------------

.format_file_size <- function(bytes) {
  if (is.null(bytes) || length(bytes) == 0) return("unknown size")
  
  bytes <- suppressWarnings(as.numeric(bytes[1]))
  
  if (is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return("unknown size")
  }
  
  if (bytes < 1024) {
    return(paste0(round(bytes), " B"))
  }
  
  if (bytes < 1024^2) {
    return(paste0(round(bytes / 1024, 1), " KB"))
  }
  
  if (bytes < 1024^3) {
    return(paste0(round(bytes / 1024^2, 1), " MB"))
  }
  
  paste0(round(bytes / 1024^3, 2), " GB")
}

#------------

.estimate_camdata_size <- function(data) {
  if (is.null(data) || length(data) == 0 || is.na(data[1]) || !file.exists(data[1])) {
    return(list(
      file_size = NA_real_,
      file_size_label = "unknown size",
      zip_uncompressed_size = NA_real_,
      zip_uncompressed_label = "unknown size",
      effective_size = NA_real_,
      effective_size_label = "unknown size",
      size_class = "unknown"
    ))
  }
  
  data <- as.character(data[1])
  
  file_size <- NA_real_
  zip_uncompressed_size <- NA_real_
  
  if (dir.exists(data)) {
    all_files <- list.files(data, recursive = TRUE, full.names = TRUE)
    all_files <- all_files[file.exists(all_files)]
    
    if (length(all_files) > 0) {
      file_size <- sum(file.info(all_files)$size, na.rm = TRUE)
    }
  } else {
    file_size <- file.info(data)$size
  }
  
  if (grepl("\\.[Zz][Ii][Pp]$", data)) {
    zip_info <- try(utils::unzip(data, list = TRUE), silent = TRUE)
    
    if (!inherits(zip_info, "try-error") && "Length" %in% names(zip_info)) {
      zip_uncompressed_size <- sum(zip_info$Length, na.rm = TRUE)
    }
  }
  
  effective_size <- suppressWarnings(max(c(file_size, zip_uncompressed_size), na.rm = TRUE))
  if (!is.finite(effective_size)) effective_size <- NA_real_
  
  size_class <- if (is.na(effective_size)) {
    "unknown"
  } else if (effective_size < 200 * 1024^2) {
    "small"
  } else if (effective_size < 1024^3) {
    "medium"
  } else if (effective_size < 5 * 1024^3) {
    "large"
  } else {
    "very_large"
  }
  
  list(
    file_size = file_size,
    file_size_label = .format_file_size(file_size),
    zip_uncompressed_size = zip_uncompressed_size,
    zip_uncompressed_label = .format_file_size(zip_uncompressed_size),
    effective_size = effective_size,
    effective_size_label = .format_file_size(effective_size),
    size_class = size_class
  )
}

#------------
.camdata_start_message <- function(data) {
  size_info <- .estimate_camdata_size(data)
  
  message("⏳ The camReport object is being created...")
  
  if (identical(size_info$size_class, "small")) {
    message("This should only take a moment.")
  } else if (identical(size_info$size_class, "medium")) {
    message("This may take a little while. Progress updates will be shown below.")
  } else if (identical(size_info$size_class, "large")) {
    message("This is a large dataset. You may want to grab a coffee while the camReport object is being created ☕. Progress updates will be shown below.")
  } else if (identical(size_info$size_class, "very_large")) {
    message("This is a very large dataset. Please keep R running; creating the camReport object may take some time ⏳. Progress updates will be shown below.")
  } else {
    message("Creating the camReport object may take some time, depending on file size and number of records. Progress updates will be shown below.")
  }
  
  invisible(size_info)
}

#------------

.camdata_done_message <- function(start_time, site_name = NULL) {
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  
  if (is.null(site_name) || length(site_name) == 0 || is.na(site_name[1]) || !nzchar(site_name[1])) {
    site_name <- "your study site"
  }
  
  message("✅ Data loaded successfully in ", .format_duration(elapsed), ".")
  message("🦌 camReport object is ready for ", site_name, ".")
  
  invisible(TRUE)
}

#------------

.eval <- function(x, env) {
  eval(parse(text = x), envir = env)
}

#------------

.make_render_env <- function(object) {
  env <- new.env(parent = parent.frame())
  
  env$object <- object
  env$cm <- object
  env$.self <- object
  
  field_names <- character()
  
  field_names <- tryCatch(
    names(object$getRefClass()$fields()),
    error = function(e) character()
  )
  
  if (length(field_names) == 0 && exists("camR")) {
    field_names <- tryCatch(
      names(camR$fields()),
      error = function(e) character()
    )
  }
  
  if (length(field_names) > 0) {
    for (nm in field_names) {
      val <- tryCatch(object[[nm]], error = function(e) NULL)
      assign(nm, val, envir = env)
    }
  }
  
  env$getFigureNumber <- function(...) object$getFigureNumber(...)
  env$getTableNumber <- function(...) object$getTableNumber(...)
  
  env
}

#------------

.rmChar <- function(x, rm, rmLast = FALSE) {
  x <- strsplit(as.character(x), "")[[1]]
  
  if (length(x) == 0) return("")
  
  rm <- rm[rm >= 1 & rm <= length(x)]
  if (length(rm) > 0) x <- x[-rm]
  
  if (isTRUE(rmLast) && length(x) > 0) {
    x <- x[-length(x)]
  }
  
  paste(x, collapse = "")
}

#------------

.findParent <- function(x, n) {
  if (length(x) == 0) return(NA)
  
  for (i in seq_along(x)) {
    if (is.list(x[[i]])) {
      out <- .findParent(x[[i]], n)
      if (!all(is.na(out))) return(out)
    } else {
      if (inherits(x[[i]], ".textSection") && identical(x[[i]]@parent, n)) {
        return(c(index = i, name = x[[i]]@name, parent = x[[i]]@parent))
      }
    }
  }
  
  NA
}

#------------

.getYear <- function(x, .interval = FALSE) {
  if (.interval) {
    x <- as.character(x)
    
    lapply(x, function(z) {
      yrs <- regmatches(z, gregexpr("\\b[0-9]{4}\\b", z))[[1]]
      unique(as.numeric(yrs))
    })
  } else {
    suppressWarnings(as.numeric(substr(as.character(x), 1, 4)))
  }
}

#------------

.getMissingTaxon_GBIF <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) == 0) {
    return(data.frame(scientificName = character(), class = character(), order = character()))
  }
  
  if (.require("taxize")) {
    .id <- try(
      as.data.frame(taxize::get_gbifid(x, rows = 1, ask = FALSE, messages = FALSE)),
      silent = TRUE
    )
    
    if (inherits(.id, "try-error") || !"ids" %in% names(.id)) {
      return(data.frame(scientificName = x, class = NA_character_, order = NA_character_))
    }
    
    .x <- try(taxize::classification(.id$ids, db = "gbif"), silent = TRUE)
    
    if (inherits(.x, "try-error")) {
      return(data.frame(scientificName = x, class = NA_character_, order = NA_character_))
    }
    
    w <- which(is.na(names(.x)))
    
    if (length(w) > 0) {
      .x <- .x[-w]
      x <- x[-w]
    }
    
    .class <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "class" %in% z$rank) {
        z$name[z$rank == "class"][1]
      } else if (is.data.frame(z) && nrow(z) >= 3) {
        z$name[3]
      } else {
        NA_character_
      }
    })
    
    .order <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "order" %in% z$rank) {
        z$name[z$rank == "order"][1]
      } else if (is.data.frame(z) && nrow(z) >= 4) {
        z$name[4]
      } else {
        NA_character_
      }
    })
    
    names(.class) <- names(.order) <- NULL
    
    data.frame(
      scientificName = x,
      class = .class,
      order = .order,
      stringsAsFactors = FALSE
    )
  } else {
    stop("You need to install the taxize package before executing this function.")
  }
}

#------------

.getMissingTaxon_NCBI <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) == 0) {
    return(data.frame(scientificName = character(), class = character(), order = character()))
  }
  
  if (.require("taxize")) {
    .id <- try(
      as.data.frame(taxize::get_uid(x, rows = 1, ask = FALSE, messages = FALSE)),
      silent = TRUE
    )
    
    if (inherits(.id, "try-error") || !"ids" %in% names(.id)) {
      return(data.frame(scientificName = x, class = NA_character_, order = NA_character_))
    }
    
    .x <- try(taxize::classification(.id$ids, db = "ncbi"), silent = TRUE)
    
    if (inherits(.x, "try-error")) {
      return(data.frame(scientificName = x, class = NA_character_, order = NA_character_))
    }
    
    .class <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "class" %in% z$rank) {
        z$name[z$rank == "class"][1]
      } else {
        NA_character_
      }
    })
    
    .order <- sapply(.x, function(z) {
      if (is.data.frame(z) && "rank" %in% names(z) && "order" %in% z$rank) {
        z$name[z$rank == "order"][1]
      } else {
        NA_character_
      }
    })
    
    names(.class) <- names(.order) <- NULL
    
    data.frame(
      scientificName = x,
      class = unlist(.class),
      order = unlist(.order),
      stringsAsFactors = FALSE
    )
  } else {
    stop("You need to install the taxize package before executing this function.")
  }
}

#------------

.get_hour <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric())
  
  x <- as.POSIXlt(x)
  x$hour + x$min / 60 + x$sec / 3600
}

#------------

.basic_corrplot <- function(x, main = "Species Co-occurrence") {
  x[upper.tri(x, diag = TRUE)] <- NA
  
  .colors <- grDevices::colorRampPalette(c("red", "white", "blue"))(100)
  
  graphics::image(
    1:ncol(x),
    1:nrow(x),
    t(x[nrow(x):1, ]),
    col = .colors,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main
  )
  
  labels <- colnames(x)
  n <- length(labels)
  
  graphics::text(
    x = 1:n,
    y = graphics::par("usr")[3] - 0.5,
    labels = labels,
    srt = 45,
    adj = 1,
    xpd = TRUE
  )
  
  graphics::text(
    x = graphics::par("usr")[1] - 0.5,
    y = 1:n,
    labels = rev(labels),
    srt = 45,
    adj = 1,
    xpd = TRUE
  )
  
  graphics::legend(
    x = n / 1.2,
    y = n,
    legend = round(seq(-1, 1, length.out = 10), 2),
    fill = grDevices::colorRampPalette(c("red", "white", "blue"))(10),
    border = NA,
    bty = "n",
    y.intersp = 1,
    cex = 0.8
  )
}

#------------

.get_projected_sf <- function(x) {
  if (!.require("sf")) return(NULL)
  
  if (is.null(sf::st_crs(x))) {
    warning("Input sf object has no CRS; assuming EPSG:4326.")
    sf::st_crs(x) <- 4326
  }
  
  if (!identical(sf::st_crs(x)$epsg, 4326L)) {
    x <- sf::st_transform(x, 4326)
  }
  
  cen <- sf::st_coordinates(
    sf::st_centroid(
      sf::st_union(sf::st_geometry(x))
    )
  )
  
  lon <- cen[1]
  lat <- cen[2]
  
  if (abs(lat) <= 84) {
    .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
    .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
    sf::st_transform(x, .epsg)
  } else {
    proj4 <- sprintf(
      "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
      lat,
      lon
    )
    
    sf::st_transform(x, proj4)
  }
}

#------------

.is.projected <- function(x) {
  e <- try(as.vector(terra::ext(x)), silent = TRUE)
  
  if (inherits(e, "try-error") || length(e) != 4) {
    return(FALSE)
  }
  
  !all(e[1:2] >= -180 & e[1:2] <= 180 & e[3:4] >= -90 & e[3:4] <= 90)
}

#------------

.get_projected_vect <- function(x) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The terra package is required for spatial projection.")
  }
  
  if (!.is.projected(x)) {
    cen <- colMeans(terra::crds(x), na.rm = TRUE)
    
    lon <- cen[1]
    lat <- cen[2]
    
    if (abs(lat) <= 84) {
      .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
      .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
      terra::project(x, paste0("EPSG:", .epsg))
    } else {
      proj4 <- sprintf(
        "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
        lat,
        lon
      )
      
      terra::project(x, proj4)
    }
  } else {
    x
  }
}

#------------

.get_Time_length <- function(x, y = NULL, unit = "days") {
  if (is.null(y)) {
    .s <- sapply(as.character(x), function(z) strsplit(z, "--", fixed = TRUE)[[1]][1])
    .e <- sapply(as.character(x), function(z) strsplit(z, "--", fixed = TRUE)[[1]][2])
    
    names(.s) <- names(.e) <- NULL
    
    as.numeric(difftime(as.POSIXct(.e), as.POSIXct(.s), units = unit))
  } else {
    as.numeric(difftime(as.POSIXct(x), as.POSIXct(y), units = unit))
  }
}

#------------

.isZip <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(FALSE)
  grepl("\\.[Zz][Ii][Pp]$", basename(x[1]))
}

#------------

.isJson <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(FALSE)
  grepl("\\.[Jj][Ss][Oo][Nn]$", basename(x[1]))
}

#------------

.firstUpper <- function(x) {
  x <- as.character(x)
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

#------------

.loadPKG <- function(pkgs) {
  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn), add = TRUE)
  
  options(warn = -1)
  all(unlist(lapply(pkgs, function(p) .require(p))))
}

#------------

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
  
  if (length(x) == 0) return(NA_character_)
  
  o <- logical(length(.dtFormats))
  
  for (i in seq_along(.dtFormats)) {
    parsed <- suppressWarnings(as.POSIXct(x, format = .dtFormats[i], tz = "UTC"))
    o[i] <- all(!is.na(parsed))
  }
  
  if (any(o)) .dtFormats[which(o)[1]] else NA_character_
}

#------------

.bind_rows <- function(x) {
  if (is.null(x) || length(x) == 0) return(data.frame())
  
  x <- x[!vapply(x, is.null, logical(1))]
  
  if (length(x) == 0) return(data.frame())
  
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

#------------

.is.POSIXct <- function(x) {
  inherits(x, "POSIXct")
}

#------------

.get_match <- function(x, y, several = TRUE, case_sensitive = FALSE) {
  if (!case_sensitive) {
    .x <- tolower(x)
    .y <- tolower(y)
    
    .yy <- try(match.arg(.x, .y, several.ok = several), silent = TRUE)
    
    if (!inherits(.yy, "try-error")) {
      o <- character()
      
      for (n in .yy) {
        w <- which(.y == n)
        o <- c(o, y[w])
      }
      
      o
    } else {
      NA
    }
  } else {
    xx <- try(match.arg(x, y, several.ok = several), silent = TRUE)
    
    if (!inherits(xx, "try-error")) xx else NA
  }
}

#------------

.file_info <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(list(path = ".", filename = NA_character_, extension = NA_character_))
  }
  
  x <- as.character(x[1])
  
  if (basename(x) == x || dirname(x) == ".") {
    .dir <- "."
  } else {
    .dir <- dirname(x)
    if (.dir == getwd()) .dir <- "."
  }
  
  w <- strsplit(basename(x), "\\.")[[1]]
  
  if (length(w) > 1) {
    .filename <- paste(w[-length(w)], collapse = "_")
    .extension <- w[length(w)]
  } else {
    .filename <- basename(x)
    .extension <- NA_character_
  }
  
  list(path = .dir, filename = .filename, extension = .extension)
}

#------------

.trim_chr <- function(x) {
  trimws(as.character(x))
}

#------------

.pick_col <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df)) return(NA_character_)
  
  hit <- candidates[candidates %in% names(df)]
  
  if (length(hit)) hit[1] else NA_character_
}

#------------

.charN <- function(x, space = TRUE) {
  if (missing(x) || is.null(x)) return(NULL)
  
  x <- as.character(x)
  
  if (length(x) > 1) {
    return(sapply(x, .charN, space = space))
  }
  
  if (is.na(x) || !nzchar(trimws(x))) return(0)
  
  x <- .trim(x)
  x <- strsplit(x, "")[[1]]
  
  if (space) length(x) else length(x[x != " "])
}

#------------

.wordN <- function(x) {
  if (missing(x) || is.null(x)) return(NULL)
  
  x <- as.character(x)
  
  if (length(x) > 1) {
    return(sapply(x, .wordN))
  }
  
  if (is.na(x) || !nzchar(trimws(x))) return(0)
  
  length(strsplit(.trim(x), "\\s+")[[1]])
}

#------------

.word <- function(x, start = NULL, end = NULL) {
  if (missing(x) || is.null(x)) return(NULL)
  
  x <- as.character(x)
  
  if (length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))) {
    return(NULL)
  }
  
  x <- .trim(x[1])
  .w <- unlist(strsplit(x, "[ ,;:.]+"))
  .w <- .w[nzchar(.w)]
  
  if (length(.w) == 0) return(.w)
  
  if (!is.null(start) && is.numeric(start) && start != 0) {
    if (start < 0) {
      start <- abs(as.integer(start))
      if (start > length(.w)) start <- length(.w)
      end <- length(.w)
      start <- length(.w) - start + 1
    } else {
      if (start <= length(.w)) {
        if (is.null(end)) {
          end <- start
        } else if (!is.numeric(end) || end > length(.w)) {
          end <- start
        } else if (end < start) {
          warning('The "end" argument cannot be lower than "start".')
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

#------------

.pretty_label <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x <- x[nzchar(x)]
  
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x)
  if (length(x) == 2) return(paste(x, collapse = " and "))
  
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}