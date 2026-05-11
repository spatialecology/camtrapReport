# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : May 2026
# Version 3.2
# Licence MIT
#--------

.parse_cam_datetime <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  
  if (is.null(x)) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz))
  }
  
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  
  out <- as.POSIXct(
    rep(NA_real_, length(x_chr)),
    origin = "1970-01-01",
    tz = tz
  )
  
  x_try <- x_chr
  x_try <- gsub("Z$", "+0000", x_try)
  x_try <- gsub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x_try)
  
  formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS%z",
    "%Y/%m/%dT%H:%M:%OS%z",
    "%Y/%m/%d %H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d %H:%M:%OS",
    "%Y/%m/%dT%H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M%z",
    "%Y-%m-%d %H:%M%z",
    "%Y-%m-%dT%H:%M",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%dT%H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )
  
  for (fmt in formats) {
    missing_i <- is.na(out) & !is.na(x_try)
    if (!any(missing_i)) break
    
    parsed <- suppressWarnings(
      as.POSIXct(x_try[missing_i], format = fmt, tz = tz)
    )
    
    ok <- !is.na(parsed)
    out[which(missing_i)[ok]] <- parsed[ok]
  }
  
  missing_i <- is.na(out) & !is.na(x_chr)
  
  if (any(missing_i) && requireNamespace("lubridate", quietly = TRUE)) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        x_chr[missing_i],
        orders = c(
          "ymd HMS z", "ymd HMS",
          "ymd HM z",  "ymd HM",
          "ymd z",     "ymd",
          "Ymd HMS z", "Ymd HMS",
          "ymdT HMS z", "ymdT HMS",
          "ymdT HM z",  "ymdT HM"
        ),
        tz = tz,
        quiet = TRUE
      )
    )
    
    ok <- !is.na(parsed)
    out[which(missing_i)[ok]] <- as.POSIXct(parsed[ok], tz = tz)
  }
  
  out
}

#--------

.first_non_missing <- function(x) {
  x <- unique(x[!is.na(x)])
  if (length(x) == 0) return(NA)
  x[1]
}

#--------

.safe_min_time <- function(x, tz = "UTC") {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz))
  }
  as.POSIXct(min(x), tz = tz)
}

#--------

.safe_max_time <- function(x, tz = "UTC") {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz))
  }
  as.POSIXct(max(x), tz = tz)
}

#--------

.getSequences <- function(media) {
  if (!.require("data.table")) {
    stop("The data.table package is not installed.")
  }
  
  if (is.null(media) || !is.data.frame(media) || nrow(media) == 0) {
    return(data.frame(
      sequenceID = character(),
      sequence_interval = lubridate::interval(
        as.POSIXct(character()),
        as.POSIXct(character())
      ),
      deploymentID = character(),
      captureMethod = character(),
      nrphotos = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  required_cols <- c("deploymentID", "sequenceID", "timestamp", "captureMethod")
  missing_cols <- setdiff(required_cols, names(media))
  
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) {
      media[[cc]] <- NA
    }
  }
  
  media$timestamp <- .parse_cam_datetime(media$timestamp, tz = "UTC")
  
  sequences <- media |>
    dplyr::distinct() |>
    dplyr::select(
      dplyr::all_of(c("deploymentID", "sequenceID", "timestamp", "captureMethod"))
    )
  
  sequences <- sequences[!is.na(sequences$sequenceID), , drop = FALSE]
  
  if (nrow(sequences) == 0) {
    return(data.frame(
      sequenceID = character(),
      sequence_interval = lubridate::interval(
        as.POSIXct(character()),
        as.POSIXct(character())
      ),
      deploymentID = character(),
      captureMethod = character(),
      nrphotos = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  sequences <- data.table::data.table(sequences, key = "sequenceID")
  
  sequences <- sequences[, list(
    deploymentID = .first_non_missing(deploymentID),
    captureMethod = .first_non_missing(captureMethod),
    start = .safe_min_time(timestamp, tz = "UTC"),
    end = .safe_max_time(timestamp, tz = "UTC"),
    nrphotos = sum(!is.na(timestamp))
  ), by = sequenceID]
  
  sequences <- sequences |>
    dplyr::as_tibble() |>
    dplyr::arrange(.data$deploymentID, .data$sequenceID) |>
    dplyr::mutate(
      sequence_interval = lubridate::interval(.data$start, .data$end)
    ) |>
    dplyr::relocate(.data$sequence_interval, .before = .data$start) |>
    dplyr::select(-dplyr::all_of(c("start", "end")))
  
  as.data.frame(sequences)
}

#--------

.get_Taxonomic_DF <- function(x) {
  w <- sapply(x, function(z) {
    length(names(z$vernacularNames))
  })
  
  if (all(w == 0)) {
    dplyr::bind_rows(lapply(x, function(z) {
      .x <- strsplit(z$taxonID, "/")[[1]]
      
      .x <- data.frame(
        taxonID = .x[length(.x)],
        scientificName = z$scientificName,
        family = z$family,
        order = z$order,
        class = NA,
        taxonRank = z$taxonRank,
        stringsAsFactors = FALSE
      )
      
      if (length(z$vernacularNames) > 0) {
        .x[["vernacularNames"]] <- z$vernacularNames
      } else {
        .x[["vernacularNames"]] <- NA
      }
      
      .x
    }))
    
  } else if (any(w > 0)) {
    .w <- max(w, na.rm = TRUE)
    
    .tmp <- unlist(lapply(x, function(z) {
      names(z$vernacularNames)
    }))
    
    if (length(unique(.tmp[!is.na(.tmp)])) > .w) {
      n <- rep(NA, .w)
      
      for (i in seq_len(.w)) {
        n[i] <- names(sort(table(sapply(x, function(z) {
          names(z$vernacularNames)[i]
        })), decreasing = TRUE))[1]
      }
    } else {
      ww <- which.max(w)
      n <- names(x[[ww]]$vernacularNames)
    }
    
    .xx <- data.frame(
      taxonID = "",
      scientificName = "",
      family = "",
      order = "",
      class = NA,
      taxonRank = "",
      stringsAsFactors = FALSE
    )
    
    if (length(n) > 0) {
      .n <- paste0("vernacularNames.", n)
      for (i in seq_along(.n)) {
        .xx[[.n[i]]] <- ""
      }
    }
    
    dplyr::bind_rows(lapply(x, function(z) {
      .x <- .xx
      
      .tmp <- strsplit(z$taxonID, "/")[[1]]
      .x$taxonID <- .tmp[length(.tmp)]
      .x$scientificName <- z$scientificName
      .x$family <- z$family
      .x$order <- z$order
      .x$taxonRank <- z$taxonRank
      
      if (length(z$vernacularNames) > 0) {
        if (!is.null(names(z$vernacularNames))) {
          .n <- names(z$vernacularNames)
          
          for (i in seq_along(.n)) {
            col_name <- paste0("vernacularNames.", .n[i])
            if (col_name %in% names(.x)) {
              .x[[col_name]] <- z$vernacularNames[[i]]
            }
          }
        } else {
          for (i in seq_along(z$vernacularNames)) {
            if ((6 + i) <= ncol(.x)) {
              .x[[6 + i]] <- z$vernacularNames[[i]]
            }
          }
        }
      }
      
      .x
    }))
  } else {
    data.frame()
  }
}

#--------

.read_camdp <- function(file, path = NULL, tz = "") {
  
  if (!.require("jsonlite")) {
    stop("The jsonlite package is not installed; please install it first.")
  }
  
  if (!.require("data.table")) {
    stop("The data.table package is not installed; please install it first.")
  }
  
  if (is.null(tz) || length(tz) == 0 || is.na(tz) || !nzchar(tz)) {
    tz <- "UTC"
  }
  
  .d <- list()
  
  if (.isZip(file)) {
    if (!is.null(path) && is.character(path)) {
      .path <- file.path(
        path.expand(path),
        gsub(basename(file), pattern = ".zip", replacement = "", ignore.case = TRUE)
      )
    } else {
      .path <- gsub(basename(file), pattern = ".zip", replacement = "", ignore.case = TRUE)
    }
    
    file <- utils::unzip(file, exdir = .path)
    
  } else if (dir.exists(file)) {
    if (all(c("datapackage.json", "deployments.csv", "observations.csv") %in% tolower(dir(file)))) {
      .path <- file
      file <- dir(file, full.names = TRUE)
    } else {
      if (any(c("datapackage.json", "deployments.csv", "observations.csv") %in% tolower(dir(file)))) {
        required_files <- c("datapackage.json", "deployments.csv", "observations.csv", "media.csv")
        .w <- !required_files %in% tolower(dir(file))
        
        stop(paste0(
          "The standard data files (",
          paste(required_files[.w], collapse = ", "),
          ") are not available in the specified folder."
        ))
      } else {
        stop("The specified folder does not have the standard Camtrap-DP files.")
      }
    }
  } else {
    stop("The specified input is not a zip file or a directory.")
  }
  
  .w <- grepl("observations.csv", file, ignore.case = TRUE)
  if (any(.w)) {
    .d$observations <- as.data.frame(data.table::fread(file[.w][1], tz = tz))
  } else {
    stop("observations.csv is not available in the dataset.")
  }
  
  .w <- grepl("deployments.csv", file, ignore.case = TRUE)
  if (any(.w)) {
    .d$deployments <- as.data.frame(data.table::fread(file[.w][1], tz = tz))
  } else {
    stop("deployments.csv is not available in the dataset.")
  }
  
  .w <- grepl("media.csv", file, ignore.case = TRUE)
  if (any(.w)) {
    .d$media <- as.data.frame(data.table::fread(file[.w][1], tz = tz))
  } else {
    stop("media.csv is not available in the dataset.")
  }
  
  .w <- grepl("datapackage.json", file, ignore.case = TRUE)
  if (any(.w)) {
    .js <- jsonlite::read_json(file[.w][1])
  } else {
    stop("datapackage.json is not available in the dataset.")
  }
  
  if ("deploymentStart" %in% names(.d$deployments)) {
    .d$deployments$deploymentStart <- .parse_cam_datetime(.d$deployments$deploymentStart, tz = tz)
  }
  
  if ("deploymentEnd" %in% names(.d$deployments)) {
    .d$deployments$deploymentEnd <- .parse_cam_datetime(.d$deployments$deploymentEnd, tz = tz)
  }
  
  if ("timestamp" %in% names(.d$media)) {
    .d$media$timestamp <- .parse_cam_datetime(.d$media$timestamp, tz = tz)
  }
  
  if ("eventStart" %in% names(.d$observations)) {
    .d$observations$eventStart <- .parse_cam_datetime(.d$observations$eventStart, tz = tz)
  }
  
  if ("eventEnd" %in% names(.d$observations)) {
    .d$observations$eventEnd <- .parse_cam_datetime(.d$observations$eventEnd, tz = tz)
  }
  
  if ("classificationTimestamp" %in% names(.d$observations)) {
    .d$observations$classificationTimestamp <- .parse_cam_datetime(
      .d$observations$classificationTimestamp,
      tz = tz
    )
  }
  
  .d$locations <- unique(
    .d$deployments[, c("locationID", "locationName", "longitude", "latitude")]
  )
  
  .d$deployments <- .d$deployments[
    , -which(colnames(.d$deployments) %in% c("locationName", "longitude", "latitude")),
    drop = FALSE
  ]
  
  .d$deployments$Year <- .getYear(.d$deployments$deploymentStart)
  
  .d$deployments <- .d$deployments |>
    dplyr::mutate(
      deployment_interval = lubridate::interval(.data$deploymentStart, .data$deploymentEnd),
      deployment_interval = lubridate::int_standardize(.data$deployment_interval)
    ) |>
    dplyr::relocate(.data$deployment_interval, .before = .data$deploymentStart)
  
  if (!"observationLevel" %in% names(.d$observations)) {
    .d$observations$observationLevel <- NA_character_
  }
  
  needed_obs_cols <- c(
    "eventID",
    "individualID",
    "individualPositionRadius",
    "individualPositionAngle",
    "eventStart",
    "eventEnd",
    "mediaID"
  )
  
  for (cc in needed_obs_cols) {
    if (!cc %in% names(.d$observations)) {
      .d$observations[[cc]] <- NA
    }
  }
  
  .media.obs <- .d$observations[.d$observations$observationLevel == "media", , drop = FALSE]
  
  if (nrow(.media.obs) > 0) {
    obs_first_radius_angle <- .media.obs |>
      dplyr::filter(
        !is.na(.data$individualPositionRadius),
        !is.na(.data$individualPositionAngle)
      ) |>
      dplyr::group_by(.data$eventID, .data$individualID) |>
      dplyr::slice_min(.data$eventStart, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(
        dplyr::all_of(c(
          "eventID",
          "individualID",
          "individualPositionRadius",
          "individualPositionAngle"
        ))
      ) |>
      dplyr::rename_with(
        ~ paste0("media_", .x),
        dplyr::starts_with("individualPosition")
      )
  } else {
    obs_first_radius_angle <- data.frame(
      eventID = character(),
      individualID = character(),
      media_individualPositionRadius = numeric(),
      media_individualPositionAngle = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  .obs <- .d$observations[.d$observations$observationLevel == "event", , drop = FALSE]
  
  if (nrow(.obs) == 0) {
    .obs <- .d$observations
  }
  
  .obs <- .obs |>
    dplyr::left_join(
      obs_first_radius_angle,
      by = c("eventID", "individualID")
    ) |>
    dplyr::mutate(
      individualPositionAngle = dplyr::if_else(
        is.na(.data$individualPositionAngle),
        .data$media_individualPositionAngle,
        .data$individualPositionAngle
      ),
      individualPositionRadius = dplyr::if_else(
        is.na(.data$individualPositionRadius),
        .data$media_individualPositionRadius,
        .data$individualPositionRadius
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "media_individualPositionAngle",
        "media_individualPositionRadius"
      ))
    )
  
  .d$observations <- .obs
  
  rm(.obs, obs_first_radius_angle)
  
  if ("classificationTimestamp" %in% names(.d$observations)) {
    .d$observations$observation_timestamp <- .d$observations$classificationTimestamp
    .d$observations$classificationTimestamp <- NULL
  } else {
    .d$observations$observation_timestamp <- as.POSIXct(
      rep(NA_real_, nrow(.d$observations)),
      origin = "1970-01-01",
      tz = tz
    )
  }
  
  if ("cameraSetupType" %in% names(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "cameraSetupType"] <- "cameraSetup"
  } else {
    .d$observations$cameraSetup <- NA
  }
  
  if ("individualSpeed" %in% names(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "individualSpeed"] <- "speed"
  }
  
  if ("individualPositionRadius" %in% names(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "individualPositionRadius"] <- "radius"
  }
  
  if ("individualPositionAngle" %in% names(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "individualPositionAngle"] <- "angle"
  }
  
  .w <- which(grepl("^bbox", colnames(.d$observations)))
  if (length(.w) > 0) {
    .d$observations <- .d$observations[, -.w, drop = FALSE]
  }
  
  if (!"taxonID" %in% colnames(.d$observations)) {
    .d$observations$taxonID <- NA_character_
  }
  
  if (!"taxonIDReference" %in% colnames(.d$observations)) {
    .d$observations$taxonIDReference <- NA_character_
  }
  
  if ("classificationProbability" %in% colnames(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "classificationProbability"] <- "classificationConfidence"
  }
  
  if (!"mediaID" %in% names(.d$observations)) {
    .d$observations$mediaID <- NA_character_
  }
  
  .d$observations$mediaID <- ifelse(
    .d$observations$mediaID == "",
    NA,
    .d$observations$mediaID
  )
  
  .event_obs <- .d$observations[
    is.na(.d$observations$mediaID) & !is.na(.d$observations$eventID),
    c("eventID", "deploymentID", "eventStart", "eventEnd"),
    drop = FALSE
  ]
  
  .event_obs$eventStart <- .parse_cam_datetime(.event_obs$eventStart, tz = tz)
  .event_obs$eventEnd <- .parse_cam_datetime(.event_obs$eventEnd, tz = tz)
  
  if ("eventID" %in% names(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "eventID"] <- "sequenceID"
  } else {
    .d$observations$sequenceID <- NA
  }
  
  if ("eventStart" %in% names(.d$observations)) {
    colnames(.d$observations)[colnames(.d$observations) == "eventStart"] <- "timestamp"
  }
  
  if (!"timestamp" %in% names(.d$observations)) {
    .d$observations$timestamp <- as.POSIXct(
      rep(NA_real_, nrow(.d$observations)),
      origin = "1970-01-01",
      tz = tz
    )
  }
  
  .d$observations$timestamp <- .parse_cam_datetime(.d$observations$timestamp, tz = tz)
  
  if (nrow(.event_obs) > 0) {
    by <- dplyr::join_by(
      deploymentID,
      dplyr::between(timestamp, eventStart, eventEnd)
    )
    
    .media <- .d$media |>
      dplyr::full_join(.event_obs, by) |>
      dplyr::rename(sequenceID = "eventID") |>
      dplyr::select(-dplyr::any_of(c("eventStart", "eventEnd"))) |>
      dplyr::relocate(.data$sequenceID, .after = .data$deploymentID)
  } else {
    .media <- .d$media
    if (!"sequenceID" %in% names(.media)) {
      .media$sequenceID <- NA
    }
  }
  
  if ("filePublic" %in% names(.media)) {
    .media$filePublic <- NULL
  }
  
  if ("favorite" %in% names(.media)) {
    colnames(.media)[colnames(.media) == "favorite"] <- "favourite"
  }
  
  if ("mediaComments" %in% names(.media)) {
    colnames(.media)[colnames(.media) == "mediaComments"] <- "comments"
  }
  
  if (!"_id" %in% names(.media)) {
    .media$`_id` <- NA
  }
  
  if (!"captureMethod" %in% names(.media)) {
    .media$captureMethod <- NA
  }
  
  .media <- .media |>
    dplyr::mutate(
      captureMethod = factor(
        ifelse(
          .data$captureMethod == "activityDetection",
          "motionDetection",
          as.character(.data$captureMethod)
        )
      )
    )
  
  .media$timestamp <- .parse_cam_datetime(.media$timestamp, tz = tz)
  
  .d$media <- .media
  rm(.media)
  
  .d$sequences <- .getSequences(.d$media)
  
  .d$taxonomy <- .get_Taxonomic_DF(.js$taxonomic)
  
  if ("order" %in% names(.d$taxonomy)) {
    .d$taxonomy$order[.d$taxonomy$order == ""] <- NA
  }
  
  if (.require("taxize")) {
    .w <- .getMissingTaxon_GBIF(
      .d$taxonomy$scientificName[!is.na(.d$taxonomy$scientificName)]
    )
    
    for (i in seq_len(nrow(.w))) {
      w <- which(.d$taxonomy$scientificName == .w$scientificName[i])
      .d$taxonomy[w, "class"] <- .w$class[i]
      
      if (is.na(.d$taxonomy[w, "order"])) {
        .d$taxonomy[w, "order"] <- .w$order[i]
      }
    }
    
    rm(.w, w)
  }
  
  if ("taxonID" %in% names(.d$observations)) {
    .d$observations$taxonID <- NULL
  }
  
  .d$observations$taxonID <- dplyr::left_join(
    .d$observations,
    .d$taxonomy,
    by = "scientificName"
  )$taxonID
  
  list(
    data = .d,
    json = .js,
    directory = normalizePath(.path, winslash = "/", mustWork = FALSE)
  )
}

#--------

#' Read Camera-Trap Data in Camtrap-DP Format
#'
#' Creates a `camReport` object from a Camtrap-DP dataset.
#'
#' @param data Path to a Camtrap-DP `.zip` file or an unzipped Camtrap-DP folder.
#' @param habitat Optional data frame containing habitat information.
#' @param study_area Optional study-area polygon, either as a file path, `SpatVector` or `sf` object.
#' @param ... Additional arguments.
#'
#' @return A `camReport` object.
#'
#' @export
setGeneric(
  "camData",
  function(data, habitat, study_area, ...)
    standardGeneric("camData")
)

#' @rdname camData
#' @export
setMethod(
  "camData",
  signature(data = "character"),
  function(data, habitat, study_area = NULL, ...) {
    
    .camdata_start_time <- Sys.time()
    .camdata_start_message(data)
    
    if (missing(habitat) || !is.data.frame(habitat)) {
      habitat <- NULL
    }
    
    if (missing(study_area)) {
      study_area <- NULL
    }
    
    .d <- .read_camdp(data)
    
    cm <- camR$new()
    cm$setting$locationLegend <- TRUE
    
    cm$data <- .d$data
    cm$info$json <- .d$json
    cm$info$directory <- .d$directory
    
    if (!is.null(habitat)) {
      cm$habitat <- habitat
    }
    
    if (!is.null(study_area)) {
      study_area_file <- file.path(cm$info$directory, "study_area.map")
      
      if (is.character(study_area)) {
        if (file.exists(study_area)) {
          .v <- try(terra::vect(study_area), silent = TRUE)
          
          if (!inherits(.v, "try-error")) {
            terra::saveRDS(.v, study_area_file)
            cm$study_area$path <- study_area_file
            cm$study_area$object <- .v
            rm(.v)
          } else {
            warning("The specified study_area file could not be read.")
          }
        } else {
          warning("study_area filename is not available and was ignored.")
        }
        
      } else if (inherits(study_area, "SpatVector")) {
        cm$study_area$object <- study_area
        terra::saveRDS(study_area, study_area_file)
        cm$study_area$path <- study_area_file
        
      } else if (inherits(study_area, "sf")) {
        cm$study_area$object <- terra::vect(study_area)
        terra::saveRDS(cm$study_area$object, study_area_file)
        cm$study_area$path <- study_area_file
        
      } else {
        warning("study_area was ignored; it should be a filename or a spatial object.")
      }
    }
    
    cm$filterExclude <- list(
      scientificName = c(
        "Homo sapiens",
        "Canis lupus familiaris",
        "Felis catus",
        "Ovis aries",
        "Bos taurus",
        "Equus caballus",
        "Capra hircus",
        "Sus scrofa domesticus",
        "Equus africanus asinus",
        "Oryctolagus cuniculus",
        "Camelus dromedarius",
        "Camelus bactrianus",
        "Rangifer tarandus domesticus"
      )
    )
    
    cm$filterKeep <- list(
      observationType = c("animal"),
      class = NULL
    )
    
    cm$add_group(
      "large_mammals",
      list(order = c("Artiodactyla", "Carnivora"))
    )
    
    cm$filterCount <- 25
    
    cm$add_group(
      "domestic",
      list(
        scientificName = c(
          "Homo sapiens",
          "Canis lupus familiaris",
          "Felis catus",
          "Ovis aries",
          "Bos taurus",
          "Equus caballus",
          "Capra hircus",
          "Sus scrofa domesticus",
          "Equus africanus asinus",
          "Oryctolagus cuniculus",
          "Camelus dromedarius",
          "Camelus bactrianus",
          "Rangifer tarandus domesticus"
        )
      )
    )
    
    if (is.null(cm$setting$focus_groups)) {
      cm$setting$focus_groups <- "large_mammals"
    }
    
    if (!is.null(.d$json$project$title) && .d$json$project$title != "") {
      cm$siteName <- .pretty_label(.d$json$project$title)
    } else {
      cm$siteName <- "Unnamed Site"
    }
    
    .summarize_spatial(cm)
    .Temporal(cm)
    .Essentials(cm)
    .Annotation(cm)
    .Validation(cm)
    .Species(cm)
    .Visuals_capture_method(cm)
    
    country <- cm$data_status$Spatial$country
    fg <- .pretty_label(.paste_comma_and(.firstUpper(cm$setting$focus_groups)))
    site_Name <- cm$siteName
    
    cm$title <- .pretty_label(as.character(glue::glue(
      "Camera-Trap Report: {fg} at {site_Name}, {country}"
    )))
    
    cm$subtitle <- .pretty_label(
      "Ecological Report based on Camera Trap Data for Wildlife Monitoring"
    )
    
    rm(.d)
    gc()
    
    cm$setup()
    
    .project_info(cm)
    .get_sampling_text(cm)
    
    cm$authors <- .get_authors_text(cm)
    cm$institute <- .get_institute(cm)
    
    cm$description <- paste0(
      "The study was conducted in ", cm$siteName,
      ", located in ", cm$data_status$Spatial$country, ". ",
      "The site is geographically defined by the coordinates ",
      cm$data_status$Spatial$coordinate_range,
      " and covers an area of approximately ",
      round(cm$data_status$Spatial$MCArea, 2), " km2. ",
      cm$reportObjectElements$habitat_text, " ",
      cm$reportObjectElements$message, " ",
      "The site supports a diverse range of wildlife, with approximately ",
      cm$data_status$Species$Keep_sp_n, " species recorded. ",
      "The most frequently observed species include ",
      cm$reportTextElements$most_observed_sp_text, "."
    )
    
    .attach_modules(cm, n = "all")
    .attach_status_modules(cm, n = "all")
    
    .camdata_done_message(.camdata_start_time, cm$siteName)
    
    cm
  }
)

#--------
