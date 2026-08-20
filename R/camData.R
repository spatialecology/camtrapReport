# Functions for reading Camtrap DP datasets and creating camReport objects
# Licence: MIT
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

  if (any(missing_i) && .require("lubridate")) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time2(
        x_chr[missing_i],
        orders = c(
          "Ymd HMS",
          "Ymd HM"
        ),
        tz = tz
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
    stop("The data.table package is not installed...!")
  }

  sequences <- media |>
    dplyr::distinct() |>
    dplyr::select(
      deploymentID,
      sequenceID,
      timestamp,
      captureMethod
    ) |>
    data.table::data.table(key = "sequenceID")

  sequences <- sequences[!is.na(sequences$sequenceID), ]

  sequences <- sequences[
    ,
    list(
      deploymentID = unique(deploymentID),
      captureMethod = unique(captureMethod),
      start = min(timestamp),
      end = max(timestamp),
      nrphotos = length(timestamp)
    ),
    by = sequenceID
  ]

  sequences <- sequences |>
    dplyr::as_tibble() |>
    dplyr::arrange(deploymentID, sequenceID) |>
    dplyr::mutate(
      sequence_interval = lubridate::interval(start, end)
    ) |>
    dplyr::relocate(sequence_interval, .before = start) |>
    dplyr::select(-start, -end)

  as.data.frame(sequences)
}
#--------

.get_Taxonomic_DF <- function(x) {
  w <- vapply(x, function(z) {
    length(names(z$vernacularNames))
  }, integer(1))

  if (all(w == 0)) {
    dplyr::bind_rows(lapply(x, function(z) {
      .x <- strsplit(z$taxonID, "/", fixed = TRUE)[[1]]

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
        n[i] <- names(sort(table(vapply(x, function(z) {
          nm <- names(z$vernacularNames)
          if (length(nm) >= i) nm[i] else NA_character_
        }, character(1))), decreasing = TRUE))[1]
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

      .tmp <- strsplit(z$taxonID, "/", fixed = TRUE)[[1]]
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
        gsub(
          basename(file),
          pattern = ".zip",
          replacement = "",
          ignore.case = TRUE
        )
      )
    } else {
      .path <- gsub(
        basename(file),
        pattern = ".zip",
        replacement = "",
        ignore.case = TRUE
      )
    }

    file <- utils::unzip(file, exdir = .path)

  } else if (dir.exists(file)) {
    if (all(
      c("datapackage.json", "deployments.csv", "observations.csv") %in%
        tolower(dir(file))
    )) {
      .path <- file
      file <- dir(file, full.names = TRUE)
    } else {
      if (any(
        c("datapackage.json", "deployments.csv", "observations.csv") %in%
          tolower(dir(file))
      )) {
        required_files <- c(
          "datapackage.json",
          "deployments.csv",
          "observations.csv",
          "media.csv"
        )
        .w <- !required_files %in% tolower(dir(file))

        stop(
          "The standard data files (",
          toString(required_files[.w]),
          ") are not available in the specified folder."
        )
      } else {
        stop(
          "The specified folder does not have the standard Camtrap DP files."
        )
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
    .d$deployments$deploymentStart <- .parse_cam_datetime(
      .d$deployments$deploymentStart,
      tz = tz
    )
  }

  if ("deploymentEnd" %in% names(.d$deployments)) {
    .d$deployments$deploymentEnd <- .parse_cam_datetime(
      .d$deployments$deploymentEnd,
      tz = tz
    )
  }

  if ("timestamp" %in% names(.d$media)) {
    .d$media$timestamp <- .parse_cam_datetime(.d$media$timestamp, tz = tz)
  }

  if ("eventStart" %in% names(.d$observations)) {
    .d$observations$eventStart <- .parse_cam_datetime(
      .d$observations$eventStart,
      tz = tz
    )
  }

  if ("eventEnd" %in% names(.d$observations)) {
    .d$observations$eventEnd <- .parse_cam_datetime(
      .d$observations$eventEnd,
      tz = tz
    )
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
    , -which(
      colnames(.d$deployments) %in%
        c("locationName", "longitude", "latitude")
    ),
    drop = FALSE
  ]

  .d$deployments$Year <- .getYear(.d$deployments$deploymentStart)

  .d$deployments <- .d$deployments |>
    dplyr::mutate(
      deployment_interval = lubridate::interval(
        deploymentStart,
        deploymentEnd
      ),
      deployment_interval = lubridate::int_standardize(
        deployment_interval
      )
    ) |>
    dplyr::relocate(
      deployment_interval,
      .before = deploymentStart
    )

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

  .media.obs <- .d$observations[
    .d$observations$observationLevel == "media",
    ,
    drop = FALSE
  ]

  if (nrow(.media.obs) > 0) {
    obs_first_radius_angle <- .media.obs |>
      dplyr::filter(
        !is.na(individualPositionRadius),
        !is.na(individualPositionAngle)
      ) |>
      dplyr::group_by(eventID, individualID) |>
      dplyr::slice_min(
        eventStart,
        n = 1,
        with_ties = FALSE
      ) |>
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

  .obs <- .d$observations[
    .d$observations$observationLevel == "event",
    ,
    drop = FALSE
  ]

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
        is.na(individualPositionAngle),
        media_individualPositionAngle,
        individualPositionAngle
      ),
      individualPositionRadius = dplyr::if_else(
        is.na(individualPositionRadius),
        media_individualPositionRadius,
        individualPositionRadius
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
    .d$observations$observation_timestamp <-
      .d$observations$classificationTimestamp
    .d$observations$classificationTimestamp <- NULL
  } else {
    .d$observations$observation_timestamp <- as.POSIXct(
      rep(NA_real_, nrow(.d$observations)),
      origin = "1970-01-01",
      tz = tz
    )
  }

  if ("cameraSetupType" %in% names(.d$observations)) {
    colnames(.d$observations)[
      colnames(.d$observations) == "cameraSetupType"
    ] <- "cameraSetup"
  } else {
    .d$observations$cameraSetup <- NA
  }

  if ("individualSpeed" %in% names(.d$observations)) {
    colnames(.d$observations)[
      colnames(.d$observations) == "individualSpeed"
    ] <- "speed"
  }

  if ("individualPositionRadius" %in% names(.d$observations)) {
    colnames(.d$observations)[
      colnames(.d$observations) == "individualPositionRadius"
    ] <- "radius"
  }

  if ("individualPositionAngle" %in% names(.d$observations)) {
    colnames(.d$observations)[
      colnames(.d$observations) == "individualPositionAngle"
    ] <- "angle"
  }

  .w <- grep("^bbox", colnames(.d$observations))
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
    colnames(.d$observations)[
      colnames(.d$observations) == "classificationProbability"
    ] <- "classificationConfidence"
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
    colnames(.d$observations)[
      colnames(.d$observations) == "eventID"
    ] <- "sequenceID"
  } else {
    .d$observations$sequenceID <- NA
  }

  if ("eventStart" %in% names(.d$observations)) {
    colnames(.d$observations)[
      colnames(.d$observations) == "eventStart"
    ] <- "timestamp"
  }

  if (!"timestamp" %in% names(.d$observations)) {
    .d$observations$timestamp <- as.POSIXct(
      rep(NA_real_, nrow(.d$observations)),
      origin = "1970-01-01",
      tz = tz
    )
  }

  .d$observations$timestamp <- .parse_cam_datetime(
    .d$observations$timestamp,
    tz = tz
  )

  if (nrow(.event_obs) > 0) {
    by <- dplyr::join_by(
      deploymentID,
      dplyr::between(timestamp, eventStart, eventEnd)
    )

    .media <- .d$media |>
      dplyr::full_join(
        .event_obs,
        by
      ) |>
      dplyr::rename(sequenceID = "eventID") |>
      dplyr::select(
        -dplyr::any_of(c("eventStart", "eventEnd"))
      ) |>
      dplyr::relocate(
        sequenceID,
        .after = deploymentID
      )
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
          captureMethod == "activityDetection",
          "motionDetection",
          as.character(captureMethod)
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
.camdata_cache_paths <- function(data) {
  
  data <- path.expand(data)
  
  if (dir.exists(data)) {
    
    dataset_dir <- normalizePath(
      data,
      winslash = "/",
      mustWork = TRUE
    )
    
  } else {
    
    dataset_dir <- file.path(
      normalizePath(
        dirname(data),
        winslash = "/",
        mustWork = TRUE
      ),
      tools::file_path_sans_ext(basename(data))
    )
  }
  
  writable_dir <- if (dir.exists(dataset_dir)) {
    dataset_dir
  } else {
    dirname(dataset_dir)
  }
  
  if (file.access(writable_dir, 2L) == 0L) {
    
    return(list(
      cache_file = file.path(
        dataset_dir,
        "__camReport_Object.rds"
      ),
      study_area_file = file.path(
        dataset_dir,
        "_study_area.map"
      )
    ))
  }
  
  cache_dir <- tools::R_user_dir(
    "camtrapReport",
    which = "cache"
  )
  
  dir.create(
    cache_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  data_key <- normalizePath(
    data,
    winslash = "/",
    mustWork = FALSE
  )
  
  data_key <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    data_key
  )
  
  list(
    cache_file = file.path(
      cache_dir,
      paste0(data_key, "__camReport_Object.rds")
    ),
    study_area_file = file.path(
      cache_dir,
      paste0(data_key, "_study_area.map")
    )
  )
}

#--------
setGeneric(
  "camData",
  function(data, habitat, study_area, update, ...) {
    methods::standardGeneric("camData")
  }
)

#' Read camera-trap data in Camtrap DP format
#'
#' Create a [`camReport`][camReport-classes] object from a Camtrap DP dataset.
#'
#' The function reads the input Camtrap DP dataset and creates a `camReport`
#' object. The resulting object contains processed camera-trap data, metadata,
#' summaries, report text, report modules, and data-status information used to
#' generate automated reports.
#'
#' If habitat information is provided, it is linked to camera locations and
#' used in habitat-related summaries. If a study-area boundary is provided, it
#' is stored with the object and used in spatial summaries where relevant.
#'
#' The `camReport` object is normally saved within the dataset directory as
#' `__camReport_Object.rds`. A later call using the same dataset reuses the
#' saved object unless `update = TRUE`, in which case the object is recreated.
#' If the dataset location is not writable, a package-specific user cache
#' directory is used instead.
#'
#' @param data A character string giving the path to a Camtrap DP dataset,
#'   provided as a ZIP file or an extracted dataset directory.
#' @param habitat An optional data frame containing habitat information for
#'   camera locations. The default is `NULL`.
#' @param study_area An optional study-area boundary provided as a spatial file
#'   path, a `SpatVector`, or an `sf` object. The default is `NULL`.
#' @param update A logical value (default `FALSE`) specifying whether to
#'   recreate a previously saved `camReport` object.
#' @param ... Additional arguments. These are currently reserved for future use.
#'
#' @return A [`camReport`][camReport-classes] object.
#'
#' @seealso [report()], [status()], [info()], [gui()]
#' @family camtrapReport data
#'
#' @usage camData(data, habitat, study_area, update, ...)
#' @rdname camData
#' @aliases camData
#' @keywords spatial species camera-trap
#'

#' @examples
#' example_dataset <- system.file(
#'   "external",
#'   "dataset",
#'   package = "camtrapReport"
#' )
#'
#' cm <- camData(example_dataset)
#' cm
setMethod(
  "camData",
  signature(data = "character"),
  function(data, habitat, study_area = NULL, update = FALSE, ...) {

    if (missing(update)) update <- FALSE
    cache_paths <- .camdata_cache_paths(data)
    
    cache_file <- cache_paths$cache_file
    study_area_file <- cache_paths$study_area_file


    if (!update && file.exists(cache_file)) {
      cm <- readRDS(cache_file)
      return(cm)
    }

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


      if (is.character(study_area)) {
        if (file.exists(study_area)) {
          .v <- try(terra::vect(study_area), silent = TRUE)

          if (inherits(.v, "try-error")) {
            warning("The specified study_area file could not be read.")
          } else {
            terra::saveRDS(.v, study_area_file)
            cm$study_area$path <- study_area_file
            cm$study_area$object <- .v
            rm(.v)
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
        warning(
          "study_area was ignored; it should be a filename or a spatial object."
        )
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
      observationType = "animal",
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
      "Camera-Trap Monitoring Report for {site_Name}, {country}"
    )))

    cm$subtitle <- .pretty_label(
      "Ecological insights from camera-trap data for wildlife monitoring"
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
      cm$data_status$Spatial$MCArea_text, ". ",
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

    saveRDS(cm, cache_file)

    cm
  }
)

#--------
