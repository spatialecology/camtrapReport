# Internal analysis and report-module helper functions for camtrapReport
# Licence: MIT
#--------

# ------------------------------------------------------------------
# Effort = deployment duration
# Returns both effort in requested units and raw seconds
# ------------------------------------------------------------------
.calc_effort <- function(x, by = NULL, unit = c("day", "hour", "minute", "second", "week")) {
  unit <- match.arg(unit)
  
  dep <- x$deployments
  # if (is.null(dep) || nrow(dep) == 0) stop("x$deployments is empty.")
  # 
  # start_col <- .first_existing(dep, c("deploymentStart", "start"))
  # end_col   <- .first_existing(dep, c("deploymentEnd", "end"))
  
  dep <- .eval('dep |>
    dplyr::mutate(
      effort_seconds = as.numeric(difftime(.data[["deploymentEnd"]], .data[["deploymentStart"]], units = "secs"))
    ) |>
    dplyr::filter(!is.na(.data$effort_seconds), .data$effort_seconds >= 0)',environment())
  
  if (!is.null(x$locations) &&
      nrow(x$locations) > 0 &&
      "locationID" %in% names(dep) &&
      "locationID" %in% names(x$locations)) {
    dep <- .eval('dplyr::left_join(dep, x$locations, by = "locationID", multiple = "all")',environment())
  }
  
  divisor <- c(
    second = 1,
    minute = 60,
    hour   = 3600,
    day    = 86400,
    week   = 604800
  )[[unit]]
  
  dep <- .eval('dep |>
    dplyr::mutate(effort = .data$effort_seconds / divisor)',environment())
  
  if (is.null(by)) {
    out <- .eval('dep |>
      dplyr::summarise(
        effort_seconds = sum(.data$effort_seconds, na.rm = TRUE),
        effort = sum(.data$effort, na.rm = TRUE),
        unit = unit,
        .groups = "drop"
      )',environment())
  } else {
    by <- unique(by)
    missing_by <- setdiff(by, names(dep))
    if (length(missing_by) > 0) {
      stop(
        "Grouping columns not found in deployments/locations: ",
        paste(missing_by, collapse = ", ")
      )
    }
    
    out <- .eval('dep |>
      dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
      dplyr::summarise(
        effort_seconds = sum(.data$effort_seconds, na.rm = TRUE),
        effort = sum(.data$effort, na.rm = TRUE),
        unit = unit,
        .groups = "drop"
      )',environment())
  }
  
  out
}


#---------------
# a customised version of the merige_tibbles in the ctdp package (developed by: Henjo de Knegt)
.merge_data <- function(x, dropMedia = TRUE) {
  
  keys <- list(
    locations = "locationID",
    deployments = "deploymentID",
    sequences = "sequenceID",
    observations = "observationID",
    taxonomy = "taxonID",
    media = "mediaID"
  )
  
  linkStructure <- list(
    locations = c("locations"),
    deployments = c("locations", "deployments"),
    sequences = c("deployments", "sequences"),
    observations = c("sequences", "observations", "taxonomy"),
    media = c("sequences", "media"),
    taxonomy = c("taxonomy")
  )
  
  #---------------- helper functions ----------------
  
  .drop_any_of <- function(df, cols) {
    if (is.null(df) || !is.data.frame(df)) {
      return(data.frame())
    }
    
    df[, setdiff(names(df), cols), drop = FALSE]
  }
  
  .move_to_front <- function(df, cols) {
    cols <- cols[cols %in% names(df)]
    df[, c(cols, setdiff(names(df), cols)), drop = FALSE]
  }
  
  .relocate_after <- function(df, col, after) {
    if (!col %in% names(df) || !after %in% names(df)) {
      return(df)
    }
    
    other_cols <- setdiff(names(df), col)
    pos <- match(after, other_cols)
    
    new_order <- append(other_cols, values = col, after = pos)
    df[, new_order, drop = FALSE]
  }
  
  .safe_left_join <- function(lhs, rhs, by) {
    
    if (is.null(lhs) || !is.data.frame(lhs)) {
      stop("Left-hand join input must be a data.frame.")
    }
    
    if (is.null(rhs) || !is.data.frame(rhs)) {
      stop("Right-hand join input must be a data.frame.")
    }
    
    missing_lhs <- setdiff(by, names(lhs))
    missing_rhs <- setdiff(by, names(rhs))
    
    if (length(missing_lhs) > 0) {
      stop("Missing join column(s) in left table: ", paste(missing_lhs, collapse = ", "))
    }
    
    if (length(missing_rhs) > 0) {
      stop("Missing join column(s) in right table: ", paste(missing_rhs, collapse = ", "))
    }
    
    lhs_id <- ".camr_lhs_row_id__"
    rhs_id <- ".camr_rhs_row_id__"
    
    while (lhs_id %in% names(lhs) || lhs_id %in% names(rhs)) {
      lhs_id <- paste0(lhs_id, "_")
    }
    
    while (rhs_id %in% names(lhs) || rhs_id %in% names(rhs)) {
      rhs_id <- paste0(rhs_id, "_")
    }
    
    lhs[[lhs_id]] <- seq_len(nrow(lhs))
    rhs[[rhs_id]] <- seq_len(nrow(rhs))
    
    out <- merge(
      x = lhs,
      y = rhs,
      by = by,
      all.x = TRUE,
      sort = FALSE,
      suffixes = c(".x", ".y")
    )
    
    if (nrow(out) > 0) {
      out <- out[
        order(out[[lhs_id]], out[[rhs_id]], na.last = TRUE),
        ,
        drop = FALSE
      ]
    }
    
    out[[lhs_id]] <- NULL
    out[[rhs_id]] <- NULL
    
    rownames(out) <- NULL
    out
  }
  
  .make_nested_media <- function(media_df) {
    
    if (is.null(media_df) || !is.data.frame(media_df) || nrow(media_df) == 0) {
      out <- data.frame(sequenceID = character(), stringsAsFactors = FALSE)
      out$media <- I(list())
      return(out)
    }
    
    if (!"sequenceID" %in% names(media_df)) {
      stop("Column 'sequenceID' is required in media when dropMedia = FALSE.")
    }
    
    media_df <- .move_to_front(media_df, "sequenceID")
    
    seq_ids <- unique(media_df[["sequenceID"]])
    
    nested <- lapply(seq_ids, function(id) {
      idx <- if (is.na(id)) {
        is.na(media_df[["sequenceID"]])
      } else {
        !is.na(media_df[["sequenceID"]]) & media_df[["sequenceID"]] == id
      }
      
      m <- media_df[idx, setdiff(names(media_df), "sequenceID"), drop = FALSE]
      rownames(m) <- NULL
      m
    })
    
    out <- data.frame(
      sequenceID = seq_ids,
      stringsAsFactors = FALSE
    )
    
    out$media <- I(nested)
    out
  }
  
  #---------------- protect input ----------------
  
  if (
    !is.null(x$observations) &&
    is.data.frame(x$observations) &&
    "scientificName" %in% names(x$observations)
  ) {
    x$observations <- x$observations[
      ,
      setdiff(names(x$observations), "scientificName"),
      drop = FALSE
    ]
  }
  
  #---------------- select columns ----------------
  
  omitKeys <- unlist(keys[!names(keys) %in% linkStructure$media], use.names = FALSE)
  y1 <- .drop_any_of(x$media, as.character(omitKeys))
  
  omitKeys <- unlist(keys[!names(keys) %in% linkStructure$sequences], use.names = FALSE)
  y2 <- .drop_any_of(x$sequences, as.character(omitKeys))
  
  omitKeys <- unlist(keys[!names(keys) %in% linkStructure$observations], use.names = FALSE)
  y3 <- .drop_any_of(x$observations, as.character(omitKeys))
  
  omitKeys <- unlist(keys[!names(keys) %in% linkStructure$deployments], use.names = FALSE)
  y4 <- .drop_any_of(x$deployments, as.character(omitKeys))
  
  omitKeys <- unlist(keys[!names(keys) %in% linkStructure$locations], use.names = FALSE)
  y5 <- .drop_any_of(x$locations, as.character(omitKeys))
  
  omitKeys <- unlist(keys[!names(keys) %in% linkStructure$taxonomy], use.names = FALSE)
  y6 <- .drop_any_of(x$taxonomy, as.character(omitKeys))
  
  #---------------- nested media, only when requested ----------------
  
  if (!dropMedia) {
    omitKeys <- unlist(keys[!names(keys) %in% linkStructure$media], use.names = FALSE)
    
    lc_media <- .drop_any_of(x$media, as.character(omitKeys))
    lc_media <- .move_to_front(lc_media, "sequenceID")
    lc_media <- .make_nested_media(lc_media)
  }
  
  #---------------- merge data ----------------
  
  y <- .safe_left_join(y2, y3, by = keys$sequences)
  y <- .safe_left_join(y, y4, by = keys$deployments)
  y <- .safe_left_join(y, y5, by = keys$locations)
  y <- .safe_left_join(y, y6, by = keys$taxonomy)
  
  key_order <- as.character(unlist(keys[names(keys) != "media"], use.names = FALSE))
  key_order <- key_order[key_order %in% names(y)]
  
  y <- .move_to_front(y, key_order)
  
  if (!dropMedia) {
    y <- .safe_left_join(y, lc_media, by = keys$sequences)
    y <- .relocate_after(y, "media", "nrphotos")
  }
  
  rownames(y) <- NULL
  y
}
#-----------
# # x: list of camera-trap data.frames
# .get_classes <- function(x,count=TRUE) {
#   .x <- table(x$taxonomy$class)
#   if (count) {
#     .x <- sort(.x,decreasing = TRUE)
#     data.frame(class=names(.x),count=as.numeric(.x))
#   } else names(.x)
# }

.get_classes <- function(x, count = TRUE) {
  y <- .merge_data(x, dropMedia = TRUE)
  
  cls <- y$class
  cls <- cls[!is.na(cls) & nzchar(cls)]
  
  if (!count) return(sort(unique(cls)))
  
  tab <- sort(table(cls), decreasing = TRUE)
  dplyr::tibble(class = names(tab), count = as.integer(tab))
}
#------

#-----------
# x: list of camera-trap data.frames
# only_species -> T (exclude families, etc., keeps only species with names at the species level)
# exlude_sp. -> T (exclude names with pattern "genus_name sp.")
.get_species <- function(x,count=TRUE,only_species=TRUE,exclude_sp.=TRUE) {
  .x <- table(x$observations$scientificName)
  .x <- .x[names(.x) != '']
  
  if (only_species) {
    .x <- .x[grepl('\\s',names(.x))]
    
  }
  
  if (exclude_sp.) {
    .x <- .x[!grepl(' sp.$',names(.x))]
  }
  
  if (count) {
    .x <- sort(.x,decreasing = TRUE)
    data.frame(scientificName=names(.x),count=as.numeric(.x))
  } else names(.x)
}
#-----------
# ------------------------------------------------------------------
# Captures / capture rate / RAI
#
# Standard behavior:
# - if eventID is available -> count distinct eventID
# - else if sequenceID is available -> count distinct sequenceID
# - else fall back to distinct observationID
#
# Returns:
# - captures      = number of distinct capture units
# - individuals   = summed max(count) per capture unit (if count exists)
# - capture_rate  = captures / effort in requested unit
# - rai           = 100 * captures / effort_days
# ------------------------------------------------------------------
.captures <- function(x,
                      onlyAnimal = TRUE,
                      class = NULL,
                      species = NULL,
                      by = NULL,
                      capture_unit = c("auto", "event", "sequence", "observation"),
                      effort_unit = c("day", "hour", "minute", "second", "week"),
                      rate_multiplier = 100) {
  
  capture_unit <- match.arg(capture_unit)
  effort_unit  <- match.arg(effort_unit)
  
  y <- .merge_data(x, dropMedia = TRUE)
  
  #---------------- helper functions ----------------
  
  .make_row_key <- function(df, cols) {
    if (length(cols) == 0) {
      return(rep(".__all__.", nrow(df)))
    }
    
    vals <- lapply(cols, function(col) {
      z <- as.character(df[[col]])
      z[is.na(z)] <- "<NA>"
      z
    })
    
    do.call(paste, c(vals, sep = "\r"))
  }
  
  .distinct_rows <- function(df, cols) {
    if (nrow(df) == 0) {
      return(df[0, , drop = FALSE])
    }
    
    df[!duplicated(df[, cols, drop = FALSE]), , drop = FALSE]
  }
  
  .count_by <- function(df, group_cols, out_col = "captures") {
    if (nrow(df) == 0) {
      out <- df[0, group_cols, drop = FALSE]
      out[[out_col]] <- integer(0)
      return(out)
    }
    
    if (length(group_cols) == 0) {
      out <- data.frame(stringsAsFactors = FALSE)
      out[[out_col]] <- nrow(df)
      return(out)
    }
    
    key <- .make_row_key(df, group_cols)
    key_unique <- unique(key)
    grp <- match(key, key_unique)
    
    first_row <- match(key_unique, key)
    counts <- tabulate(grp, nbins = length(key_unique))
    
    out <- df[first_row, group_cols, drop = FALSE]
    out[[out_col]] <- as.integer(counts)
    rownames(out) <- NULL
    
    out
  }
  
  .summarise_numeric_by <- function(df, group_cols, value_col, out_col, fun) {
    if (nrow(df) == 0) {
      out <- df[0, group_cols, drop = FALSE]
      out[[out_col]] <- numeric(0)
      return(out)
    }
    
    if (length(group_cols) == 0) {
      out <- data.frame(stringsAsFactors = FALSE)
      out[[out_col]] <- fun(df[[value_col]])
      return(out)
    }
    
    key <- .make_row_key(df, group_cols)
    key_unique <- unique(key)
    first_row <- match(key_unique, key)
    
    idx <- split(seq_along(key), factor(key, levels = key_unique))
    
    vals <- vapply(
      idx,
      function(ii) fun(df[[value_col]][ii]),
      numeric(1)
    )
    
    out <- df[first_row, group_cols, drop = FALSE]
    out[[out_col]] <- as.numeric(vals)
    rownames(out) <- NULL
    
    out
  }
  
  .left_join_one <- function(x, y, by) {
    if (is.null(y) || !is.data.frame(y)) {
      return(x)
    }
    
    add_cols <- setdiff(names(y), by)
    
    if (length(add_cols) == 0) {
      return(x)
    }
    
    if (length(by) == 0) {
      for (cc in add_cols) {
        if (nrow(x) == 0) {
          x[[cc]] <- y[[cc]][0]
        } else if (nrow(y) == 0) {
          x[[cc]] <- NA
        } else {
          x[[cc]] <- rep(y[[cc]][1], nrow(x))
        }
      }
      return(x)
    }
    
    x_key <- .make_row_key(x, by)
    y_key <- .make_row_key(y, by)
    matched <- match(x_key, y_key)
    
    for (cc in add_cols) {
      x[[cc]] <- y[[cc]][matched]
    }
    
    x
  }
  
  #---------------- filters ----------------
  
  if (onlyAnimal && "observationType" %in% names(y)) {
    keep <- !is.na(y[["observationType"]]) & y[["observationType"]] == "animal"
    y <- y[keep, , drop = FALSE]
  }
  
  if (!is.null(class) && "class" %in% names(y)) {
    class <- intersect(class, unique(stats::na.omit(y[["class"]])))
    
    if (length(class) > 0) {
      y <- y[y[["class"]] %in% class, , drop = FALSE]
    }
  }
  
  if (!is.null(species) && "scientificName" %in% names(y)) {
    species <- intersect(species, unique(stats::na.omit(y[["scientificName"]])))
    
    if (length(species) > 0) {
      y <- y[y[["scientificName"]] %in% species, , drop = FALSE]
    }
  }
  
  #---------------- choose capture unit ----------------
  
  if (capture_unit == "auto") {
    if ("eventID" %in% names(y) && any(!is.na(y[["eventID"]]))) {
      capture_unit <- "event"
    } else if ("sequenceID" %in% names(y) && any(!is.na(y[["sequenceID"]]))) {
      capture_unit <- "sequence"
    } else {
      capture_unit <- "observation"
    }
  }
  
  id_col <- switch(
    capture_unit,
    event = "eventID",
    sequence = "sequenceID",
    observation = "observationID"
  )
  
  if (!id_col %in% names(y)) {
    stop("Capture unit '", capture_unit, "' is not available in the merged data.")
  }
  
  tax_cols <- intersect(
    c(
      "taxonID",
      "scientificName",
      "vernacularNames.eng",
      "vernacularNames",
      "class",
      "order"
    ),
    names(y)
  )
  
  group_cols <- unique(c(by, tax_cols))
  
  missing_group_cols <- setdiff(group_cols, names(y))
  if (length(missing_group_cols) > 0) {
    stop(
      "Grouping column(s) not found in merged data: ",
      paste(missing_group_cols, collapse = ", ")
    )
  }
  
  #---------------- count distinct capture units ----------------
  
  y_valid <- y[!is.na(y[[id_col]]), , drop = FALSE]
  
  distinct_cols <- unique(c(group_cols, id_col))
  cap_distinct <- .distinct_rows(y_valid, distinct_cols)
  
  cap <- .count_by(
    cap_distinct,
    group_cols = group_cols,
    out_col = "captures"
  )
  
  #---------------- optional individuals ----------------
  
  if ("count" %in% names(y)) {
    
    ind0 <- y_valid
    
    ind0[[".n"]] <- as.numeric(ind0[["count"]])
    ind0[[".n"]][is.na(ind0[[".n"]])] <- 1
    
    # max(count) within each capture unit
    ind_by_capture <- .summarise_numeric_by(
      ind0,
      group_cols = unique(c(group_cols, id_col)),
      value_col = ".n",
      out_col = "individuals",
      fun = function(z) max(z, na.rm = TRUE)
    )
    
    # sum individuals across capture units
    ind <- .summarise_numeric_by(
      ind_by_capture,
      group_cols = group_cols,
      value_col = "individuals",
      out_col = "individuals",
      fun = function(z) sum(z, na.rm = TRUE)
    )
    
    cap <- .left_join_one(cap, ind, by = group_cols)
  }
  
  #---------------- effort ----------------
  
  effort_tbl <- .calc_effort(x, by = by, unit = effort_unit)
  
  effort_day <- .calc_effort(x, by = by, unit = "day")
  names(effort_day)[names(effort_day) == "effort"] <- "effort_days"
  
  if (is.null(by)) {
    
    effort_value <- if ("effort" %in% names(effort_tbl) && nrow(effort_tbl) > 0) {
      effort_tbl[["effort"]][1]
    } else {
      NA_real_
    }
    
    effort_day_value <- if ("effort_days" %in% names(effort_day) && nrow(effort_day) > 0) {
      effort_day[["effort_days"]][1]
    } else {
      NA_real_
    }
    
    cap[["effort"]] <- rep(effort_value, nrow(cap))
    cap[["effort_days"]] <- rep(effort_day_value, nrow(cap))
    
  } else {
    
    effort_tbl <- effort_tbl[, c(by, "effort"), drop = FALSE]
    effort_day <- effort_day[, c(by, "effort_days"), drop = FALSE]
    
    cap <- .left_join_one(cap, effort_tbl, by = by)
    cap <- .left_join_one(cap, effort_day, by = by)
  }
  
  #---------------- rates ----------------
  
  cap[["capture_rate"]] <- cap[["captures"]] / cap[["effort"]]
  cap[["rai"]] <- rate_multiplier * cap[["captures"]] / cap[["effort_days"]]
  cap[["capture_unit"]] <- rep(capture_unit, nrow(cap))
  cap[["effort_unit"]] <- rep(effort_unit, nrow(cap))
  
  if ("individuals" %in% names(cap)) {
    cap[["individual_rate"]] <- cap[["individuals"]] / cap[["effort"]]
    cap[["individual_rai"]] <- rate_multiplier * cap[["individuals"]] / cap[["effort_days"]]
  }
  
  rownames(cap) <- NULL
  
  cap
}


#---------
# adjusted from the package camtrapDensity:
# copied (and adjusted) from the fit_detmodel function in the camtrapDensity package:
.fit_detmodel <- function (formula, dat, species = NULL, newdata = NULL, unit = c("m", "km", "cm", "degree", "radian"), ...) {
  unit <- match.arg(unit)
  allvars <- all.vars(formula)
  depvar <- allvars[1]
  covars <- tail(allvars, -1)
  
  if (is.null(species)) stop('species is not specified!')
  else if (!species %in% dat$scientificName) stop('species is not available in the dataset!')
  
  #dat <- package$data$observations
  if (!all(allvars %in% names(dat))) stop("Can't find all model variables in data")
  
  #if ("useDeployment" %in% names(dat)) dat <- subset(dat, useDeployment)
  if ("useDeployment" %in% names(dat)) {
    keep <- as.logical(dat[["useDeployment"]])
    keep[is.na(keep)] <- FALSE
    dat <- dat[keep, , drop = FALSE]
  }
  
  dat <- dat[dat$scientificName %in% species,allvars,drop=FALSE]
  dat <- as.data.frame(na.omit(dat))
  
  if (nrow(dat) == 0) stop("There are no usable position data")
  
  if (inherits(dat[[depvar]],"numeric")) {
    colnames(dat)[which(colnames(dat) == depvar)] <- 'distance'
    dat$distance <- abs(dat$distance)
  } else {
    cats <- strsplit(as.character(dat[,depvar]),"-")
    dat$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    dat$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    dat$distance <- (dat$distbegin + dat$distend)/2
  }
  type <- if (unit %in% c("m", "km", "cm")) "point" else "line"
  args <- c(list(data = dat, formula = formula[-2], transect = type), list(...))
  
  mod <- suppressWarnings(suppressMessages(.eval('do.call(Distance::ds,args)$ddf',environment())))
  
  if (length(covars) == 0) newdata <- data.frame(x = 0)
  else {
    if (is.null(newdata)) {
      newdata <- dat |> 
        dplyr::select(dplyr::all_of(covars)) |> 
        lapply(function(x) if (is.numeric(x))
          mean(x, na.rm = T)
          else sort(unique(x))) |> 
        expand.grid()
    } else {
      if (!all(covars %in% names(newdata))) 
        stop("Can't find all model covariates in newdata")
    }
  }
  prdn <- predict(mod, newdata, esw = TRUE, se.fit = TRUE)
  if (mod$meta.data$point) {
    prdn$se.fit <- 0.5 * prdn$se.fit/(pi * prdn$fitted)^0.5
    prdn$fitted <- sqrt(prdn$fitted/pi)
  }
  ed <- cbind(estimate = prdn$fitted, se = prdn$se.fit)
  if (length(covars) >= 1) ed <- cbind(newdata, ed)
  mod$edd <- ed
  mod$unit <- unit
  mod$proportion_used <- nrow(mod$data)/nrow(dat)
  mod
}

#-----------------
# adjusted from the package camtrapDensity:
.fit_speedmodel <- function(dat,
                            species = NULL,
                            newdata = NULL,
                            formula = speed ~ 1,
                            reps = 1000,
                            distUnit = c("m", "km", "cm"),
                            timeUnit = c("second", "minute", "hour", "day"),
                            ...) {
  
  distUnit <- match.arg(distUnit)
  timeUnit <- match.arg(timeUnit)
  
  required_cols <- c("scientificName", "speed")
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) for speed model: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  dat <- dat[
    dat$scientificName %in% species &
      !is.na(dat$speed) &
      dat$speed > 0.01 &
      dat$speed < 10,
    "speed",
    drop = FALSE
  ]
  
  dat <- as.data.frame(stats::na.omit(dat))
  
  if (nrow(dat) == 0) {
    stop("There are no usable speed data for ", species)
  }
  
  args <- c(
    list(formula = formula, data = dat),
    list(...)
  )
  
  res <- do.call(sbd::sbm, args)
  
  res$unit <- paste(distUnit, timeUnit, sep = "/")
  
  # Important: .get_parameter_table() expects this
  if (is.null(res$data)) {
    res$data <- dat
  }
  
  res
}
#-------------
# adjusted from the package camtrapDensity:
.fit_actmodel <- function(dat,
                          species = NULL,
                          reps = 999,
                          obsdef = c("individual", "sequence")) {
  
  obsdef <- match.arg(obsdef)
  
  required_cols <- c(
    "scientificName",
    "deploymentID",
    "sequenceID",
    "timestamp",
    "latitude",
    "longitude"
  )
  
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s) for activity model: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  obs <- dat[
    !is.na(dat$scientificName) &
      dat$scientificName %in% species,
    intersect(
      c("deploymentID", "sequenceID", "timestamp", "latitude", "longitude", "count"),
      names(dat)
    ),
    drop = FALSE
  ]
  
  if (nrow(obs) < 2) {
    stop("There are fewer than two observations for activity modelling.")
  }
  
  obs$timestamp <- as.POSIXct(obs$timestamp, tz = "UTC")
  obs$latitude  <- suppressWarnings(as.numeric(obs$latitude))
  obs$longitude <- suppressWarnings(as.numeric(obs$longitude))
  
  obs <- obs[
    !is.na(obs$timestamp) &
      !is.na(obs$latitude) &
      !is.na(obs$longitude),
    ,
    drop = FALSE
  ]
  
  if (nrow(obs) < 2) {
    stop("There are fewer than two valid timestamp/location records for activity modelling.")
  }
  
  if (!"count" %in% names(obs)) {
    obs$count <- 1L
  }
  
  obs$count <- suppressWarnings(as.numeric(obs$count))
  obs$count[is.na(obs$count) | obs$count < 1] <- 1
  obs$count <- as.integer(round(obs$count))
  
  if (obsdef == "individual") {
    i <- rep(seq_len(nrow(obs)), obs$count)
  } else {
    i <- !duplicated(obs$sequenceID)
  }
  
  obs <- obs[i, , drop = FALSE]
  
  if (nrow(obs) < 2) {
    stop("There are fewer than two usable activity observations after applying obsdef.")
  }
  
  suntimes <- activity::get_suntimes(
    obs$timestamp,
    obs$latitude,
    obs$longitude,
    0
  )
  
  timeshift <- pi - mean(suntimes[, 1] + suntimes[, 3] / 2, na.rm = TRUE) * pi / 12
  
  obs$solartime <- activity::wrap(
    activity::solartime(
      obs$timestamp,
      obs$latitude,
      obs$longitude,
      0
    )$solar + timeshift
  )
  
  activity::fitact(
    obs$solartime,
    adj = 1.5,
    sample = "data",
    reps = reps
  )
}
#---------
# adjusted from the package camtrapDensity:
.get_traprate_data <- function(dat,
                               species = NULL,
                               unit = c("day", "hour", "minute", "second")) {
  
  unit <- match.arg(unit)
  
  #---------------- helper: base left join ----------------
  
  .base_left_join <- function(x, y, by) {
    if (is.null(x) || !is.data.frame(x)) {
      stop("'x' must be a data.frame.")
    }
    if (is.null(y) || !is.data.frame(y)) {
      stop("'y' must be a data.frame.")
    }
    
    missing_x <- setdiff(by, names(x))
    missing_y <- setdiff(by, names(y))
    
    if (length(missing_x) > 0) {
      stop("Missing join column(s) in x: ", paste(missing_x, collapse = ", "))
    }
    if (length(missing_y) > 0) {
      stop("Missing join column(s) in y: ", paste(missing_y, collapse = ", "))
    }
    
    row_id <- ".camr_row_id__"
    while (row_id %in% names(x) || row_id %in% names(y)) {
      row_id <- paste0(row_id, "_")
    }
    
    x[[row_id]] <- seq_len(nrow(x))
    
    out <- merge(
      x = x,
      y = y,
      by = by,
      all.x = TRUE,
      sort = FALSE
    )
    
    out <- out[order(out[[row_id]]), , drop = FALSE]
    out[[row_id]] <- NULL
    rownames(out) <- NULL
    
    out
  }
  
  #---------------- empty output ----------------
  
  empty_out <- function() {
    data.frame(
      locationName = character(),
      latitude = numeric(),
      longitude = numeric(),
      n = integer(),
      effort = numeric(),
      effort_unit = character(),
      scientificName = character(),
      stringsAsFactors = FALSE
    )
  }
  
  #---------------- checks ----------------
  
  if (is.null(dat$deployments) || !is.data.frame(dat$deployments)) {
    stop("dat$deployments must be a data.frame.")
  }
  
  if (is.null(dat$locations) || !is.data.frame(dat$locations)) {
    stop("dat$locations must be a data.frame.")
  }
  
  if (is.null(dat$observations) || !is.data.frame(dat$observations)) {
    return(empty_out())
  }
  
  required_obs <- c("deploymentID", "scientificName")
  missing_obs <- setdiff(required_obs, names(dat$observations))
  
  if (length(missing_obs) > 0) {
    stop(
      "Missing required column(s) in dat$observations: ",
      paste(missing_obs, collapse = ", ")
    )
  }
  
  #---------------- deployments + locations ----------------
  
  dep <- .base_left_join(
    dat$deployments,
    dat$locations,
    by = "locationID"
  )
  
  #---------------- effort ----------------
  
  eff <- .calc_effort(dat, by = "deploymentID", unit = unit)
  
  if (!"deploymentID" %in% names(eff) || !"effort" %in% names(eff)) {
    stop(".calc_effort() must return deploymentID and effort columns.")
  }
  
  eff[["effort"]] <- as.numeric(eff[["effort"]])
  
  #---------------- count observations per deployment/species ----------------
  
  obs <- dat$observations[, c("deploymentID", "scientificName"), drop = FALSE]
  obs[["scientificName"]] <- as.character(obs[["scientificName"]])
  
  if (!is.null(species)) {
    obs <- obs[
      !is.na(obs[["scientificName"]]) &
        obs[["scientificName"]] %in% species,
      ,
      drop = FALSE
    ]
  } else {
    obs <- obs[
      !is.na(obs[["scientificName"]]) &
        obs[["scientificName"]] != "" &
        grepl("\\s", obs[["scientificName"]]),
      ,
      drop = FALSE
    ]
  }
  
  if (nrow(obs) == 0) {
    return(empty_out())
  }
  
  obs[["n"]] <- 1L
  
  a <- stats::aggregate(
    x = list(n = obs[["n"]]),
    by = list(
      deploymentID = obs[["deploymentID"]],
      scientificName = obs[["scientificName"]]
    ),
    FUN = sum
  )
  
  #---------------- joins ----------------
  
  res <- .base_left_join(a, dep, by = "deploymentID")
  res <- .base_left_join(res, eff, by = "deploymentID")
  
  required_res <- c("locationName", "latitude", "longitude", "n", "effort")
  missing_res <- setdiff(required_res, names(res))
  
  if (length(missing_res) > 0) {
    stop(
      "Missing required column(s) after joins: ",
      paste(missing_res, collapse = ", ")
    )
  }
  
  #---------------- summarise by locationName ----------------
  
  locs <- unique(res[["locationName"]])
  
  out <- lapply(locs, function(loc) {
    
    idx <- if (is.na(loc)) {
      is.na(res[["locationName"]])
    } else {
      !is.na(res[["locationName"]]) & res[["locationName"]] == loc
    }
    
    data.frame(
      locationName = loc,
      latitude = mean(res[["latitude"]][idx], na.rm = TRUE),
      longitude = mean(res[["longitude"]][idx], na.rm = TRUE),
      n = sum(res[["n"]][idx], na.rm = TRUE),
      effort = sum(res[["effort"]][idx], na.rm = TRUE),
      effort_unit = unit,
      scientificName = paste(species, collapse = "|"),
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  
  out
}
#-------
# adjusted from the package camtrapDensity:
.get_parameter_table <- function(traprate_data,
                                 radius_model,
                                 angle_model,
                                 speed_model,
                                 activity_model,
                                 reps = 999) {
  
  extract_est_se <- function(x, object_name = "model") {
    
    if (is.data.frame(x) || is.matrix(x)) {
      x <- as.data.frame(x)
      
      est_col <- intersect(c("estimate", "est", "Estimate", "mean"), names(x))
      se_col  <- intersect(c("se", "SE", "std.error", "std_error"), names(x))
      
      if (length(est_col) == 0 || length(se_col) == 0) {
        stop("Could not find estimate/se columns in ", object_name)
      }
      
      return(
        data.frame(
          estimate = as.numeric(x[[est_col[1]]][1]),
          se = as.numeric(x[[se_col[1]]][1])
        )
      )
    }
    
    if (is.numeric(x)) {
      if (!is.null(names(x))) {
        est_name <- intersect(c("estimate", "est", "Estimate", "mean"), names(x))
        se_name  <- intersect(c("se", "SE", "std.error", "std_error"), names(x))
        
        if (length(est_name) > 0 && length(se_name) > 0) {
          return(
            data.frame(
              estimate = as.numeric(x[[est_name[1]]]),
              se = as.numeric(x[[se_name[1]]])
            )
          )
        }
      }
      
      if (length(x) >= 2) {
        return(
          data.frame(
            estimate = as.numeric(x[1]),
            se = as.numeric(x[2])
          )
        )
      }
    }
    
    stop("Could not extract estimate and se from ", object_name)
  }
  
  rad <- extract_est_se(radius_model$edd, "radius_model$edd")
  
  ang <- extract_est_se(angle_model$edd, "angle_model$edd")
  ang$estimate <- ang$estimate * 2
  ang$se <- ang$se * 2
  
  if (is.null(speed_model$estimate)) {
    stop("speed_model$estimate is missing")
  }
  
  spd <- extract_est_se(speed_model$estimate, "speed_model$estimate")
  
  if (is.null(activity_model)) {
    stop("activity_model is NULL")
  }
  
  if (!methods::is(activity_model, "actmod")) {
    stop("activity_model is not an activity::fitact model")
  }
  
  act_raw <- activity_model@act[1:2]
  
  act <- data.frame(
    estimate = as.numeric(act_raw[1]),
    se = as.numeric(act_raw[2])
  )
  
  res <- rbind(rad, ang, spd, act)
  rownames(res) <- c("radius", "angle", "active_speed", "activity_level")
  
  overall_speed <- res["active_speed", "estimate"] * res["activity_level", "estimate"]
  
  overall_speed_se <- overall_speed *
    sqrt(
      sum(
        (
          res[c("active_speed", "activity_level"), "se"] /
            res[c("active_speed", "activity_level"), "estimate"]
        )^2,
        na.rm = TRUE
      )
    )
  
  res <- rbind(
    res,
    overall_speed = c(
      estimate = overall_speed,
      se = overall_speed_se
    )
  )
  
  res$cv <- res$se / res$estimate
  res$lcl95 <- res$estimate - 1.96 * res$se
  res$ucl95 <- res$estimate + 1.96 * res$se
  
  n_radius <- if (!is.null(radius_model$data)) nrow(radius_model$data) else NA_integer_
  n_angle  <- if (!is.null(angle_model$data)) nrow(angle_model$data) else NA_integer_
  n_speed  <- if (!is.null(speed_model$data)) nrow(speed_model$data) else NA_integer_
  n_act    <- if (!is.null(activity_model@data)) length(activity_model@data) else NA_integer_
  
  res$n <- c(n_radius, n_angle, n_speed, n_act, NA_integer_)
  
  res$unit <- c(
    radius_model$unit,
    angle_model$unit,
    speed_model$unit,
    "none",
    speed_model$unit
  )
  
  if (is.null(traprate_data) || !is.data.frame(traprate_data) || nrow(traprate_data) == 0) {
    stop("traprate_data is empty")
  }
  
  traprate <- camtrapDensity::get_trap_rate(
    traprate_data,
    strata = NULL,
    reps = reps
  )
  
  if (is.null(rownames(traprate)) || !"trap_rate" %in% rownames(traprate)) {
    rownames(traprate)[1] <- "trap_rate"
  }
  
  proportion_used <- radius_model$proportion_used
  if (is.null(proportion_used) || is.na(proportion_used)) {
    proportion_used <- 1
  }
  
  j <- intersect(c("estimate", "se", "lcl95", "ucl95"), colnames(traprate))
  traprate[, j] <- traprate[, j, drop = FALSE] * proportion_used
  
  out <- rbind(res, traprate)
  
  camtrapDensity::convert_units(out)
}
#-------------

# checks whether required data are available for REM analysis:
.any_data_for_rem <- function(dat, sp = NULL, min_rows = 1) {
  
  tab <- dat$observations
  
  if (is.null(tab) || !is.data.frame(tab) || nrow(tab) == 0) {
    if (!is.null(sp)) return(stats::setNames(rep(FALSE, length(sp)), sp))
    return(NA)
  }
  
  required_cols <- c("scientificName", "speed", "radius", "angle")
  missing_cols <- setdiff(required_cols, names(tab))
  
  if (length(missing_cols) > 0) {
    if (!is.null(sp)) return(stats::setNames(rep(FALSE, length(sp)), sp))
    
    if ("scientificName" %in% names(tab)) {
      spp <- unique(tab$scientificName)
      spp <- spp[!is.na(spp) & nzchar(spp) & grepl("\\s", spp)]
      return(stats::setNames(rep(FALSE, length(spp)), spp))
    }
    
    return(NA)
  }
  
  if (!is.null(sp)) {
    tab <- tab[tab$scientificName %in% sp, , drop = FALSE]
  }
  
  tab <- tab[
    !is.na(tab$scientificName) &
      tab$scientificName != "" &
      grepl("\\s", tab$scientificName),
    ,
    drop = FALSE
  ]
  
  if (nrow(tab) == 0) {
    if (!is.null(sp)) return(stats::setNames(rep(FALSE, length(sp)), sp))
    return(NA)
  }
  
  spp <- unique(tab$scientificName)
  out <- stats::setNames(rep(FALSE, length(spp)), spp)
  
  for (s in spp) {
    
    x <- tab[tab$scientificName == s, , drop = FALSE]
    
    valid_rem_rows <- !is.na(x$speed) &
      x$speed > 0.01 &
      x$speed < 10 &
      !is.na(x$radius) &
      x$radius > 0 &
      !is.na(x$angle)
    
    out[s] <- sum(valid_rem_rows, na.rm = TRUE) >= min_rows
  }
  
  out
}
#-------
.rem <- function (parameters) {
  required_rows <- c("trap_rate", "overall_speed", "radius", "angle")
  required_cols <- c("estimate", "se", "unit")
  if (!all(required_rows %in% rownames(parameters)) | !all(required_cols %in% colnames(parameters))) 
    stop(paste("parameters must have (at least) row names:", 
               paste(required_rows, collapse = ", "), ";\nand (at least) column names:", 
               paste(required_cols, collapse = ", ")))
  param <- .eval("camtrapDensity::convert_units(parameters[required_rows, ])",environment())
  wtd_est <- param$estimate + c(0, 0, 0, 2)
  pwr_est <- wtd_est^c(1, -1, -1, -1)
  CVs <- param$se/wtd_est
  density <- pi * prod(pwr_est)
  cv <- sqrt(sum(CVs^2))
  se <- density * cv
  ci <- unname(.eval("camtrapDensity::lnorm_confint(density, se)",environment()))
  parameters["density", "estimate"] <- density
  parameters["density", "se"] <- se
  if ("cv" %in% names(parameters)) parameters["density", "cv"] <- cv
  if ("lcl95" %in% names(parameters)) parameters["density", "lcl95"] <- ci[1]
  if ("ucl95" %in% names(parameters)) parameters["density", "ucl95"] <- ci[2]
  parameters["density", "unit"] <- "n/km2"
  parameters
}
############


#########################################
#    functions to read report modules:  #
#########################################


.read_section_module <- function(path) {
  m <- rmarkdown::yaml_front_matter(path)  # returns a list
  # yaml_front_matter() reads YAML from Rmd/MD files.
  
  # basic validation
  if (is.null(m$name)) {
    stop("Module must define at least `name:` in YAML: ", path)
  }
  
  if (is.null(m$title)) {
    m$title <- m$name
  }
  
  if (!is.null(m$parent) && identical(tolower(as.character(m$parent)), "null")) m$parent <- NULL
  
  m
}
#------


# the following, reads the setting lines (setting, name, packages, etc.) in the code
# the setting lines in the template can be started with #|
#   #| setting: echo=FALSE, results='asis'
#   #| packages: echo=FALSE, results='asis'


.parse_setting_lines <- function(code_text,key=NULL) {
  
  if (is.null(key)) stop('key is not provided...!')
  if (!is.character(key)) stop('key should be character...!')
  #-----------------
  out <- list()
  for (k in key) {
    lines <- strsplit(code_text %||% "", "\n", fixed = TRUE)[[1]]
    opt <- grep(paste0("^\\s*#\\|\\s*",k,"\\s*:"), lines, value = TRUE)
    
    if (length(opt)) {
      rhs <- sub(paste0("^\\s*#\\|\\s*",k,"\\s*:\\s*"), "", opt[1])
      parts <- trimws(strsplit(rhs, ",", fixed = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts)) out[[k]] <- parts
      # remove that option line from code
      lines <- lines[!grepl(paste0("^\\s*#\\|\\s*",k,"\\s*:"), lines)]
    }
    code_text <- paste(lines, collapse = "\n")
  }
  
  list(setting = out, code = code_text)
}

#-------

.read_yml <- function(x) {
  f <- .read_section_module(x)
  code <- NULL
  
  f$title <- .trim(f$title)
  
  if (substr(f$title,1,1) == "#") {
    .h <- 0
    .w <- strsplit(f$title ,"")[[1]]
    for (i in 1:4) {
      if (.w[i] == '#') {
        .h <- .h + 1
      } else {
        .w <- .w[-c(1:.h)]
        .w <- .trim(paste(.w,collapse = ''))
        break
      }
    }
    #------
    if (length(.w) > 1) {
      .w <- .w[-c(1:.h)]
      .w <- .trim(paste(.w,collapse = ''))
      .h <- 3
    }
    #---
    f$title <- .w
  } else .h <- 1
  #-----
  .txt <- .getTextObj(name=f$name,title = f$title,parent = f$parent,headLevel = .h,txt = f$text)
  
  if (length(which(grepl('code', names(f)))) == 1 && is.null(f[[which(grepl('code', names(f)))]])) {
    return(.txt)
  } else if (length(which(grepl('code', names(f)))) > 0) {
    codeList <- list()
    .w <- which(grepl('code', names(f)))
    for (i in .w) {
      
      code <- .parse_setting_lines(f[[i]],key=c('name','packages','setting'))
      #---
      if (!is.null(code)) {
        if (is.null(code$setting$name)) code$setting$name <- paste0(f$name,'__code')
        else code$setting$name <- paste0(f$name,'__',code$setting$name)
        #---
        if (!is.null(code$setting$setting)) code$setting$setting <- paste(code$setting$setting,collapse = ', ')
        else code$setting$setting <- NULL
        
        codeList[[code$setting$name]] <- new('.Rchunk',parent = f$name,name = code$setting$name,setting = code$setting$setting,packages=code$setting$packages,code=code$code)
      }
    }
    #---
    if (length(codeList) > 0) {
      if (length(codeList) > 1) .txt@Rchunk <- codeList
      else .txt@Rchunk <- codeList[[1]]
    }
  }
  
  .txt
  
}
#----------
.effort_table <- function(x, startend = FALSE) {
  
  x_start <- x$data$deployments$deploymentStart
  x_end <- x$data$deployments$deploymentEnd
  
  dt <- as.numeric(max(x_end)) - as.numeric(min(x_start))
  
  effort <- data.frame(
    time = c(
      min(x_start) - 0.025 * dt,
      x_start,
      x_end,
      max(x_end) + 0.025 * dt
    ),
    add = c(
      0L,
      rep(1L, length(x_start)),
      rep(-1L, length(x_end)),
      0L
    ),
    stringsAsFactors = FALSE
  )
  
  # Equivalent of:
  # group_by(time) |> summarize(add = sum(add)) |> ungroup()
  effort <- stats::aggregate(
    x = list(add = effort[["add"]]),
    by = list(time = effort[["time"]]),
    FUN = sum
  )
  
  # Equivalent of arrange(time)
  effort <- effort[order(effort[["time"]]), , drop = FALSE]
  
  # Equivalent of mutate(nrCams = cumsum(add)) |> select(-add)
  effort[["nrCams"]] <- cumsum(effort[["add"]])
  effort[["add"]] <- NULL
  
  rownames(effort) <- NULL
  
  if (startend) {
    n <- nrow(effort)
    
    effort <- data.frame(
      time = c(
        effort[["time"]][1],
        rep(effort[["time"]][-1], each = 2),
        effort[["time"]][n]
      ),
      nrCams = rep(effort[["nrCams"]], each = 2),
      stringsAsFactors = FALSE
    )
    
    rownames(effort) <- NULL
  }
  
  effort
}

.plot_effort <- function (x, dynamic = TRUE, main = "Effort", xlab = "time", 
                          ylab = "nr of active cams", ...) {
  z <- .effort_table(x, startend = TRUE)
  
  z <- na.omit(z)
  if (dynamic) {
    series <- .eval('xts(z$nrCams, order.by = z$time, tz = "GMT")',env=environment())
    .eval("dygraph(series, main = main, xlab = xlab, ylab = ylab, ...) |> 
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) |> 
      dyRangeSelector()",env=environment())
  } else {
    plot(z$time, z$nrCams, type = "n", main = main, xlab = xlab, 
         ylab = ylab, ...)
    xx <- c(z$time, rev(z$time))
    yy <- c(z$nrCams, rep(0, length(z$nrCams)))
    polygon(xx, yy, border = NA, col = 8)
    lines(z$time, z$nrCams, type = "s")
  }
}
#--------
.left_join <- function(d1,d2,by) {
  if (length(by) == 1) {
    if(!by %in% colnames(d1) & by %in% colnames(d2)) stop('the "by" column does not exist in both data!')
    merge(d1,d2,by=by,all.x=TRUE)
  } else if (length(by) == 2) {
    if(!by[1] %in% colnames(d1) & by[2] %in% colnames(d2)) stop('the "by" columns do not exist in the data!')
    merge(d1,d2,by.x=by[1],by.y=by[2],all.x=TRUE)
  }
}


#----------

.pivot_wider <- function(data,
                         id_cols = NULL,
                         names_from,
                         values_from,
                         fill = 0,
                         agg_fun = sum) {
  stopifnot(is.data.frame(data))
  
  caller_env <- parent.frame()
  
  .parse_colspec <- function(expr, data, caller_env, allow_null = FALSE) {
    if (is.null(expr)) {
      if (allow_null) return(NULL)
      stop("NULL is not allowed here.")
    }
    
    # bare symbol, e.g. scientificName
    if (is.symbol(expr)) {
      nm <- as.character(expr)
      
      # if it matches a data column, use it directly
      if (nm %in% names(data)) {
        return(nm)
      }
      
      # otherwise allow things like ids <- "locationID"
      val <- try(eval(expr, envir = caller_env), silent = TRUE)
      if (!inherits(val, "try-error")) {
        if (is.null(val) && allow_null) return(NULL)
        if (is.character(val)) return(val)
      }
      
      stop("Unknown column specification: ", nm)
    }
    
    # c(a, b) or c("a", "b")
    if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
      out <- vapply(as.list(expr)[-1], function(e) {
        if (is.symbol(e)) {
          as.character(e)
        } else {
          val <- eval(e, envir = caller_env)
          if (!is.character(val) || length(val) != 1L) {
            stop("Could not interpret one element of column specification.")
          }
          val
        }
      }, character(1))
      return(out)
    }
    
    # quoted strings, e.g. "scientificName"
    val <- eval(expr, envir = caller_env)
    if (is.null(val) && allow_null) return(NULL)
    if (is.character(val)) return(val)
    
    stop("Could not interpret column specification.")
  }
  
  # IMPORTANT: capture here, not inside .parse_colspec()
  names_from_expr  <- substitute(names_from)
  values_from_expr <- substitute(values_from)
  id_cols_expr     <- if (missing(id_cols)) NULL else substitute(id_cols)
  
  names_from  <- .parse_colspec(names_from_expr,  data, caller_env)
  values_from <- .parse_colspec(values_from_expr, data, caller_env)
  id_cols     <- .parse_colspec(id_cols_expr,     data, caller_env, allow_null = TRUE)
  
  if (length(names_from) != 1L) {
    stop("'names_from' should specify exactly one column.")
  }
  if (length(values_from) != 1L) {
    stop("'values_from' should specify exactly one column.")
  }
  
  if (!all(c(names_from, values_from) %in% names(data))) {
    stop("'names_from' and/or 'values_from' are not in 'data'.")
  }
  
  # pivot_wider-like default
  if (is.null(id_cols)) {
    id_cols <- setdiff(names(data), c(names_from, values_from))
  }
  
  if (length(id_cols) == 0L) {
    stop("No 'id_cols' remain after removing 'names_from' and 'values_from'.")
  }
  
  if (!all(id_cols %in% names(data))) {
    stop("Some 'id_cols' are not in 'data'.")
  }
  
  x <- data[, c(id_cols, names_from, values_from), drop = FALSE]
  names(x)[names(x) == names_from]  <- "..name"
  names(x)[names(x) == values_from] <- "..value"
  
  agg <- stats::aggregate(
    x = list(..value = x$..value),
    by = c(x[id_cols], list(..name = x$..name)),
    FUN = agg_fun
  )
  
  out <- stats::reshape(
    agg,
    idvar = id_cols,
    timevar = "..name",
    direction = "wide"
  )
  
  rownames(out) <- NULL
  
  prefix <- "..value."
  w <- startsWith(names(out), prefix)
  names(out)[w] <- substring(names(out)[w], nchar(prefix) + 1L)
  
  wide_cols <- setdiff(names(out), id_cols)
  for (nm in wide_cols) {
    out[[nm]][is.na(out[[nm]])] <- fill
  }
  
  out
}
################

.get_module_names <- function() {
  m <- list_Modules(tree = FALSE,brief = TRUE)
  m$name[m$valid]
}
#-------

# check if the parent of selected modules are available!
.check_parent <- function(n) {
  ml <- list_Modules()
  ml <- ml[ml$name %in% n,]
  #-----
  .x <- c()
  for (i in 1:nrow(ml)) {
    if (ml$parent[i] == '.root') next
    else if (ml$parent[i] %in% ml$name) next
    else .x <- c(.x,ml$name[i])
  }
  if (length(.x) > 0) return(.x)
  
}
#-----------

.attach_modules <- function(cm,n='all') {
  if (missing(n)) n <- 'all'
  
  if (is.character(n)) {
    if ('all' %in% n) n <- .get_module_names()
    else {
      nn <- .get_module_names()
      n <- nn[nn %in% n]
      #---------
      nn <- .check_parent(n)
      if (!is.null(nn)) {
        warning(paste0('Some sections (',.paste_comma_and(nn),') are excluded because their parents are not available!'))
        n <- n[!n %in% nn]
      }
      if (length(n) == 0) stop('No modules (report sections) are selected!')
    }
  }
  #------
  if (!all(is.na(cm$reportObjectElements$Modules_info$tested))) {
    .w <- cm$reportObjectElements$Modules_info$tested
    .w <- sort(c(which(is.na(.w)),which(.w[!is.na(.w)])))
    if (length(.w) > 0) {
      .w <- cm$reportObjectElements$Modules_info$name[.w]
      n <- n[n %in% .w]
    }
  }
  #---------
  mods <- cm$reportObjectElements$Modules[n]
  cm$reportObjects <- list()
  for (i in seq_along(mods)) {
    cm$addReportObject(mods[[i]])
  }
  
}
#-------

.attach_status_modules <- function(cm,n='all') {
  if (missing(n)) n <- 'all'
  
  if (is.character(n)) {
    if ('all' %in% n) n <- cm$reportObjectElements$Status_modules_info$name
    else {
      nn <- cm$reportObjectElements$Status_modules_info$name
      n <- nn[nn %in% n]
      
      if (length(n) == 0) stop('No data status modules (status_report sections) are selected!')
    }
  }
  #------
  if (!all(is.na(cm$reportObjectElements$Status_modules_info$tested))) {
    .w <- cm$reportObjectElements$Status_modules_info$tested
    .w <- sort(c(which(is.na(.w)),which(.w[!is.na(.w)])))
    if (length(.w) > 0) {
      .w <- cm$reportObjectElements$Status_modules_info$name[.w]
      n <- n[n %in% .w]
    }
  }
  #---------
  mods <- cm$reportObjectElements$Status_modules[n]
  cm$statusReportObjects <- list()
  for (i in seq_along(mods)) {
    cm$addStatusReportObject(mods[[i]])
  }
  
}
#-------

.get_module_packages <- function() {
  .module_dir <- .section_dir("camtrapReport")
  
  mods <- .read_modules(
    level0 = c("introduction", "methods", "results",
               "acknowledgements", "appendix"),
    package = "camtrapReport",
    dir = .module_dir,
    write_info = TRUE
  )
  
  unique(unlist(lapply(mods,function(x) {
    if (!is.null(x@Rchunk)) {
      if (is.list(x@Rchunk)) {
        unique(unlist(lapply(x@Rchunk,function(y) y@packages)))
      } else x@Rchunk@packages
    }
  })))
}
