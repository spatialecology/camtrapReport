# REM cache bookkeeping. Model inputs and statistical calculations stay in camR.
.rem_source <- function(data) {
  # Serialization also detects data.table updates by reference. Keep one source
  # snapshot per object, rather than duplicating it for every species-year.
  serialize(data[c("observations", "deployments", "locations", "taxonomy")],
            NULL, version = 2)
}

.require_rem_angle_unit <- function(unit) {
  if (length(unit) != 1L || is.na(unit) || !unit %in% c("degree", "radian")) {
    stop(errorCondition(paste0("REM angle units are unknown. Confirm 'degree' or 'radian' ",
         "from the source of the observations or calibration. For saved observations set ",
         "object$setting$rem_angle_unit or reimport the Camtrap DP data."),
         class = "rem_angle_unit_error", call = NULL))
  }
  as.character(unit)
}

# Retain the existing per-year recovery for ordinary fit failures while giving
# the caller the information needed to confirm an external calibration.
.try_rem_result <- function(expr) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "rem_angle_unit_error")) stop(e)
    NULL
  })
}

.sync_rem_angle_unit <- function(object) {
  previous <- object$info$rem_angle_cache_unit
  # Older fitted models labelled Camtrap DP degrees as radians. Neither a
  # fitted label nor the magnitude identifies the unit of a saved column.
  unit <- object$setting$rem_angle_unit %||% previous %||% "unknown"
  if (!identical(unit, "unknown") && !identical(unit, "degree") &&
      !identical(unit, "radian")) {
    stop("Set rem_angle_unit to 'degree' or 'radian' for the stored angle values.")
  }
  if (identical(previous, unit)) return(invisible(NULL))

  # Keep the first pre-versioned calibration intact even if the upgraded object
  # is explicitly saved later. Active fits are recomputed from confirmed inputs.
  legacy <- is.null(previous) && (length(object$.rem_params) || length(object$rem) ||
                                    length(object$.act_models))
  if (legacy && is.null(object$info$rem_legacy_calibration)) {
    object$info$rem_legacy_calibration <- list(parameters = object$.rem_params,
      activity = object$.act_models, densities = object$rem, observation_unit = unit)
    if (identical(unit, "unknown")) {
      warning("REM angle units are unknown. Original calibration and density caches ",
              "are retained in object$info$rem_legacy_calibration. Confirm the stored ",
              "observation unit or reimport before fitting REM.", call. = FALSE)
    }
  }
  # Rebind saved package methods, including dependencies used as bare method
  # names by other reference methods. Preserve user-supplied functions.
  for (name in c("get_speciesNames", "get_data_subset", ".get_REM_Param", "fit_REM",
                 "get_REM", "prepare_REM", "setup", "richness")) {
    method <- get0(name, envir = object, inherits = FALSE)
    if (inherits(method, "refMethodDef") && identical(method@refClassName, "camReport")) {
      rm(list = name, envir = object)
      do.call("$", list(object, name))
    }
  }
  object$setting$rem_angle_unit <- unit
  object$info$rem_angle_cache_unit <- unit
  object$rem <- list()
  has_angle <- vapply(object$.rem_params, function(p) is.list(p) &&
    (!is.null(p$angle_model) || "angle" %in% rownames(p)), logical(1))
  object$.rem_params <- object$.rem_params[!has_angle]
  object$.any_data_for_rem <- logical()
  invisible(NULL)
}

.sync_rem_cache <- function(object) {
  .sync_rem_angle_unit(object)
  source <- list(data = .rem_source(object$data), years = object$years,
                 signature = .rem_analysis_signature())
  if (!identical(object$info$rem_cache_source, source)) {
    object$rem <- list()
    object$.rem_params <- list()
    object$.act_models <- list()
    object$.any_data_for_rem <- logical()
    object$info$rem_empty_years <- character()
    object$info$rem_cache_source <- source
  }
  # Exposure changes invalidate density results, not shared calibration models
  # or the global analysis signature (which also migrates legacy profiles).
  if (!identical(object$info$rem_exposure_signature, "full-exposure-v1")) {
    object$rem <- list()
    object$info$rem_exposure_signature <- "full-exposure-v1"
  }
  invisible(NULL)
}

.rem_cached <- function(x) {
  is.data.frame(x) && nrow(x) > 0L
}

.rem_missing_years <- function(object, sp, reps, seed) {
  group <- object$get_focus_group(sp)
  years <- object$years
  keys <- vapply(years, function(year) {
    .rem_result_key(sp, year, "annual", reps, seed)
  }, character(1))
  present <- vapply(keys, function(key) {
    .rem_cached(object$rem[[group]][[key]])
  }, logical(1))
  pending <- years[!present & !as.character(years) %in% object$info$rem_empty_years]
  # Skip observation-free and REM-ineligible species-years before requesting
  # calculations, while retaining errors for absent deployments.
  pending[vapply(pending, function(year) {
    ids <- object$data$deployments$deploymentID[object$data$deployments$Year == year]
    !length(ids) || .rem_year_eligible(object, sp, year)
  }, logical(1))]
}

.eow_rem_settings <- function(object) {
  list(reps = object$setting$eow_rem_reps %||% 999L,
       seed = object$setting$eow_rem_seed %||% 42L)
}

.validate_rem_settings <- function(reps, seed) {
  if (length(reps) != 1L || !is.finite(reps) || reps < 1L) {
    stop("'reps' must be a positive integer.")
  }
  if (length(seed) != 1L || !is.finite(seed)) {
    stop("'seed' must be a finite integer.")
  }
}

# Apply the existing REM input requirements to the same deployment-year subset
# used by the annual fitter, without copying unrelated media or spatial data.
.rem_year_eligible <- function(object, sp, year) {
  ids <- object$data$deployments$deploymentID[object$data$deployments$Year == year]
  observations <- object$data$observations[
    object$data$observations$deploymentID %in% ids, , drop = FALSE
  ]
  any(.any_data_for_rem(list(observations = observations), sp, object$setting$rem_angle_unit) %in% TRUE)
}
