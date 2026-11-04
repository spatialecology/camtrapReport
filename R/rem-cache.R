# REM cache bookkeeping. Model inputs and statistical calculations stay in camR.
.rem_source <- function(data) {
  # Serialization also detects data.table updates by reference. Keep one source
  # snapshot per object, rather than duplicating it for every species-year.
  serialize(data[c("observations", "deployments", "locations", "taxonomy")],
            NULL, version = 2)
}

.sync_rem_cache <- function(object) {
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
  # The existing fitter skips observation-free years. Recognize these before
  # announcing new calculations, while retaining errors for absent deployments.
  pending[vapply(pending, function(year) {
    ids <- object$data$deployments$deploymentID[object$data$deployments$Year == year]
    !length(ids) || any(object$data$observations$deploymentID %in% ids)
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
