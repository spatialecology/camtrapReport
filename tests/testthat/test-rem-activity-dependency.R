# Regression tests for rOpenSci review issue #24.
# Simulate an unavailable optional dependency without altering installed packages.

.test_rem_fixture <- function() {
  report <- camtrap_test_report()$copy(shallow = FALSE)
  .sync_rem_cache(report)
  species <- unique(as.character(report$observed_counts$scientificName))
  species <- species[!is.na(species) & nzchar(species)]
  stopifnot(length(species) > 0L)
  list(report = report, species = species[[1L]])
}

.test_rem_dependency_mock <- function(package) {
  if (identical(package, "activity")) return(FALSE)
  if (identical(package, "camtrapDensity")) return(TRUE)
  base::requireNamespace(package, quietly = TRUE)
}

test_that("activity-only REM estimation warns if activity is unavailable", {
  fixture <- .test_rem_fixture()

  result <- NULL
  testthat::with_mocked_bindings(
    expect_warning(
      result <- fixture$report$.get_REM_Param(
        fixture$species, activity_only = TRUE, reps = 973L, seed = 841L
      ),
      'The "activity" package is not installed',
      info = "Missing activity must be reported before fitting"
    ),
    .require = .test_rem_dependency_mock,
    .fit_actmodel = function(...) stop("Activity fitting must not run"),
    .package = "camtrapReport"
  )
  expect_null(result)
})

test_that("full REM parameter estimation warns if activity is unavailable", {
  fixture <- .test_rem_fixture()

  result <- NULL
  testthat::with_mocked_bindings(
    expect_warning(
      result <- fixture$report$.get_REM_Param(
        fixture$species, reps = 974L, seed = 842L
      ),
      'The "activity" package is not installed',
      info = "Missing activity must be reported before fitting"
    ),
    .require = .test_rem_dependency_mock,
    .fit_detmodel = function(...) stop("Detection fitting must not run"),
    .fit_speedmodel = function(...) stop("Speed fitting must not run"),
    .fit_actmodel = function(...) stop("Activity fitting must not run"),
    .package = "camtrapReport"
  )
  expect_null(result)
})

test_that("cached activity and full REM parameters do not require activity", {
  fixture <- .test_rem_fixture()
  report <- fixture$report
  species <- fixture$species
  reps <- 975L
  seed <- 843L
  key <- .rem_parameter_key(species, reps = reps, seed = seed)

  cached_activity <- list(activity_model = "already fitted")
  cached_full <- list(
    radius_model = "cached", angle_model = "cached",
    speed_model = "cached", activity_model = "cached"
  )

  report$.act_models[[key]] <- cached_activity
  testthat::with_mocked_bindings(
    expect_identical(
      report$.get_REM_Param(species, activity_only = TRUE, reps = reps, seed = seed),
      cached_activity
    ),
    .require = .test_rem_dependency_mock,
    .package = "camtrapReport"
  )

  report$.rem_params[[key]] <- cached_full
  testthat::with_mocked_bindings(
    expect_identical(
      report$.get_REM_Param(species, reps = reps, seed = seed),
      cached_full
    ),
    .require = .test_rem_dependency_mock,
    .package = "camtrapReport"
  )
})

test_that("setup warns once per call for missing activity and retains suncalc warnings", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  required <- .require
  state <- list2env(list(activity_checks = 0L), parent = emptyenv())
  local_mocked_bindings(
    .require = function(package) {
      if (identical(package, "activity")) {
        state$activity_checks <- state$activity_checks + 1L
        return(FALSE)
      }
      if (identical(package, "suncalc")) return(FALSE)
      required(package)
    },
    .fit_actmodel = function(...) stop("Missing activity must prevent fitting"),
    .package = "camtrapReport"
  )
  for (i in 1:2) {
    state$activity_checks <- 0L
    result <- capture_report_conditions(cm$setup())
    expect_gt(state$activity_checks, 1L)
    expect_length(result$warnings, 2L)
    expect_equal(sum(grepl('The "activity" package is not installed',
                           result$warnings, fixed = TRUE)), 1L)
    expect_true(any(grepl("install.packages('activity')", result$warnings,
                          fixed = TRUE)))
    expect_equal(sum(grepl('The "suncalc" package is not installed',
                           result$warnings, fixed = TRUE)), 1L)
  }
})

test_that("setup retains activity fitting, caches and unrelated repeated warnings", {
  skip_if_not_installed("activity")
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  required <- .require
  fit_state <- list2env(list(fitted = character()), parent = emptyenv())
  local_mocked_bindings(
    .require = function(package) {
      if (identical(package, "suncalc")) return(FALSE)
      required(package)
    },
    .fit_actmodel = function(data, species, reps) {
      fit_state$fitted <- c(fit_state$fitted, species)
      warning("Unrelated activity fitting warning", call. = FALSE)
      list(species = species, reps = reps)
    }, .package = "camtrapReport"
  )
  result <- capture_report_conditions(cm$setup())
  species <- cm$get_speciesNames(cm$setting$focus_groups)
  expect_gt(length(species), 1L)
  expect_setequal(fit_state$fitted, species)
  expect_length(fit_state$fitted, length(species))
  expect_equal(sum(result$warnings == "Unrelated activity fitting warning"),
               length(species))
  expect_equal(sum(grepl('The "suncalc" package is not installed',
                         result$warnings, fixed = TRUE)), 1L)
  expect_length(result$warnings, length(species) + 1L)
  cached <- cm$.act_models
  fit_state$fitted <- character()
  result <- capture_report_conditions(cm$setup())
  expect_length(fit_state$fitted, 0L)
  expect_identical(cm$.act_models, cached)
  expect_length(result$warnings, 1L)
  expect_match(result$warnings, 'The "suncalc" package is not installed',
               fixed = TRUE)
})
