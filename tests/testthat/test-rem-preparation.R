test_that("annual preparation reuses complete fits and repairs only missing years", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  set.seed(101)
  rng <- .Random.seed
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  expect_identical(.Random.seed, rng)
  expect_length(state$fits, 4L)
  expect_identical(state$results, 2L)
  before <- cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE)
  expect_length(before, 2L)
  expect_identical(vapply(before, function(x) x$estimate, numeric(1)),
                   c(`Vulpes vulpes_2022` = 1.5, `Vulpes vulpes_2023` = 3.5))
  expect_true(all(vapply(before, function(x) identical(x$lcl95, 0.5) &&
                          identical(x$ucl95, 5), logical(1))))
  expect_silent(cm$prepare_REM())
  expect_identical(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L), before)
  expect_length(state$fits, 4L)
  expect_identical(state$results, 2L)

  key <- .rem_result_key("Vulpes vulpes", 2023, "annual", 999L, 42L)
  cm$rem$large_mammals[[key]] <- NULL
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  # Missing density uses the already fitted parameters, preserving the other year.
  expect_length(state$fits, 4L)
  expect_identical(state$results, 3L)
  expect_identical(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L), before)

  cm$rem$large_mammals[[key]] <- data.frame()
  param <- .rem_parameter_key("Vulpes vulpes", "annual", 2023, 999L, 42L)
  cm$.rem_params[[param]] <- NULL
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  expect_length(state$fits, 6L)
  expect_identical(state$results, 4L)
  expect_identical(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L), before)
})

test_that("annual failures are reported and retried without refitting completed years", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  state$fail_year <- "b"
  expect_warning(suppressMessages(cm$prepare_REM()), "Vulpes vulpes in 2023")
  expect_length(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE), 1L)
  expect_identical(state$results, 1L)
  state$fail_year <- NULL
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  expect_length(state$fits, 5L)
  expect_identical(state$results, 2L)
  expect_silent(cm$prepare_REM())
})

test_that("source changes and settings cannot reuse stale REM results", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  suppressMessages(cm$prepare_REM())
  before <- cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE)
  cm$setting$eow_rem_reps <- 123L
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  cm$setting$eow_rem_seed <- 43L
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  expect_length(state$fits, 12L)
  expect_identical(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE), before)
  for (table in c("observations", "deployments", "locations", "taxonomy")) {
    cm$data[[table]]$cache_test <- seq_len(nrow(cm$data[[table]]))
    expect_null(cm$get_REM("Vulpes vulpes", "annual", 123L, 43L, fit = FALSE))
    expect_length(cm$.rem_params, 0L)
    expect_message(cm$prepare_REM(), "Calculating annual REM")
  }
  expect_length(state$fits, 28L)
  cm$data$observations$speed <- cm$data$observations$speed * 2
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  after <- cm$get_REM("Vulpes vulpes", "annual", 123L, 43L, fit = FALSE)
  expect_identical(after[[1]]$estimate, before[[1]]$estimate * 2)
  expect_error(cm$get_REM("Vulpes vulpes", "annual", 0L), "positive integer")
})

test_that("pooled calculations retain their scope, defaults and combined estimate", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  pooled <- cm$get_REM("Vulpes vulpes")
  expect_length(state$fits, 2L)
  expect_identical(state$results, 3L)
  expect_identical(state$fits[[1]]$deployments, c("a", "b"))
  expect_length(pooled, 3L)
  expect_true(all(vapply(pooled, function(x) x$parameterScope == "pooled" &&
                          x$remReps == 10L && x$remSeed == 42L, logical(1))))
  expect_identical(cm$get_REM("Vulpes vulpes"), pooled)
  expect_length(state$fits, 2L)
  cm$get_REM("Vulpes vulpes", reps = 20L)
  cm$get_REM("Vulpes vulpes", seed = 43L)
  expect_length(state$fits, 6L)
  suppressMessages(cm$prepare_REM())
  expect_identical(cm$get_REM("Vulpes vulpes"), pooled)
})

test_that("unusable data and cache-only access never initiate fitting", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  expect_null(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE))
  cm$data$observations$speed <- NA_real_
  expect_silent(cm$prepare_REM())
  expect_length(state$fits, 0L)
  expect_length(cm$.act_models, 0L)
})

test_that("serialized objects reuse annual results and legacy caches are invalidated", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  suppressMessages(cm$prepare_REM())
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(cm, file)
  restored <- readRDS(file)
  expect_silent(restored$prepare_REM())
  expect_length(state$fits, 4L)
  expect_identical(restored$rem, cm$rem)
  restored$info$rem_analysis_signature <- "old"
  restored <- .upgrade_camreport_analysis(restored)
  expect_length(restored$rem, 0L)
  expect_length(restored$.rem_params, 0L)
  expect_message(restored$prepare_REM(), "Calculating annual REM")
})
test_that("report prepares annual modules before rendering, including attached profiles", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  state <- new.env()
  state$prepared <- 0L
  assign("prepare_REM", function(...) {
    state$prepared <- state$prepared + 1L
    invisible(cm)
  }, envir = cm)
  testthat::local_mocked_bindings(
    .generate_report = function(object, output_file, rmd_file, toc) {
      expect_identical(state$prepared, state$expected)
      output_file
    }, .package = "camtrapReport"
  )
  state$expected <- 1L
  suppressMessages(report(cm, filename = tempfile(), profile = "eow"))
  state$expected <- 2L
  suppressMessages(report(cm, filename = tempfile()))
  state$expected <- 3L
  suppressMessages(report(cm, filename = tempfile(), profile = .resolve_profile("EOW", cm)))
  suppressMessages(report(cm, filename = tempfile(), profile = "default"))
  expect_identical(state$prepared, 3L)
})

test_that("empty years do not cause repeated annual preparations", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  cm$data$observations <- cm$data$observations[1:2, ]
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  expect_length(state$fits, 2L)
  expect_silent(cm$prepare_REM())
  expect_length(state$fits, 2L)
})

test_that("data.table reference edits invalidate source snapshots", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  cm$data$observations <- data.table::as.data.table(cm$data$observations)
  # Use data frames in the existing year-subset method for this source-only test.
  .sync_rem_cache(cm)
  cm$rem$large_mammals <- list(marker = data.frame(estimate = 1))
  data.table::set(cm$data$observations, j = "speed", value = rep(2, 4))
  .sync_rem_cache(cm)
  expect_length(cm$rem, 0L)
})
test_that("camData update rebuilds rather than reusing stored REM results", {
  directory <- copy_camtrap_test_dataset()
  old <- camtrap_test_report()$copy(shallow = FALSE)
  old$info$directory <- directory
  old$rem <- list(stale = data.frame(estimate = 999))
  old$.rem_params <- list(stale = list())
  saveRDS(old, file.path(directory, "__camReport_Object.rds"))
  # Keep this import test offline and independent of optional model packages.
  required <- .require
  testthat::local_mocked_bindings(
    .require = function(package) {
      if (package %in% c("jsonlite", "lubridate", "dplyr", "terra", "knitr",
                          "data.table", "shiny", "glue", "rmarkdown", "methods")) {
        required(package)
      } else FALSE
    }, .package = "camtrapReport"
  )
  fresh <- suppressWarnings(suppressMessages(camData(directory, update = TRUE)))
  expect_false("stale" %in% names(fresh$rem))
  expect_false("stale" %in% names(fresh$.rem_params))
  expect_false(any(grepl("^annual::", unlist(lapply(fresh$rem, names)))))
})
test_that("explicit alternative parameter data cannot reuse or poison the object cache", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  dat <- cm$get_data_subset(2022)
  original <- cm$.get_REM_Param("Vulpes vulpes", parameter_scope = "annual",
                                year = 2022, data = dat, reps = 999L)
  dat$observations$radius <- dat$observations$radius * 2
  changed <- cm$.get_REM_Param("Vulpes vulpes", parameter_scope = "annual",
                               year = 2022, data = dat, reps = 999L)
  expect_identical(changed$radius_model$estimate, original$radius_model$estimate * 2)
  restored <- cm$.get_REM_Param("Vulpes vulpes", parameter_scope = "annual",
                                year = 2022, data = cm$get_data_subset(2022), reps = 999L)
  expect_identical(restored, original)
  expect_length(state$fits, 4L)
})
test_that("a failed pooled fit cannot suppress independent annual preparation", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  .sync_rem_cache(cm)
  cm$.any_data_for_rem <- c(`Vulpes vulpes` = FALSE)
  expect_message(cm$prepare_REM(), "Calculating annual REM")
  expect_length(state$fits, 4L)
  expect_length(cm$get_REM("Vulpes vulpes", "annual", 999L, 42L, fit = FALSE), 2L)
})
test_that("annual parameter lookup without explicit data still uses its sampling year", {
  cm <- rem_cache_fixture()
  state <- new.env()
  local_rem_estimators(state)
  cm$.get_REM_Param("Vulpes vulpes", parameter_scope = "annual", year = 2022, reps = 999L)
  expect_identical(state$fits[[1]]$deployments, "a")
  cm$.get_REM_Param("Vulpes vulpes", parameter_scope = "annual", year = 2022,
                    data = cm$get_data_subset(2022), reps = 999L)
  expect_length(state$fits, 2L)
})
test_that("legacy reference methods and attached EOW modules are upgraded together", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  # Older serialized objects retain bound methods even when a new class method
  # (prepare_REM) is dynamically visible. Simulate that historical signature.
  assign("get_REM", function(.sp, parameter_scope = NULL, reps = NULL, seed = NULL) {
    stop("Legacy method must be replaced")
  }, envir = cm)
  cm$info$rem_analysis_signature <- "rem-v2-annual-scope-individual-count"
  cm$reportObjects <- list(
    cm$reportObjectElements$Modules$results,
    list(cm$reportObjectElements$Modules$population_density_annual,
         cm$reportObjectElements$Modules$model_parameters_eow,
         cm$reportObjectElements$Modules$population_densities_eow)
  )
  general <- cm$reportObjectElements$Profiles$default
  cm$rem <- list(stale = list())
  upgraded <- .upgrade_camreport_analysis(cm)
  expect_true("fit" %in% names(formals(upgraded$get_REM)))
  expect_identical(body(upgraded$get_REM), body(camR$new()$get_REM))
  expect_identical(body(upgraded$fit_REM), body(camR$new()$fit_REM))
  expect_length(upgraded$rem, 0L)
  selected <- .flatten_attached_sections(upgraded$reportObjects)
  names <- vapply(selected, function(x) x@name, character(1))
  expect_false("model_parameters_eow" %in% names)
  expect_true("population_densities_eow" %in% names)
  density <- selected[[which(names == "population_densities_eow")]]
  expect_identical(density, upgraded$reportObjectElements$Modules$population_densities_eow)
  expect_identical(upgraded$reportObjectElements$Profiles$default, general)
})
