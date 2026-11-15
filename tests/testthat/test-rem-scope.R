rem_scope_fixture <- function() {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$years <- c(2018, 2021, 2024)
  species <- paste("Species", c("alpha", "beta", "gamma", "delta"))
  observations <- do.call(rbind, lapply(species, function(sp) {
    x <- data.frame(scientificName = sp, deploymentID = rep(c("a", "b", "c"), each = 2),
                    speed = 1:6, radius = 5, angle = 0.5, useDeployment = TRUE)
    if (sp == "Species beta") {
      x$radius[x$deploymentID == "b"] <- NA_real_
      x$angle[x$deploymentID == "c"] <- NA_real_
    }
    if (sp == "Species gamma") x$speed <- NA_real_
    x
  }))
  cm$data <- list(observations = observations,
    deployments = data.frame(deploymentID = c("a", "b", "c"), locationID = c("a", "b", "c"), Year = cm$years),
    locations = data.frame(locationID = c("a", "b", "c"), latitude = 52, longitude = 5),
    taxonomy = data.frame(scientificName = species, vernacularNames = species),
    media = data.frame(deploymentID = character()), sequences = data.frame(deploymentID = character()))
  groups <- list(large_mammals = species[1], wild_mammals = species[1:3], birds = species[4],
                 ineligible = species[3], empty = character())
  cm$species_summary <- c(list(count = data.frame(Group = names(groups), Name = names(groups))),
    lapply(groups, function(sp) list(site_list = data.frame(scientificName = sp))))
  cm$setting$focus_groups <- "wild_mammals"
  cm
}

scope_fit_pairs <- function(state) {
  vapply(state$fits, function(x) paste(x$species, paste(x$deployments, collapse = ",")), "")
}

local_scope_estimators <- function(state, .local_envir = parent.frame()) {
  local_rem_estimators(state, .local_envir)
  local_mocked_bindings(.get_traprate_data = function(dat, species) {
    dat$observations <- dat$observations[dat$observations$scientificName == species, ]
    dat
  }, .package = "camtrapReport", .env = .local_envir)
}

test_that("annual REM respects scope and species-year eligibility", {
  cm <- rem_scope_fixture()
  state <- new.env(parent = emptyenv())
  local_scope_estimators(state)
  output <- capture_report_conditions(cm$prepare_REM())
  expect_length(output$warnings, 0L)
  expect_true(any(grepl("Skipping annual REM for Species beta in 2021", output$messages, fixed = TRUE)))
  expect_identical(scope_fit_pairs(state), rep(c("Species alpha a", "Species alpha b", "Species alpha c", "Species beta a"), each = 2))
  expect_length(cm$get_REM("Species beta", "annual", 999L, 42L, fit = FALSE), 1L)
  expect_null(cm$get_REM("Species gamma", "annual", 999L, 42L, fit = FALSE))
  expect_silent(cm$prepare_REM())
  expect_length(state$fits, 8L)
  key <- .rem_result_key("Species beta", 2021, "annual", 999L, 42L)
  cm$rem$wild_mammals[[key]] <- data.frame(scientificName = "Species beta", Year = 2021, estimate = 999)
  expect_identical(names(cm$get_REM("Species beta", "annual", 999L, 42L, fit = FALSE)), "Species beta_2018")
  suppressMessages(cm$fit_REM("Species beta", "annual", 999L, 42L))
  expect_null(cm$rem$wild_mammals[[key]])
  expect_length(state$fits, 8L)

  cm$set_focus_group("birds")
  suppressMessages(cm$prepare_REM())
  expect_identical(tail(scope_fit_pairs(state), 6), rep(c("Species delta a", "Species delta b", "Species delta c"), each = 2))
  expect_length(state$fits, 14L)
  module <- .read_section_module(system.file("reportSections", "model_parameters_EOW.yml", package = "camtrapReport"))
  env <- list2env(list(object = cm), parent = globalenv())
  invisible(capture.output(eval(parse(text = module$code), env)))
  expect_identical(unique(env$estimates_rem$scientificName), "Species delta")
  expect_length(state$fits, 14L)
  cm$set_focus_group("wild_mammals")
  expect_silent(cm$prepare_REM())
  invisible(capture.output(eval(parse(text = module$code), env)))
  expect_setequal(unique(env$estimates_rem$scientificName), c("Species alpha", "Species beta"))
  expect_false(any(env$estimates_rem$scientificName == "Species beta" & env$estimates_rem$Year != 2018))
  for (group in c("ineligible", "empty")) {
    cm$set_focus_group(group)
    expect_silent(cm$prepare_REM())
    output <- capture.output(eval(parse(text = module$code), env))
    expect_match(paste(output, collapse = "\n"), "No annual REM parameter estimates", fixed = TRUE)
    expect_equal(nrow(env$estimates_rem), 0L)
    expect_length(state$fits, 14L)
  }
})

test_that("EOW and custom annual profiles preserve selected scope and defaults", {
  cm <- rem_scope_fixture()
  state <- new.env(parent = emptyenv())
  local_scope_estimators(state)
  local_mocked_bindings(.generate_report = function(object, output_file, ...) output_file,
                        .package = "camtrapReport")
  suppressMessages(report(cm, profile = "EOW", filename = tempfile()))
  expect_identical(cm$setting$focus_groups, "wild_mammals")
  expect_length(state$fits, 8L)
  add_profile(cm, reportProfile("annual_custom", report = c("results", "population_density_annual", "model_parameters_eow")))
  cm$set_focus_group("large_mammals")
  suppressMessages(report(cm, profile = "annual_custom", filename = tempfile()))
  expect_identical(cm$setting$focus_groups, "large_mammals")
  expect_length(state$fits, 8L)
  expect_identical(.eow_rem_settings(cm), list(reps = 999L, seed = 42L))
  cm$setting$eow_rem_reps <- 123L
  suppressMessages(cm$prepare_REM())
  expect_length(state$fits, 14L)
  cm$setting$eow_rem_seed <- 43L
  suppressMessages(cm$prepare_REM())
  expect_length(state$fits, 20L)
  expect_silent(cm$prepare_REM())
  cm$years <- 2018
  suppressMessages(cm$prepare_REM())
  expect_length(state$fits, 22L)
})

test_that("changed calibration inputs invalidate annual caches", {
  cm <- rem_scope_fixture()
  state <- new.env(parent = emptyenv())
  local_scope_estimators(state)
  suppressMessages(cm$prepare_REM())
  expect_length(state$fits, 8L)
  take <- cm$data$observations$scientificName == "Species beta"
  cm$data$observations$radius[take] <- 5
  cm$data$observations$angle[take] <- 0.5
  expect_null(cm$get_REM("Species beta", "annual", 999L, 42L, fit = FALSE))
  suppressMessages(cm$prepare_REM())
  expect_length(state$fits, 20L)
  expect_length(cm$get_REM("Species beta", "annual", 999L, 42L, fit = FALSE), 3L)
  expect_silent(cm$prepare_REM())
})

test_that("pooled and supplied cached parameters remain usable across years", {
  cm <- rem_scope_fixture()
  state <- new.env(parent = emptyenv())
  local_scope_estimators(state)
  result <- cm$get_REM("Species beta", "pooled")
  expect_length(result, 4L)
  expect_length(state$fits, 2L)
  expect_true(all(vapply(result, function(x) x$parameterScope == "pooled", logical(1))))
  expect_identical(state$fits[[1]]$deployments, c("a", "b", "c"))
  # The existing parameter cache can supply pooled models without refitting.
  parameters <- cm$.rem_params[["Species beta"]]
  cm$rem <- list()
  expect_identical(cm$get_REM("Species beta", "pooled"), result)
  expect_length(state$fits, 2L)
  expect_identical(cm$.rem_params[["Species beta"]], parameters)
  annual <- suppressMessages(cm$get_REM("Species beta", "annual", 999L, 42L))
  expect_identical(names(annual), "Species beta_2018")
  expect_length(state$fits, 4L)
  expect_identical(cm$get_REM("Species beta", "pooled"), result)
})
