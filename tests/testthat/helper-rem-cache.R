# Tiny deterministic model inputs; no optional estimator is run by these tests.
rem_cache_fixture <- function() {
  cm <- camR$new()
  cm$years <- c(2022, 2023)
  cm$data <- list(
    observations = data.frame(
      scientificName = rep("Vulpes vulpes", 4),
      deploymentID = rep(c("a", "b"), each = 2),
      speed = c(1, 2, 3, 4), radius = 5, angle = 0.5,
      useDeployment = TRUE
    ),
    deployments = data.frame(deploymentID = c("a", "b"),
                             locationID = c("a", "b"), Year = cm$years),
    locations = data.frame(locationID = c("a", "b"), latitude = 52, longitude = 5),
    taxonomy = data.frame(scientificName = "Vulpes vulpes",
                          vernacularNames = "Fox"),
    media = data.frame(deploymentID = character()),
    sequences = data.frame(deploymentID = character())
  )
  group <- function(sp) "large_mammals"
  species <- function(...) "Vulpes vulpes"
  environment(group) <- environment(species) <- baseenv()
  assign("get_focus_group", group, envir = cm)
  assign("get_speciesNames", species, envir = cm)
  cm$setting$focus_groups <- "large_mammals"
  cm
}

local_rem_estimators <- function(state, .local_envir = parent.frame()) {
  state$fits <- list()
  state$results <- 0L
  state$fail_year <- NULL
  testthat::local_mocked_bindings(
    .require = function(...) TRUE,
    .fit_detmodel = function(formula, data, species, ...) {
      state$fits[[length(state$fits) + 1L]] <- list(
        species = species, deployments = unique(data$deploymentID), formula = formula
      )
      if (identical(unique(data$deploymentID), state$fail_year)) stop("failed model")
      list(estimate = mean(data$radius))
    },
    .fit_speedmodel = function(data, species, reps) list(reps = reps),
    .fit_actmodel = function(data, species, reps) list(reps = reps),
    .get_traprate_data = function(dat, species) dat,
    .get_parameter_table = function(trdat, radius_model, angle_model,
                                    speed_model, activity_model, reps, seed) {
      state$results <- state$results + 1L
      stopifnot(speed_model$reps == reps, activity_model$reps == reps)
      data.frame(estimate = mean(trdat$observations$speed), se = 0.1,
                 lcl95 = 0.5, ucl95 = 5, unit = "n/km2", n = 2L,
                 row.names = "density")
    },
    .rem = function(parameters) parameters,
    .eval = function(code, envir) get(".density_estimates", envir),
    .package = "camtrapReport", .env = .local_envir
  )
}
