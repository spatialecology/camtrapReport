test_that("EOW displays annual parameters without altering their estimates", {
  module <- rmarkdown::yaml_front_matter(system.file(
    "reportSections", "model_parameters_EOW.yml", package = "camtrapReport"
  ))
  expected <- data.frame(
    scientificName = "Vulpes vulpes", Year = 2023L,
    Metric = c("radius", "angle", "active_speed", "activity_level",
               "overall_speed", "trap_rate", "density"),
    estimate = c(5, 0.6, 1, 0.5, 0.5, 0.1, 2), se = 0.01,
    unit = c("m", "radian", "m/s", "proportion", "m/s", "1/day", "1/km2"),
    n = 10L, parameterScope = "annual"
  )
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  env <- new.env(parent = globalenv())
  env$object <- list(
    setting = list(focus_groups = "all"),
    get_speciesNames = function(groups) "Vulpes vulpes",
    get_REM = function(sp, parameter_scope, reps, seed) {
      state$calls[[length(state$calls) + 1L]] <-
        list(sp, parameter_scope, reps, seed)
      expected
    }
  )
  output <- capture.output(eval(parse(text = module$code), env))
  expect_true(any(grepl("Year-specific REM parameter", output, fixed = TRUE)))
  expect_true(any(grepl("Vulpes vulpes", output, fixed = TRUE)))
  expect_identical(state$calls, list(list("Vulpes vulpes", "annual", 999L, 42L)))
  expect_equal(env$estimates_rem, expected)
  expect_equal(env$parameter_table$Estimate,
               expected$estimate[match(env$parameter_table$Parameter,
                                       expected$Metric)])
  expect_named(env$parameter_table,
               c("Species", "Year", "Parameter", "Estimate",
                 "Standard error", "Unit", "Sample size"))
})
