test_that("EOW omits the parameter table and retains annual density plots and intervals", {
  skip_if_not_installed("ggplot2")
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  eow <- .resolve_profile("EOW", cm)
  expect_false("model_parameters_eow" %in% eow@report$module)
  expect_true(all(c("population_density_annual", "population_densities_eow") %in%
                    eow@report$module))
  .apply_profile(cm, eow)
  headings <- vapply(.flatten_attached_sections(cm$reportObjects),
                     function(x) x@title, character(1))
  expect_false(any(grepl("Annual REM Model Parameters", headings, fixed = TRUE)))
  expect_true(any(grepl("Population Density Estimates", headings, fixed = TRUE)))

  module <- rmarkdown::yaml_front_matter(system.file(
    "reportSections", "pop_densities_EOW.yml", package = "camtrapReport"
  ))
  expected <- data.frame(
    scientificName = "Vulpes vulpes", Year = c(2022, 2023),
    Metric = "density", estimate = c(2, 3), se = c(0.1, 0.2),
    lcl95 = c(1.8, 2.6), ucl95 = c(2.2, 3.4), unit = "n/km2",
    n = 10L, parameterScope = "annual"
  )
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  env <- new.env(parent = globalenv())
  env$object <- list(
    setting = list(focus_groups = "all"),
    reportObjectElements = list(color_palette = "#855C75"),
    getFigureNumber = function() "Figure.1:",
    get_speciesNames = function(groups) "Vulpes vulpes",
    get_REM = function(sp, parameter_scope, reps, seed, fit) {
      state$calls[[length(state$calls) + 1L]] <-
        list(sp, parameter_scope, reps, seed, fit)
      list(expected)
    }
  )
  figure <- tempfile(fileext = ".pdf")
  grDevices::pdf(figure)
  on.exit({ grDevices::dev.off(); unlink(figure) }, add = TRUE)
  output <- capture.output(eval(parse(text = module$code), env))
  expect_true(any(grepl("Whiskers represent 95% confidence intervals", output, fixed = TRUE)))
  expect_false(any(grepl("Year-specific REM parameter", output, fixed = TRUE)))
  expect_identical(state$calls, list(list("Vulpes vulpes", "annual", 999L, 42L, FALSE)))
  expect_identical(env$estimates_rem, expected)
  expect_length(env$plot_list, 1L)
  expect_identical(env$plot_list[[1]]$data$estimate, expected$estimate)
  expect_identical(env$plot_list[[1]]$data$lcl95, expected$lcl95)
  expect_identical(env$plot_list[[1]]$data$ucl95, expected$ucl95)
  expect_true(env$density_tabs_rendered)
  expect_false(exists("parameter_table", env, inherits = FALSE))

  # Even another module's stale or pooled frame cannot bypass the cache reader.
  env$estimates_rem <- transform(expected, estimate = 999, parameterScope = "pooled")
  env$object$setting$eow_rem_reps <- 123L
  env$object$setting$eow_rem_seed <- 43L
  capture.output(eval(parse(text = module$code), env))
  expect_identical(state$calls[[2]], list("Vulpes vulpes", "annual", 123L, 43L, FALSE))
  expect_identical(env$estimates_rem, expected)
})
