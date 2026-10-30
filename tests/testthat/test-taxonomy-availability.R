test_that("setup diagnoses missing classification without changing counts", {
  cm <- camtrap_test_report()
  before <- list(data = cm$data, groups = cm$group_definition,
                 stats = cm$species_stats)
  result <- capture_report_conditions(cm$setup())
  messages <- result$messages
  expect_true(all(grepl('The "suncalc" package is not installed',
                        result$warnings, fixed = TRUE)))
  expect_true(any(grepl("missing classification does not indicate ecological",
                        messages, fixed = TRUE)))
  expect_identical(cm$data, before$data)
  expect_identical(cm$group_definition, before$groups)
  expect_identical(cm$species_stats, before$stats)
  expect_false("birds" %in% names(cm$group_definition))
})

test_that("class-specific captures never silently use wholly missing classes", {
  cm <- camtrap_test_report()
  data <- cm$data
  before <- .captures(data)
  expect_error(.captures(data, class = "Mammalia"),
               "Class-based captures require taxonomic class information",
               fixed = TRUE)
  expect_identical(.captures(data), before)
  data$taxonomy$class <- NULL
  expect_error(.captures(data, class = "Mammalia"),
               "Class-based captures require taxonomic class information",
               fixed = TRUE)
  expect_identical(
    .captures(data),
    before[, setdiff(names(before), "class"), drop = FALSE]
  )
})

test_that("partial classes warn while retaining the existing capture filter", {
  cm <- camtrap_test_report()
  data <- cm$data
  data$taxonomy$class <- rep("Mammalia", nrow(data$taxonomy))
  complete <- capture_report_conditions(.captures(data, class = "Mammalia"))
  expect_length(complete$warnings, 0L)
  expect_identical(complete$value, .captures(data))
  data$taxonomy$class[1] <- NA_character_
  partial <- capture_report_conditions(.captures(data, class = "Mammalia"))
  expect_true(any(grepl("class-based capture results may be incomplete",
                        partial$warnings, fixed = TRUE)))
  expect_identical(data$taxonomy$class[1], NA_character_)
  reference <- data
  reference$taxonomy$class[1] <- "Aves"
  expect_identical(partial$value, .captures(reference, class = "Mammalia"))
  expect_error(.captures(data, class = "Aves"),
               "Class-based captures require taxonomic class information",
               fixed = TRUE)
})
