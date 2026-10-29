setup_zone_without_optional_sun <- function(cm, tz = NULL) {
  original_require <- .require
  original_read_modules <- .read_modules
  testthat::with_mocked_bindings(
    capture_report_conditions(cm$setup(tz = tz)),
    .require = function(package) {
      if (identical(package, "suncalc")) return(FALSE)
      original_require(package)
    },
    .read_modules = function(..., write_info = TRUE) {
      original_read_modules(..., write_info = FALSE)
    },
    .package = "camtrapReport"
  )
}

test_that("setup uses UTC only when no analysis timezone was supplied", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$setting$tz <- NULL
  cm$data$settings <- NULL
  before <- cm$data

  captured <- setup_zone_without_optional_sun(cm)

  expect_identical(cm$setting$tz, "UTC")
  expect_identical(cm$data$observations$timestamp,
                   before$observations$timestamp)
  expect_identical(cm$data$media$timestamp, before$media$timestamp)
  expect_identical(cm$data$deployments$deploymentStart,
                   before$deployments$deploymentStart)
  expect_identical(cm$data$deployments$deploymentEnd,
                   before$deployments$deploymentEnd)
  expect_true(any(grepl('"suncalc" package is not installed',
                        captured$warnings, fixed = TRUE)))
})

test_that("setup preserves the explicit, existing and data timezone priority", {
  cases <- list(
    list(existing = "Pacific/Auckland", data = "Europe/Rome", explicit = NULL,
         expected = "Pacific/Auckland"),
    list(existing = NULL, data = "Asia/Kolkata", explicit = NULL,
         expected = "Asia/Kolkata"),
    list(existing = "Europe/Rome", data = "Asia/Tokyo",
         explicit = "America/New_York", expected = "America/New_York"),
    list(existing = "CET", data = "UTC", explicit = NULL, expected = "CET"),
    list(existing = "CET", data = "Europe/Rome", explicit = "UTC",
         expected = "UTC")
  )
  for (case in cases) {
    cm <- camtrap_test_report()$copy(shallow = FALSE)
    cm$setting$tz <- case$existing
    cm$data$settings <- list(tz = case$data)
    before <- cm$data$observations$timestamp

    setup_zone_without_optional_sun(cm, tz = case$explicit)

    expect_identical(cm$setting$tz, case$expected)
    expect_identical(cm$data$observations$timestamp, before)
  }
})

test_that("a year with no deployments fails before model fitting", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  missing_year <- max(cm$data$deployments$Year, na.rm = TRUE) + 1
  expected <- paste0("No deployments found for year ", missing_year, ".")
  expect_error(cm$get_data_subset(missing_year), expected, fixed = TRUE)

  state <- new.env(parent = emptyenv())
  state$parameter_calls <- 0L
  assign(".get_REM_Param", function(...) {
    state$parameter_calls <- state$parameter_calls + 1L
    stop("Model fitting should not start for an absent deployment year")
  }, envir = cm)
  cm$years <- missing_year
  species <- cm$frequent_species$scientificName[1]
  expect_error(cm$fit_REM(species), expected, fixed = TRUE)
  expect_identical(state$parameter_calls, 0L)
})

test_that("valid year subsets retain their table shape, row order and values", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  before <- cm$data
  year <- cm$years[1]
  deployments <- before$deployments[before$deployments$Year == year, ]
  ids <- deployments$deploymentID
  expected <- list(
    deployments = deployments,
    media = before$media[before$media$deploymentID %in% ids, ],
    observations = before$observations[
      before$observations$deploymentID %in% ids,
    ],
    locations = before$locations[
      before$locations$locationID %in% deployments$locationID,
    ],
    taxonomy = before$taxonomy[
      before$taxonomy$scientificName %in%
        before$observations$scientificName[
          before$observations$deploymentID %in% ids
        ],
    ],
    sequences = before$sequences[before$sequences$deploymentID %in% ids, ]
  )

  actual <- cm$get_data_subset(year)

  expect_named(actual, names(expected))
  for (name in names(expected)) expect_identical(actual[[name]], expected[[name]])
  expect_identical(cm$data, before)
})
