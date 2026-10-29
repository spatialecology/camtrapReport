test_that("ISO offsets cross New Year according to the explicit UTC timezone", {
  parsed <- .parse_cam_datetime(c(
    "2023-12-31T23:30:00-02:00", "2024-01-01T00:30:00+02:00"
  ), tz = "UTC")
  expect_identical(
    format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    c("2024-01-01 01:30:00", "2023-12-31 22:30:00")
  )
  expect_identical(.getYear(parsed), c(2024, 2023))
})

test_that("cross-year intervals currently use their deployment-start cohort", {
  # Characterization only: this does not choose a new annual-analysis policy.
  start <- as.POSIXct("2023-12-15 00:00:00", tz = "UTC")
  end <- as.POSIXct("2024-01-15 00:00:00", tz = "UTC")
  interval <- lubridate::interval(start, end)
  expect_identical(.getYear(interval), 2023)
  expect_identical(.getYear(interval, .interval = TRUE), list(c(2023, 2024)))

  timestamps <- as.POSIXct(
    c("2023-12-20 12:00:00", "2024-01-05 12:00:00"), tz = "UTC"
  )
  cm <- camR$new()
  cm$data <- list(
    deployments = data.frame(
      deploymentID = "dep-1", locationID = "loc-1", Year = 2023,
      deploymentStart = start, deploymentEnd = end,
      deployment_interval = interval, stringsAsFactors = FALSE
    ),
    media = data.frame(
      mediaID = c("media-1", "media-2"), deploymentID = "dep-1",
      timestamp = timestamps, stringsAsFactors = FALSE
    ),
    observations = data.frame(
      observationID = c("obs-1", "obs-2"), deploymentID = "dep-1",
      scientificName = "Vulpes vulpes", timestamp = timestamps,
      stringsAsFactors = FALSE
    ),
    locations = data.frame(
      locationID = "loc-1", locationName = "Camera 1",
      stringsAsFactors = FALSE
    ),
    taxonomy = data.frame(
      taxonID = "tax-1", scientificName = "Vulpes vulpes",
      stringsAsFactors = FALSE
    ),
    sequences = data.frame(
      sequenceID = c("seq-1", "seq-2"), deploymentID = "dep-1",
      stringsAsFactors = FALSE
    )
  )
  expect_identical(cm$extractYears(update = TRUE), 2023)
  subset <- cm$get_data_subset(2023)
  expect_named(subset, names(cm$data))
  expect_identical(subset$observations, cm$data$observations)
  expect_identical(.getYear(subset$observations$timestamp), c(2023, 2024))
  expect_identical(subset$media, cm$data$media)
  expect_identical(subset$deployments$deploymentID, "dep-1")
  expect_identical(nrow(subset$deployments), 1L)
  expect_identical(nrow(subset$observations), 2L)
  expect_error(cm$get_data_subset(2024),
               "No deployments found for year 2024.", fixed = TRUE)
})
