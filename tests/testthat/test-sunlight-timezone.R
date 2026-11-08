test_that("hour extraction respects Amsterdam standard time, DST, and UTC", {
  timestamps <- as.POSIXct(
    c("2024-01-15 06:15:00", "2024-07-15 06:15:00"), tz = "UTC"
  )

  expect_equal(.get_hour(timestamps, tz = "Europe/Amsterdam"), c(7.25, 8.25))
  expect_equal(.get_hour(timestamps, tz = "UTC"), c(6.25, 6.25))
  expect_equal(.get_hour(timestamps), c(6.25, 6.25))
})

test_that("setup retains the configured timezone in annual sunlight summaries", {
  skip_if_not_installed("suncalc")
  original <- camtrap_test_report()
  dates <- as.Date(c("2024-01-15", "2024-07-15"))
  requested_zones <- character()

  # Synthetic instants isolate clock conversion from astronomical calculations.
  local_mocked_bindings(
    getSunlightTimes = function(data, keep, tz) {
      requested_zones <<- c(requested_zones, tz)
      sunrise <- as.POSIXct(paste(dates, "06:15:00"), tz = "UTC")
      sunset <- as.POSIXct(paste(dates, "18:45:00"), tz = "UTC")
      attr(sunrise, "tzone") <- tz
      attr(sunset, "tzone") <- tz
      data.frame(date = dates, sunrise = sunrise, sunset = sunset)
    },
    .package = "suncalc"
  )

  cases <- list(
    list(tz = "Europe/Amsterdam", sunrise = 7.75, sunset = 20.25),
    list(tz = "UTC", sunrise = 6.25, sunset = 18.75)
  )
  for (case in cases) {
    cm <- original$copy(shallow = FALSE)
    capture_report_conditions(cm$setup(tz = case$tz))

    expect_identical(cm$setting$tz, case$tz)
    expect_equal(cm$sun_times$year, 2024)
    expect_equal(cm$sun_times$avg_sunrise, case$sunrise)
    expect_equal(cm$sun_times$avg_sunset, case$sunset)
  }
  expect_identical(requested_zones, c("Europe/Amsterdam", "UTC"))
})
