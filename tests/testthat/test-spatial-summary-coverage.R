run_spatial_summary_coverage <- function(
  locations,
    unavailable = "lutz"
) {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  cm$data$locations <- locations

  if (is.null(cm$data$settings)) {
    cm$data$settings <- list()
  }

  cm$data$settings$tz <- "Europe/Amsterdam"

  original_require <- ct_internal(".require")

  testthat::with_mocked_bindings(
    ct_internal(".summarize_spatial")(cm),
    .require = function(x) {
      package <- as.character(x)[1]

      if (
        !is.na(package) &&
          package %in% unavailable
      ) {
        return(FALSE)
      }

      original_require(x)
    },
    .package = "camtrapReport"
  )
}


test_that("spatial summary handles a missing locations table", {
  out <- run_spatial_summary_coverage(NULL)

  expect_identical(out$total_locationsrow, 0L)
  expect_identical(out$total_unique_locations, 0L)

  expect_match(
    out$message_missing,
    "No valid cm$data$locations table found",
    fixed = TRUE
  )

  expect_match(
    out$status_MCArea,
    "No valid camera-location coordinates",
    fixed = TRUE
  )

  expect_match(
    out$status_spatial,
    "Too few locations",
    fixed = TRUE
  )
})


test_that("spatial summary handles an empty locations table", {
  locations <- data.frame(
    locationID = character(),
    locationName = character(),
    longitude = numeric(),
    latitude = numeric(),
    stringsAsFactors = FALSE
  )

  out <- run_spatial_summary_coverage(locations)

  expect_identical(out$total_locationsrow, 0L)
  expect_identical(out$total_unique_locations, 0L)
  expect_identical(out$number_missing_rows, 0L)
  expect_identical(out$num_duplicated_coordinate, 0L)

  expect_true(is.na(out$MCArea))
  expect_identical(out$MCArea_method, "Not estimated")

  expect_match(
    out$status_MCArea,
    "No valid camera-location coordinates",
    fixed = TRUE
  )

  expect_match(
    out$outliers_status,
    "No valid locations available",
    fixed = TRUE
  )
})


test_that("spatial summary handles one camera location", {
  locations <- data.frame(
    locationID = "loc-1",
    locationName = "Utrecht",
    longitude = 5.1214,
    latitude = 52.0907,
    stringsAsFactors = FALSE
  )

  out <- run_spatial_summary_coverage(locations)

  expect_identical(out$total_unique_locations, 1L)

  expect_true(is.finite(out$MCArea))
  expect_gt(out$MCArea, 0)

  expect_identical(
    out$MCArea_method,
    "1 km buffer around one camera location"
  )

  expect_true(is.na(out$mean_distance_cam))
  expect_true(is.na(out$min_distance_cam))
  expect_true(is.na(out$max_distance_cam))

  expect_match(
    out$status_MCArea,
    "one distinct camera location",
    fixed = TRUE
  )

  expect_match(
    out$status_spatial,
    "Too few locations",
    fixed = TRUE
  )
})


test_that("spatial summary handles two camera locations", {
  locations <- data.frame(
    locationID = c("loc-1", "loc-2"),
    locationName = c("Utrecht", "Vianen"),
    longitude = c(5.1214, 5.0913),
    latitude = c(52.0907, 51.9920),
    stringsAsFactors = FALSE
  )

  out <- run_spatial_summary_coverage(locations)

  expect_identical(out$total_unique_locations, 2L)

  expect_true(is.finite(out$mean_distance_cam))
  expect_true(is.finite(out$min_distance_cam))
  expect_true(is.finite(out$max_distance_cam))

  expect_gt(out$mean_distance_cam, 0)
  expect_gt(out$MCArea, 0)

  expect_identical(
    out$MCArea_method,
    "1 km buffers around two camera locations"
  )

  expect_match(
    out$status_MCArea,
    "two distinct camera locations",
    fixed = TRUE
  )
})


test_that(
  "spatial summary handles missing and duplicated location information",
  {
    locations <- data.frame(
      locationID = c(
        "loc-1",
        "loc-1",
        "loc-3",
        "loc-4",
        "loc-5"
      ),
      locationName = c(
        "Site A",
        "Site A",
        "Site C",
        "Site D",
        "Site E"
      ),
      longitude = c(
        "5.12",
        "5.12",
        "5,20",
        NA,
        "5.30"
      ),
      latitude = c(
        "52.09",
        "52.09",
        "52.15",
        "52.20",
        "52.05"
      ),
      stringsAsFactors = FALSE
    )

    out <- run_spatial_summary_coverage(locations)

    expect_identical(out$total_locationsrow, 5L)
    expect_identical(out$number_missing_rows, 1L)

    expect_identical(out$num_dup_locationID, 1L)
    expect_identical(out$num_dup_locationName, 1L)
    expect_identical(out$num_duplicated_coordinate, 1L)

    expect_identical(out$total_unique_locations, 3L)

    expect_true(is.finite(out$MCArea))
    expect_gt(out$MCArea, 0)

    expect_identical(
      out$MCArea_method,
      "Minimum convex polygon"
    )

    expect_match(
      out$message_missing,
      "1 rows with missing data",
      fixed = TRUE
    )

    expect_match(
      out$status_duplicated_coordinate,
      "Duplicate coordinates found",
      fixed = TRUE
    )
  }
)


test_that(
  "spatial summary handles unavailable spatial-pattern packages",
  {
    grid <- expand.grid(
      longitude = c(5.0, 5.1, 5.2),
      latitude = c(52.0, 52.1, 52.2),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    locations <- data.frame(
      locationID = paste0(
        "loc-",
        seq_len(nrow(grid))
      ),
      locationName = paste0(
        "Site ",
        seq_len(nrow(grid))
      ),
      longitude = grid$longitude,
      latitude = grid$latitude,
      stringsAsFactors = FALSE
    )

    out <- run_spatial_summary_coverage(
      locations,
      unavailable = c(
        "lutz",
        "spatstat.geom",
        "spatstat.explore"
      )
    )

    expect_identical(out$total_unique_locations, 9L)

    expect_match(
      out$status_spatial,
      "spatstat.geom and/or spatstat.explore are not installed",
      fixed = TRUE
    )
  }
)
