make_traprate_test_data <- function() {
  start_time <- as.POSIXct(
    "2025-01-01 00:00:00",
    tz = "UTC"
  )
  
  list(
    deployments = data.frame(
      deploymentID = c("D1", "D2"),
      locationID = c("L1", "L2"),
      deploymentStart = c(
        start_time,
        start_time
      ),
      deploymentEnd = c(
        start_time + 86400,
        start_time + (2 * 86400)
      ),
      stringsAsFactors = FALSE
    ),
    
    locations = data.frame(
      locationID = c("L1", "L2"),
      locationName = c("Site A", "Site B"),
      latitude = c(52.10, 52.20),
      longitude = c(5.10, 5.20),
      stringsAsFactors = FALSE
    ),
    
    observations = data.frame(
      deploymentID = c(
        "D1",
        "D1",
        "D2",
        "D1",
        "D2",
        "D2"
      ),
      scientificName = c(
        "Vulpes vulpes",
        "Vulpes vulpes",
        "Vulpes vulpes",
        "Fox",
        "",
        NA_character_
      ),
      stringsAsFactors = FALSE
    )
  )
}


test_that("trap-rate data are calculated for a selected species", {
  dat <- make_traprate_test_data()
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = "Vulpes vulpes",
    unit = "day"
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_named(
    result,
    c(
      "locationName",
      "latitude",
      "longitude",
      "n",
      "effort",
      "effort_unit",
      "scientificName"
    )
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  site_a <- result[
    result$locationName == "Site A",
    ,
    drop = FALSE
  ]
  
  site_b <- result[
    result$locationName == "Site B",
    ,
    drop = FALSE
  ]
  
  expect_equal(
    site_a$n,
    2
  )
  
  expect_equal(
    site_b$n,
    1
  )
  
  expect_identical(
    site_a$effort,
    1
  )
  
  expect_identical(
    site_b$effort,
    2
  )
  
  expect_identical(
    site_a$effort_unit,
    "day"
  )
  
  expect_identical(
    site_b$effort_unit,
    "day"
  )
  
  expect_identical(
    result$scientificName,
    rep("Vulpes vulpes", 2)
  )
})


test_that("trap-rate effort supports all available time units", {
  dat <- make_traprate_test_data()
  
  multipliers <- c(
    day = 1,
    hour = 24,
    minute = 1440,
    second = 86400
  )
  
  for (unit_name in names(multipliers)) {
    result <- ct_internal(".get_traprate_data")(
      dat = dat,
      species = "Vulpes vulpes",
      unit = unit_name
    )
    
    site_a <- result[
      result$locationName == "Site A",
      ,
      drop = FALSE
    ]
    
    site_b <- result[
      result$locationName == "Site B",
      ,
      drop = FALSE
    ]
    
    expect_identical(
      site_a$effort,
      unname(multipliers[[unit_name]])
    )
    
    expect_identical(
      site_b$effort,
      2 * unname(multipliers[[unit_name]])
    )
    
    expect_true(
      all(result$effort_unit == unit_name)
    )
  }
})


test_that("trap-rate data automatically retain scientific names", {
  dat <- make_traprate_test_data()
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = NULL,
    unit = "day"
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_equal(
    sum(result$n),
    3
  )
  
  expect_false(
    anyNA(result$n)
  )
  
  expect_false(
    any(result$n < 0)
  )
})


test_that("trap-rate data return an empty result when no species match", {
  dat <- make_traprate_test_data()
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = "Species not present",
    unit = "day"
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    nrow(result),
    0L
  )
  
  expect_named(
    result,
    c(
      "locationName",
      "latitude",
      "longitude",
      "n",
      "effort",
      "effort_unit",
      "scientificName"
    )
  )
  
  expect_type(
    result$locationName,
    "character"
  )
  
  expect_type(
    result$latitude,
    "double"
  )
  
  expect_type(
    result$n,
    "integer"
  )
})


test_that("trap-rate data return an empty result without observations", {
  dat <- make_traprate_test_data()
  dat$observations <- NULL
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = "Vulpes vulpes",
    unit = "day"
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    nrow(result),
    0L
  )
  
  expect_named(
    result,
    c(
      "locationName",
      "latitude",
      "longitude",
      "n",
      "effort",
      "effort_unit",
      "scientificName"
    )
  )
})


test_that("trap-rate data handle an empty observations table", {
  dat <- make_traprate_test_data()
  
  dat$observations <- data.frame(
    deploymentID = character(),
    scientificName = character(),
    stringsAsFactors = FALSE
  )
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = "Vulpes vulpes",
    unit = "day"
  )
  
  expect_identical(
    nrow(result),
    0L
  )
})


test_that("trap-rate data validate the main input tables", {
  dat <- make_traprate_test_data()
  
  missing_deployments <- dat
  missing_deployments$deployments <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_deployments,
      species = "Vulpes vulpes"
    ),
    "dat\\$deployments must be a data.frame"
  )
  
  missing_locations <- dat
  missing_locations$locations <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_locations,
      species = "Vulpes vulpes"
    ),
    "dat\\$locations must be a data.frame"
  )
  
  invalid_deployments <- dat
  invalid_deployments$deployments <- "not a data frame"
  
  expect_error(
    ct_internal(".get_traprate_data")(
      invalid_deployments,
      species = "Vulpes vulpes"
    ),
    "dat\\$deployments must be a data.frame"
  )
  
  invalid_locations <- dat
  invalid_locations$locations <- "not a data frame"
  
  expect_error(
    ct_internal(".get_traprate_data")(
      invalid_locations,
      species = "Vulpes vulpes"
    ),
    "dat\\$locations must be a data.frame"
  )
})


test_that("trap-rate data validate observation columns", {
  dat <- make_traprate_test_data()
  
  missing_deployment_id <- dat
  missing_deployment_id$observations$deploymentID <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_deployment_id,
      species = "Vulpes vulpes"
    ),
    "Missing required column.*deploymentID"
  )
  
  missing_scientific_name <- dat
  missing_scientific_name$observations$scientificName <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_scientific_name,
      species = "Vulpes vulpes"
    ),
    "Missing required column.*scientificName"
  )
})


test_that("trap-rate data validate deployment-location join columns", {
  dat <- make_traprate_test_data()
  
  missing_deployment_location_id <- dat
  
  missing_deployment_location_id$
    deployments$locationID <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_deployment_location_id,
      species = "Vulpes vulpes"
    ),
    "Missing join column.*in x.*locationID"
  )
  
  missing_location_location_id <- dat
  
  missing_location_location_id$
    locations$locationID <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_location_location_id,
      species = "Vulpes vulpes"
    ),
    "Missing join column.*in y.*locationID"
  )
})


test_that("trap-rate data validate columns required after joining", {
  dat <- make_traprate_test_data()
  
  missing_location_name <- dat
  missing_location_name$locations$locationName <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_location_name,
      species = "Vulpes vulpes"
    ),
    "Missing required column.*locationName"
  )
  
  missing_latitude <- dat
  missing_latitude$locations$latitude <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_latitude,
      species = "Vulpes vulpes"
    ),
    "Missing required column.*latitude"
  )
  
  missing_longitude <- dat
  missing_longitude$locations$longitude <- NULL
  
  expect_error(
    ct_internal(".get_traprate_data")(
      missing_longitude,
      species = "Vulpes vulpes"
    ),
    "Missing required column.*longitude"
  )
})


test_that("trap-rate data handle missing location names", {
  dat <- make_traprate_test_data()
  
  dat$locations$locationName[
    dat$locations$locationID == "L2"
  ] <- NA_character_
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = "Vulpes vulpes",
    unit = "day"
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_true(
    anyNA(result$locationName)
  )
  
  missing_location <- result[
    is.na(result$locationName),
    ,
    drop = FALSE
  ]
  
  expect_equal(
    missing_location$n,
    1
  )
  
  expect_identical(
    missing_location$effort,
    2
  )
})


test_that("trap-rate joins handle internal row-name collisions", {
  dat <- make_traprate_test_data()
  
  dat$deployments$.camr_row_id__ <- c(
    "existing-1",
    "existing-2"
  )
  
  result <- ct_internal(".get_traprate_data")(
    dat = dat,
    species = "Vulpes vulpes",
    unit = "day"
  )
  
  expect_identical(
    nrow(result),
    2L
  )
  
  expect_equal(
    sum(result$n),
    3
  )
})


test_that("trap-rate data reject an unsupported effort unit", {
  dat <- make_traprate_test_data()
  
  expect_error(
    ct_internal(".get_traprate_data")(
      dat = dat,
      species = "Vulpes vulpes",
      unit = "week"
    ),
    "arg.*one of"
  )
})


test_that("trap-rate data validate calculated effort output", {
  dat <- make_traprate_test_data()
  
  testthat::local_mocked_bindings(
    .calc_effort = function(...) {
      data.frame(
        deploymentID = c("D1", "D2"),
        stringsAsFactors = FALSE
      )
    },
    .package = "camtrapReport"
  )
  
  expect_error(
    ct_internal(".get_traprate_data")(
      dat = dat,
      species = "Vulpes vulpes",
      unit = "day"
    ),
    "\\.calc_effort\\(\\) must return deploymentID and effort"
  )
})
