run_temporal_coverage <- function(deployments, observations) {
  cm <- camR$new()

  cm$data <- list(
    deployments = deployments,
    observations = observations
  )

  cm$data_status <- list()

  ct_internal(".Temporal")(cm)

  cm$data_status$Temporal
}


run_essentials_coverage <- function(data) {
  cm <- camR$new()

  cm$data <- data
  cm$data_status <- list()

  ct_internal(".Essentials")(cm)

  cm$data_status$Essentials
}


test_that(
  "Temporal handles a single deployment without gaps",
  {
    deployments <- data.frame(
      deploymentID = "d1",
      deployment_interval = "2020-01-01--2020-01-03",
      deploymentStart = "2020-01-01 00:00:00",
      deploymentEnd = "2020-01-03 00:00:00",
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = "2020-01-02 12:00:00",
      stringsAsFactors = FALSE
    )

    out <- run_temporal_coverage(
      deployments,
      observations
    )

    expect_identical(
      out$dep_years,
      "2020"
    )

    expect_match(
      out$dep_calendar_coverage,
      "3 of 3 days"
    )

    expect_match(
      out$dep_max_gap,
      "None"
    )

    expect_match(
      out$dep_min_gap,
      "None"
    )

    expect_gt(
      length(out$dep_month_coverage_lines),
      0
    )

    expect_match(
      out$message_first_last,
      "on/after"
    )
  }
)


test_that(
  "Temporal identifies reversed intervals, gaps, and temporal problems",
  {
    deployments <- data.frame(
      deploymentID = c(
        "d1",
        "d2",
        "d3"
      ),
      deployment_interval = c(
        "2020-01-03--2020-01-01",
        "2020-01-10--2020-01-10",
        "2040-02-01--2040-03-01"
      ),
      deploymentStart = c(
        "2020-01-01 00:00:00",
        "2020-01-10 00:00:00",
        "2040-02-01 00:00:00"
      ),
      deploymentEnd = c(
        "2020-01-03 00:00:00",
        "2020-01-10 00:00:00",
        "2040-03-01 00:00:00"
      ),
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = c(
        "2019-12-31 12:00:00",
        "2020-01-02 12:00:00",
        "2020-01-05",
        rep(
          "2100-01-01 12:00:00",
          11
        )
      ),
      stringsAsFactors = FALSE
    )

    out <- run_temporal_coverage(
      deployments,
      observations
    )

    expect_match(
      out$dep_missing_intervals,
      "1 invalid"
    )

    expect_match(
      out$dep_zero_length,
      "1 zero-length"
    )

    expect_match(
      out$dep_max_gap,
      "days"
    )

    expect_false(
      grepl(
        "Same as max gap",
        out$dep_min_gap,
        fixed = TRUE
      )
    )

    expect_match(
      out$dep_years_message,
      "missing:"
    )

    expect_match(
      out$temporal_outliers,
      "Years:"
    )

    expect_match(
      out$temporal_inconsistency,
      "Deployments exist"
    )

    expect_match(
      out$temporal_inconsistency,
      "Observations exist"
    )

    expect_match(
      out$message_first_last,
      "earlier than the first deployment"
    )

    expect_match(
      out$invalid_timestamp_format,
      "1 timestamp"
    )

    expect_match(
      out$obs_future_timestamps,
      "11 observation"
    )

    expect_match(
      out$obs_future_timestamps,
      "\\.\\.\\."
    )

    expect_gt(
      length(out$dep_month_coverage_lines),
      0
    )
  }
)


test_that(
  "Temporal reports deployments without gaps",
  {
    deployments <- data.frame(
      deploymentID = c(
        "d1",
        "d2"
      ),
      deployment_interval = c(
        "2020-01-01--2020-01-03",
        "2020-01-04--2020-01-06"
      ),
      deploymentStart = c(
        "2020-01-01 00:00:00",
        "2020-01-04 00:00:00"
      ),
      deploymentEnd = c(
        "2020-01-03 00:00:00",
        "2020-01-06 00:00:00"
      ),
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = c(
        "2020-01-02 12:00:00",
        "2020-01-05 12:00:00"
      ),
      stringsAsFactors = FALSE
    )

    out <- run_temporal_coverage(
      deployments,
      observations
    )

    expect_match(
      out$dep_max_gap,
      "0 days"
    )

    expect_match(
      out$dep_min_gap,
      "no gaps"
    )

    expect_match(
      out$temporal_inconsistency,
      "are the same"
    )

    expect_match(
      out$message_first_last,
      "on/after"
    )

    expect_match(
      out$obs_future_timestamps,
      "None"
    )

    expect_match(
      out$invalid_timestamp_format,
      "None"
    )
  }
)


test_that(
  "Temporal reports a single deployment gap",
  {
    deployments <- data.frame(
      deploymentID = c(
        "d1",
        "d2"
      ),
      deployment_interval = c(
        "2020-01-01--2020-01-03",
        "2020-01-05--2020-01-06"
      ),
      deploymentStart = c(
        "2020-01-01 00:00:00",
        "2020-01-05 00:00:00"
      ),
      deploymentEnd = c(
        "2020-01-03 00:00:00",
        "2020-01-06 00:00:00"
      ),
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = "2020-01-02 12:00:00",
      stringsAsFactors = FALSE
    )

    out <- run_temporal_coverage(
      deployments,
      observations
    )

    expect_match(
      out$dep_max_gap,
      "1 days"
    )

    expect_match(
      out$dep_min_gap,
      "Same as max gap"
    )
  }
)


test_that(
  "Temporal formats isolated missing years",
  {
    deployments <- data.frame(
      deploymentID = c(
        "d1",
        "d2"
      ),
      deployment_interval = c(
        "2020-01-01--2020-01-02",
        "2022-01-01--2022-01-02"
      ),
      deploymentStart = c(
        "2020-01-01 00:00:00",
        "2022-01-01 00:00:00"
      ),
      deploymentEnd = c(
        "2020-01-02 00:00:00",
        "2022-01-02 00:00:00"
      ),
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = c(
        "2020-01-01 12:00:00",
        "2022-01-01 12:00:00"
      ),
      stringsAsFactors = FALSE
    )

    out <- run_temporal_coverage(
      deployments,
      observations
    )

    expect_match(
      out$dep_years_message,
      "2021"
    )
  }
)


test_that(
  "Essentials reports completely missing core tables",
  {
    out <- run_essentials_coverage(
      list()
    )

    expect_match(
      out$loc$long,
      "missing cm\\$data\\$locations"
    )

    expect_match(
      out$loc$lat,
      "missing cm\\$data\\$locations"
    )

    expect_match(
      out$obs$status,
      "Missing table"
    )

    expect_match(
      out$dep$status,
      "Missing table"
    )
  }
)


test_that(
  "Essentials handles missing columns across data tables",
  {
    one_column <- data.frame(
      x = 1,
      stringsAsFactors = FALSE
    )

    out <- run_essentials_coverage(
      list(
        locations = one_column,
        observations = one_column,
        deployments = one_column,
        media = one_column,
        sequences = one_column,
        taxonomy = one_column
      )
    )

    expect_match(
      out$loc$long,
      "missing longitude"
    )

    expect_match(
      out$loc$lat,
      "missing latitude"
    )

    expect_match(
      out$obs$timestamp,
      "Missing column: timestamp"
    )

    expect_match(
      out$obs$obsType_status,
      "Missing column"
    )

    expect_match(
      out$obs$behavior,
      "cannot filter animals"
    )

    expect_match(
      out$dep$depID,
      "Missing column"
    )

    expect_match(
      out$dep$setupBy_status,
      "Missing column"
    )

    expect_match(
      out$media$file.path,
      "Missing column"
    )

    expect_match(
      out$seq$captureMethod,
      "Missing column"
    )

    expect_match(
      out$tax$taxonID,
      "Missing column"
    )

    expect_match(
      out$tax$scientificName,
      "Missing column"
    )
  }
)


test_that(
  "Essentials detects mixed coordinates and incomplete metadata",
  {
    locations <- data.frame(
      locationID = c(
        "",
        "L2",
        "L3",
        "L4",
        "L5",
        "L6"
      ),
      locationName = c(
        "A",
        "",
        "C",
        "D",
        "E",
        "F"
      ),
      longitude = c(
        "5",
        "6",
        "5000",
        "6000",
        "oops",
        ""
      ),
      latitude = c(
        "52",
        "53",
        "5200",
        "5300",
        "",
        ""
      ),
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = c(
        "2020-01-01",
        "2100-01-01 12:00:00",
        "2020-01-04",
        "2020-01-03 12:00:00"
      ),
      observationType = c(
        "animal",
        "animal",
        "unknown",
        "human"
      ),
      count = c(
        NA,
        1,
        2,
        3
      ),
      classifiedBy = c(
        "",
        "machine",
        NA,
        "person"
      ),
      classificationMethod = c(
        "human",
        "machine",
        NA,
        "human"
      ),
      taxonID = c(
        "",
        "tx2",
        "tx3",
        "tx4"
      ),
      behavior = c(
        "",
        "",
        "",
        ""
      ),
      sex = c(
        "F",
        "M",
        "",
        ""
      ),
      lifeStage = c(
        "adult",
        "",
        "",
        ""
      ),
      individualPositionAngle = c(
        "1",
        "bad",
        "",
        ""
      ),
      individualPositionRadius = c(
        NA,
        NA,
        NA,
        NA
      ),
      individualSpeed = c(
        "1",
        "2",
        "",
        ""
      ),
      individualID = c(
        "",
        "",
        "",
        ""
      ),
      stringsAsFactors = FALSE
    )

    deployments <- data.frame(
      deploymentID = c(
        "d1",
        ""
      ),
      locationID = c(
        "L1",
        ""
      ),
      baitUse = c(
        "",
        "none"
      ),
      cameraHeight = c(
        NA,
        1
      ),
      habitat = c(
        "",
        "forest"
      ),
      deployment_interval = c(
        "2020-01-01",
        "2020-01-02--2020-01-01"
      ),
      deploymentStart = c(
        "2020-01-01",
        "2100-01-01 00:00:00"
      ),
      deploymentEnd = c(
        "2020-01-02 00:00:00",
        "2020-01-03 00:00:00"
      ),
      setupBy = c(
        "",
        "observer"
      ),
      stringsAsFactors = FALSE
    )

    media <- data.frame(
      comments = c(
        "",
        "ok"
      ),
      favourite = c(
        NA,
        TRUE
      ),
      filePath = c(
        "",
        "image.jpg"
      ),
      timestamp = c(
        "2020-01-01",
        "2100-01-01 00:00:00"
      ),
      stringsAsFactors = FALSE
    )

    sequences <- data.frame(
      captureMethod = c(
        "",
        "video"
      ),
      nrphotos = c(
        NA,
        2
      ),
      stringsAsFactors = FALSE
    )

    taxonomy <- data.frame(
      taxonID = c(
        "",
        "tx2"
      ),
      scientificName = c(
        "",
        "Species two"
      ),
      vernacularNames.eng = c(
        "",
        "two"
      ),
      stringsAsFactors = FALSE
    )

    out <- run_essentials_coverage(
      list(
        locations = locations,
        observations = observations,
        deployments = deployments,
        media = media,
        sequences = sequences,
        taxonomy = taxonomy
      )
    )

    expect_match(
      out$loc$long,
      "different coordinate system"
    )

    expect_match(
      out$loc$long,
      "rows 3, 4"
    )

    expect_match(
      out$obs$timestamp,
      "invalid format"
    )

    expect_match(
      out$obs$timestamp,
      "future"
    )

    expect_match(
      out$obs$obsType_status,
      "Partial"
    )

    expect_match(
      out$obs$taxonID,
      "Partial"
    )

    expect_match(
      out$obs$behavior,
      "Incomplete"
    )

    expect_match(
      out$obs$sex,
      "Complete"
    )

    expect_match(
      out$obs$lifeStage,
      "Partial"
    )

    expect_match(
      out$obs$angle,
      "Partial"
    )

    expect_match(
      out$obs$radius,
      "Incomplete"
    )

    expect_match(
      out$obs$speed,
      "Complete"
    )

    expect_match(
      out$dep$dep_interval,
      "Incomplete"
    )

    expect_match(
      out$dep$depStart,
      "invalid format"
    )

    expect_match(
      out$dep$depStart,
      "future"
    )

    expect_match(
      out$tax$nld,
      "Missing column"
    )
  }
)


test_that(
  "Essentials handles empty but structurally valid tables",
  {
    locations <- data.frame(
      longitude = character(),
      latitude = character(),
      locationID = character(),
      locationName = character(),
      stringsAsFactors = FALSE
    )

    observations <- data.frame(
      timestamp = character(),
      observationType = character(),
      count = numeric(),
      classifiedBy = character(),
      classificationMethod = character(),
      taxonID = character(),
      behavior = character(),
      sex = character(),
      lifeStage = character(),
      individualPositionAngle = numeric(),
      individualPositionRadius = numeric(),
      individualSpeed = numeric(),
      individualID = character(),
      stringsAsFactors = FALSE
    )

    deployments <- data.frame(
      deploymentID = character(),
      locationID = character(),
      baitUse = character(),
      cameraHeight = numeric(),
      habitat = character(),
      deployment_interval = NULL,
      deploymentStart = character(),
      deploymentEnd = character(),
      setupBy = character(),
      stringsAsFactors = FALSE
    )

    media <- data.frame(
      comments = character(),
      favourite = logical(),
      filePath = character(),
      timestamp = character(),
      stringsAsFactors = FALSE
    )

    sequences <- data.frame(
      captureMethod = character(),
      nrphotos = numeric(),
      stringsAsFactors = FALSE
    )

    taxonomy <- data.frame(
      taxonID = character(),
      scientificName = character(),
      vernacularNames.eng = character(),
      vernacularNames.nld = character(),
      stringsAsFactors = FALSE
    )

    out <- run_essentials_coverage(
      list(
        locations = locations,
        observations = observations,
        deployments = deployments,
        media = media,
        sequences = sequences,
        taxonomy = taxonomy
      )
    )

    expect_match(
      out$loc$locID,
      "No data"
    )

    expect_match(
      out$obs$timestamp,
      "No data"
    )

    expect_match(
      out$obs$behavior,
      "No animal observations"
    )

    expect_match(
      out$dep$depID,
      "No data"
    )

    expect_match(
      out$media$comments,
      "No data"
    )

    expect_match(
      out$seq$captureMethod,
      "No data"
    )

    expect_match(
      out$tax$scientificName,
      "No data"
    )
  }
)


test_that(
  "Essentials abbreviates long lists of problematic rows",
  {
    n <- 25

    locations <- data.frame(
      locationID = c(
        rep("", n - 1),
        "L25"
      ),
      locationName = paste0(
        "site",
        seq_len(n)
      ),
      longitude = rep(
        "5",
        n
      ),
      latitude = rep(
        "52",
        n
      ),
      stringsAsFactors = FALSE
    )

    out <- run_essentials_coverage(
      list(
        locations = locations
      )
    )

    expect_match(
      out$loc$locID,
      "\\.\\.\\."
    )

    expect_match(
      out$loc$locID,
      "Partial"
    )
  }
)
