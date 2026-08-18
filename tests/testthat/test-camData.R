test_that(
  "the bundled Leuven Camtrap DP example dataset is available and linked",
  {
  path <- camtrap_test_dataset()
  
  deployments <- data.table::fread(
    file.path(path, "deployments.csv")
  )
  
  media <- data.table::fread(
    file.path(path, "media.csv")
  )
  
  observations <- data.table::fread(
    file.path(path, "observations.csv")
  )
  
  media_event_ids <- sub(
    "^sequenceID:",
    "",
    media$mediaComments
  )
  
  expect_true(dir.exists(path))
  
  expect_true(
    all(
      file.exists(
        file.path(
          path,
          c(
            "datapackage.json",
            "deployments.csv",
            "media.csv",
            "observations.csv"
          )
        )
      )
    )
  )
  
  expect_identical(
    data.table::uniqueN(deployments$locationID),
    8L
  )
  
  expect_setequal(
    unique(deployments$locationID),
    c(
      "LEUVEN_416",
      "LEUVEN_881",
      "LEUVEN_930",
      "LEUVEN_1210",
      "LEUVEN_1304",
      "LEUVEN_1434",
      "LEUVEN_1473",
      "LEUVEN_1713"
    )
  )
  
  expect_setequal(
    unique(substr(deployments$deploymentStart, 1L, 4L)),
    c(
      "2018",
      "2019",
      "2020",
      "2021",
      "2022",
      "2023"
    )
  )
  
  expect_false(
    anyDuplicated(deployments$deploymentID) > 0L
  )
  
  expect_false(
    anyDuplicated(media$mediaID) > 0L
  )
  
  expect_false(
    anyDuplicated(observations$observationID) > 0L
  )
  
  expect_true(
    all(
      media$deploymentID %in%
        deployments$deploymentID
    )
  )
  
  expect_true(
    all(
      observations$deploymentID %in%
        deployments$deploymentID
    )
  )
  
  expect_true(
    all(
      observations$mediaID[
        observations$observationLevel == "media"
      ] %in% media$mediaID
    )
  )
  
  expect_true(
    all(
      observations$eventID %in%
        media_event_ids
    )
  )
  
  expect_setequal(
    unique(observations$observationType),
    c(
      "animal",
      "blank",
      "human",
      "unclassified",
      "unknown"
    )
  )
  }
)


test_that(
  "camData reads the bundled Leuven dataset into a complete camReport",
  {
  cm <- camtrap_test_report()
  
  expect_s4_class(
    cm,
    "camReport"
  )
  
  expect_named(
    cm$data,
    c(
      "observations",
      "deployments",
      "media",
      "locations",
      "sequences",
      "taxonomy"
    )
  )
  
  row_counts <- vapply(
    cm$data[
      c(
        "deployments",
        "observations",
        "media"
      )
    ],
    nrow,
    integer(1)
  )
  
  expect_identical(
    unname(row_counts),
    c(
      87L,
      10798L,
      16797L
    )
  )
  
  expect_identical(
    cm$siteName,
    "GMU8 LEUVEN"
  )
  
  expect_identical(
    nrow(cm$data$locations),
    8L
  )
  
  cache_dir <- file.path(
    tempdir(),
    "camtrapReport"
  )
  
  cache_files <- list.files(
    cache_dir,
    pattern = "__camReport_Object\\.rds$",
    full.names = TRUE
  )
  
  expect_gte(
    length(cache_files),
    1L
  )
  
  expect_true(
    all(file.exists(cache_files))
  )
  }
)


test_that("camData reuses its saved camReport object", {
  cm <- camtrap_test_report()
  
  cached <- camData(
    cm$info$directory
  )
  
  expect_s4_class(
    cached,
    "camReport"
  )
  
  expect_identical(
    cached$siteName,
    cm$siteName
  )
  
  expect_identical(
    nrow(cached$data$observations),
    nrow(cm$data$observations)
  )
}
)