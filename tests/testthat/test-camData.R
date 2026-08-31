test_that("the bundled Leuven Camtrap DP subset is available and linked", {
  path <- camtrap_test_dataset()
  deployments <- data.table::fread(file.path(path, "deployments.csv"))
  media <- data.table::fread(file.path(path, "media.csv"))
  observations <- data.table::fread(file.path(path, "observations.csv"))
  metadata <- jsonlite::fromJSON(file.path(path, "datapackage.json"))
  media_event_ids <- sub("^sequenceID:", "", media$mediaComments)
  expect_true(dir.exists(path))
  expect_true(all(file.exists(file.path(
    path,
    c("datapackage.json", "deployments.csv", "media.csv", "observations.csv")
  ))))
  expect_false(file.exists(file.path(path, "__camReport_Object.rds")))
  expect_identical(metadata$name, "gmu8_leuven_camtrapreport_subset")
  expect_identical(metadata$project$title, "GMU8_LEUVEN")
  expect_match(metadata$title, "representative subset", fixed = TRUE)
  expect_match(metadata$project$description, "south of Leuven", fixed = TRUE)
  expect_false("email" %in% names(metadata$contributors))
  expect_identical(
    c(
      deployments = nrow(deployments),
      media = nrow(media),
      observations = nrow(observations)
    ),
    c(deployments = 87L, media = 15492L, observations = 11092L)
  )
  expect_identical(data.table::uniqueN(deployments$locationID), 8L)
  expect_setequal(
    unique(deployments$locationID),
    paste0("LEUVEN_", c(416, 881, 930, 1210, 1304, 1434, 1473, 1713))
  )
  expect_setequal(
    unique(substr(deployments$deploymentStart, 1L, 4L)),
    as.character(2018:2023)
  )
  expect_false(anyDuplicated(deployments$deploymentID) > 0L)
  expect_false(anyDuplicated(media$mediaID) > 0L)
  expect_false(anyDuplicated(observations$observationID) > 0L)
  expect_true(all(media$deploymentID %in% deployments$deploymentID))
  expect_true(all(observations$deploymentID %in% deployments$deploymentID))
  expect_true(all(
    observations$mediaID[observations$observationLevel == "media"] %in%
      media$mediaID
  ))
  expect_true(all(observations$eventID %in% media_event_ids))
  expect_setequal(
    unique(observations$observationType),
    c("animal", "blank", "human", "unclassified", "unknown")
  )
  expect_identical(data.table::uniqueN(observations$eventID), 10182L)
})

test_that("camData reads the Leuven subset into a complete camReport", {
  cm <- camtrap_test_report()
  metadata <- jsonlite::fromJSON(file.path(
    camtrap_test_dataset(),
    "datapackage.json"
  ))

  expect_s4_class(cm, "camReport")
  expect_named(
    cm$data,
    c("observations", "deployments", "media", "locations", "sequences", "taxonomy")
  )

  row_counts <- vapply(
    cm$data[c("deployments", "observations", "media")],
    nrow,
    integer(1)
  )

  expect_identical(unname(row_counts), c(87L, 10798L, 16797L))
  expect_identical(cm$siteName, "GMU8 LEUVEN")
  expect_false(identical(
    normalizePath(cm$info$directory, winslash = "/"),
    normalizePath(camtrap_test_dataset(), winslash = "/")
  ))
  expect_true(file.exists(file.path(
    cm$info$directory,
    "__camReport_Object.rds"
  )))
})

test_that("camData reuses its saved camReport object", {
  cm <- camtrap_test_report()

  cached <- camData(cm$info$directory)

  expect_s4_class(cached, "camReport")
  expect_identical(cached$siteName, cm$siteName)
  expect_identical(nrow(cached$data$observations), nrow(cm$data$observations))
})
