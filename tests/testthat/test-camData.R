test_that("the bundled Camtrap DP toy dataset is available and linked", {
  path <- camtrap_test_dataset()
  deployments <- data.table::fread(file.path(path, "deployments.csv"))
  media <- data.table::fread(file.path(path, "media.csv"))
  observations <- data.table::fread(file.path(path, "observations.csv"))
  metadata <- jsonlite::fromJSON(file.path(path, "datapackage.json"))
  media_event_ids <- sub("^sequenceID:", "", media$mediaComments)
  contributor_emails <- metadata$contributors$email
  contributor_emails <- contributor_emails[
    !is.na(contributor_emails) & nzchar(contributor_emails)
  ]

  expect_true(dir.exists(path))
  expect_true(all(file.exists(file.path(
    path,
    c("datapackage.json", "deployments.csv", "media.csv", "observations.csv")
  ))))
  expect_false(file.exists(file.path(path, "__camReport_Object.rds")))
  expect_identical(metadata$name, "camtrapreport-toy-dataset")
  expect_match(metadata$project$title, "Toy", fixed = TRUE)
  expect_match(metadata$project$description, "fictional example.org", fixed = TRUE)
  expect_true(all(grepl("@example\\.org$", contributor_emails)))
  expect_identical(data.table::uniqueN(deployments$locationID), 8L)
  expect_setequal(
    unique(substr(deployments$deploymentStart, 1L, 4L)),
    c("2022", "2023", "2024")
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
    c("animal", "blank", "human", "unclassified", "unknown", "vehicle")
  )
})

test_that("camData reads the bundled toy dataset into a complete camReport", {
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

  expect_identical(unname(row_counts), c(24L, 444L, 4616L))
  expect_identical(cm$siteName, metadata$project$title)
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
