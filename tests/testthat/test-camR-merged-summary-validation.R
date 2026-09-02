make_minimal_merged_summary_fixture <- function() {
  list(
    data = list(
      deployments = data.frame(
        deploymentID = "dep-1",
        locationID = "loc-1",
        setupBy = "Observer",
        baitUse = "none",
        Year = 2024,
        stringsAsFactors = FALSE
      ),
      locations = data.frame(
        locationID = "loc-1",
        locationName = "Location 1",
        stringsAsFactors = FALSE
      ),
      sequences = data.frame(
        sequenceID = "seq-1",
        deploymentID = "dep-1",
        captureMethod = "motionDetection",
        nrphotos = 2,
        stringsAsFactors = FALSE
      ),
      observations = data.frame(
        observationID = "obs-1",
        sequenceID = "seq-1",
        classifiedBy = "Observer",
        taxonID = "tax-1",
        stringsAsFactors = FALSE
      ),
      taxonomy = data.frame(
        taxonID = "tax-1",
        scientificName = "Vulpes vulpes",
        stringsAsFactors = FALSE
      )
    )
  )
}


test_that("merged summary rejects missing deployment join column", {
  cm <- make_minimal_merged_summary_fixture()
  cm$data$deployments$locationID <- NULL
  
  expect_error(
    .camr_getMergedSummary(cm),
    "Missing join column(s) in x: locationID",
    fixed = TRUE
  )
})


test_that("merged summary rejects missing location join column", {
  cm <- make_minimal_merged_summary_fixture()
  cm$data$locations$locationID <- NULL
  
  expect_error(
    .camr_getMergedSummary(cm),
    "Missing join column(s) in y: locationID",
    fixed = TRUE
  )
})


test_that("merged summary rejects non-data-frame deployments", {
  cm <- make_minimal_merged_summary_fixture()
  cm$data$deployments <- list()
  
  expect_error(
    .camr_getMergedSummary(cm),
    "'x' must be a data.frame.",
    fixed = TRUE
  )
})


test_that("merged summary rejects non-data-frame locations", {
  cm <- make_minimal_merged_summary_fixture()
  cm$data$locations <- list()
  
  expect_error(
    .camr_getMergedSummary(cm),
    "'y' must be a data.frame.",
    fixed = TRUE
  )
})


test_that("merged summary ignores invalid sequence structures", {
  cm <- make_minimal_merged_summary_fixture()
  
  cm$data$sequences <- data.frame(
    unrelated = "value",
    stringsAsFactors = FALSE
  )
  
  result <- .camr_getMergedSummary(cm)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0L)
  
  expect_true(
    all(
      is.na(result$CaptureMethod_List) |
        result$CaptureMethod_List == ""
    )
  )
  
  expect_true(
    all(result$Total_Photos == 0)
  )
})


test_that("merged summary ignores incomplete taxonomy", {
  cm <- make_minimal_merged_summary_fixture()
  
  cm$data$taxonomy <- data.frame(
    taxonID = "tax-1",
    stringsAsFactors = FALSE
  )
  
  result <- .camr_getMergedSummary(cm)
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0L)
  
  expect_true(
    all(
      is.na(result$Species_List) |
        result$Species_List == ""
    )
  )
})


test_that("merged summary excludes invalid one-word taxa", {
  cm <- make_minimal_merged_summary_fixture()
  
  cm$data$taxonomy$scientificName <- "Unknown"
  
  result <- .camr_getMergedSummary(cm)
  
  expect_true(
    all(
      is.na(result$Species_List) |
        result$Species_List == ""
    )
  )
})


