make_merged_summary_fixture <- function() {
  list(
    data = list(
      deployments = data.frame(
        deploymentID = c("dep-1", "dep-2", "dep-3"),
        locationID = c("loc-1", "loc-1", "loc-2"),
        setupBy = c("Alice", "Bob", "Alice"),
        baitUse = c("none", "none", "food"),
        Year = c(2022, 2023, 2023),
        stringsAsFactors = FALSE
      ),
      
      locations = data.frame(
        locationID = c("loc-1", "loc-2"),
        locationName = c("Forest site", "Open site"),
        habitat = c("Mixed_Forest", "Other"),
        stringsAsFactors = FALSE
      ),
      
      sequences = data.frame(
        sequenceID = c("seq-1", "seq-2", "seq-3", "seq-4"),
        deploymentID = c("dep-1", "dep-1", "dep-2", "dep-3"),
        captureMethod = c(
          "timeLapse",
          "motionDetection",
          "motionDetection",
          "motionDetection"
        ),
        nrphotos = c(2, 3, 4, 5),
        stringsAsFactors = FALSE
      ),
      
      observations = data.frame(
        observationID = c("obs-1", "obs-2", "obs-3", "obs-4"),
        sequenceID = c("seq-1", "seq-2", "seq-3", "seq-4"),
        classifiedBy = c("Alice", "Bob", "Alice", "Carol"),
        taxonID = c("tax-1", "tax-2", "tax-1", "tax-3"),
        stringsAsFactors = FALSE
      ),
      
      taxonomy = data.frame(
        taxonID = c("tax-1", "tax-2", "tax-3"),
        scientificName = c(
          "Vulpes vulpes",
          "Capreolus capreolus",
          "Sus scrofa"
        ),
        stringsAsFactors = FALSE
      )
    )
  )
}


test_that("merged spatial summary returns the expected structure", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  
  expected_columns <- c(
    "locationID",
    "deploymentID",
    "deploymentID_List",
    "Num_Deployments",
    "CaptureMethod_List",
    "Setup_By_List",
    "Classify_By_List",
    "BaitUse_List",
    "Year_List",
    "Total_Photos",
    "Species_List",
    "Habitat_Type"
  )
  
  expect_true(
    all(expected_columns %in% names(result))
  )
  
  expect_false(anyNA(result$locationID))
  expect_false(anyNA(result$deploymentID))
  expect_false(anyNA(result$Num_Deployments))
  expect_false(anyNA(result$Total_Photos))
})


test_that("merged spatial summary calculates deployment counts", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  counts <- unique(
    result[
      ,
      c("locationID", "Num_Deployments"),
      drop = FALSE
    ]
  )
  
  loc1_count <- counts$Num_Deployments[
    counts$locationID == "loc-1"
  ]
  
  loc2_count <- counts$Num_Deployments[
    counts$locationID == "loc-2"
  ]
  
  expect_identical(
    as.integer(loc1_count),
    2L
  )
  
  expect_identical(
    as.integer(loc2_count),
    1L
  )
})


test_that("merged spatial summary lists deployment identifiers", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  loc1 <- result[
    result$locationID == "loc-1",
    ,
    drop = FALSE
  ]
  
  expect_gt(nrow(loc1), 0L)
  
  listed_ids <- trimws(
    unlist(
      strsplit(
        loc1$deploymentID_List[1],
        ",",
        fixed = TRUE
      )
    )
  )
  
  expect_setequal(
    listed_ids,
    c("dep-1", "dep-2")
  )
})


test_that("merged spatial summary aggregates capture methods", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  loc1 <- result[
    result$locationID == "loc-1",
    ,
    drop = FALSE
  ]
  
  capture_methods <- trimws(
    unlist(
      strsplit(
        loc1$CaptureMethod_List[1],
        ",",
        fixed = TRUE
      )
    )
  )
  
  expect_setequal(
    capture_methods,
    c("motionDetection", "timeLapse")
  )
})


test_that("merged spatial summary aggregates annotators", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  loc1 <- result[
    result$locationID == "loc-1",
    ,
    drop = FALSE
  ]
  
  classifiers <- trimws(
    unlist(
      strsplit(
        loc1$Classify_By_List[1],
        ",",
        fixed = TRUE
      )
    )
  )
  
  expect_setequal(
    classifiers,
    c("Alice", "Bob")
  )
})


test_that("merged spatial summary calculates photograph totals", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  totals <- unique(
    result[
      ,
      c("locationID", "Total_Photos"),
      drop = FALSE
    ]
  )
  
  loc1_total <- totals$Total_Photos[
    totals$locationID == "loc-1"
  ]
  
  loc2_total <- totals$Total_Photos[
    totals$locationID == "loc-2"
  ]
  
  expect_equal(
    as.numeric(loc1_total),
    9
  )
  
  expect_equal(
    as.numeric(loc2_total),
    5
  )
})


test_that("merged spatial summary creates species lists", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  loc1 <- result[
    result$locationID == "loc-1",
    ,
    drop = FALSE
  ]
  
  species <- trimws(
    unlist(
      strsplit(
        loc1$Species_List[1],
        ",",
        fixed = TRUE
      )
    )
  )
  
  expect_setequal(
    species,
    c(
      "Capreolus capreolus",
      "Vulpes vulpes"
    )
  )
})


test_that("merged spatial summary converts habitat labels", {
  cm <- make_merged_summary_fixture()
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  loc1_habitat <- unique(
    result$Habitat_Type[
      result$locationID == "loc-1"
    ]
  )
  
  loc2_habitat <- unique(
    result$Habitat_Type[
      result$locationID == "loc-2"
    ]
  )
  
  expect_identical(
    loc1_habitat,
    "Mixed Forest"
  )
  
  expect_identical(
    loc2_habitat,
    "Unclassified Habitat"
  )
})


test_that("merged spatial summary handles missing sequences", {
  cm <- make_merged_summary_fixture()
  cm$data$sequences <- NULL
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  
  expect_true(
    all(
      is.na(result$CaptureMethod_List) |
        result$CaptureMethod_List == ""
    )
  )
  
  expect_true(
    all(
      is.na(result$Classify_By_List) |
        result$Classify_By_List == ""
    )
  )
  
  expect_true(
    all(
      is.na(result$Species_List) |
        result$Species_List == ""
    )
  )
  
  expect_true(
    all(result$Total_Photos == 0)
  )
})


test_that("merged spatial summary handles missing observations", {
  cm <- make_merged_summary_fixture()
  cm$data$observations <- NULL
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  
  expect_true(
    all(
      is.na(result$Classify_By_List) |
        result$Classify_By_List == ""
    )
  )
  
  expect_true(
    all(
      is.na(result$Species_List) |
        result$Species_List == ""
    )
  )
})


test_that("merged spatial summary handles missing optional deployment columns", {
  cm <- make_merged_summary_fixture()
  
  cm$data$deployments$setupBy <- NULL
  cm$data$deployments$baitUse <- NULL
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
  
  expect_true(
    "Setup_By_List" %in% names(result)
  )
  
  expect_true(
    "BaitUse_List" %in% names(result)
  )
  
  expect_true(
    all(
      is.na(result$Setup_By_List) |
        result$Setup_By_List == ""
    )
  )
  
  expect_true(
    all(
      is.na(result$BaitUse_List) |
        result$BaitUse_List == ""
    )
  )
})


test_that("merged spatial summary handles absent habitat information", {
  cm <- make_merged_summary_fixture()
  cm$data$locations$habitat <- NULL
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  expect_true(
    "Habitat_Type" %in% names(result)
  )
  
  expect_true(
    all(is.na(result$Habitat_Type))
  )
})


test_that("merged spatial summary removes duplicate species names", {
  cm <- make_merged_summary_fixture()
  
  duplicate_observation <- cm$data$observations[1, , drop = FALSE]
  duplicate_observation$observationID <- "obs-duplicate"
  
  cm$data$observations <- rbind(
    cm$data$observations,
    duplicate_observation
  )
  
  result <- camtrapReport:::.camr_getMergedSummary(cm)
  
  loc1 <- result[
    result$locationID == "loc-1",
    ,
    drop = FALSE
  ]
  
  species <- trimws(
    unlist(
      strsplit(
        loc1$Species_List[1],
        ",",
        fixed = TRUE
      )
    )
  )
  
  expect_identical(
    sum(species == "Vulpes vulpes"),
    1L
  )
})
