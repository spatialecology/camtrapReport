make_sampling_cm <- function(
  sampling_design = character(),
  camera_model = character(),
  bait = character(),
  height = numeric(),
  capture_method = character(),
  individual_animals = character(),
  is_eow = FALSE
) {
  cm <- camR$new()

  cm$data <- list(
    deployments = data.frame(
      cameraModel = camera_model,
      baitUse = bait,
      cameraHeight = height,
      stringsAsFactors = FALSE
    )
  )

  cm$info <- list(
    is.EOW = is_eow,
    json = list(
      project = list(
        samplingDesign = sampling_design,
        captureMethod = capture_method,
        individualAnimals = individual_animals
      )
    )
  )

  cm$reportTextElements <- list()

  cm
}


test_that(
  "sampling text describes a single standard design",
  {
    cm <- make_sampling_cm(
      sampling_design = "simpleRandom",
      camera_model = "Model A",
      bait = "FALSE",
      height = 0.5,
      capture_method = "activityDetection",
      individual_animals = "FALSE",
      is_eow = TRUE
    )

    ct_internal(".get_sampling_text")(cm)

    text <- cm$reportTextElements$sampling

    expect_match(
      text,
      "EOW camera-trap protocol",
      fixed = TRUE
    )

    expect_match(
      text,
      "simple random",
      fixed = TRUE
    )

    expect_match(
      text,
      "Model A",
      fixed = TRUE
    )

    expect_match(
      text,
      "No bait was used",
      fixed = TRUE
    )

    expect_match(
      text,
      "0.5 m",
      fixed = TRUE
    )

    expect_match(
      text,
      "activity detection",
      fixed = TRUE
    )

    expect_match(
      text,
      "broader wildlife monitoring",
      fixed = TRUE
    )
  }
)


test_that(
  "sampling text handles multiple survey settings",
  {
    cm <- make_sampling_cm(
      sampling_design = c(
        "simpleRandom",
        "targeted",
        "opportunistic"
      ),
      camera_model = c(
        "A",
        "B",
        "C",
        "D"
      ),
      bait = c(
        "TRUE",
        "FALSE"
      ),
      height = c(
        0.2,
        0.5,
        1,
        1.5
      ),
      capture_method = c(
        "motionDetection",
        "timeLapse",
        "audio"
      ),
      individual_animals = c(
        "TRUE",
        "FALSE"
      )
    )

    ct_internal(".get_sampling_text")(cm)

    text <- cm$reportTextElements$sampling

    expect_match(
      text,
      "combines the following approaches",
      fixed = TRUE
    )

    expect_match(
      text,
      "Multiple camera models",
      fixed = TRUE
    )

    expect_match(
      text,
      "mixture of baited and unbaited",
      fixed = TRUE
    )

    expect_match(
      text,
      "ranging from 0.2 to 1.5",
      fixed = TRUE
    )

    expect_match(
      text,
      "motion detection",
      fixed = TRUE
    )

    expect_match(
      text,
      "time-lapse",
      fixed = TRUE
    )

    expect_match(
      text,
      "audio recording",
      fixed = TRUE
    )

    expect_match(
      text,
      "both the identification of individual animals",
      fixed = TRUE
    )
  }
)


test_that(
  "sampling text handles two designs and unknown designs",
  {
    cm_two <- make_sampling_cm(
      sampling_design = c(
        "systematicRandom",
        "experimental"
      )
    )

    ct_internal(".get_sampling_text")(cm_two)

    expect_match(
      cm_two$reportTextElements$sampling,
      "systematic random and experimental",
      fixed = TRUE
    )

    cm_unknown <- make_sampling_cm(
      sampling_design = "customDesign"
    )

    ct_internal(".get_sampling_text")(cm_unknown)

    expect_match(
      cm_unknown$reportTextElements$sampling,
      "customDesign",
      fixed = TRUE
    )
  }
)


test_that(
  "sampling text handles absent optional deployment metadata",
  {
    cm <- camR$new()

    cm$data <- list(
      deployments = data.frame(
        deploymentID = "d1",
        stringsAsFactors = FALSE
      )
    )

    cm$info <- list(
      is.EOW = FALSE,
      json = list(
        project = list(
          samplingDesign = character(),
          captureMethod = character(),
          individualAnimals = character()
        )
      )
    )

    cm$reportTextElements <- list()

    ct_internal(".get_sampling_text")(cm)

    expect_type(
      cm$reportTextElements$sampling,
      "character"
    )
  }
)


test_that(
  "get_speciesNames handles all supported group selections",
  {
    cm <- camR$new()

    cm$data <- list(
      observations = data.frame(
        scientificName = c(
          "Vulpes vulpes",
          "Cervus elaphus",
          "Vulpes vulpes"
        ),
        stringsAsFactors = FALSE
      )
    )

    cm$species_summary <- list(
      Mammals = list(
        site_list = data.frame(
          scientificName = c(
            "Vulpes vulpes",
            "Cervus elaphus"
          ),
          stringsAsFactors = FALSE
        )
      ),
      Birds = list(
        site_list = data.frame(
          scientificName = "Buteo buteo",
          stringsAsFactors = FALSE
        )
      ),
      count = data.frame(
        Name = c(
          "foxes",
          "raptors"
        ),
        Group = c(
          "Mammals",
          "Birds"
        ),
        stringsAsFactors = FALSE
      )
    )

    expect_setequal(
      cm$get_speciesNames(all = TRUE),
      c(
        "Vulpes vulpes",
        "Cervus elaphus"
      )
    )

    expect_setequal(
      cm$get_speciesNames(),
      c(
        "Vulpes vulpes",
        "Cervus elaphus",
        "Buteo buteo"
      )
    )

    expect_setequal(
      cm$get_speciesNames("Mammals"),
      c(
        "Vulpes vulpes",
        "Cervus elaphus"
      )
    )

    expect_setequal(
      cm$get_speciesNames("foxes"),
      c(
        "Vulpes vulpes",
        "Cervus elaphus"
      )
    )

    expect_setequal(
      cm$get_speciesNames(
        c(
          "Mammals",
          "raptors"
        )
      ),
      c(
        "Vulpes vulpes",
        "Cervus elaphus",
        "Buteo buteo"
      )
    )

    expect_error(
      cm$get_speciesNames("unknown"),
      "group is unknown",
      fixed = TRUE
    )

    expect_error(
      cm$get_speciesNames(
        c(
          "unknown-a",
          "unknown-b"
        )
      ),
      "group is unknown",
      fixed = TRUE
    )

    expect_error(
      cm$get_speciesNames(1),
      "group is unknown",
      fixed = TRUE
    )
  }
)


test_that(
  "get_speciesNames requires species summary information",
  {
    cm <- camR$new()

    cm$species_summary <- list()

    expect_error(
      cm$get_speciesNames(),
      "No species summay information",
      fixed = TRUE
    )
  }
)


test_that(
  "build_capture_table handles an empty capture summary",
  {
    testthat::local_mocked_bindings(
      .captures = function(pkg, by = NULL) {
        data.frame()
      },
      .package = "camtrapReport"
    )

    out <- ct_internal(".build_capture_table")(
      list(),
      "2025",
      "locationID"
    )

    expect_identical(
      names(out),
      c(
        "Species_Name",
        "scientificName",
        "Year",
        "Captures",
        "Capture_Rate",
        "RAI",
        "Locations"
      )
    )

    expect_equal(
      nrow(out),
      0
    )
  }
)


test_that(
  "build_capture_table validates taxonomic keys",
  {
    testthat::local_mocked_bindings(
      .captures = function(pkg, by = NULL) {
        data.frame(
          captures = 1,
          capture_rate = 1,
          rai = 1
        )
      },
      .package = "camtrapReport"
    )

    expect_error(
      ct_internal(".build_capture_table")(
        list(),
        "2025",
        "locationID"
      ),
      "No stable taxonomic key",
      fixed = TRUE
    )

    testthat::local_mocked_bindings(
      .captures = function(pkg, by = NULL) {
        data.frame(
          taxonID = "t1",
          captures = 1,
          capture_rate = 1,
          rai = 1
        )
      },
      .package = "camtrapReport"
    )

    expect_error(
      ct_internal(".build_capture_table")(
        list(),
        "2025",
        "locationID"
      ),
      "scientificName",
      fixed = TRUE
    )
  }
)


test_that(
  "build_capture_table counts locations and standardizes output",
  {
    species_summary <- data.frame(
      taxonID = c(
        "t2",
        "t1",
        "t1"
      ),
      scientificName = c(
        "Species zebra",
        "Species antelope",
        "Species antelope"
      ),
      vernacularNames.eng = c(
        "Zebra",
        "Antelope",
        "Antelope"
      ),
      captures = c(
        2,
        4,
        4
      ),
      capture_rate = c(
        0.2,
        0.4,
        0.4
      ),
      rai = c(
        2,
        4,
        4
      ),
      stringsAsFactors = FALSE
    )

    station_summary <- data.frame(
      taxonID = c(
        "t1",
        "t1",
        "t2"
      ),
      scientificName = c(
        "Species antelope",
        "Species antelope",
        "Species zebra"
      ),
      locationID = c(
        "L1",
        "L2",
        "L1"
      ),
      stringsAsFactors = FALSE
    )

    testthat::local_mocked_bindings(
      .captures = function(pkg, by = NULL) {
        if (is.null(by)) {
          species_summary
        } else {
          station_summary
        }
      },
      .package = "camtrapReport"
    )

    out <- ct_internal(".build_capture_table")(
      list(),
      "2025",
      "locationID"
    )

    expect_identical(
      out$Species_Name,
      c(
        "Antelope",
        "Zebra"
      )
    )

    expect_identical(
      out$Locations,
      c(
        2L,
        1L
      )
    )

    expect_true(
      all(out$Year == "2025")
    )

    expect_equal(
      nrow(out),
      2
    )
  }
)


test_that(
  "build_capture_table handles missing station summaries",
  {
    species_summary <- data.frame(
      taxonID = "t1",
      scientificName = "Species one",
      captures = 3,
      capture_rate = 0.3,
      rai = 3,
      stringsAsFactors = FALSE
    )

    testthat::local_mocked_bindings(
      .captures = function(pkg, by = NULL) {
        if (is.null(by)) {
          species_summary
        } else {
          data.frame()
        }
      },
      .package = "camtrapReport"
    )

    out <- ct_internal(".build_capture_table")(
      list(),
      "2025",
      "locationID"
    )

    expect_identical(
      out$Locations,
      0L
    )

    expect_identical(
      out$Species_Name,
      "Species one"
    )
  }
)


test_that(
  "build_capture_table validates station-level taxonomic keys",
  {
    species_summary <- data.frame(
      taxonID = "t1",
      scientificName = "Species one",
      captures = 3,
      capture_rate = 0.3,
      rai = 3,
      stringsAsFactors = FALSE
    )

    station_summary <- data.frame(
      scientificName = "Species one",
      locationID = "L1",
      stringsAsFactors = FALSE
    )

    testthat::local_mocked_bindings(
      .captures = function(pkg, by = NULL) {
        if (is.null(by)) {
          species_summary
        } else {
          station_summary
        }
      },
      .package = "camtrapReport"
    )

    expect_error(
      ct_internal(".build_capture_table")(
        list(),
        "2025",
        "locationID"
      ),
      "Missing key column",
      fixed = TRUE
    )
  }
)
