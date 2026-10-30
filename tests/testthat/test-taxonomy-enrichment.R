
test_that("taxonomy enrichment requires explicit consent and preserves data", {
  path <- camtrap_test_dataset()
  require_package <- .require
  
  query_state <- new.env(parent = emptyenv())
  query_state$queried <- NULL
  
  testthat::local_mocked_bindings(
    .require = function(package) {
      if (identical(package, "taxize")) {
        TRUE
      } else {
        require_package(package)
      }
    },
    .getMissingTaxon_GBIF = function(x) {
      query_state$queried <- x
      
      data.frame(
        scientificName = x,
        class = "Mammalia",
        order = "Synthetic order",
        stringsAsFactors = FALSE
      )
    },
    .package = "camtrapReport"
  )
  
  # Offline import must not invoke GBIF enrichment.
  offline <- capture_report_conditions(
    .read_camdp(path, enrich_taxonomy = FALSE)
  )
  
  expect_null(query_state$queried)
  expect_length(offline$messages, 0L)
  
  # The default is offline even when taxize is available.
  default <- capture_report_conditions(.read_camdp(path))
  expect_null(query_state$queried)
  expect_identical(default$value$data, offline$value$data)
  expect_length(default$messages, 0L)

  # Only explicit consent invokes GBIF enrichment.
  online <- capture_report_conditions(
    .read_camdp(path, enrich_taxonomy = TRUE)
  )
  
  expect_setequal(
    query_state$queried,
    offline$value$data$taxonomy$scientificName
  )
  
  expect_true(
    all(online$value$data$taxonomy$class == "Mammalia")
  )
  
  expect_identical(
    online$value$data$taxonomy$order,
    offline$value$data$taxonomy$order
  )
  
  # Other Camtrap-DP tables must remain unchanged.
  other_tables <- setdiff(
    names(offline$value$data),
    "taxonomy"
  )
  
  expect_identical(
    online$value$data[other_tables],
    offline$value$data[other_tables]
  )
  
  expect_true(any(grepl(
    "scientific names to GBIF",
    online$messages,
    fixed = TRUE
  )))
  
  expect_true(any(grepl(
    "enrich_taxonomy = FALSE",
    online$messages,
    fixed = TRUE
  )))
})


test_that("offline import does not even check or invoke taxize", {
  require_package <- .require
  
  testthat::local_mocked_bindings(
    .require = function(package) {
      if (identical(package, "taxize")) {
        stop("Taxize must not be consulted")
      }
      
      require_package(package)
    },
    .getMissingTaxon_GBIF = function(...) {
      stop("Network lookup must not run")
    },
    .package = "camtrapReport"
  )
  
  value <- .read_camdp(camtrap_test_dataset())
  explicit <- .read_camdp(camtrap_test_dataset(), enrich_taxonomy = FALSE)
  expect_identical(value$data, explicit$data)
  
  expect_true(
    nrow(value$data$observations) > 0L
  )
  
  expect_true(
    all(is.na(value$data$taxonomy$class))
  )
  
  expect_true(any(
    value$data$taxonomy$order %in%
      c("Carnivora", "Artiodactyla")
  ))
})


test_that("camData forwards explicit and default taxonomy choices", {
  selection_state <- new.env(parent = emptyenv())
  selection_state$selected <- NULL
  original_reader <- .read_camdp
  
  testthat::local_mocked_bindings(
    .read_camdp = function(file, enrich_taxonomy, overwrite = FALSE) {
      selection_state$selected <- enrich_taxonomy
      
      stop("Reader forwarding probe", call. = FALSE)
    },
    .package = "camtrapReport"
  )
  
  path <- camtrap_test_dataset()
  
  # Default must not enable taxonomy enrichment.
  expect_error(
    suppressMessages(
      camData(path, update = TRUE)
    ),
    "Reader forwarding probe",
    fixed = TRUE
  )
  
  expect_identical(
    selection_state$selected,
    FALSE
  )
  
  # Explicit TRUE authorizes enrichment.
  expect_error(
    suppressMessages(
      camData(
        path,
        update = TRUE,
        enrich_taxonomy = TRUE
      )
    ),
    "Reader forwarding probe",
    fixed = TRUE
  )
  
  expect_identical(
    selection_state$selected,
    TRUE
  )

  expect_error(
    suppressMessages(camData(path, update = TRUE, enrich_taxonomy = FALSE)),
    "Reader forwarding probe", fixed = TRUE
  )
  expect_identical(selection_state$selected, FALSE)
  
  # Invalid arguments must be rejected.
  for (invalid in list(
    NA, NULL, c(TRUE, FALSE), "FALSE", 0L
  )) {
    expect_error(
      camData(path, enrich_taxonomy = invalid),
      "'enrich_taxonomy' must be TRUE or FALSE",
      fixed = TRUE
    )
    expect_error(
      original_reader(path, enrich_taxonomy = invalid),
      "'enrich_taxonomy' must be TRUE or FALSE",
      fixed = TRUE
    )
  }
})


test_that("offline camData retains counts and order-based report groups", {
  cm <- camtrap_test_report()
  
  expect_true(
    "large_mammals" %in% names(cm$group_definition)
  )
  
  expect_gt(
    length(cm$get_speciesNames("large_mammals")),
    0L
  )
  
  expect_identical(
    vapply(
      cm$data[c("deployments", "observations", "media")],
      nrow,
      integer(1)
    ),
    c(
      deployments = 87L,
      observations = 10798L,
      media = 16797L
    )
  )
})
