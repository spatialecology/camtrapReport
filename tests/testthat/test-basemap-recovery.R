test_that("offline backgrounds never request a browser tile layer", {
  cm <- camR$new()
  calls <- new.env(parent = emptyenv())
  calls$code <- character()
  calls$config <- NULL
  result <- testthat::with_mocked_bindings(
    cm$add_report_basemap(list(), bounds = c(4, 50, 5, 51)),
    .eval = function(code, env) {
      calls$code <- c(calls$code, code)
      if (grepl("htmlwidgets::onRender", code, fixed = TRUE)) {
        calls$config <- get("config", envir = env, inherits = FALSE)
      }
      get("map", envir = env, inherits = FALSE)
    },
    .package = "camtrapReport"
  )
  expect_type(result, "list")
  expect_length(calls$config$providers, 0L)
  expect_identical(sum(grepl("addPolylines", calls$code, fixed = TRUE)), 12L)
  expect_true(any(grepl("addScaleBar", calls$code, fixed = TRUE)))
  expect_error(
    cm$add_report_basemap(list(), c(NA, 50, 5, 51)),
    "finite lng1"
  )
})

test_that("online sources retain explicit attribution and finite deadlines", {
  sources <- .report_basemap_sources("openstreetmap")
  expect_identical(sources[[1]]$url,
                   "https://tile.openstreetmap.org/{z}/{x}/{y}.png")
  expect_true(sources[[1]]$requiresWebReferrer)
  expect_match(sources[[2]]$attribution, "CC-BY-SA", fixed = TRUE)
  expect_match(sources[[2]]$attribution, "SRTM", fixed = TRUE)
  expect_identical(
    .report_basemap_sources("cartodb")[[1]]$name,
    "CartoDB.Positron"
  )
  expect_identical(
    .report_basemap_sources("esri")[[1]]$name,
    "Esri.WorldGrayCanvas"
  )
  expect_length(.report_basemap_sources("offline"), 0L)
})

test_that("browser tile recovery is finite and preserves overlay groups", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is needed for this optional JS runtime test")
  script <- test_path("fixtures", "basemap-runtime-test.js")
  runtime <- system.file(
    "report-assets", "basemap-recovery.js", package = "camtrapReport",
    mustWork = TRUE
  )
  output <- system2(node, c(shQuote(script), shQuote(runtime)),
                    stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"))
  expect_true(any(grepl("basemap runtime checks passed", output, fixed = TRUE)))
})
