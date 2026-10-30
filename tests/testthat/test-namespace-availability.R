test_that("availability checks never attach packages", {
  packages <- c("splines", "tools", "parallel", "jsonlite")
  package <- packages[!paste0("package:", packages) %in% search()][1L]
  skip_if(is.na(package), "Every controlled test package is already attached.")

  before <- search()
  expect_true(.require(package))
  expect_identical(search(), before)
  expect_true(.loadPKG(package))
  expect_identical(search(), before)
  expect_true(unname(.loadLib(list(package))))
  expect_identical(search(), before)

  expect_false(.require("camtrapReportUnavailableDependency_123"))
  expect_false(.require(NA_character_))
  expect_false(.require(character()))
  expect_false(.require(""))
  expect_identical(search(), before)
})


test_that("spatial density preserves its weighted calculation without attachment", {
  skip_if_not_installed("spatstat")
  before <- search()
  object <- methods::new("camReport")
  points <- expand.grid(
    x = seq(100, 900, length.out = 5L),
    y = seq(200, 800, length.out = 4L)
  )
  points$total_observations <- seq_len(nrow(points)) %% 4L + 1L

  actual <- object$spatial_density(x = points, .crs = "EPSG:3857")
  window <- .eval(
    "spatstat.geom::owin(xrange = c(20, 980), yrange = c(140, 860))",
    environment()
  )
  pattern <- .eval(
    paste0(
      "spatstat.geom::ppp(x = points$x, y = points$y, ",
      "window = window, marks = points$total_observations)"
    ),
    environment()
  )
  reference <- stats::density(pattern, weights = points$total_observations)
  reference$v <- reference$v / max(reference$v, na.rm = TRUE)
  reference <- terra::rast(reference)

  expect_identical(
    unname(terra::values(actual)),
    unname(terra::values(reference))
  )
  expect_identical(as.vector(terra::ext(actual)),
                   as.vector(terra::ext(reference)))
  expect_identical(dim(actual), dim(reference))
  expect_identical(names(actual), "spatial_density")
  expect_identical(terra::crs(actual), terra::crs("EPSG:3857"))
  expect_identical(search(), before)
})


test_that("spatial density preserves cropping without attaching terra", {
  skip_if_not_installed("spatstat")
  before <- search()
  object <- methods::new("camReport")
  points <- expand.grid(x = c(100, 300, 700, 900),
                        y = c(200, 400, 600, 800))
  points$total_observations <- seq_len(nrow(points)) %% 3L + 1L
  boundary <- terra::as.polygons(terra::ext(0, 1000, 0, 1000))
  terra::crs(boundary) <- "EPSG:3857"
  limits <- as.vector(terra::ext(boundary))

  uncropped <- object$spatial_density(
    x = points, .crs = "EPSG:3857", .ext = limits
  )
  cropped <- object$spatial_density(
    x = points, .crs = "EPSG:3857", .ext = limits, .crop = boundary
  )
  reference <- terra::crop(uncropped, boundary, mask = TRUE)

  expect_identical(terra::values(cropped), terra::values(reference))
  expect_identical(as.vector(terra::ext(cropped)),
                   as.vector(terra::ext(reference)))
  expect_identical(search(), before)

  small_boundary <- terra::as.polygons(terra::ext(0, 500, 0, 500))
  terra::crs(small_boundary) <- "EPSG:3857"
  small_limits <- as.vector(terra::ext(small_boundary))
  uncropped <- object$spatial_density(
    x = points, .crs = "EPSG:3857", .ext = small_limits
  )
  cropped <- object$spatial_density(
    x = points, .crs = "EPSG:3857", .ext = small_limits,
    .crop = small_boundary
  )
  # The original method expands this boundary by 1.1 * the largest excess.
  reference <- terra::crop(
    uncropped, terra::buffer(small_boundary, 440), mask = TRUE
  )
  expect_identical(terra::values(cropped), terra::values(reference))
  expect_identical(as.vector(terra::ext(cropped)),
                   as.vector(terra::ext(reference)))
  expect_identical(search(), before)
})


test_that("spatial summary keeps quadrat and K calculations namespace-only", {
  skip_if_not_installed("spatstat")
  before <- search()
  object <- methods::new("camReport")
  points <- data.frame(
    longitude = c(4.70, 4.72, 4.77, 4.69, 4.73,
                  4.79, 4.71, 4.76, 4.78, 4.68),
    latitude = c(50.78, 50.80, 50.79, 50.82, 50.84,
                 50.83, 50.86, 50.87, 50.85, 50.81)
  )
  object$info <- list(json = list(
    project = list(samplingDesign = "simpleRandom")
  ))
  object$data <- list(
    locations = transform(
      points,
      locationID = paste0("fixture-", seq_len(nrow(points))),
      locationName = paste0("Fixture ", seq_len(nrow(points)))
    ),
    settings = list(tz = "UTC")
  )

  original_eval <- .eval
  recorded <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .eval = function(code, env) {
      value <- original_eval(code, env)
      if (grepl("spatstat.explore::quadrat.test", code, fixed = TRUE)) {
        recorded$quadrat <- value
      }
      if (grepl("spatstat.explore::Kest", code, fixed = TRUE)) {
        recorded$kest <- value
      }
      value
    },
    .package = "camtrapReport"
  )
  result <- capture_report_conditions(.summarize_spatial(object))
  expect_true(all(grepl("chi^2 approximation may be inaccurate",
                       result$warnings, fixed = TRUE)))
  actual_quadrat <- recorded$quadrat
  actual_kest <- recorded$kest

  xr <- range(points$longitude)
  yr <- range(points$latitude)
  xr <- xr + diff(xr) * c(-0.01, 0.01)
  yr <- yr + diff(yr) * c(-0.01, 0.01)
  window <- .eval("spatstat.geom::owin(xrange = xr, yrange = yr)",
                  environment())
  pattern <- .eval(
    paste0("spatstat.geom::ppp(x = points$longitude, ",
           "y = points$latitude, window = window)"),
    environment()
  )
  expected_quadrat <- capture_report_conditions(.eval(
    "spatstat.explore::quadrat.test(pattern, nx = 3, ny = 3)",
    environment()
  ))$value
  expected_kest <- .eval(
    "spatstat.explore::Kest(pattern, correction = 'iso')",
    environment()
  )

  expect_identical(actual_quadrat$p.value, expected_quadrat$p.value)
  expect_identical(actual_quadrat$statistic, expected_quadrat$statistic)
  expect_identical(as.data.frame(actual_kest), as.data.frame(expected_kest))
  expect_identical(result$value$total_unique_locations, 10L)
  expect_identical(
    result$value$status_spatial,
    "Regular / Possibly Linear (using point-pattern analysis)"
  )
  expect_identical(search(), before)
})


test_that("dynamic effort keeps package-qualified evaluation and settings", {
  object <- methods::new("camReport")
  object$data <- list(deployments = data.frame(
    deploymentStart = as.POSIXct(c("2023-01-01", "2023-01-03"), tz = "UTC"),
    deploymentEnd = as.POSIXct(c("2023-01-05", "2023-01-07"), tz = "UTC")
  ))
  recorded <- new.env(parent = emptyenv())
  recorded$code <- character()
  local_mocked_bindings(
    .eval = function(code, env) {
      recorded$code <- c(recorded$code, code)
      if (grepl("xts::xts", code, fixed = TRUE)) {
        recorded$series <- get("z", env)
        return(structure(list(), class = "controlled_effort_series"))
      }
      list(title = get("main", env), xlab = get("xlab", env),
           ylab = get("ylab", env))
    },
    .package = "camtrapReport"
  )

  before <- search()
  actual <- .plot_effort(object, main = "Survey effort", ylab = "Cameras")
  expect_identical(recorded$series, .effort_table(object, startend = TRUE))
  expect_length(recorded$code, 2L)
  expect_match(recorded$code[1L], "xts::xts", fixed = TRUE)
  expect_match(recorded$code[2L], "dygraphs::dygraph", fixed = TRUE)
  expect_match(recorded$code[2L], "dygraphs::dyOptions", fixed = TRUE)
  expect_match(recorded$code[2L], "dygraphs::dyRangeSelector", fixed = TRUE)
  expect_match(recorded$code[2L], "fillGraph = TRUE, fillAlpha = 0.4",
               fixed = TRUE)
  expect_identical(actual,
                   list(title = "Survey effort", xlab = "time", ylab = "Cameras"))
  expect_identical(search(), before)
})
