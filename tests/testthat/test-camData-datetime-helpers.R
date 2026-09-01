test_that("parse_cam_datetime preserves POSIXct input", {
  input <- as.POSIXct(
    c(
      "2025-01-01 10:00:00",
      "2025-01-02 11:30:00"
    ),
    tz = "UTC"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_s3_class(result, "POSIXct")
  expect_identical(result, input)
})


test_that("parse_cam_datetime converts other POSIXt input", {
  input <- as.POSIXlt(
    "2025-02-03 12:30:45",
    tz = "UTC"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_s3_class(result, "POSIXct")
  
  expect_equal(
    as.numeric(result),
    as.numeric(as.POSIXct(input))
  )
})


test_that("parse_cam_datetime handles NULL input", {
  result <- camtrapReport:::.parse_cam_datetime(
    NULL,
    tz = "UTC"
  )
  
  expect_s3_class(result, "POSIXct")
  expect_length(result, 1L)
  expect_true(is.na(result))
})


test_that("parse_cam_datetime handles missing text values", {
  input <- c(
    "",
    " ",
    "NA",
    "NaN",
    "NULL",
    "null",
    NA_character_
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_s3_class(result, "POSIXct")
  expect_length(result, length(input))
  expect_true(all(is.na(result)))
})


test_that("parse_cam_datetime parses ISO date-time formats", {
  input <- c(
    "2025-01-02T03:04:05",
    "2025-01-02 03:04:05",
    "2025/01/02T03:04:05",
    "2025/01/02 03:04:05"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_s3_class(result, "POSIXct")
  expect_false(anyNA(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    rep(
      "2025-01-02 03:04:05",
      4
    )
  )
})


test_that("parse_cam_datetime parses minute-level formats", {
  input <- c(
    "2025-03-04T05:06",
    "2025-03-04 05:06",
    "2025/03/04T05:06",
    "2025/03/04 05:06"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_false(anyNA(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    rep(
      "2025-03-04 05:06:00",
      4
    )
  )
})


test_that("parse_cam_datetime parses date-only formats", {
  input <- c(
    "2025-04-05",
    "2025/04/05"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_false(anyNA(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    rep(
      "2025-04-05 00:00:00",
      2
    )
  )
})


test_that("parse_cam_datetime parses UTC Z suffix", {
  result <- camtrapReport:::.parse_cam_datetime(
    "2025-05-06T07:08:09Z",
    tz = "UTC"
  )
  
  expect_false(is.na(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2025-05-06 07:08:09"
  )
})


test_that("parse_cam_datetime parses offsets containing a colon", {
  result <- camtrapReport:::.parse_cam_datetime(
    "2025-05-06T09:08:09+02:00",
    tz = "UTC"
  )
  
  expect_false(is.na(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2025-05-06 07:08:09"
  )
})


test_that("parse_cam_datetime parses offsets without a colon", {
  result <- camtrapReport:::.parse_cam_datetime(
    "2025-05-06 09:08:09+0200",
    tz = "UTC"
  )
  
  expect_false(is.na(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2025-05-06 07:08:09"
  )
})


test_that("parse_cam_datetime handles mixed valid and invalid inputs", {
  testthat::local_mocked_bindings(
    .require = function(package) {
      FALSE
    },
    .package = "camtrapReport"
  )
  
  input <- c(
    "2025-06-01 10:00:00",
    "not a date",
    NA_character_,
    "2025/06/02"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    input,
    tz = "UTC"
  )
  
  expect_length(result, 4L)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
  expect_false(is.na(result[4]))
})


test_that("parse_cam_datetime respects supplied timezone", {
  result <- camtrapReport:::.parse_cam_datetime(
    "2025-07-08 09:10:11",
    tz = "Europe/Amsterdam"
  )
  
  expect_false(is.na(result))
  
  expect_identical(
    attr(result, "tzone"),
    "Europe/Amsterdam"
  )
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "Europe/Amsterdam"
    ),
    "2025-07-08 09:10:11"
  )
})



test_that("parse_cam_datetime uses fallback parser", {
  testthat::local_mocked_bindings(
    .require = function(package) {
      identical(package, "lubridate")
    },
    .eval = function(x, env) {
      as.POSIXct(
        c(
          "2025-08-05 14:30:15",
          "2025-08-06 09:45:00"
        ),
        tz = "UTC"
      )
    },
    .package = "camtrapReport"
  )
  
  result <- camtrapReport:::.parse_cam_datetime(
    c(
      "20250805 14:30:15",
      "20250806 09:45"
    ),
    tz = "UTC"
  )
  
  expect_s3_class(result, "POSIXct")
  expect_length(result, 2L)
  expect_false(anyNA(result))
  
  expect_identical(
    format(
      result,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    c(
      "2025-08-05 14:30:15",
      "2025-08-06 09:45:00"
    )
  )
})