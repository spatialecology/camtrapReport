test_that("section selectors validate partial and unknown selections", {
  available <- section_names()
  first <- available[[1]]

  expect_identical(section_names(keep = first), first)
  expect_warning(
    kept <- section_names(keep = c(first, "unknown_section")),
    "not available"
  )
  expect_identical(kept, first)
  expect_error(section_names(keep = "unknown_section"), "None")

  expect_warning(
    excluded <- section_names(exclude = c(first, "unknown_section")),
    "not available"
  )
  expect_false(first %in% excluded)
  expect_error(section_names(exclude = "unknown_section"), "None")
})

test_that("sections handles invalid, partial, and failed module selections", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  known <- sections(cm)

  expect_warning(current <- sections(cm, 1), "should be character")
  expect_identical(current, known)
  expect_error(sections(cm, "unknown_section"), "None")
  expect_message(
    partial <- sections(cm, c(known[[1]], "unknown_section")),
    "unknown and ignored"
  )
  expect_s4_class(partial, "camReport")
})

test_that("metadata access validates fields and aliases", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)

  defaults <- info(cm)
  expect_named(defaults, c("title", "subtitle", "authors", "institute", "siteName", "logoPath"))
  expect_warning(fallback <- info(cm, "not_a_field"), "default fields")
  expect_named(fallback, names(defaults))

  expect_error(
    `info<-`(cm, c("title", "subtitle"), value = "x"),
    "Only one field"
  )
  expect_error(`info<-`(cm, "not_a_field", value = "x"), "not identified")

  info(cm, "study area") <- "Updated study-area description"
  expect_identical(cm$description, "Updated study-area description")
  info(cm, "acknowledgement") <- "Updated acknowledgement"
  acknowledgement <- find_test_report_section(cm$reportObjects, "acknowledgements")
  expect_identical(acknowledgement@txt, "Updated acknowledgement")
})

test_that("datetime parsing accepts Camtrap DP date variants", {
  parse_time <- camtrapReport:::.parse_cam_datetime
  original <- as.POSIXct("2024-01-01 12:30:00", tz = "UTC")
  parsed <- parse_time(c(
    "2024-01-01T12:30:00Z",
    "2024/01/02 13:45:00",
    "2024-01-03",
    "NULL"
  ))

  expect_identical(parse_time(original), original)
  expect_s3_class(parse_time(as.POSIXlt(original)), "POSIXct")
  expect_true(is.na(parse_time(NULL)))
  expect_false(any(is.na(parsed[1:3])))
  expect_true(is.na(parsed[4]))
  expect_identical(camtrapReport:::.first_non_missing(c(NA, "a", "a")), "a")
  expect_true(is.na(camtrapReport:::.first_non_missing(c(NA, NA))))
  expect_true(is.na(camtrapReport:::.safe_min_time(as.POSIXct(character()))))
  expect_true(is.na(camtrapReport:::.safe_max_time(as.POSIXct(character()))))
})

test_that("small camReport summary helpers cover alternative labels", {
  expect_identical(camtrapReport:::.ct_icons(FALSE)$green, "[OK]")
  expect_false(identical(camtrapReport:::.ct_icons(TRUE)$green, "[OK]"))
  expect_match(camtrapReport:::.format_area(0.5), "m")
  expect_match(camtrapReport:::.format_area(50), "km")
  expect_match(camtrapReport:::.format_area(5000), "km")
  expect_identical(camtrapReport:::.round_capture_metric(c(1.234, NA)), c(1.23, NA))
  expect_identical(
    camtrapReport:::.pick_station_col(list(
      locations = NULL,
      deployments = data.frame(locationID = "A")
    )),
    "locationID"
  )
})
