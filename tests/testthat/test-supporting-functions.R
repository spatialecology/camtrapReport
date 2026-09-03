test_that("species and class helpers summarise the bundled data", {
  cm <- camtrap_test_report()

  species_counts <- .get_species(cm$data, count = TRUE)
  species_names <- .get_species(cm$data, count = FALSE)
  class_counts <- .get_classes(cm$data, count = TRUE)
  class_names <- .get_classes(cm$data, count = FALSE)

  expect_s3_class(species_counts, "data.frame")
  expect_true(all(species_names %in% species_counts$scientificName))
  expect_s3_class(class_counts, "data.frame")
  expect_true("count" %in% names(class_counts))
  expect_length(class_names, nrow(class_counts))
})

test_that("merged data can retain nested media records", {
  cm <- camtrap_test_report()
  merged <- .merge_data(cm$data, dropMedia = FALSE)

  expect_s3_class(merged, "data.frame")
  expect_true("media" %in% names(merged))
  expect_type(merged$media, "list")
  expect_true(any(lengths(merged$media) > 0L))
})

test_that("effort helpers aggregate deployments and draw with base graphics", {
  cm <- camtrap_test_report()
  total <- .calc_effort(cm$data, unit = "day")
  by_location <- .calc_effort(
    cm$data,
    by = "locationID",
    unit = "hour"
  )
  step_table <- .effort_table(cm, startend = TRUE)

  expect_gt(total$effort, 0)
  expect_identical(total$unit, "day")
  expect_true(all(c("locationID", "effort") %in% names(by_location)))
  expect_true(all(c("time", "nrCams") %in% names(step_table)))
  expect_error(
    .calc_effort(cm$data, by = "missing_column"),
    "Grouping columns"
  )

  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file, force = TRUE)
    },
    add = TRUE
  )
  expect_null(.plot_effort(cm, dynamic = FALSE))
})

test_that("base left join handles equal and differently named keys", {
  left <- data.frame(id = c(1, 2), value = c("a", "b"))
  right <- data.frame(id = 2, extra = "x")
  right2 <- data.frame(other_id = 2, extra = "x")

  joined <- .left_join(left, right, "id")
  joined2 <- .left_join(left, right2, c("id", "other_id"))

  expect_identical(nrow(joined), 2L)
  expect_identical(nrow(joined2), 2L)
  expect_true(is.na(joined$extra[joined$id == 1]))
  expect_error(.left_join(left, right, "missing"))
})

test_that("the internal pivot helper supports tidy-style column specifications", {
  data <- data.frame(
    location = c("A", "A", "B"),
    species = c("fox", "hare", "fox"),
    count = c(2, 1, 3)
  )

  wide_bare <- .pivot_wider(
    data,
    id_cols = location,
    names_from = species,
    values_from = count
  )
  ids <- "location"
  wide_character <- .pivot_wider(
    data,
    id_cols = ids,
    names_from = "species",
    values_from = "count"
  )

  expect_identical(wide_bare, wide_character)
  expect_true(all(c("location", "fox", "hare") %in% names(wide_bare)))
  expect_identical(wide_bare$hare[wide_bare$location == "B"], 0)
  expect_error(
    .pivot_wider(data, id_cols = character(), species, count),
    "No 'id_cols'"
  )
  expect_error(
    .pivot_wider(data, location, unknown, count),
    "Unknown column"
  )
})
