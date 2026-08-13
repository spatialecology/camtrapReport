test_that("formatting helpers handle normal and boundary inputs", {
  paste_list <- camtrapReport:::.paste_comma_and
  format_duration <- camtrapReport:::.format_duration
  format_size <- camtrapReport:::.format_file_size

  expect_identical(paste_list(NULL), "")
  expect_identical(paste_list(c("fox", NA, "fox")), "fox")
  expect_identical(paste_list(c("fox", "hare")), "fox and hare")
  expect_identical(paste_list(c("fox", "hare", "deer")), "fox, hare, and deer")

  expect_identical(format_duration(12), "12 sec")
  expect_identical(format_duration(125), "2 min 05 sec")
  expect_identical(format_duration(7260), "2 h 1 min")
  expect_identical(format_duration(-1), "unknown time")

  expect_identical(format_size(12), "12 B")
  expect_identical(format_size(2048), "2 KB")
  expect_identical(format_size(2 * 1024^2), "2 MB")
  expect_identical(format_size(2 * 1024^3), "2 GB")
  expect_identical(format_size(NA_real_), "unknown size")
})

test_that("text parsing helpers preserve their documented contracts", {
  trim_one <- camtrapReport:::.trim
  trim_many <- camtrapReport:::.trim_chr
  chunk_name <- camtrapReport:::.extract_chunk_name

  expect_identical(trim_one("  two   words  "), "two words")
  expect_identical(trim_one("  two   words  ", squish = FALSE), "two   words")
  expect_identical(trim_one(NA_character_), "")
  expect_identical(trim_many(c(" a ", NA)), c("a", ""))

  expect_identical(
    chunk_name("#| name: Species results\nmean(1:3)"),
    "Species_results"
  )
  expect_identical(chunk_name(NULL, "fallback name"), "fallback_name")
  expect_identical(chunk_name("# ordinary comment", ""), "module")

  expect_identical(camtrapReport:::.rmChar("abcdef", c(1, 2), TRUE), "cde")
  expect_identical(camtrapReport:::.firstUpper(c("FOX", NA)), c("Fox", ""))
  expect_identical(camtrapReport:::.pretty_label(c("wild_mammals", "birds")),
                   "wild mammals and birds")
})

test_that("date, time, and filename helpers cover common input forms", {
  get_year <- camtrapReport:::.getYear
  get_hour <- camtrapReport:::.get_hour
  time_length <- camtrapReport:::.get_Time_length

  expect_identical(get_year(c("2022-01-01", "2024-03-02")), c(2022, 2024))
  expect_identical(
    get_year(c("2022-01-01--2024-01-01", NA), .interval = TRUE),
    list(c(2022, 2024), numeric())
  )
  expect_identical(get_hour("2024-01-01T13:30:00", tz = "UTC"), 13.5)
  expect_identical(get_hour(as.POSIXlt("2024-01-01 04:15:00", tz = "UTC")), 4.25)
  expect_true(is.na(get_hour("not-a-date")))

  expect_identical(
    time_length("2024-01-01 00:00:00--2024-01-03 00:00:00"),
    2
  )
  expect_identical(
    time_length("2024-01-03 00:00:00", "2024-01-01 00:00:00"),
    2
  )
  expect_true(is.na(time_length("not-an-interval")))

  expect_true(camtrapReport:::.isZip("DATA.ZIP"))
  expect_true(camtrapReport:::.isJson("data.Json"))
  expect_false(camtrapReport:::.isZip(NULL))
  expect_true(camtrapReport:::.is.POSIXct(as.POSIXct("2024-01-01", tz = "UTC")))
  expect_identical(camtrapReport:::.getFormat("2024-01-01T13:30:00"),
                   "%Y-%m-%dT%H:%M:%OS")
  expect_true(is.na(camtrapReport:::.getFormat("not a date")))
})

test_that("small data helpers return stable base objects", {
  bind_rows <- camtrapReport:::.bind_rows
  get_match <- camtrapReport:::.get_match

  expect_s3_class(bind_rows(NULL), "data.frame")
  bound <- bind_rows(list(data.frame(a = 1), NULL, data.frame(a = 2, b = 3)))
  expect_identical(bound$a, c(1, 2))
  expect_named(bound, c("a", "b"))

  expect_identical(get_match("FOX", c("fox", "hare")), "fox")
  expect_identical(get_match("fox", c("fox", "hare"), case_sensitive = TRUE), "fox")
  expect_true(is.na(get_match("deer", c("fox", "hare"))))
  expect_true(is.na(get_match(NULL, "fox")))

  expect_identical(camtrapReport:::.pick_col(data.frame(a = 1), c("b", "a")), "a")
  expect_true(is.na(camtrapReport:::.pick_col(NULL, "a")))
  expect_identical(unname(camtrapReport:::.charN(c("two words", ""))), c(9, 0))
  expect_identical(camtrapReport:::.charN("two words", space = FALSE), 8L)
  expect_identical(unname(camtrapReport:::.wordN(c("two words", ""))), c(2, 0))
  expect_identical(camtrapReport:::.word("one two three", 2, 3), c("two", "three"))
  expect_identical(camtrapReport:::.word("one two three", -2), c("two", "three"))
  expect_warning(camtrapReport:::.word("one two", 2, 1), "cannot be lower")
})

test_that("file and size helpers inspect temporary data without side effects", {
  d <- tempfile("camtrap-size-")
  dir.create(d)
  writeLines("camera trap", file.path(d, "sample.data.csv"))

  size <- camtrapReport:::.estimate_camdata_size(d)
  info <- camtrapReport:::.file_info(file.path(d, "sample.data.csv"))

  expect_identical(size$size_class, "small")
  expect_gt(size$effective_size, 0)
  expect_identical(info$filename, "sample_data")
  expect_identical(info$extension, "csv")
  expect_identical(camtrapReport:::.file_info("README")$extension, NA_character_)
  expect_identical(camtrapReport:::.estimate_camdata_size("missing")$size_class,
                   "unknown")
})

test_that("evaluation and package helpers are safe for core packages", {
  env <- new.env(parent = baseenv())
  env$x <- 2

  expect_identical(camtrapReport:::.eval("x + 3", env), 5)
  expect_null(camtrapReport:::.eval(NULL, env))
  expect_true(camtrapReport:::.require("methods"))
  expect_false(camtrapReport:::.require("a_package_that_does_not_exist_123"))
  expect_true(camtrapReport:::.loadPKG(c("methods", "stats")))
  expect_false(camtrapReport:::.loadPKG("a_package_that_does_not_exist_123"))
  expect_identical(camtrapReport:::.suppress_startup({
    message("suppressed message")
    7
  }), 7)
  expect_identical(camtrapReport:::.make_safe_module_code(NULL), "")
  expect_identical(
    camtrapReport:::.make_safe_module_code(c("x <- 1", "x + 1")),
    "x <- 1\nx + 1"
  )
})

test_that("nested section and empty taxonomy helpers return stable structures", {
  section <- reportSection("child", parent = "parent", txt = "text")
  tree <- list(root = list(child = section))
  found <- camtrapReport:::.findParent(tree, "parent")

  expect_identical(unname(found), c("1", "child", "parent"))
  expect_true(is.na(camtrapReport:::.findParent(tree, "unknown")))
  expect_true(is.na(camtrapReport:::.findParent(list(), "parent")))

  ncbi <- camtrapReport:::.getMissingTaxon_NCBI(character())
  gbif <- camtrapReport:::.getMissingTaxon_GBIF(character())
  expect_named(ncbi, c("scientificName", "class", "order"))
  expect_identical(nrow(ncbi), 0L)
  expect_identical(nrow(gbif), 0L)
})

test_that("spatial helpers recognise geographic and projected terra objects", {
  geographic <- terra::vect(
    data.frame(lon = c(4, 4.1), lat = c(52, 52.1)),
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  projected <- camtrapReport:::.get_projected_vect(geographic)
  
  expect_false(camtrapReport:::.is.projected(geographic))
  expect_true(camtrapReport:::.is.projected(projected))
  expect_s4_class(projected, "SpatVector")
  expect_identical(camtrapReport:::.get_projected_vect(projected), projected)
})

test_that("sf study-area boundaries are projected automatically", {
  testthat::skip_if_not_installed("sf")
  
  x <- sf::st_as_sf(
    data.frame(
      id = 1,
      wkt = "POLYGON((4 52, 4.1 52, 4.1 52.1, 4 52.1, 4 52))"
    ),
    wkt = "wkt",
    crs = 4326
  )
  
  projected <- camtrapReport:::.get_projected_sf(x)
  
  expect_s3_class(projected, "sf")
  expect_false(sf::st_is_longlat(projected))
  expect_false(is.na(sf::st_crs(projected)))
  expect_false(identical(sf::st_crs(projected)$epsg, 4326L))
})

test_that("the base correlation plot draws on a non-interactive device", {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  x <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  colnames(x) <- rownames(x) <- c("fox", "hare")
  
  expect_type(camtrapReport:::.basic_corrplot(x), "list")
})
