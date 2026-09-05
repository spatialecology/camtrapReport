test_that("formatting helpers handle normal and boundary inputs", {
  paste_list <- .paste_comma_and
  format_duration <- .format_duration
  format_size <- .format_file_size

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
  trim_one <- .trim
  trim_many <- .trim_chr
  chunk_name <- .extract_chunk_name

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

  expect_identical(.rmChar("abcdef", c(1, 2), TRUE), "cde")
  expect_identical(.firstUpper(c("FOX", NA)), c("Fox", ""))
  expect_identical(.pretty_label(c("wild_mammals", "birds")),
                   "wild mammals and birds")
})

test_that("date, time, and filename helpers cover common input forms", {
  get_year <- .getYear
  get_hour <- .get_hour
  time_length <- .get_Time_length

  expect_identical(get_year(c("2022-01-01", "2024-03-02")), c(2022, 2024))
  expect_identical(
    get_year(c("2022-01-01--2024-01-01", NA), .interval = TRUE),
    list(c(2022, 2024), numeric())
  )
  expect_identical(get_hour("2024-01-01T13:30:00", tz = "UTC"), 13.5)
  expect_identical(
    get_hour(as.POSIXlt("2024-01-01 04:15:00", tz = "UTC")),
    4.25
  )
  expect_true(is.na(get_hour("not-a-date")))

  expect_equal(
    time_length("2024-01-01 00:00:00--2024-01-03 00:00:00"),
    2
  )
  expect_equal(
    time_length("2024-01-03 00:00:00", "2024-01-01 00:00:00"),
    2
  )
  expect_true(is.na(time_length("not-an-interval")))

  expect_true(.isZip("DATA.ZIP"))
  expect_true(.isJson("data.Json"))
  expect_false(.isZip(NULL))
  expect_true(.is.POSIXct(as.POSIXct("2024-01-01", tz = "UTC")))
  expect_identical(.getFormat("2024-01-01T13:30:00"),
                   "%Y-%m-%dT%H:%M:%OS")
  expect_true(is.na(.getFormat("not a date")))
})

test_that("small data helpers return stable base objects", {
  bind_rows <- .bind_rows
  get_match <- .get_match

  expect_s3_class(bind_rows(NULL), "data.frame")
  bound <- bind_rows(list(data.frame(a = 1), NULL, data.frame(a = 2, b = 3)))
  expect_identical(bound$a, c(1, 2))
  expect_named(bound, c("a", "b"))

  expect_identical(get_match("FOX", c("fox", "hare")), "fox")
  expect_identical(
    get_match("fox", c("fox", "hare"), case_sensitive = TRUE),
    "fox"
  )
  expect_true(is.na(get_match("deer", c("fox", "hare"))))
  expect_true(is.na(get_match(NULL, "fox")))

  expect_identical(.pick_col(data.frame(a = 1), c("b", "a")), "a")
  expect_true(is.na(.pick_col(NULL, "a")))
  expect_identical(unname(.charN(c("two words", ""))), c(9, 0))
  expect_identical(.charN("two words", space = FALSE), 8L)
  expect_identical(unname(.wordN(c("two words", ""))), c(2, 0))
  expect_identical(.word("one two three", 2, 3), c("two", "three"))
  expect_identical(.word("one two three", -2), c("two", "three"))
  expect_warning(.word("one two", 2, 1), "cannot be lower")
})

test_that("file and size helpers inspect temporary data without side effects", {
  d <- tempfile("camtrap-size-")
  dir.create(d)
  on.exit(
    unlink(d, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  writeLines("camera trap", file.path(d, "sample.data.csv"))

  size <- .estimate_camdata_size(d)
  info <- .file_info(file.path(d, "sample.data.csv"))

  expect_identical(size$size_class, "small")
  expect_gt(size$effective_size, 0)
  expect_identical(info$filename, "sample_data")
  expect_identical(info$extension, "csv")
  expect_identical(.file_info("README")$extension, NA_character_)
  expect_identical(.estimate_camdata_size("missing")$size_class,
                   "unknown")
})

test_that("evaluation and package helpers are safe for core packages", {
  env <- new.env(parent = baseenv())
  env$x <- 2

  expect_identical(.eval("x + 3", env), 5)
  expect_null(.eval(NULL, env))
  expect_true(.require("methods"))
  expect_false(.require("a_package_that_does_not_exist_123"))
  expect_true(.loadPKG(c("methods", "stats")))
  expect_false(.loadPKG("a_package_that_does_not_exist_123"))
  expect_identical(.suppress_startup({
    message("suppressed message")
    7
  }), 7)
  expect_identical(.make_safe_module_code(NULL), "")
  expect_identical(
    .make_safe_module_code(c("x <- 1", "x + 1")),
    "x <- 1\nx + 1"
  )
})

test_that("nested section and empty taxonomy helpers return stable structures", {
  section <- reportSection("child", parent = "parent", txt = "text")
  tree <- list(root = list(child = section))
  found <- .findParent(tree, "parent")

  expect_identical(unname(found), c("1", "child", "parent"))
  expect_true(is.na(.findParent(tree, "unknown")))
  expect_true(is.na(.findParent(list(), "parent")))

  ncbi <- .getMissingTaxon_NCBI(character())
  gbif <- .getMissingTaxon_GBIF(character())
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
  projected <- .get_projected_vect(geographic)

  expect_false(.is.projected(geographic))
  expect_true(.is.projected(projected))
  expect_s4_class(projected, "SpatVector")
  expect_identical(.get_projected_vect(projected), projected)
})

test_that("the base correlation plot draws on a non-interactive device", {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit(
    {
      grDevices::dev.off()
      unlink(file, force = TRUE)
    },
    add = TRUE
  )

  x <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  colnames(x) <- rownames(x) <- c("fox", "hare")

  expect_type(.basic_corrplot(x), "list")
})
