test_that("text helpers handle missing and completely empty inputs", {
  paste_comma_and <- .paste_comma_and
  trim_one <- .trim
  trim_many <- .trim_chr
  
  expect_identical(paste_comma_and(), "")
  expect_identical(paste_comma_and(character()), "")
  expect_identical(
    paste_comma_and(c(NA_character_, "", "   ")),
    ""
  )
  
  expect_identical(trim_one(), "")
  expect_identical(trim_one(NULL), "")
  expect_identical(trim_one(character()), "")
  expect_identical(trim_one(c(" first ", "second")), "first")
  
  expect_identical(trim_many(), character())
  expect_identical(trim_many(NULL), character())
  expect_identical(trim_many(character()), character())
})


test_that("duration formatting handles rounding and invalid values", {
  format_duration <- .format_duration
  
  expect_identical(format_duration(NULL), "unknown time")
  expect_identical(format_duration(character()), "unknown time")
  expect_identical(format_duration(NA_real_), "unknown time")
  expect_identical(format_duration(Inf), "unknown time")
  expect_identical(format_duration(-Inf), "unknown time")
  
  expect_identical(format_duration(0), "0 sec")
  expect_identical(format_duration(59.4), "59 sec")
  expect_identical(format_duration(59.6), "1 min 00 sec")
  expect_identical(format_duration(60), "1 min 00 sec")
  expect_identical(format_duration(3599), "59 min 59 sec")
  expect_identical(format_duration(3600), "1 h 0 min")
  expect_identical(format_duration(3661), "1 h 1 min")
})


test_that("file-size formatting covers all boundaries", {
  format_file_size <- .format_file_size
  
  expect_identical(format_file_size(NULL), "unknown size")
  expect_identical(format_file_size(numeric()), "unknown size")
  expect_identical(format_file_size(NA_real_), "unknown size")
  expect_identical(format_file_size(Inf), "unknown size")
  expect_identical(format_file_size(-1), "unknown size")
  
  expect_identical(format_file_size(0), "0 B")
  expect_identical(format_file_size(1023), "1023 B")
  expect_identical(format_file_size(1024), "1 KB")
  expect_identical(format_file_size(1024^2), "1 MB")
  expect_identical(format_file_size(1024^3), "1 GB")
  
  # Only the first value is used.
  expect_identical(
    format_file_size(c(2048, 1024^3)),
    "2 KB"
  )
})


test_that("data-size inspection handles missing, empty, and directory inputs", {
  estimate_size <- .estimate_camdata_size
  
  missing_result <- estimate_size(NULL)
  
  expect_named(
    missing_result,
    c(
      "file_size",
      "file_size_label",
      "zip_uncompressed_size",
      "zip_uncompressed_label",
      "effective_size",
      "effective_size_label",
      "size_class"
    )
  )
  
  expect_true(is.na(missing_result$file_size))
  expect_true(is.na(missing_result$effective_size))
  expect_identical(missing_result$size_class, "unknown")
  
  empty_directory <- tempfile("camtrap-empty-directory-")
  dir.create(empty_directory)
  
  on.exit(
    unlink(
      empty_directory,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  
  empty_result <- estimate_size(empty_directory)
  
  expect_true(is.na(empty_result$file_size))
  expect_true(is.na(empty_result$effective_size))
  expect_identical(empty_result$size_class, "unknown")
  
  data_directory <- tempfile("camtrap-data-directory-")
  dir.create(data_directory)
  
  on.exit(
    unlink(
      data_directory,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )
  
  writeBin(
    raw(20),
    file.path(data_directory, "first.bin")
  )
  
  writeBin(
    raw(30),
    file.path(data_directory, "second.bin")
  )
  
  directory_result <- estimate_size(data_directory)
  
  expect_identical(directory_result$file_size, 50)
  expect_identical(directory_result$effective_size, 50)
  expect_identical(directory_result$file_size_label, "50 B")
  expect_identical(directory_result$size_class, "small")
})


test_that("camData progress messages cover known and unknown datasets", {
  start_message <- .camdata_start_message
  done_message <- .camdata_done_message
  
  size_info <- capture_expected_message(
    start_message("a-file-that-does-not-exist"),
    "The camReport object is being created",
    fixed = TRUE
  )
  
  expect_identical(
    size_info$size_class,
    "unknown"
  )
  
  result <- capture_expected_message(
    done_message(
      Sys.time() - 2,
      site_name = NULL
    ),
    "your study site",
    fixed = TRUE
  )
  
  expect_true(result)
  
  result_named <- capture_expected_message(
    done_message(
      Sys.time() - 2,
      site_name = "Veluwe"
    ),
    "Veluwe",
    fixed = TRUE
  )
  
  expect_true(result_named)
})


test_that("chunk names and HTML escaping handle malformed inputs", {
  extract_name <- .extract_chunk_name
  escape_html <- .html_escape_base
  safe_code <- .make_safe_module_code
  
  expect_identical(
    extract_name(
      NA_character_,
      fallback = NA_character_
    ),
    "module"
  )
  
  expect_identical(
    extract_name(
      "#| name: ecological results: mammals",
      fallback = "fallback"
    ),
    "ecological_results_mammals"
  )
  
  expect_identical(
    extract_name(
      "#| name:     \nmean(1:3)",
      fallback = "fallback section"
    ),
    "fallback_section"
  )
  
  expect_identical(escape_html(), "")
  expect_identical(escape_html(NULL), "")
  expect_identical(escape_html(NA_character_), "")
  
  expect_identical(
    escape_html("<tag a='one' b=\"two\">A & B</tag>"),
    "&lt;tag a=&#39;one&#39; b=&quot;two&quot;&gt;A &amp; B&lt;/tag&gt;"
  )
  
  expect_identical(safe_code(NA_character_), "")
  expect_identical(safe_code(character()), "")
})


test_that("character removal ignores invalid positions safely", {
  remove_characters <- .rmChar
  
  expect_identical(
    remove_characters("", 1),
    ""
  )
  
  expect_identical(
    remove_characters("abc", c(-1, 0, 4, 20)),
    "abc"
  )
  
  expect_identical(
    remove_characters("abc", integer()),
    "abc"
  )
  
  expect_identical(
    remove_characters("a", integer(), rmLast = TRUE),
    ""
  )
  
  expect_identical(
    remove_characters("abc", c(1, 3)),
    "b"
  )
})


test_that("year extraction handles missing and repeated values", {
  get_year <- .getYear
  
  expect_identical(
    get_year(),
    numeric()
  )
  
  expect_identical(
    get_year(NULL),
    numeric()
  )
  
  expect_identical(
    get_year(NULL, .interval = TRUE),
    list()
  )
  
  expect_identical(
    get_year(
      c(
        "Observed in 2020, repeated in 2020, and again in 2023",
        "",
        NA_character_
      ),
      .interval = TRUE
    ),
    list(
      c(2020, 2023),
      numeric(),
      numeric()
    )
  )
  
  expect_true(
    is.na(
      get_year("not-a-year")
    )
  )
})


test_that("evaluation helper uses the calling environment when omitted", {
  evaluate_text <- .eval
  
  local_value <- 8
  
  expect_identical(
    evaluate_text("local_value + 2"),
    10
  )
  
  expect_null(
    evaluate_text()
  )
  
  expect_null(
    evaluate_text(NULL)
  )
})


test_that("render environments expose objects and report counters", {
  object <- camtrap_test_report()$copy(shallow = FALSE)
  
  render_environment <- .make_render_env(
    object
  )
  
  expect_type(render_environment, "environment")
  
  expect_identical(
    render_environment$object,
    object
  )
  
  expect_identical(
    render_environment$cm,
    object
  )
  
  expect_identical(
    render_environment$.self,
    object
  )
  
  expect_type(render_environment$getFigureNumber, "closure")
  
  expect_type(render_environment$getTableNumber, "closure")
  
  expect_true(
    exists(
      ".format_duration",
      envir = render_environment,
      inherits = FALSE
    )
  )
  
  expect_true(
    exists(
      ".html_escape_base",
      envir = render_environment,
      inherits = FALSE
    )
  )
})
