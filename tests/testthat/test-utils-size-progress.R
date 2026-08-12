test_that(
  "duration formatting covers invalid, seconds, minutes, and hours",
  {
    format_duration <- getFromNamespace(
      ".format_duration",
      "camtrapReport"
    )
    expect_identical(
      format_duration(NULL),
      "unknown time"
    )

    expect_identical(
      format_duration(NA_real_),
      "unknown time"
    )

    expect_identical(
      format_duration(Inf),
      "unknown time"
    )

    expect_identical(
      format_duration(-1),
      "unknown time"
    )

    expect_identical(
      format_duration(0),
      "0 sec"
    )

    expect_identical(
      format_duration(59.6),
      "1 min 00 sec"
    )

    expect_identical(
      format_duration(61),
      "1 min 01 sec"
    )

    expect_identical(
      format_duration(3599),
      "59 min 59 sec"
    )

    expect_identical(
      format_duration(3600),
      "1 h 0 min"
    )

    expect_identical(
      format_duration(7380),
      "2 h 3 min"
    )
  }
)


test_that(
  "file-size formatting covers all size units",
  {
    format_size <- getFromNamespace(
      ".format_file_size",
      "camtrapReport"
    )

    expect_identical(
      format_size(NULL),
      "unknown size"
    )

    expect_identical(
      format_size(character()),
      "unknown size"
    )

    expect_identical(
      format_size(NA_real_),
      "unknown size"
    )

    expect_identical(
      format_size(Inf),
      "unknown size"
    )

    expect_identical(
      format_size(-1),
      "unknown size"
    )

    expect_identical(
      format_size(500),
      "500 B"
    )

    expect_identical(
      format_size(1536),
      "1.5 KB"
    )

    expect_identical(
      format_size(2.5 * 1024^2),
      "2.5 MB"
    )

    expect_identical(
      format_size(1.25 * 1024^3),
      "1.25 GB"
    )
  }
)


test_that(
  "size estimation handles missing input safely",
  {
    estimate_size <- getFromNamespace(
      ".estimate_camdata_size",
      "camtrapReport"
    )

    inputs <- list(
      NULL,
      character(),
      NA_character_,
      tempfile("missing-camtrap-data-")
    )

    for (input in inputs) {
      result <- estimate_size(input)

      expect_named(
        result,
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

      expect_true(
        is.na(result$file_size)
      )

      expect_identical(
        result$file_size_label,
        "unknown size"
      )

      expect_true(
        is.na(result$zip_uncompressed_size)
      )

      expect_identical(
        result$zip_uncompressed_label,
        "unknown size"
      )

      expect_true(
        is.na(result$effective_size)
      )

      expect_identical(
        result$effective_size_label,
        "unknown size"
      )

      expect_identical(
        result$size_class,
        "unknown"
      )
    }
  }
)


test_that(
  "size estimation reads a regular file",
  {
    estimate_size <- getFromNamespace(
      ".estimate_camdata_size",
      "camtrapReport"
    )

    test_file <- tempfile(
      "camtrapReport-size-file-",
      fileext = ".txt"
    )

    writeBin(
      as.raw(rep(1L, 2048L)),
      test_file
    )

    on.exit(
      unlink(
        test_file,
        force = TRUE
      ),
      add = TRUE
    )

    result <- estimate_size(
      test_file
    )

    expect_identical(
      result$file_size,
      2048
    )

    expect_identical(
      result$file_size_label,
      "2 KB"
    )

    expect_true(
      is.na(result$zip_uncompressed_size)
    )

    expect_identical(
      result$effective_size,
      2048
    )

    expect_identical(
      result$effective_size_label,
      "2 KB"
    )

    expect_identical(
      result$size_class,
      "small"
    )
  }
)


test_that(
  "size estimation sums files in a directory",
  {
    estimate_size <- getFromNamespace(
      ".estimate_camdata_size",
      "camtrapReport"
    )

    test_dir <- tempfile(
      "camtrapReport-size-directory-"
    )
    
    dir.create(test_dir)

    nested_dir <- file.path(
      test_dir,
      "nested"
    )

    dir.create(nested_dir)

    writeBin(
      as.raw(rep(1L, 1000L)),
      file.path(
        test_dir,
        "first.bin"
      )
    )

    writeBin(
      as.raw(rep(2L, 2000L)),
      file.path(
        nested_dir,
        "second.bin"
      )
    )

    on.exit(
      unlink(
        test_dir,
        recursive = TRUE,
        force = TRUE
      ),
      add = TRUE
    )

    result <- estimate_size(
      test_dir
    )

    expect_identical(
      result$file_size,
      3000
    )

    expect_identical(
      result$effective_size,
      3000
    )

    expect_identical(
      result$size_class,
      "small"
    )
  }
)


test_that(
  "size estimation handles an empty directory",
  {
    estimate_size <- getFromNamespace(
      ".estimate_camdata_size",
      "camtrapReport"
    )

    test_dir <- tempfile(
      "camtrapReport-empty-directory-"
    )

    dir.create(test_dir)

    on.exit(
      unlink(
        test_dir,
        recursive = TRUE,
        force = TRUE
      ),
      add = TRUE
    )

    result <- estimate_size(
      test_dir
    )

    expect_true(
      is.na(result$file_size)
    )

    expect_true(
      is.na(result$effective_size)
    )

    expect_identical(
      result$size_class,
      "unknown"
    )
  }
)


test_that(
  "size estimation reads compressed and uncompressed ZIP sizes",
  {
    estimate_size <- getFromNamespace(
      ".estimate_camdata_size",
      "camtrapReport"
    )

    test_dir <- tempfile(
      "camtrapReport-zip-source-"
    )

    dir.create(test_dir)

    source_file <- file.path(
      test_dir,
      "camera-data.txt"
    )

    writeLines(
      rep(
        "camera trap observation data",
        500
      ),
      source_file
    )

    zip_file <- tempfile(
      "camtrapReport-camera-data-",
      fileext = ".zip"
    )

    on.exit(
      {
        unlink(
          test_dir,
          recursive = TRUE,
          force = TRUE
        )

        unlink(
          zip_file,
          force = TRUE
        )
      },
      add = TRUE
    )

    utils::zip(
      zipfile = zip_file,
      files = source_file
    )

    expect_true(
      file.exists(zip_file)
    )

    result <- estimate_size(
      zip_file
    )

    expect_false(
      is.na(result$file_size)
    )

    expect_false(
      is.na(result$zip_uncompressed_size)
    )

    expect_gt(
      result$zip_uncompressed_size,
      0
    )

    expect_identical(
      result$effective_size,
      max(
        result$file_size,
        result$zip_uncompressed_size
      )
    )

    expect_identical(
      result$size_class,
      "small"
    )
  }
)

test_that(
  "camdata start message reports small datasets",
  {
    camdata_start_message <- getFromNamespace(
      ".camdata_start_message",
      "camtrapReport"
    )

    size_info <- list(
      file_size = 1024,
      file_size_label = "1 KB",
      zip_uncompressed_size = NA_real_,
      zip_uncompressed_label = "unknown size",
      effective_size = 1024,
      effective_size_label = "1 KB",
      size_class = "small"
    )

    testthat::local_mocked_bindings(
      .estimate_camdata_size = function(data) {
        size_info
      },
      .package = "camtrapReport"
    )

    expect_message(
      result <- camdata_start_message(
        "dummy-data"
      ),
      "File size looks modest",
      fixed = TRUE
    )

    expect_identical(
      result,
      size_info
    )
  }
)


test_that(
  "camdata start message reports compressed ZIP size",
  {
    camdata_start_message <- getFromNamespace(
      ".camdata_start_message",
      "camtrapReport"
    )
    
    size_info <- list(
      file_size = 1024,
      file_size_label = "1 KB",
      zip_uncompressed_size = 4096,
      zip_uncompressed_label = "4 KB",
      effective_size = 4096,
      effective_size_label = "4 KB",
      size_class = "small"
    )

    testthat::local_mocked_bindings(
      .estimate_camdata_size = function(data) {
        size_info
      },
      .package = "camtrapReport"
    )

    messages <- capture_messages(
      result <- camdata_start_message(
        "dummy.zip"
      )
    )

    expect_true(
      any(
        grepl(
          "1 KB compressed; about 4 KB after unzip",
          messages,
          fixed = TRUE
        )
      )
    )

    expect_identical(
      result,
      size_info
    )
  }
)


test_that(
  "camdata start message covers all size classes",
  {
    camdata_start_message <- getFromNamespace(
      ".camdata_start_message",
      "camtrapReport"
    )

    cases <- list(
      medium = "This may take several minutes",
      large = "This is a large dataset",
      very_large = "This is a very large dataset",
      unknown = paste(
        "Creating the camReport object",
        "may take some time"
      )
    )

    for (size_class in names(cases)) {
      size_info <- list(
        file_size = NA_real_,
        file_size_label = "unknown size",
        zip_uncompressed_size = NA_real_,
        zip_uncompressed_label = "unknown size",
        effective_size = NA_real_,
        effective_size_label = "unknown size",
        size_class = size_class
      )

      testthat::local_mocked_bindings(
        .estimate_camdata_size = function(data) {
          size_info
        },
        .package = "camtrapReport"
      )

      expect_message(
        result <- camdata_start_message(
          "dummy-data"
        ),
        cases[[size_class]],
        fixed = TRUE
      )

      expect_identical(
        result,
        size_info
      )
    }
  }
)
