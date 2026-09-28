test_that("bundled profiles select ordered modules from both pools", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  expect_setequal(
    profile_names(cm),
    c("default", "EOW")
  )
  
  eow <- sections(
    cm,
    profile = "EOW"
  )
  
  # Replacing the generic appendix with appendix_eow does not
  # change the total number of entries in the EOW profile.
  expect_length(eow, 22L)
  
  expect_true(
    "report::location_eow" %in% eow
  )
  
  expect_true(
    "status::spatial" %in% eow
  )
  
  # The EOW ecological report must use its own appendix.
  expect_true(
    "report::appendix_eow" %in% eow
  )
  
  expect_false(
    "report::appendix" %in% eow
  )
  
  report_pool_before <- names(
    cm$reportObjectElements$Modules
  )
  
  status_pool_before <- names(
    cm$reportObjectElements$Status_modules
  )
  
  # Both appendix modules should remain available in the module pool:
  # the generic appendix for the default workflow and appendix_eow
  # for the EOW profile.
  expect_true(
    "appendix" %in% report_pool_before
  )
  
  expect_true(
    "appendix_eow" %in% report_pool_before
  )
  
  selected <- sections(
    cm,
    n = eow,
    profile = "EOW",
    report_type = "report"
  )
  
  expect_s4_class(
    selected,
    "camReport"
  )
  
  # Applying a profile must not alter the underlying module pools.
  expect_named(
    cm$reportObjectElements$Modules,
    report_pool_before
  )
  
  expect_named(
    cm$reportObjectElements$Status_modules,
    status_pool_before
  )
  
  # Check the EOW appendix itself.
  appendix_eow <- find_test_report_section(
    cm$reportObjects,
    "appendix_eow"
  )
  
  expect_s4_class(
    appendix_eow,
    ".textSection"
  )
  
  expect_identical(
    appendix_eow@title,
    "Appendix"
  )
  
  expect_false(
    grepl(
      ".tabset",
      appendix_eow@title,
      fixed = TRUE
    )
  )
  
  # Status modules inserted in the EOW ecological report must
  # be children of appendix_eow.
  expect_s4_class(
    find_test_report_section(
      cm$reportObjects,
      "abstract"
    ),
    ".textSection"
  )
  
  expect_identical(
    find_test_report_section(
      cm$reportObjects,
      "abstract"
    )@parent,
    "appendix_eow"
  )
  
  expect_s4_class(
    find_test_report_section(
      cm$reportObjects,
      "spatial"
    ),
    ".textSection"
  )
  
  expect_identical(
    find_test_report_section(
      cm$reportObjects,
      "spatial"
    )@parent,
    "appendix_eow"
  )
  
  expect_s4_class(
    find_test_report_section(
      cm$reportObjects,
      "temporal"
    ),
    ".textSection"
  )
  
  expect_identical(
    find_test_report_section(
      cm$reportObjects,
      "temporal"
    )@parent,
    "appendix_eow"
  )
  
  expect_s4_class(
    find_test_report_section(
      cm$reportObjects,
      "availability"
    ),
    ".textSection"
  )
  
  expect_identical(
    find_test_report_section(
      cm$reportObjects,
      "availability"
    )@parent,
    "appendix_eow"
  )
})


test_that("generic appendix remains available for the default workflow", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  generic_appendix <-
    cm$reportObjectElements$Modules[["appendix"]]
  
  expect_s4_class(
    generic_appendix,
    ".textSection"
  )
  
  expect_identical(
    generic_appendix@name,
    "appendix"
  )
  
  # The existing generic appendix is intentionally left unchanged.
  expect_true(
    grepl(
      ".tabset",
      generic_appendix@title,
      fixed = TRUE
    )
  )
})


test_that("profiles can be written, read, registered, and edited", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  brief <- reportProfile(
    name = "brief",
    description = "A test profile",
    report = c(
      "introduction",
      "methods"
    ),
    status = c(
      "abstract",
      "availability"
    )
  )
  
  file <- tempfile(
    fileext = ".yml"
  )
  
  on.exit(
    unlink(
      file,
      force = TRUE
    ),
    add = TRUE
  )
  
  # Keep the explicit forward-slash normalization.
  # This is required for cross-platform behaviour on Windows.
  expect_identical(
    write_profile(
      brief,
      file
    ),
    normalizePath(
      file,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  expect_identical(
    read_profile(file),
    brief
  )
  
  add_profile(
    cm,
    file
  )
  
  expect_true(
    "brief" %in% profile_names(cm)
  )
  
  expect_identical(
    sections(
      cm,
      profile = "brief"
    ),
    c(
      "report::introduction",
      "report::methods"
    )
  )
  
  sections(
    cm,
    n = "report::introduction",
    profile = "brief",
    report_type = "report"
  )
  
  expect_identical(
    sections(
      cm,
      profile = "brief"
    ),
    "report::introduction"
  )
  
  sections(
    cm,
    n = c(
      "status::abstract",
      "status::availability"
    ),
    profile = "brief",
    report_type = "status"
  )
  
  expect_identical(
    sections(
      cm,
      profile = "brief",
      report_type = "status"
    ),
    c(
      "status::abstract",
      "status::availability"
    )
  )
})


test_that("section_names distinguishes pools and profile selections", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  all_names <- section_names(
    object = cm,
    report_type = "all"
  )
  
  profile_modules <- section_names(
    object = cm,
    profile = "EOW",
    report_type = "report"
  )
  
  expect_true(
    any(
      startsWith(
        all_names,
        "report::"
      )
    )
  )
  
  expect_true(
    any(
      startsWith(
        all_names,
        "status::"
      )
    )
  )
  
  expect_true(
    all(
      grepl(
        "::",
        profile_modules,
        fixed = TRUE
      )
    )
  )
  
  expect_true(
    "status::availability" %in% profile_modules
  )
  
  expect_true(
    "report::appendix_eow" %in% profile_modules
  )
  
  expect_false(
    "report::appendix" %in% profile_modules
  )
  
  without_methods <- section_names(
    object = cm,
    profile = "EOW",
    exclude = "methods"
  )
  
  expect_false(
    "report::methods" %in% without_methods
  )
  
  expect_false(
    "report::study_area" %in% without_methods
  )
  
  expect_false(
    "report::location_eow" %in% without_methods
  )
})


test_that("invalid profile hierarchies do not replace the attached tree", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  before <- names(
    cm$reportObjects
  )
  
  invalid <- reportProfile(
    name = "orphan",
    report = "study_area"
  )
  
  add_profile(
    cm,
    invalid
  )
  
  expect_error(
    sections(
      cm,
      n = "study_area",
      profile = "orphan"
    ),
    "parent 'methods' must appear earlier"
  )
  
  expect_named(
    cm$reportObjects,
    before
  )
})


test_that("PDF output is inferred and uses the HTML conversion route", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  output <- tempfile(
    fileext = ".pdf"
  )
  
  rmd <- sub(
    "\\.pdf$",
    ".Rmd",
    output
  )
  
  on.exit(
    unlink(
      c(
        output,
        rmd
      ),
      force = TRUE
    ),
    add = TRUE
  )
  
  result <- testthat::with_mocked_bindings(
    report(
      cm,
      filename = output,
      view = FALSE,
      test = FALSE
    ),
    
    .generate_report = function(
    object,
    output_file,
    rmd_file,
    toc = TRUE
    ) {
      writeLines(
        "<html><body>test</body></html>",
        output_file
      )
      
      writeLines(
        "---",
        rmd_file
      )
      
      output_file
    },
    
    .convert_html_to_pdf = function(
    html_file,
    pdf_file
    ) {
      expect_true(
        file.exists(html_file)
      )
      
      writeLines(
        "PDF test",
        pdf_file
      )
      
      pdf_file
    },
    
    .package = "camtrapReport"
  )
  
  expect_identical(
    normalizePath(
      result,
      winslash = "/",
      mustWork = TRUE
    ),
    normalizePath(
      output,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  expect_true(
    file.exists(output)
  )
  
  expect_true(
    file.exists(rmd)
  )
  
  expect_error(
    report(
      cm,
      filename = output,
      format = "html"
    ),
    "disagree"
  )
})


test_that("HTML keeps the TOC while PDF disables it", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  
  seen <- new.env(
    parent = emptyenv()
  )
  
  seen$html_toc <- NA
  seen$pdf_toc <- NA
  
  html_output <- tempfile(
    fileext = ".html"
  )
  
  pdf_output <- tempfile(
    fileext = ".pdf"
  )
  
  html_rmd <- sub(
    "\\.html$",
    ".Rmd",
    html_output
  )
  
  pdf_rmd <- sub(
    "\\.pdf$",
    ".Rmd",
    pdf_output
  )
  
  on.exit(
    unlink(
      c(
        html_output,
        pdf_output,
        html_rmd,
        pdf_rmd
      ),
      force = TRUE
    ),
    add = TRUE
  )
  
  # Normal HTML report should retain the TOC.
  testthat::with_mocked_bindings(
    report(
      cm,
      filename = html_output,
      view = FALSE,
      test = FALSE
    ),
    
    .generate_report = function(
    object,
    output_file,
    rmd_file,
    toc = TRUE
    ) {
      seen$html_toc <- toc
      
      writeLines(
        "<html><body>HTML test</body></html>",
        output_file
      )
      
      writeLines(
        "---",
        rmd_file
      )
      
      output_file
    },
    
    .package = "camtrapReport"
  )
  
  expect_true(
    isTRUE(seen$html_toc)
  )
  
  # PDF should use an intermediate HTML report with its TOC disabled.
  testthat::with_mocked_bindings(
    report(
      cm,
      filename = pdf_output,
      view = FALSE,
      test = FALSE
    ),
    
    .generate_report = function(
    object,
    output_file,
    rmd_file,
    toc = TRUE
    ) {
      seen$pdf_toc <- toc
      
      writeLines(
        "<html><body>PDF source test</body></html>",
        output_file
      )
      
      writeLines(
        "---",
        rmd_file
      )
      
      output_file
    },
    
    .convert_html_to_pdf = function(
    html_file,
    pdf_file
    ) {
      expect_true(
        file.exists(html_file)
      )
      
      writeLines(
        "PDF test",
        pdf_file
      )
      
      pdf_file
    },
    
    .package = "camtrapReport"
  )
  
  expect_false(
    seen$pdf_toc
  )
  
  expect_true(
    file.exists(html_output)
  )
  
  expect_true(
    file.exists(pdf_output)
  )
})