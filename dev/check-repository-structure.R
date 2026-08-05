check_camtrap_repository <- function(root = ".") {
  root <- normalizePath(
    root,
    winslash = "/",
    mustWork = TRUE
  )
  
  old_directory <- getwd()
  setwd(root)
  
  on.exit(
    setwd(old_directory),
    add = TRUE
  )
  
  results <- data.frame(
    status = character(),
    item = character(),
    details = character(),
    stringsAsFactors = FALSE
  )
  
  add_result <- function(
    condition,
    item,
    success_details = "Correct",
    failure_details = "Problem detected",
    severity = "ERROR"
  ) {
    condition <- isTRUE(condition)
    
    results <<- rbind(
      results,
      data.frame(
        status = if (condition) "OK" else severity,
        item = item,
        details = if (condition) {
          success_details
        } else {
          failure_details
        },
        stringsAsFactors = FALSE
      )
    )
  }
  
  # ------------------------------------------------------------
  # 1. Confirm repository root
  # ------------------------------------------------------------
  
  add_result(
    file.exists("DESCRIPTION"),
    "Repository root",
    paste0("DESCRIPTION found in: ", root),
    paste0(
      "DESCRIPTION is not present in: ",
      root,
      ". Run the script from the camtrapReport root folder."
    )
  )
  
  if (!file.exists("DESCRIPTION")) {
    print(results, row.names = FALSE)
    stop("This is not the root of an R package.")
  }
  
  # ------------------------------------------------------------
  # 2. Required package files and folders
  # ------------------------------------------------------------
  
  required_paths <- c(
    "DESCRIPTION",
    "NAMESPACE",
    "R",
    "man",
    "tests",
    "tests/testthat",
    "tests/testthat.R",
    "vignettes",
    "inst",
    "README.md",
    "NEWS.md",
    "LICENSE",
    ".Rbuildignore",
    ".gitignore",
    ".github",
    ".github/workflows",
    ".github/CONTRIBUTING.md"
  )
  
  for (path in required_paths) {
    add_result(
      file.exists(path) || dir.exists(path),
      paste0("Required path: ", path),
      paste0(path, " is present"),
      paste0(path, " is missing")
    )
  }
  
  # ------------------------------------------------------------
  # 3. Optional but useful files
  # ------------------------------------------------------------
  
  optional_paths <- c(
    "CITATION.cff",
    "LICENSE.md",
    "_pkgdown.yml",
    "pkgdown",
    "docs"
  )
  
  for (path in optional_paths) {
    add_result(
      file.exists(path) || dir.exists(path),
      paste0("Optional path: ", path),
      paste0(path, " is present"),
      paste0(path, " is not present"),
      severity = "INFO"
    )
  }
  
  # ------------------------------------------------------------
  # 4. DESCRIPTION checks
  # ------------------------------------------------------------
  
  description <- read.dcf("DESCRIPTION")
  
  required_description_fields <- c(
    "Package",
    "Title",
    "Version",
    "Description",
    "License",
    "Encoding"
  )
  
  description_fields <- colnames(description)
  
  for (field in required_description_fields) {
    present <- field %in% description_fields &&
      nzchar(trimws(description[1, field]))
    
    add_result(
      present,
      paste0("DESCRIPTION field: ", field),
      paste0(field, " is defined"),
      paste0(field, " is missing or empty")
    )
  }
  
  package_name <- if ("Package" %in% description_fields) {
    description[1, "Package"]
  } else {
    NA_character_
  }
  
  package_version <- if ("Version" %in% description_fields) {
    description[1, "Version"]
  } else {
    NA_character_
  }
  
  add_result(
    identical(package_name, "camtrapReport"),
    "Package name",
    "Package name is camtrapReport",
    paste0(
      "Expected camtrapReport, found: ",
      package_name
    )
  )
  
  has_author_information <-
    "Authors@R" %in% description_fields ||
    all(c("Author", "Maintainer") %in% description_fields)
  
  add_result(
    has_author_information,
    "DESCRIPTION author information",
    "Author information is available",
    "Add Authors@R or both Author and Maintainer"
  )
  
  # ------------------------------------------------------------
  # 5. Source, documentation and tests
  # ------------------------------------------------------------
  
  r_files <- list.files(
    "R",
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  rd_files <- list.files(
    "man",
    pattern = "\\.Rd$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  test_files <- list.files(
    "tests/testthat",
    pattern = "^test-.*\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  vignette_files <- list.files(
    "vignettes",
    pattern = "\\.(Rmd|qmd)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  workflow_files <- list.files(
    ".github/workflows",
    pattern = "\\.(yml|yaml)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  add_result(
    length(r_files) > 0L,
    "R source files",
    paste(length(r_files), "R source files found"),
    "No R source files found under R/"
  )
  
  add_result(
    length(rd_files) > 0L,
    "Documentation files",
    paste(length(rd_files), "Rd files found"),
    "No Rd documentation found under man/"
  )
  
  add_result(
    length(test_files) > 0L,
    "testthat files",
    paste(length(test_files), "test files found"),
    "No test-*.R files found under tests/testthat/"
  )
  
  add_result(
    length(vignette_files) > 0L,
    "Vignette sources",
    paste(length(vignette_files), "vignette files found"),
    "No Rmd or qmd files found under vignettes/",
    severity = "WARNING"
  )
  
  add_result(
    length(workflow_files) > 0L,
    "GitHub Actions workflows",
    paste(length(workflow_files), "workflow files found"),
    "No GitHub Actions workflows found"
  )
  
  # ------------------------------------------------------------
  # 6. Detect misplaced R and test files
  # ------------------------------------------------------------
  
  root_r_files <- list.files(
    ".",
    pattern = "\\.[Rr]$",
    recursive = FALSE,
    full.names = FALSE
  )
  
  add_result(
    length(root_r_files) == 0L,
    "R files in repository root",
    "No misplaced R files in the repository root",
    paste(
      "R files found in the repository root:",
      paste(root_r_files, collapse = ", ")
    ),
    severity = "WARNING"
  )
  
  all_test_files <- list.files(
    ".",
    pattern = "^test-.*\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  misplaced_tests <- all_test_files[
    !grepl(
      "^\\./?tests/testthat/",
      gsub("\\\\", "/", all_test_files)
    )
  ]
  
  add_result(
    length(misplaced_tests) == 0L,
    "Test file locations",
    "All test-*.R files are under tests/testthat/",
    paste(
      "Misplaced test files:",
      paste(misplaced_tests, collapse = ", ")
    )
  )
  
  # ------------------------------------------------------------
  # 7. NEWS.md checks
  # ------------------------------------------------------------
  
  if (file.exists("NEWS.md")) {
    news_lines <- readLines(
      "NEWS.md",
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    news_has_version <- any(
      grepl(
        package_version,
        news_lines,
        fixed = TRUE
      )
    )
    
    add_result(
      news_has_version,
      "NEWS.md package version",
      paste0(
        "NEWS.md contains version ",
        package_version
      ),
      paste0(
        "NEWS.md does not mention current version ",
        package_version
      ),
      severity = "WARNING"
    )
  }
  
  # ------------------------------------------------------------
  # 8. CONTRIBUTING.md → NEWS.md link
  # ------------------------------------------------------------
  
  if (file.exists(".github/CONTRIBUTING.md")) {
    contributing <- readLines(
      ".github/CONTRIBUTING.md",
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    has_correct_news_link <- any(
      grepl(
        "\\[.*NEWS\\.md.*\\]\\(\\.\\./NEWS\\.md\\)",
        contributing,
        perl = TRUE
      )
    )
    
    add_result(
      has_correct_news_link,
      "CONTRIBUTING.md link to NEWS.md",
      "Correct relative link found: ../NEWS.md",
      paste0(
        "Use this Markdown link: ",
        "[`NEWS.md`](../NEWS.md)"
      )
    )
  }
  
  # ------------------------------------------------------------
  # 9. Check local Markdown links
  # ------------------------------------------------------------
  
  markdown_files <- list.files(
    ".",
    pattern = "\\.md$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  markdown_files <- markdown_files[
    !grepl(
      "^\\./?(docs|pkgdown)/",
      gsub("\\\\", "/", markdown_files)
    )
  ]
  
  extract_links <- function(file) {
    text <- paste(
      readLines(
        file,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
    
    matches <- gregexpr(
      "\\[[^\\]]*\\]\\(([^)]+)\\)",
      text,
      perl = TRUE
    )
    
    links <- regmatches(text, matches)[[1]]
    
    if (
      length(links) == 0L ||
      identical(links, character(0))
    ) {
      return(character())
    }
    
    sub(
      "^.*\\]\\(([^)]+)\\)$",
      "\\1",
      links,
      perl = TRUE
    )
  }
  
  broken_links <- character()
  
  for (markdown_file in markdown_files) {
    links <- extract_links(markdown_file)
    
    for (link in links) {
      link <- trimws(link)
      
      link <- sub(
        "[[:space:]]+[\"'].*$",
        "",
        link
      )
      
      if (
        !nzchar(link) ||
        startsWith(link, "#") ||
        grepl(
          "^(https?|mailto|ftp):",
          link,
          ignore.case = TRUE
        )
      ) {
        next
      }
      
      link <- sub("#.*$", "", link)
      link <- sub("\\?.*$", "", link)
      link <- URLdecode(link)
      
      if (!nzchar(link)) {
        next
      }
      
      candidate <- if (startsWith(link, "/")) {
        file.path(
          root,
          substring(link, 2)
        )
      } else {
        file.path(
          dirname(markdown_file),
          link
        )
      }
      
      if (!file.exists(candidate) && !dir.exists(candidate)) {
        broken_links <- c(
          broken_links,
          paste0(
            markdown_file,
            " → ",
            link
          )
        )
      }
    }
  }
  
  broken_links <- unique(broken_links)
  
  add_result(
    length(broken_links) == 0L,
    "Local Markdown links",
    "No broken local Markdown links detected",
    paste(
      "Broken links:",
      paste(broken_links, collapse = " | ")
    )
  )
  
  # ------------------------------------------------------------
  # 10. Check files tracked by Git
  # ------------------------------------------------------------
  
  git_path <- Sys.which("git")
  
  if (nzchar(git_path)) {
    tracked_files <- system2(
      git_path,
      args = "ls-files",
      stdout = TRUE,
      stderr = FALSE
    )
    
    files_that_should_be_tracked <- c(
      "DESCRIPTION",
      "NAMESPACE",
      "README.md",
      "NEWS.md",
      "LICENSE",
      ".github/CONTRIBUTING.md",
      ".github/workflows/test-coverage.yaml",
      "tests/testthat/test-report-branches.R",
      "tests/testthat/test-report-object-branches.R",
      "tests/testthat/test-traprate-data.R"
    )
    
    for (file in files_that_should_be_tracked) {
      if (file.exists(file)) {
        add_result(
          file %in% tracked_files,
          paste0("Git tracking: ", file),
          paste0(file, " is tracked by Git"),
          paste0(
            file,
            " exists locally but is not tracked by Git"
          )
        )
      }
    }
    
    branch <- system2(
      git_path,
      args = c(
        "branch",
        "--show-current"
      ),
      stdout = TRUE,
      stderr = FALSE
    )
    
    add_result(
      identical(trimws(branch), "main"),
      "Current Git branch",
      "Current branch is main",
      paste0(
        "Current branch is ",
        paste(branch, collapse = "")
      ),
      severity = "WARNING"
    )
    
    remote <- system2(
      git_path,
      args = c(
        "remote",
        "get-url",
        "origin"
      ),
      stdout = TRUE,
      stderr = FALSE
    )
    
    add_result(
      any(
        grepl(
          "spatialecology/camtrapReport",
          remote,
          fixed = TRUE
        )
      ),
      "Git origin",
      paste0(
        "Origin points to ",
        paste(remote, collapse = "")
      ),
      paste0(
        "Unexpected Git origin: ",
        paste(remote, collapse = "")
      ),
      severity = "WARNING"
    )
    
    git_status <- system2(
      git_path,
      args = c(
        "status",
        "--short"
      ),
      stdout = TRUE,
      stderr = FALSE
    )
    
    add_result(
      length(git_status) == 0L,
      "Git working tree",
      "No uncommitted or untracked changes",
      paste(
        "Working tree contains changes:",
        paste(git_status, collapse = " | ")
      ),
      severity = "WARNING"
    )
  } else {
    add_result(
      FALSE,
      "Git installation",
      failure_details = "Git was not found",
      severity = "WARNING"
    )
  }
  
  # ------------------------------------------------------------
  # 11. Development and generated artifacts
  # ------------------------------------------------------------
  
  suspicious_patterns <- c(
    "^coverage.*\\.rds$",
    "^coverage.*\\.csv$",
    "\\.RData$",
    "\\.Rhistory$",
    "\\.tar\\.gz$",
    "\\.Rcheck/",
    "00LOCK",
    "pkgcheck-parser-fix-backup",
    "camR-before-Rchunk-index-fix"
  )
  
  repository_files <- list.files(
    ".",
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  
  repository_files <- gsub(
    "\\\\",
    "/",
    repository_files
  )
  
  suspicious_files <- unique(
    unlist(
      lapply(
        suspicious_patterns,
        function(pattern) {
          grep(
            pattern,
            repository_files,
            value = TRUE,
            ignore.case = TRUE
          )
        }
      ),
      use.names = FALSE
    )
  )
  
  add_result(
    length(suspicious_files) == 0L,
    "Development artifacts",
    "No suspicious generated or backup files detected",
    paste(
      "Review these generated or backup files:",
      paste(suspicious_files, collapse = " | ")
    ),
    severity = "WARNING"
  )
  
  # ------------------------------------------------------------
  # 12. .Rbuildignore checks
  # ------------------------------------------------------------
  
  if (file.exists(".Rbuildignore")) {
    build_ignore <- readLines(
      ".Rbuildignore",
      warn = FALSE,
      encoding = "UTF-8"
    )
    
    is_build_ignored <- function(path) {
      any(
        vapply(
          build_ignore,
          function(pattern) {
            tryCatch(
              grepl(pattern, path),
              error = function(e) FALSE
            )
          },
          logical(1)
        )
      )
    }
    
    generated_paths <- c(
      ".github",
      "docs",
      "pkgdown",
      "dev",
      "_pkgdown.yml"
    )
    
    for (path in generated_paths) {
      if (file.exists(path) || dir.exists(path)) {
        add_result(
          is_build_ignored(path),
          paste0(".Rbuildignore: ", path),
          paste0(path, " is excluded from package builds"),
          paste0(
            path,
            " exists but is not matched by .Rbuildignore"
          ),
          severity = "WARNING"
        )
      }
    }
  }
  
  # ------------------------------------------------------------
  # 13. Case-insensitive duplicate paths
  # ------------------------------------------------------------
  
  all_paths <- list.files(
    ".",
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  
  lower_paths <- tolower(all_paths)
  
  duplicated_case_paths <- unique(
    all_paths[
      duplicated(lower_paths) |
        duplicated(lower_paths, fromLast = TRUE)
    ]
  )
  
  add_result(
    length(duplicated_case_paths) == 0L,
    "Filename case collisions",
    "No case-insensitive path collisions detected",
    paste(
      "Potential filename case collisions:",
      paste(duplicated_case_paths, collapse = " | ")
    )
  )
  
  # ------------------------------------------------------------
  # Report
  # ------------------------------------------------------------
  
  status_order <- c(
    "ERROR",
    "WARNING",
    "INFO",
    "OK"
  )
  
  results$status <- factor(
    results$status,
    levels = status_order
  )
  
  results <- results[
    order(results$status, results$item),
    ,
    drop = FALSE
  ]
  
  results$status <- as.character(results$status)
  rownames(results) <- NULL
  
  cat(
    "\n",
    "============================================================\n",
    "camtrapReport repository structure check\n",
    "Root: ",
    root,
    "\n",
    "============================================================\n\n",
    sep = ""
  )
  
  print(
    results,
    row.names = FALSE
  )
  
  n_errors <- sum(results$status == "ERROR")
  n_warnings <- sum(results$status == "WARNING")
  
  cat(
    "\nSummary:\n",
    "  Errors:   ", n_errors, "\n",
    "  Warnings: ", n_warnings, "\n",
    "  Checks:   ", nrow(results), "\n",
    sep = ""
  )
  
  if (n_errors > 0L) {
    stop(
      "Repository structure problems were detected.",
      call. = FALSE
    )
  }
  
  cat(
    "\nRepository structure check completed successfully.\n"
  )
  
  invisible(results)
}