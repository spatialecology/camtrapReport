test_that("installation helpers validate and classify package names", {
  is_installed <- ct_internal(".is.installed")
  load_lib <- ct_internal(".loadLib")
  
  empty_result <- is_installed(character())
  
  expect_type(empty_result, "logical")
  expect_length(empty_result, 0L)
  expect_named(empty_result, character())
  
  installed <- is_installed(
    c(
      "methods",
      "stats",
      "",
      NA_character_,
      "a_package_that_does_not_exist_12345"
    )
  )
  
  expect_true(unname(installed["methods"]))
  expect_true(unname(installed["stats"]))
  
  expect_false(
    unname(installed[3])
  )
  
  expect_false(
    unname(installed[4])
  )
  
  expect_false(
    unname(
      installed["a_package_that_does_not_exist_12345"]
    )
  )
  
  expect_identical(
    unname(
      load_lib(
        list(
          "methods",
          c("stats", "methods")
        )
      )
    ),
    c(TRUE, TRUE)
  )
  
  expect_false(
    unname(
      load_lib(
        list(
          "a_package_that_does_not_exist_12345"
        )
      )
    )[1]
  )
})


test_that("install_All rejects invalid arguments before installation", {
  expect_error(
    install_All(update = NA),
    "'update' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    install_All(update = 1),
    "'update' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    install_All(update = c(TRUE, FALSE)),
    "'update' must be TRUE or FALSE",
    fixed = TRUE
  )
  
  expect_error(
    install_All(pkgs = 1),
    "'pkgs' must be NULL or a character vector",
    fixed = TRUE
  )
  
  expect_error(
    install_All(pkgs = c("methods", NA_character_)),
    "'pkgs' must be NULL or a character vector",
    fixed = TRUE
  )
})


test_that("install_All reports when all requested packages are installed", {
  state <- new.env(parent = emptyenv())
  state$checked_packages <- character()
  
  testthat::local_mocked_bindings(
    .getPackageList = function() {
      c("methods", "stats")
    },
    .getPackageGitHubList = function() {
      character()
    },
    .is.installed = function(n) {
      n <- as.character(n)
      
      state$checked_packages <- unique(
        c(
          state$checked_packages,
          n
        )
      )
      
      result <- rep(TRUE, length(n))
      names(result) <- n
      result
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    # nolint next: implicit_assignment_linter.
    result <- install_All(
      pkgs = c(" methods ", "", "stats"),
      update = FALSE
    ),
    "All required packages have already been installed",
    fixed = TRUE
  )
  
  expect_null(result)
  
  expect_true(
    all(
      c("methods", "stats") %in% state$checked_packages
    )
  )
})


test_that("install_All attempts only missing CRAN packages", {
  state <- new.env(parent = emptyenv())
  state$installed <- FALSE
  state$installation_calls <- character()
  
  testthat::local_mocked_bindings(
    .getPackageList = function() {
      c("methods", "missingPackage")
    },
    .getPackageGitHubList = function() {
      character()
    },
    .is.installed = function(n) {
      n <- as.character(n)
      
      result <- vapply(
        n,
        function(package) {
          if (identical(package, "methods")) {
            return(TRUE)
          }
          
          if (identical(package, "missingPackage")) {
            return(state$installed)
          }
          
          FALSE
        },
        logical(1)
      )
      
      names(result) <- n
      result
    },
    install.packages = function(pkgs, ...) {
      state$installation_calls <- c(
        state$installation_calls,
        as.character(pkgs)
      )
      
      state$installed <- TRUE
      invisible(NULL)
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    # nolint next: implicit_assignment_linter.
    result <- install_All(update = FALSE),
    "1 package was successfully installed",
    fixed = TRUE
  )
  
  expect_null(result)
  
  expect_identical(
    state$installation_calls,
    "missingPackage"
  )
})


test_that("install_All reports CRAN installation failures safely", {
  state <- new.env(parent = emptyenv())
  state$installation_calls <- character()
  
  testthat::local_mocked_bindings(
    .getPackageList = function() {
      c("missingOne", "missingTwo")
    },
    .getPackageGitHubList = function() {
      character()
    },
    .is.installed = function(n) {
      n <- as.character(n)
      
      result <- rep(FALSE, length(n))
      names(result) <- n
      result
    },
    install.packages = function(pkgs, ...) {
      state$installation_calls <- c(
        state$installation_calls,
        as.character(pkgs)
      )
      
      stop("Synthetic installation failure")
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    # nolint next: implicit_assignment_linter.
    result <- install_All(update = FALSE),
    "The following packages could not be installed:",
    fixed = TRUE
  )
  
  expect_null(result)
  
  expect_setequal(
    state$installation_calls,
    c("missingOne", "missingTwo")
  )
})


test_that(
  "install_All handles missing GitHub packages without network access",
  {
    state <- new.env(parent = emptyenv())
    state$github_calls <- character()
    
    testthat::local_mocked_bindings(
      .getPackageList = function() {
        character()
      },
      .getPackageGitHubList = function() {
        c(
          githubPackage = "example/exampleRepository"
        )
      },
      .is.installed = function(n) {
        n <- as.character(n)
        
        result <- rep(FALSE, length(n))
        names(result) <- n
        result
      },
      .installGitHub = function(repository) {
        state$github_calls <- c(
          state$github_calls,
          repository
        )
        
        FALSE
      },
      .package = "camtrapReport"
    )
    
    expect_output(
      # nolint next: implicit_assignment_linter.
      result <- install_All(update = FALSE),
      "githubPackage",
      fixed = TRUE
    )
    
    expect_null(result)
    
    expect_identical(
      state$github_calls,
      "example/exampleRepository"
    )
  }
)


test_that("install_All counts successful mocked GitHub installations", {
  state <- new.env(parent = emptyenv())
  state$github_installed <- FALSE
  
  testthat::local_mocked_bindings(
    .getPackageList = function() {
      character()
    },
    .getPackageGitHubList = function() {
      c(
        githubPackage = "example/exampleRepository"
      )
    },
    .is.installed = function(n) {
      n <- as.character(n)
      
      result <- vapply(
        n,
        function(package) {
          identical(package, "githubPackage") &&
            state$github_installed
        },
        logical(1)
      )
      
      names(result) <- n
      result
    },
    .installGitHub = function(repository) {
      expect_identical(
        repository,
        "example/exampleRepository"
      )
      
      state$github_installed <- TRUE
      TRUE
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    # nolint next: implicit_assignment_linter.
    result <- install_All(update = FALSE),
    "1 package was successfully installed",
    fixed = TRUE
  )
  
  expect_null(result)
})


test_that(
  "install_All update mode handles an empty optional package list",
  {
    testthat::local_mocked_bindings(
      .getPackageList = function() {
        character()
      },
      .getPackageGitHubList = function() {
        character()
      },
      .package = "camtrapReport"
    )
    
    expect_output(
      # nolint next: implicit_assignment_linter.
      result <- install_All(update = TRUE),
      "There are no optional packages to update",
      fixed = TRUE
    )
    
    expect_null(result)
  }
)


test_that("install_All update mode reinstalls mocked CRAN packages", {
  state <- new.env(parent = emptyenv())
  state$installation_calls <- character()
  state$reinstalled <- FALSE
  
  testthat::local_mocked_bindings(
    .getPackageList = function() {
      "optionalPackage"
    },
    .getPackageGitHubList = function() {
      character()
    },
    .is.installed = function(n) {
      n <- as.character(n)
      
      result <- vapply(
        n,
        function(package) {
          identical(package, "optionalPackage") &&
            !state$reinstalled
        },
        logical(1)
      )
      
      if (state$reinstalled) {
        result[n == "optionalPackage"] <- TRUE
      }
      
      names(result) <- n
      result
    },
    install.packages = function(pkgs, ...) {
      state$installation_calls <- c(
        state$installation_calls,
        as.character(pkgs)
      )
      
      state$reinstalled <- TRUE
      invisible(NULL)
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    # nolint next: implicit_assignment_linter.
    result <- install_All(update = TRUE),
    "successfully reinstalled",
    fixed = TRUE
  )
  
  expect_null(result)
  
  expect_identical(
    state$installation_calls,
    "optionalPackage"
  )
})


test_that(
  "install_All update mode reports unsuccessful reinstallations",
  {
    state <- new.env(parent = emptyenv())
    state$installation_calls <- character()
    
    testthat::local_mocked_bindings(
      .getPackageList = function() {
        "optionalPackage"
      },
      .getPackageGitHubList = function() {
        character()
      },
      .is.installed = function(n) {
        n <- as.character(n)
        
        result <- rep(TRUE, length(n))
        
        if (length(state$installation_calls) > 0L) {
          result[] <- FALSE
        }
        
        names(result) <- n
        result
      },
      install.packages = function(pkgs, ...) {
        state$installation_calls <- c(
          state$installation_calls,
          as.character(pkgs)
        )
        
        stop("Synthetic reinstall failure")
      },
      .package = "camtrapReport"
    )
    
    expect_output(
      # nolint next: implicit_assignment_linter.
      result <- install_All(update = TRUE),
      "The following packages could not be installed:",
      fixed = TRUE
    )
    
    expect_null(result)
    
    expect_identical(
      state$installation_calls,
      "optionalPackage"
    )
  }
)
