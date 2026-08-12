test_that("installation helpers validate and classify package names", {
  is_installed <- camtrapReport:::.is.installed
  load_lib <- camtrapReport:::.loadLib
  
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
          c("methods"),
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
          c("a_package_that_does_not_exist_12345")
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
  checked_packages <- character()
  
  testthat::local_mocked_bindings(
    .getPackageList = function() {
      c("methods", "stats")
    },
    .getPackageGitHubList = function() {
      character()
    },
    .is.installed = function(n) {
      n <- as.character(n)
      
      checked_packages <<- unique(
        c(
          checked_packages,
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
      c("methods", "stats") %in% checked_packages
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
  installation_calls <- character()
  
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
      installation_calls <<- c(
        installation_calls,
        as.character(pkgs)
      )
      
      stop("Synthetic installation failure")
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    result <- install_All(update = FALSE),
    "The following packages could not be installed:",
    fixed = TRUE
  )
  
  expect_null(result)
  
  expect_setequal(
    installation_calls,
    c("missingOne", "missingTwo")
  )
})


test_that(
  "install_All handles missing GitHub packages without network access",
  {
    github_calls <- character()
    
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
        github_calls <<- c(
          github_calls,
          repository
        )
        
        FALSE
      },
      .package = "camtrapReport"
    )
    
    expect_output(
      result <- install_All(update = FALSE),
      "githubPackage",
      fixed = TRUE
    )
    
    expect_null(result)
    
    expect_identical(
      github_calls,
      "example/exampleRepository"
    )
  }
)


test_that("install_All counts successful mocked GitHub installations", {
  github_installed <- FALSE
  
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
            github_installed
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
      
      github_installed <<- TRUE
      TRUE
    },
    .package = "camtrapReport"
  )
  
  expect_output(
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
      result <- install_All(update = TRUE),
      "There are no optional packages to update",
      fixed = TRUE
    )
    
    expect_null(result)
  }
)


test_that("install_All update mode reinstalls mocked CRAN packages", {
  installation_calls <- character()
  reinstalled <- FALSE
  
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
            !reinstalled
        },
        logical(1)
      )
      
      if (reinstalled) {
        result[n == "optionalPackage"] <- TRUE
      }
      
      names(result) <- n
      result
    },
    install.packages = function(pkgs, ...) {
      installation_calls <<- c(
        installation_calls,
        as.character(pkgs)
      )
      
      reinstalled <<- TRUE
      invisible(NULL)
    },
    .package = "camtrapReport"
  )
  
  expect_output(
    result <- install_All(update = TRUE),
    "successfully reinstalled",
    fixed = TRUE
  )
  
  expect_null(result)
  
  expect_identical(
    installation_calls,
    "optionalPackage"
  )
})


test_that(
  "install_All update mode reports unsuccessful reinstallations",
  {
    installation_calls <- character()
    
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
        
        if (length(installation_calls) > 0L) {
          result[] <- FALSE
        }
        
        names(result) <- n
        result
      },
      install.packages = function(pkgs, ...) {
        installation_calls <<- c(
          installation_calls,
          as.character(pkgs)
        )
        
        stop("Synthetic reinstall failure")
      },
      .package = "camtrapReport"
    )
    
    expect_output(
      result <- install_All(update = TRUE),
      "The following packages could not be installed:",
      fixed = TRUE
    )
    
    expect_null(result)
    
    expect_identical(
      installation_calls,
      "optionalPackage"
    )
  }
)