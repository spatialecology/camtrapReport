# Functions for installing optional camtrapReport dependencies
# Licence: MIT
#--------

#--------


#--------

.is.installed <- function(n) {
  if (length(n) == 0L) {
    return(setNames(logical(0), character(0)))
  }
  
  n <- as.character(n)
  
  installed <- vapply(
    n,
    function(pkg) {
      !is.na(pkg) &&
        nzchar(pkg) &&
        nzchar(system.file(package = pkg))
    },
    logical(1)
  )
  
  names(installed) <- n
  installed
}

#--------

#--------

.loadLib <- function(pkgs) {
  suppressWarnings(
    vapply(
      pkgs,
      function(x) {
        all(vapply(x, .require, logical(1)))
      },
      logical(1)
    )
  )
}

#--------

.getPackageList <- function() {
  packages <- .get_module_packages()
  
  config_file <- system.file(
    "external",
    "camtrapReportConfig.rds",
    package = "camtrapReport"
  )
  
  if (nzchar(config_file) && file.exists(config_file)) {
    config <- readRDS(config_file)
    
    if (!is.null(config$packages)) {
      packages <- unique(c(packages, config$packages))
    }
  }
  
  packages <- as.character(packages)
  packages <- packages[!is.na(packages) & nzchar(packages)]
  
  unique(packages)
}

#--------

.getPackageGitHubList <- function() {
  config_file <- system.file(
    "external",
    "camtrapReportConfig.rds",
    package = "camtrapReport"
  )
  
  if (!nzchar(config_file) || !file.exists(config_file)) {
    return(character(0))
  }
  
  config <- readRDS(config_file)
  
  if (is.null(config$github)) {
    return(character(0))
  }
  
  unlist(config$github, use.names = TRUE)
}

#--------

.getPackageGitLabList <- function() {
  config_file <- system.file(
    "external",
    "camtrapReportConfig.rds",
    package = "camtrapReport"
  )
  
  if (!nzchar(config_file) || !file.exists(config_file)) {
    return(character(0))
  }
  
  config <- readRDS(config_file)
  
  if (is.null(config$gitlab)) {
    return(character(0))
  }
  
  unlist(config$gitlab, use.names = TRUE)
}

#--------

.installGitHub <- function(repository) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    warning(
      "Package 'remotes' is required to install packages from GitHub.",
      call. = FALSE
    )
    return(FALSE)
  }
  
  result <- try(
    remotes::install_github(
      repository,
      quiet = TRUE,
      force = TRUE
    ),
    silent = TRUE
  )
  
  !inherits(result, "try-error")
}

#--------

setGeneric(
  "install_All",
  function(pkgs = NULL, update = FALSE, ...) {
    methods::standardGeneric("install_All")
  }
)

#' Install packages required by camtrapReport
#'
#' Install packages required for the full camtrapReport workflow, including
#' packages used by optional report modules.
#'
#' The function checks the package list used by camtrapReport and installs
#' packages that are not currently available. Packages listed in the package
#' configuration as GitHub dependencies are installed with
#' [remotes::install_github()].
#'
#' @param pkgs An optional character vector of additional CRAN package names.
#'   The default is `NULL`.
#' @param update A logical value. If `TRUE`, optional packages are reinstalled.
#'   The default is `FALSE`.
#' @param ... Additional arguments passed to [install.packages()].
#'
#' @return Called for its side effects. The function returns `NULL` invisibly.
#'
#' @seealso [camData()], [report()], [status()]
#' @family optional dependencies
#'
#' @usage install_All(pkgs = NULL, update = FALSE, ...)
#' @rdname install_All
#' @aliases install_All
#'
#' @examples
#' if (interactive()) {
#'   # These calls install packages into the user's R library.
#'   install_All()
#'   install_All(pkgs = "remotes")
#'   install_All(update = TRUE)
#' }
setMethod(
  "install_All",
  signature(pkgs = "ANY"),
  function(pkgs = NULL, update = FALSE, ...) {
    if (
      !is.logical(update) ||
      length(update) != 1L ||
      is.na(update)
    ) {
      stop("'update' must be TRUE or FALSE.", call. = FALSE)
    }
    
    if (!is.null(pkgs)) {
      if (!is.character(pkgs) || anyNA(pkgs)) {
        stop(
          "'pkgs' must be NULL or a character vector of package names.",
          call. = FALSE
        )
      }
      
      pkgs <- trimws(pkgs)
      pkgs <- pkgs[nzchar(pkgs)]
    }
    
    cran_packages <- .getPackageList()
    
    if (length(pkgs) > 0L) {
      cran_packages <- unique(c(cran_packages, pkgs))
    }
    
    github_repositories <- .getPackageGitHubList()
    github_packages <- names(github_repositories)
    
    if (is.null(github_packages)) {
      github_packages <- character(0)
    }
    
    installed_count <- 0L
    
    if (!update) {
      missing_cran <- cran_packages[!.is.installed(cran_packages)]
      
      for (pkg in missing_cran) {
        result <- try(
          install.packages(pkg, ...),
          silent = TRUE
        )
        
        if (
          !inherits(result, "try-error") &&
          isTRUE(unname(.is.installed(pkg)))
        ) {
          installed_count <- installed_count + 1L
        }
      }
      
      missing_github <- github_packages[
        !.is.installed(github_packages)
      ]
      
      if (length(missing_github) > 0L) {
        repositories <- github_repositories[missing_github]
        
        for (pkg in names(repositories)) {
          result <- .installGitHub(repositories[[pkg]])
          
          if (
            isTRUE(result) &&
            isTRUE(unname(.is.installed(pkg)))
          ) {
            installed_count <- installed_count + 1L
          }
        }
      }
      
      required_packages <- unique(c(cran_packages, github_packages))
      failed_packages <- required_packages[
        !.is.installed(required_packages)
      ]
      
      if (installed_count > 0L) {
        message(
          installed_count,
          if (installed_count == 1L) {
            " package was successfully installed."
          } else {
            " packages were successfully installed."
          }
        )
      }
      
      if (length(failed_packages) > 0L) {
        warning(
          "The following packages could not be installed: ",
          toString(failed_packages),
          call. = FALSE
        )
      } else if (installed_count == 0L) {
        message("All required packages have already been installed.")
      }
      
      return(invisible(NULL))
    }
    
    is_protected_package <- vapply(
      cran_packages,
      function(pkg) {
        priority <- suppressWarnings(
          utils::packageDescription(
            pkg,
            fields = "Priority"
          )
        )
        
        length(priority) == 1L &&
          !is.na(priority) &&
          priority %in% c("base", "recommended")
      },
      logical(1)
    )
    
    cran_to_update <- cran_packages[!is_protected_package]
    
    if (
      length(cran_to_update) == 0L &&
      length(github_packages) == 0L
    ) {
      message("There are no optional packages to update.")
      return(invisible(NULL))
    }
    
    if (length(cran_to_update) > 0L) {
      for (pkg in cran_to_update) {
        result <- try(
          install.packages(pkg, ...),
          silent = TRUE
        )
        
        if (
          !inherits(result, "try-error") &&
          isTRUE(unname(.is.installed(pkg)))
        ) {
          installed_count <- installed_count + 1L
        }
      }
    }
    
    if (length(github_packages) > 0L) {
      for (pkg in github_packages) {
        result <- .installGitHub(
          github_repositories[[pkg]]
        )
        
        if (
          isTRUE(result) &&
          isTRUE(unname(.is.installed(pkg)))
        ) {
          installed_count <- installed_count + 1L
        }
      }
    }
    
    checked_packages <- unique(c(cran_to_update, github_packages))
    failed_packages <- checked_packages[
      !.is.installed(checked_packages)
    ]
    
    if (installed_count > 0L) {
      message(
        installed_count,
        if (installed_count == 1L) {
          " package was successfully reinstalled."
        } else {
          " packages were successfully reinstalled."
        }
      )
    }
    
    if (length(failed_packages) > 0L) {
      warning(
        "The following packages could not be installed: ",
        toString(failed_packages),
        call. = FALSE
      )
    } else {
      message("All requested optional packages are installed.")
    }
    
    invisible(NULL)
  }
)

#--------
