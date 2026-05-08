# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  May 2026
# Version 1.1
# Licence GPL v3
#--------

.eval <- function(x, env) {
  eval(parse(text = x), envir = env)
}

# Check whether packages are installed
.is.installed <- function(n) {
  n <- as.character(n)
  names(n) <- n
  
  sapply(n, function(x) {
    length(
      unlist(
        lapply(
          .libPaths(),
          function(lib) find.package(x, lib, quiet = TRUE, verbose = FALSE)
        )
      )
    ) > 0
  })
}

# Quiet package loader for installAll.R only
# Important: do NOT redefine .require() here, because .require() is already defined in utils.R
.quiet_require <- function(x) {
  x <- as.character(x)[1]
  
  if (!requireNamespace(x, quietly = TRUE)) {
    return(FALSE)
  }
  
  ok <- suppressWarnings(
    suppressMessages(
      suppressPackageStartupMessages(
        require(
          x,
          character.only = TRUE,
          quietly = TRUE,
          warn.conflicts = FALSE
        )
      )
    )
  )
  
  isTRUE(ok)
}

# Quietly load a list/vector of packages
.loadLib <- function(pkgs) {
  old_warn <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = old_warn), add = TRUE)
  
  unlist(
    lapply(pkgs, function(x) {
      all(unlist(lapply(x, function(p) .quiet_require(p))))
    })
  )
}

.getPackageList <- function() {
  c(
    "camtrapDensity",
    "camtraptor",
    "gridExtra",
    "kableExtra",
    "knitr",
    "leaflet",
    "lubridate",
    "purrr",
    "tidyverse",
    "magick",
    "sf",
    "spatstat",
    "terra",
    "unmarked",
    "devtools",
    "remotes",
    "rgbif",
    "ggplot2",
    "iNEXT",
    "taxadb",
    "corrplot",
    "httr",
    "jsonlite",
    "suncalc",
    "plyr",
    "taxize",
    "data.table",
    "dplyr",
    "stringr",
    "xml2",
    "curl",
    "camtrapR",
    "spOccupancy",
    "leaflet.extras"
  )
}

# Packages installed from GitHub
.getPackageGitHubList <- function() {
  n <- c(
    "frictionlessdata/frictionless-r",
    "inbo/camtraptor",
    "MarcusRowcliffe/camtrapDensity"
  )
  
  names(n) <- c(
    "frictionless",
    "camtraptor",
    "camtrapDensity"
  )
  
  n
}

# Safe fallback if no GitLab package list exists elsewhere
.getPackageGitLabList_safe <- function() {
  if (exists(".getPackageGitLabList", mode = "function")) {
    .getPackageGitLabList()
  } else {
    list()
  }
}

.install_cran_packages <- function(pkgs, ...) {
  if (length(pkgs) == 0) return(0)
  
  n_success <- 0
  
  for (p in pkgs) {
    message("Installing CRAN package: ", p)
    
    s <- try(
      install.packages(p, ...),
      silent = TRUE
    )
    
    if (!inherits(s, "try-error")) {
      n_success <- n_success + 1
    }
  }
  
  n_success
}

.install_github_packages <- function(repos) {
  if (length(repos) == 0) return(0)
  
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("The devtools package is required to install GitHub packages.")
  }
  
  n_success <- 0
  
  for (repo in repos) {
    message("Installing GitHub package: ", repo)
    
    s <- try(
      devtools::install_github(
        repo,
        quiet = TRUE,
        force = TRUE
      ),
      silent = TRUE
    )
    
    if (!inherits(s, "try-error")) {
      n_success <- n_success + 1
    }
  }
  
  n_success
}

.install_gitlab_packages <- function(repos) {
  if (length(repos) == 0) return(0)
  
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("The devtools package is required to install GitLab packages.")
  }
  
  n_success <- 0
  
  for (i in seq_along(repos)) {
    repo <- repos[[i]][["repo"]]
    host <- repos[[i]][["host"]]
    
    message("Installing GitLab package: ", repo)
    
    s <- try(
      devtools::install_gitlab(
        repo,
        host = host,
        quiet = TRUE,
        force = TRUE
      ),
      silent = TRUE
    )
    
    if (!inherits(s, "try-error")) {
      n_success <- n_success + 1
    }
  }
  
  n_success
}

if (!isGeneric("install_All")) {
  setGeneric("install_All", function(pkgs, update, ...)
    standardGeneric("install_All"))
}

setMethod(
  "install_All",
  signature(pkgs = "ANY"),
  function(pkgs, update = FALSE, ...) {
    
    if (missing(update)) update <- FALSE
    
    cran_pkgs <- .getPackageList()
    github_pkgs <- .getPackageGitHubList()
    gitlab_pkgs <- .getPackageGitLabList_safe()
    
    github_names <- names(github_pkgs)
    gitlab_names <- names(gitlab_pkgs)
    
    n_success <- 0
    
    if (!update) {
      
      cran_to_install <- cran_pkgs[!.is.installed(cran_pkgs)]
      github_to_install <- github_pkgs[!.is.installed(github_names)]
      gitlab_to_install <- gitlab_pkgs[!.is.installed(gitlab_names)]
      
      if (
        length(cran_to_install) == 0 &&
        length(github_to_install) == 0 &&
        length(gitlab_to_install) == 0
      ) {
        cat("\nAll required packages have already been installed!\n")
        return(invisible(TRUE))
      }
      
      n_success <- n_success + .install_cran_packages(cran_to_install, ...)
      n_success <- n_success + .install_github_packages(github_to_install)
      n_success <- n_success + .install_gitlab_packages(gitlab_to_install)
      
      all_expected <- unique(c(cran_pkgs, github_names, gitlab_names))
      still_missing <- all_expected[!.is.installed(all_expected)]
      
      if (n_success > 0) {
        cat("\n", n_success, " package(s) were successfully installed.\n", sep = "")
      }
      
      if (length(still_missing) > 0) {
        cat(
          "The following package(s) could not be installed:\n.... ",
          paste(still_missing, collapse = ", "),
          "\n",
          sep = ""
        )
      }
      
      return(invisible(length(still_missing) == 0))
    }
    
    # update = TRUE
    base_pkgs <- c(
      "stats",
      "utils",
      "parallel",
      "base",
      "grDevice",
      "tools",
      "methods",
      "graphics",
      "compiler",
      "datasets",
      "profile",
      "grid"
    )
    
    cran_to_update <- cran_pkgs[!cran_pkgs %in% base_pkgs]
    
    if (length(cran_to_update) > 0) {
      installed_cran <- cran_to_update[.is.installed(cran_to_update)]
      
      if (length(installed_cran) > 0) {
        try(remove.packages(installed_cran), silent = TRUE)
      }
      
      n_success <- n_success + .install_cran_packages(cran_to_update, ...)
    }
    
    if (length(github_names) > 0) {
      installed_github <- github_names[.is.installed(github_names)]
      
      if (length(installed_github) > 0) {
        try(remove.packages(installed_github), silent = TRUE)
      }
      
      n_success <- n_success + .install_github_packages(github_pkgs)
    }
    
    if (length(gitlab_names) > 0) {
      installed_gitlab <- gitlab_names[.is.installed(gitlab_names)]
      
      if (length(installed_gitlab) > 0) {
        try(remove.packages(installed_gitlab), silent = TRUE)
      }
      
      n_success <- n_success + .install_gitlab_packages(gitlab_pkgs)
    }
    
    all_expected <- unique(c(cran_pkgs, github_names, gitlab_names))
    still_missing <- all_expected[!.is.installed(all_expected)]
    
    if (n_success > 0) {
      cat("\n", n_success, " package(s) were successfully installed or updated.\n", sep = "")
    }
    
    if (length(still_missing) > 0) {
      cat(
        "The following package(s) could not be installed:\n.... ",
        paste(still_missing, collapse = ", "),
        "\n",
        sep = ""
      )
    } else if (n_success == 0) {
      cat("\nAll required packages are already installed. Nothing was updated.\n")
    }
    
    invisible(length(still_missing) == 0)
  }
)