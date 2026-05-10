# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update : May 2026
# Version 1.3
# Licence GPL v3
#--------

.eval <- function(x, env) {
  eval(parse(text = x), envir = env)
}

# Check whether packages are installed
.is.installed <- function(pkgs) {
  pkgs <- unique(as.character(pkgs))
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  
  if (length(pkgs) == 0) {
    out <- logical(0)
    return(out)
  }
  
  out <- vapply(
    pkgs,
    function(pkg) {
      requireNamespace(pkg, quietly = TRUE)
    },
    logical(1)
  )
  
  names(out) <- pkgs
  out
}

# Quiet package loader for installAll.R only
# Important: do NOT redefine .require() here, because .require() is already defined in utils.R
.quiet_require <- function(pkg) {
  pkg <- as.character(pkg)[1]
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(FALSE)
  }
  
  ok <- suppressWarnings(
    suppressMessages(
      suppressPackageStartupMessages(
        require(
          pkg,
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
  
  pkgs <- unique(as.character(unlist(pkgs)))
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  
  unlist(
    lapply(pkgs, function(pkg) {
      .quiet_require(pkg)
    })
  )
}

# Main CRAN package list used by camtrapReport workflows and report modules
.getPackageList <- function(include_dev = FALSE) {
  
  pkgs <- c(
    "activity",
    "camtrapR",
    "corrplot",
    "curl",
    "data.table",
    "Distance",
    "dplyr",
    "DT",
    "dygraphs",
    "ggplot2",
    "ggrepel",
    "glue",
    "gridExtra",
    "gt",
    "htmltools",
    "htmlwidgets",
    "httr",
    "iNEXT",
    "jsonlite",
    "kableExtra",
    "knitr",
    "leaflet",
    "leaflet.extras",
    "lubridate",
    "lutz",
    "magick",
    "plyr",
    "purrr",
    "remotes",
    "rgbif",
    "rmarkdown",
    "scales",
    "sf",
    "shiny",
    "sp",
    "spatstat",
    "spOccupancy",
    "stringr",
    "suncalc",
    "taxadb",
    "taxize",
    "terra",
    "tibble",
    "tidyr",
    "unmarked",
    "xml2"
  )
  
  if (isTRUE(include_dev)) {
    pkgs <- c(
      pkgs,
      "covr",
      "devtools",
      "gitcreds",
      "pkgdown",
      "testthat",
      "usethis"
    )
  }
  
  unique(pkgs)
}

# Packages installed from GitHub
.getPackageGitHubList <- function() {
  repos <- c(
    "frictionlessdata/frictionless-r",
    "inbo/camtraptor",
    "MarcusRowcliffe/camtrapDensity"
  )
  
  names(repos) <- c(
    "frictionless",
    "camtraptor",
    "camtrapDensity"
  )
  
  repos
}

# Safe fallback if no GitLab package list exists elsewhere
.getPackageGitLabList_safe <- function() {
  if (exists(".getPackageGitLabList", mode = "function")) {
    .getPackageGitLabList()
  } else {
    list()
  }
}

.install_cran_packages <- function(pkgs, update = FALSE, ...) {
  pkgs <- unique(as.character(pkgs))
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  
  if (length(pkgs) == 0) return(0)
  
  n_success <- 0
  
  for (pkg in pkgs) {
    message("Installing CRAN package: ", pkg)
    
    s <- try(
      install.packages(pkg, dependencies = TRUE, ...),
      silent = TRUE
    )
    
    if (!inherits(s, "try-error") && requireNamespace(pkg, quietly = TRUE)) {
      n_success <- n_success + 1
    }
  }
  
  n_success
}

.install_github_packages <- function(repos, update = FALSE) {
  if (length(repos) == 0) return(0)
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    message("Installing CRAN package: remotes")
    install.packages("remotes", dependencies = TRUE)
  }
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The remotes package is required to install GitHub packages.")
  }
  
  n_success <- 0
  
  for (i in seq_along(repos)) {
    repo <- unname(repos[i])
    pkg_name <- names(repos)[i]
    
    message("Installing GitHub package: ", repo)
    
    s <- try(
      remotes::install_github(
        repo,
        quiet = TRUE,
        upgrade = "never",
        force = isTRUE(update)
      ),
      silent = TRUE
    )
    
    if (!inherits(s, "try-error") && requireNamespace(pkg_name, quietly = TRUE)) {
      n_success <- n_success + 1
    }
  }
  
  n_success
}

.install_gitlab_packages <- function(repos, update = FALSE) {
  if (length(repos) == 0) return(0)
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    message("Installing CRAN package: remotes")
    install.packages("remotes", dependencies = TRUE)
  }
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("The remotes package is required to install GitLab packages.")
  }
  
  n_success <- 0
  
  for (i in seq_along(repos)) {
    repo <- repos[[i]][["repo"]]
    host <- repos[[i]][["host"]]
    
    message("Installing GitLab package: ", repo)
    
    s <- try(
      remotes::install_gitlab(
        repo,
        host = host,
        quiet = TRUE,
        upgrade = "never",
        force = isTRUE(update)
      ),
      silent = TRUE
    )
    
    if (!inherits(s, "try-error")) {
      n_success <- n_success + 1
    }
  }
  
  n_success
}

#' Install packages required by camtrapReport
#'
#' Installs packages required for the full camtrapReport workflow, including
#' packages used by optional report modules.
#'
#' @param pkgs Optional character vector of package names to install. If `NULL`,
#'   the default camtrapReport package list is used.
#' @param update Logical. If `TRUE`, reinstall GitHub/GitLab packages and attempt
#'   to install or update CRAN packages.
#' @param include_dev Logical. If `TRUE`, also install developer packages such as
#'   `testthat`, `pkgdown`, `covr`, `usethis` and `devtools`.
#' @param include_github Logical. If `TRUE`, install required GitHub packages.
#' @param include_gitlab Logical. If `TRUE`, install required GitLab packages if
#'   a GitLab package list is available.
#' @param ... Additional arguments passed to `install.packages()`.
#'
#' @return Invisibly returns `TRUE` if all required packages are available,
#'   otherwise `FALSE`.
#'
#' @export
install_All <- function(
    pkgs = NULL,
    update = FALSE,
    include_dev = FALSE,
    include_github = TRUE,
    include_gitlab = TRUE,
    ...
) {
  
  if (is.null(pkgs)) {
    cran_pkgs <- .getPackageList(include_dev = include_dev)
    github_pkgs <- if (isTRUE(include_github)) .getPackageGitHubList() else character()
    gitlab_pkgs <- if (isTRUE(include_gitlab)) .getPackageGitLabList_safe() else list()
  } else {
    cran_pkgs <- unique(as.character(pkgs))
    github_pkgs <- character()
    gitlab_pkgs <- list()
  }
  
  cran_pkgs <- unique(cran_pkgs)
  github_names <- names(github_pkgs)
  gitlab_names <- names(gitlab_pkgs)
  
  cran_installed <- .is.installed(cran_pkgs)
  github_installed <- .is.installed(github_names)
  gitlab_installed <- .is.installed(gitlab_names)
  
  if (isTRUE(update)) {
    cran_to_install <- cran_pkgs
    github_to_install <- github_pkgs
    gitlab_to_install <- gitlab_pkgs
  } else {
    cran_to_install <- cran_pkgs[!cran_installed]
    github_to_install <- github_pkgs[!github_installed]
    gitlab_to_install <- gitlab_pkgs[!gitlab_installed]
  }
  
  if (
    length(cran_to_install) == 0 &&
    length(github_to_install) == 0 &&
    length(gitlab_to_install) == 0
  ) {
    cat("\nAll required packages have already been installed!\n")
    return(invisible(TRUE))
  }
  
  n_success <- 0
  
  n_success <- n_success + .install_cran_packages(
    cran_to_install,
    update = update,
    ...
  )
  
  n_success <- n_success + .install_github_packages(
    github_to_install,
    update = update
  )
  
  n_success <- n_success + .install_gitlab_packages(
    gitlab_to_install,
    update = update
  )
  
  all_expected <- unique(c(cran_pkgs, github_names, gitlab_names))
  still_missing <- all_expected[!.is.installed(all_expected)]
  
  if (n_success > 0) {
    cat("\n", n_success, " package(s) were successfully installed or updated.\n", sep = "")
  }
  
  if (length(still_missing) > 0) {
    cat(
      "\nThe following package(s) could not be installed or are still missing:\n.... ",
      paste(still_missing, collapse = ", "),
      "\n",
      sep = ""
    )
    return(invisible(FALSE))
  }
  
  cat("\nAll required packages are now available.\n")
  invisible(TRUE)
}
