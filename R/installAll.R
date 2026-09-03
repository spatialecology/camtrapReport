# Functions for installing optional camtrapReport dependencies
# Licence: MIT
#--------

.detachPackage <- function(n, unload = TRUE, force = TRUE) {
  n <- unique(as.character(n))
  n <- n[!is.na(n) & nzchar(n)]
  
  for (pkg in n) {
    package_name <- paste0("package:", pkg)
    
    if (package_name %in% search()) {
      try(
        detach(
          package_name,
          force = force,
          character.only = TRUE,
          unload = unload
        ),
        silent = TRUE
      )
    }
  }
  
  invisible(NULL)
}

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
#--------

.pakRemoteReferences <- function(repositories, source) {
  source <- match.arg(source, c("github", "gitlab"))
  
  if (length(repositories) == 0L) {
    return(setNames(character(0), character(0)))
  }
  
  package_names <- names(repositories)
  repositories <- as.character(repositories)
  
  if (
    is.null(package_names) ||
    anyNA(package_names) ||
    !all(nzchar(package_names))
  ) {
    stop(
      sprintf(
        "%s package repositories must be a named character vector.",
        tools::toTitleCase(source)
      ),
      call. = FALSE
    )
  }
  
  repositories <- trimws(repositories)
  
  if (anyNA(repositories) || !all(nzchar(repositories))) {
    stop(
      sprintf(
        "%s repository references must not be empty.",
        tools::toTitleCase(source)
      ),
      call. = FALSE
    )
  }
  
  references <- paste0(
    package_names,
    "=",
    source,
    "::",
    repositories
  )
  
  names(references) <- package_names
  references
}

#--------

.pakReinstallReferences <- function(references) {
  if (length(references) == 0L) {
    return(references)
  }
  
  separator <- ifelse(
    grepl("?", references, fixed = TRUE),
    "&",
    "?"
  )
  
  result <- paste0(
    references,
    separator,
    "reinstall"
  )
  
  names(result) <- names(references)
  result
}

#--------

.installPak <- function(references, ...) {
  if (length(references) == 0L) {
    return(invisible(NULL))
  }
  
  if (!requireNamespace("pak", quietly = TRUE)) {
    stop(
      "Package 'pak' is required to install optional dependencies.",
      call. = FALSE
    )
  }
  
  dots <- list(...)
  
  if (length(dots) > 0L) {
    dot_names <- names(dots)
    
    if (
      is.null(dot_names) ||
      anyNA(dot_names) ||
      !all(nzchar(dot_names))
    ) {
      stop(
        "All arguments passed through '...' must be named.",
        call. = FALSE
      )
    }
    
    if ("pkg" %in% dot_names) {
      stop(
        "'pkg' cannot be supplied through '...'.",
        call. = FALSE
      )
    }
  }
  
  # install_All() has historically been non-interactive.
  # Users can override this with ask = TRUE.
  if (!"ask" %in% names(dots)) {
    dots$ask <- FALSE
  }
  
  do.call(
    pak::pkg_install,
    c(
      list(pkg = unname(references)),
      dots
    )
  )
}

#--------

setGeneric("install_All",
  function(pkgs = NULL, update = FALSE, github = TRUE, gitlab = FALSE, ...) {
    methods::standardGeneric("install_All")
  }
)

#' Install packages required by camtrapReport
#'
#' Install packages required for the full camtrapReport workflow, including
#' packages used by optional report modules.
#'
#' The function checks the package list used by camtrapReport and installs
#' packages that are not currently available. Packages configured as GitHub
#' or GitLab dependencies are installed from their corresponding repositories.
#' All package installation is handled by [pak::pkg_install()].
#'
#' Unlike a direct call to `pak`, `install_All()` discovers the packages
#' declared
#' by the currently available YAML report modules, including modules added or
#' modified by users, and combines them with any additional packages supplied
#' through `pkgs`. It then delegates resolution and installation to `pak`. The
#' function is opt-in and is never called when the package is loaded or a report
#' is rendered.
#'
#' When `update = TRUE`, the requested optional packages are force-reinstalled
#' from their configured sources using the `pak` `reinstall` parameter.
#' Dependencies are upgraded only when required by the requested packages,
#' unless `upgrade = TRUE` is supplied through `...`.
#'
#' @param pkgs An optional character vector of additional package names.
#'   The default is `NULL`.
#' @param update A logical value. If `TRUE`, optional packages are
#'   force-reinstalled. The default is `FALSE`.
#' @param github A logical value (default `TRUE`) specifying whether packages
#'   configured in GitHub repositories are included.
#' @param gitlab A logical value (default `FALSE`) specifying whether packages
#'   configured in GitLab repositories are included.
#' @param ... Additional named arguments passed to [pak::pkg_install()],
#'   such as `lib`, `ask`, `dependencies`, or `upgrade`.
#'
#' @return Called for its side effects. The function returns `NULL` invisibly.
#'
#' @seealso [camData()], [report()], [status()]
#' @family optional dependencies
#'
#' @usage install_All(pkgs = NULL, update = FALSE, github = TRUE,
#'   gitlab = FALSE, ...)
#' @rdname install_all
#' @aliases install_All
#'
#' @examples
#' # Package installation changes the user's R library, so these examples are
#' # deliberately restricted to interactive sessions.
#' if (interactive()) {
#' install_All()
#' install_All(pkgs = "readr")
#' install_All(update = TRUE)
#'
#' # Reinstall the requested packages and also update their dependencies.
#' install_All(update = TRUE, upgrade = TRUE)
#' }
setMethod("install_All",signature(pkgs = "ANY"),
  function(pkgs = NULL, update = FALSE,github = TRUE, gitlab = FALSE, ...) {
    if (
      !is.logical(update) ||
      length(update) != 1L ||
      is.na(update)
    ) {
      stop(
        "'update' must be TRUE or FALSE.",
        call. = FALSE
      )
    }
    
    if (missing(github) || !is.logical(github)) github <- TRUE
    if (missing(gitlab) || !is.logical(gitlab)) gitlab <- FALSE
    
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
    
    if (github) {
      github_repositories <- .getPackageGitHubList()
      github_references <- .pakRemoteReferences(
        github_repositories,
        source = "github"
      )
      github_packages <- names(github_references)
    } else github_packages <- NULL
  
    if (gitlab) {
      gitlab_repositories <- .getPackageGitLabList()
      gitlab_references <- .pakRemoteReferences(
        gitlab_repositories,
        source = "gitlab"
      )
      gitlab_packages <- names(gitlab_references)
    } else gitlab_packages <- NULL
    
    # Any duplication? 
    duplicated_remote_packages <- intersect(
      github_packages,
      gitlab_packages
    )
    
    if (length(duplicated_remote_packages) > 0L) {
      stop(
        "The following packages are configured for both GitHub and GitLab: ",
        paste(duplicated_remote_packages, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    remote_packages <- unique(c(
      github_packages,
      gitlab_packages
    ))
    
    # A configured remote source takes precedence over the standard
    # repository version of the same package.
    cran_packages <- setdiff(
      cran_packages,
      remote_packages
    )
    
    required_packages <- unique(c(
      cran_packages,
      remote_packages
    ))
    
    if (length(required_packages) == 0L) {
      cat("\nNo optional packages are configured for installation.\n")
      return(invisible(NULL))
    }
    
    if (!update) {
      missing_packages <- required_packages[
        !.is.installed(required_packages)
      ]
      
      if (length(missing_packages) == 0L) {
        cat("\nAll required packages have already been installed.\n")
        return(invisible(NULL))
      }
      
      references <- c(cran_packages[cran_packages %in% missing_packages])
      
      if (github) {
        references <- c(
          references,
          github_references[names(github_references) %in% missing_packages]
        )
      }
      if (gitlab) {
        references <- c(
          references,
          gitlab_references[names(gitlab_references) %in% missing_packages]
        )
      }
      
      .installPak(references,...)
      
      return(invisible(NULL))
    }
    
    protected <- vapply(
      cran_packages,
      function(package) {
        priority <- tryCatch(
          utils::packageDescription(package, fields = "Priority"),
          warning = function(condition) NA_character_,
          error = function(condition) NA_character_
        )

        !is.na(priority) && priority %in% c("base", "recommended")
      },
      logical(1)
    )

    protected_packages <- cran_packages[protected]
    
    cran_to_update <- setdiff(
      cran_packages,
      protected_packages
    )
    
    update_packages <- unique(c(cran_to_update,remote_packages))
    
    if (length(update_packages) == 0L) {
      cat("\nThere are no optional packages to update.\n")
      return(invisible(NULL))
    }
    
    # Detach requested packages before replacing them. This is particularly
    # useful for packages containing compiled code.
    .detachPackage(update_packages)
    
    references <- cran_to_update
    if (github) references <- c(references, github_references)
    if (gitlab) references <- c(references, gitlab_references)

    # Force reinstall of the explicitly requested packages without
    # automatically upgrading all of their dependencies.
    references <- .pakReinstallReferences(references)
    
    .installPak(references,...)
    
    invisible(NULL)
  }
)

#--------
