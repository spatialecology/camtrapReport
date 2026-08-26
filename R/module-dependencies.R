# Functions for installing report-module dependencies
# Licence: MIT
#--------

.module_registry_file <- function() {
  configured <- getOption("camtrapReport.module_registry_file")
  
  if (
    is.character(configured) &&
    length(configured) == 1L &&
    !is.na(configured) &&
    nzchar(configured)
  ) {
    return(configured)
  }
  
  file.path(
    tools::R_user_dir("camtrapReport", which = "config"),
    "module-registries.rds"
  )
}

.registered_module_dirs <- function() {
  registry_file <- .module_registry_file()
  
  if (!file.exists(registry_file)) {
    return(character())
  }
  
  dirs <- tryCatch(
    base::readRDS(registry_file),
    error = function(e) character()
  )
  
  dirs <- as.character(dirs)
  valid <- !is.na(dirs) & nzchar(dirs) & dir.exists(dirs)
  valid <- valid & file.exists(file.path(dirs, "__modulesList.csv"))
  dirs <- dirs[valid]
  
  unique(normalizePath(dirs, winslash = "/", mustWork = TRUE))
}

.register_module_dir <- function(dir) {
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  registry_file <- .module_registry_file()
  registry_parent <- dirname(registry_file)
  
  if (!dir.exists(registry_parent)) {
    created <- dir.create(
      registry_parent,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    if (!created && !dir.exists(registry_parent)) {
      stop(
        "Could not create the camtrapReport configuration directory: ",
        registry_parent,
        call. = FALSE
      )
    }
  }
  
  dirs <- unique(c(.registered_module_dirs(), dir))
  base::saveRDS(dirs, registry_file)
  
  invisible(dir)
}

.module_directories <- function() {
  bundled_dir <- .section_dir(package = "camtrapReport")
  
  unique(c(
    normalizePath(bundled_dir, winslash = "/", mustWork = TRUE),
    .registered_module_dirs()
  ))
}

.module_dependencies <- function() {
  dependencies <- lapply(.module_directories(), function(module_dir) {
    modules <- .read_modules(
      level0 = c(
        "introduction",
        "methods",
        "results",
        "acknowledgements",
        "appendix"
      ),
      package = "camtrapReport",
      dir = module_dir,
      write_info = FALSE
    )
    
    .collect_module_packages(modules)
  })
  
  framework_packages <- c("knitr", "rmarkdown")
  
  sort(.normalize_packages(c(
    framework_packages,
    unlist(dependencies, use.names = FALSE)
  )))
}

.resolve_module_package_references <- function(
    packages,
    package_references = NULL) {
  packages <- .normalize_packages(packages)
  
  if (is.null(package_references) || length(package_references) == 0L) {
    return(packages)
  }
  
  if (!is.character(package_references) || anyNA(package_references)) {
    stop(
      "'package_references' must be NULL or a character vector.",
      call. = FALSE
    )
  }
  
  package_references <- trimws(package_references)
  package_references <- package_references[nzchar(package_references)]
  reference_names <- names(package_references)
  
  if (!is.null(reference_names)) {
    replace <- nzchar(reference_names)
    packages <- packages[!packages %in% reference_names[replace]]
  }
  
  unique(c(packages, unname(package_references)))
}

.missing_module_packages <- function(packages) {
  packages <- .normalize_packages(packages)
  packages[!vapply(packages, .require, logical(1))]
}

.missing_package_references <- function(package_references) {
  if (is.null(package_references) || length(package_references) == 0L) {
    return(package_references)
  }

  reference_names <- names(package_references)
  if (is.null(reference_names)) {
    return(package_references)
  }

  named <- nzchar(reference_names)
  installed <- rep(FALSE, length(package_references))
  installed[named] <- vapply(
    reference_names[named],
    .require,
    logical(1)
  )

  package_references[!installed]
}

.pak_install_module_dependencies <- function(
    packages,
    lib,
    upgrade,
    ask) {
  pak::pkg_install(
    packages,
    lib = lib,
    upgrade = upgrade,
    ask = ask
  )
}

.validate_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(
      sprintf("'%s' must be TRUE or FALSE.", name),
      call. = FALSE
    )
  }
  
  invisible(x)
}

#' Install all report-module dependencies
#'
#' Automatically discover packages required to render reports and dependencies
#' declared by active report modules, then install them with
#' [`pak::pkg_install()`]. Module dependencies are read from the `packages`
#' option in each module's R Markdown code block.
#'
#' `install_all()` scans both the module registry bundled with `camtrapReport`
#' and every user-managed registry previously used with [add_Module()]. Module
#' directories are registered automatically when they are used, so no path is
#' required when calling `install_all()`. Packages that are already installed
#' are excluded before [`pak::pkg_install()`] is called. This means that core
#' package imports, such as `dplyr` and `terra`, are never submitted for an
#' update by `install_all()`.
#' `package_references` can provide the source of a package that cannot be
#' resolved by name from a configured repository. For example,
#' `c(myPackage = "owner/myPackage")` replaces the discovered package name with
#' its GitHub package reference.
#'
#' @param package_references An optional character vector of
#'   [`pak` package references][pak::pkg_install].
#'   A named element replaces the corresponding discovered package name; an
#'   unnamed element is added to the installation request.
#' @param lib An optional character vector of library paths passed to
#'   [`pak::pkg_install()`].
#' @param upgrade Retained for backward compatibility. Installed packages are
#'   never updated by `install_all()`, so this value does not change the
#'   installation request.
#' @param ask A logical value controlling confirmation before replacing an
#'   installed package version. The default is [interactive()].
#'
#' @return Invisibly returns the result from [`pak::pkg_install()`], or `NULL`
#'   when no dependencies are declared.
#'
#' @seealso [add_Module()], [list_Modules()], [reportSection()]
#' @family report modules
#'
#' @examples
#' if (interactive()) {
#'   install_all()
#'
#'   install_all(
#'     package_references = c(
#'       camtrapDensity = "MarcusRowcliffe/camtrapDensity"
#'     )
#'   )
#' }
#' @export
install_all <- function(
    package_references = NULL,
    lib = NULL,
    upgrade = FALSE,
    ask = interactive()) {
  .validate_logical_scalar(upgrade, "upgrade")
  .validate_logical_scalar(ask, "ask")
  
  package_references <- .missing_package_references(package_references)
  packages <- .resolve_module_package_references(
    .missing_module_packages(.module_dependencies()),
    package_references = package_references
  )
  
  if (length(packages) == 0L) {
    message("All declared module dependencies are already installed.")
    return(invisible(NULL))
  }
  
  if (!.require("pak")) {
    stop(
      "Package 'pak' is required to install module dependencies. ",
      "Install it with install.packages('pak').",
      call. = FALSE
    )
  }
  
  result <- .pak_install_module_dependencies(
    packages = packages,
    lib = lib,
    upgrade = FALSE,
    ask = ask
  )
  
  invisible(result)
}
