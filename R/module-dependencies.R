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
#' required when calling `install_all()`. The discovered package names are
#' passed to [`pak::pkg_install()`].
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
#' @param upgrade A logical value. If `TRUE`, update the requested packages and
#'   their dependencies. The default is `FALSE`.
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
  if (!is.logical(upgrade) || length(upgrade) != 1L || is.na(upgrade)) {
    stop("'upgrade' must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(ask) || length(ask) != 1L || is.na(ask)) {
    stop("'ask' must be TRUE or FALSE.", call. = FALSE)
  }

  packages <- .resolve_module_package_references(
    .module_dependencies(),
    package_references = package_references
  )

  if (length(packages) == 0L) {
    message("No package dependencies are declared by the selected modules.")
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
    upgrade = upgrade,
    ask = ask
  )

  invisible(result)
}
