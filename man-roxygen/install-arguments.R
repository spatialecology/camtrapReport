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
