# The official pkgcheck action uses pak before checking the package. pak does
# not read DESCRIPTION's Additional_repositories field when resolving packages.
# Configure this CI session only; retain the image's existing repositories.
local({
  repositories <- getOption("repos")
  if (is.null(repositories)) repositories <- character()
  repositories <- repositories[
    !is.na(repositories) & nzchar(repositories)
  ]
  repositories[repositories == "@CRAN@"] <- "https://cloud.r-project.org"
  universe <- "https://camera-traps.r-universe.dev"
  repositories <- repositories[repositories != universe]
  if (!length(repositories)) {
    repositories <- c(CRAN = "https://cloud.r-project.org")
  }
  options(repos = c(repositories, camera_traps = universe))
})

# Temporary upstream workaround: get_gp_text() inserts raw CI print data
# (a list) into the Markdown report, which the action cannot writeLines().
# Render those details with pkgcheck's own Markdown renderer instead.
# Apply on namespace load so this also works before library(pkgcheck).
# Remove once the action image contains the upstream formatter correction.
local({
  patch_markdown <- function(...) {
    namespace <- asNamespace("pkgcheck")
    if (!exists("get_gp_text", envir = namespace, inherits = FALSE)) return()
    formatter <- get("get_gp_text", envir = namespace, inherits = FALSE)
    replace_ci_call <- function(expr) {
      if (identical(expr, quote(print_check(checks, "ci")))) {
        return(quote(print_check_md(checks, "ci", asNamespace("pkgcheck"))))
      }
      if (is.call(expr)) {
        for (i in seq_along(expr)) expr[[i]] <- replace_ci_call(expr[[i]])
      }
      expr
    }
    corrected_body <- replace_ci_call(body(formatter))
    # Already-corrected upstream versions are left untouched.
    if (identical(corrected_body, body(formatter))) return()
    body(formatter) <- corrected_body
    was_locked <- bindingIsLocked("get_gp_text", namespace)
    if (was_locked) {
      unlockBinding("get_gp_text", namespace)
      on.exit(lockBinding("get_gp_text", namespace), add = TRUE)
    }
    assign("get_gp_text", formatter, envir = namespace)
    message("Applied pkgcheck CI Markdown formatter workaround.")
  }
  setHook(packageEvent("pkgcheck", "onLoad"), patch_markdown, action = "append")
  if ("pkgcheck" %in% loadedNamespaces()) patch_markdown()
})
