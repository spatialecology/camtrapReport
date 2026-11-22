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

# pkgcheck 0.3.4.005 queries workflow runs without authentication, then hides
# lookup errors as missing CI. Patch only that request; retain upstream run
# filtering and success/failure checks. Remove when the action image is fixed.
local({
  patch_ci_lookup <- function(...) {
    if (utils::packageVersion("pkgcheck") != "0.3.4.005") return()
    namespace <- asNamespace("pkgcheck")
    if (!exists("ci_results_gh", envir = namespace, inherits = FALSE)) return()
    lookup <- get("ci_results_gh", envir = namespace, inherits = FALSE)
    if (isTRUE(attr(lookup, "authenticated_ci_lookup"))) return()
    replacements <- 0L
    replace_request <- function(expr) {
      if (identical(expr, quote(httr2::req_perform(qry)))) {
        replacements <<- replacements + 1L
        return(quote(tryCatch({
          token <- as.character(gh::gh_token())
          if (!nzchar(token)) {
            message("pkgcheck CI lookup: no GitHub token is available.")
            stop("GitHub token required for CI lookup.", call. = FALSE)
          }
          qry <- httr2::req_auth_bearer_token(qry, token)
          qry <- httr2::req_timeout(qry, seconds = 20)
          qry <- httr2::req_retry(
            qry, max_tries = 3, max_seconds = 60, retry_on_failure = TRUE,
            is_transient = function(resp) {
              status <- httr2::resp_status(resp)
              status %in% c(429L, 500L, 502L, 503L, 504L) ||
                (status == 403L &&
                  (identical(httr2::resp_header(resp, "x-ratelimit-remaining"), "0") ||
                    !is.null(httr2::resp_header(resp, "retry-after"))))
            }
          )
          httr2::req_perform(qry)
        }, error = function(e) {
          # Do not print condition messages, requests, headers or tokens.
          http_class <- grep("^httr2_http_[0-9]{3}$", class(e), value = TRUE)
          detail <- if (length(http_class)) {
            paste("HTTP", sub("^httr2_http_", "", http_class[1L]))
          } else {
            "transport/request error"
          }
          # message() survives pkgcheck's suppressWarnings()/tryCatch(NULL).
          message("pkgcheck CI lookup failed: ", detail,
                  ". Check API availability, rate limits and actions: read permission.")
          stop("GitHub Actions workflow lookup failed.", call. = FALSE)
        })))
      }
      if (is.call(expr)) {
        for (i in seq_along(expr)) expr[i] <- list(replace_request(expr[[i]]))
      }
      expr
    }
    corrected_body <- replace_request(body(lookup))
    if (replacements != 1L) {
      message("pkgcheck CI lookup workaround skipped: unexpected upstream function.")
      return()
    }
    body(lookup) <- corrected_body
    attr(lookup, "authenticated_ci_lookup") <- TRUE
    was_locked <- bindingIsLocked("ci_results_gh", namespace)
    if (was_locked) {
      unlockBinding("ci_results_gh", namespace)
      on.exit(lockBinding("ci_results_gh", namespace), add = TRUE)
    }
    assign("ci_results_gh", lookup, envir = namespace)
    message("Applied pkgcheck 0.3.4.005 authenticated CI lookup workaround.")
  }
  setHook(packageEvent("pkgcheck", "onLoad"), patch_ci_lookup, action = "append")
  if ("pkgcheck" %in% loadedNamespaces()) patch_ci_lookup()
})
