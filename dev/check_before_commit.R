cat("\n==============================\n")
cat("camtrapReport pre-commit check\n")
cat("==============================\n\n")

stop_now <- function(...) {
  stop(paste0(...), call. = FALSE)
}

if (!file.exists("DESCRIPTION") || !dir.exists("R")) {
  stop_now("Run this script from the package root.")
}

cat("1. Checking source files...\n")

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)

cat("2. Checking important internal helpers...\n")

helper_defs <- c(
  ".paste_comma_and",
  ".trim",
  ".trim_chr",
  ".pick_col",
  ".pretty_label",
  ".require"
)

all_code <- unlist(lapply(r_files, readLines, warn = FALSE), use.names = FALSE)

missing_helper_defs <- helper_defs[
  !vapply(helper_defs, function(h) {
    any(grepl(paste0("^\\s*", gsub("\\.", "\\\\.", h), "\\s*<-\\s*function"), all_code))
  }, logical(1))
]

if (length(missing_helper_defs) > 0) {
  stop_now(
    "Missing helper definition(s): ",
    paste(missing_helper_defs, collapse = ", ")
  )
}

cat("3. Checking common bare dplyr calls...\n")

dplyr_funs <- c(
  "distinct",
  "n_distinct",
  "mutate",
  "left_join",
  "group_by",
  "summarise",
  "bind_rows",
  "filter",
  "select",
  "arrange",
  "rename",
  "count",
  "coalesce",
  "case_when",
  "if_else",
  "pull",
  "everything",
  "any_of",
  "all_of",
  "across"
)

bare_hits <- lapply(dplyr_funs, function(fun) {
  pattern <- paste0("(?<![:A-Za-z0-9_.])", fun, "\\s*\\(")
  
  out <- lapply(r_files, function(f) {
    if (basename(f) == "imports.R") return(NULL)
    
    x <- readLines(f, warn = FALSE)
    h <- grep(pattern, x, perl = TRUE)
    
    # ignore comments
    h <- h[!grepl("^\\s*#", x[h])]
    
    if (length(h) == 0) return(NULL)
    
    data.frame(
      function_name = fun,
      file = f,
      line = h,
      text = trimws(x[h]),
      stringsAsFactors = FALSE
    )
  })
  
  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
})

bare_hits <- Filter(Negate(is.null), bare_hits)
bare_hits <- if (length(bare_hits) == 0) NULL else do.call(rbind, bare_hits)

if (!is.null(bare_hits) && nrow(bare_hits) > 0) {
  cat("\nBare dplyr-style calls found.\n")
  cat("These are allowed only if imported in NAMESPACE, but explicit dplyr:: is safer.\n\n")
  print(bare_hits, row.names = FALSE)
}

cat("4. Regenerating NAMESPACE/documentation...\n")
devtools::document()

cat("5. Checking required NAMESPACE imports...\n")

ns <- readLines("NAMESPACE", warn = FALSE)

required_imports <- c(
  "importFrom(dplyr,distinct)",
  "importFrom(dplyr,n_distinct)",
  "importFrom(dplyr,mutate)",
  "importFrom(dplyr,left_join)",
  "importFrom(dplyr,group_by)",
  "importFrom(dplyr,summarise)",
  "importFrom(dplyr,bind_rows)",
  "importFrom(dplyr,filter)",
  "importFrom(dplyr,select)",
  "importFrom(dplyr,arrange)",
  "importFrom(dplyr,coalesce)",
  "importFrom(rlang,.data)"
)

missing_imports <- required_imports[!required_imports %in% ns]

if (length(missing_imports) > 0) {
  cat("\nMissing imports:\n")
  cat(paste0(" - ", missing_imports, collapse = "\n"), "\n")
  stop_now("Fix R/imports.R and run devtools::document().")
}

cat("6. Removing old installed package if present...\n")

pkg_dirs <- file.path(.libPaths(), "camtrapReport")
if (any(file.exists(pkg_dirs))) {
  unlink(pkg_dirs[file.exists(pkg_dirs)], recursive = TRUE, force = TRUE)
}

cat("7. Loading package from source...\n")
devtools::load_all(reset = TRUE, recompile = TRUE)

cat("8. Checking helpers in namespace...\n")

missing_helpers_ns <- helper_defs[
  !vapply(helper_defs, exists, logical(1),
          envir = asNamespace("camtrapReport"),
          inherits = FALSE)
]

if (length(missing_helpers_ns) > 0) {
  stop_now(
    "Helpers missing from namespace: ",
    paste(missing_helpers_ns, collapse = ", ")
  )
}

cat("9. Running package check...\n")
devtools::check(document = FALSE, error_on = "error")

cat("\n✅ Pre-commit check passed.\n")
