# Lightweight source metadata for advisory camData cache checks. No source
# contents are read or hashed, and cache reuse never rewrites the saved object.
.camdata_source_candidates <- function(directory, paths) {
  directories <- unique(c(".", dirname(paths)))
  candidates <- unlist(lapply(directories, function(path) {
    entries <- list.files(
      file.path(directory, path), all.files = TRUE, recursive = FALSE
    )
    entries <- entries[tolower(entries) %in% .camdp_core_files()]
    if (identical(path, ".")) entries else file.path(path, entries)
  }), use.names = FALSE)
  sort(unique(candidates))
}

.camdata_source_metadata <- function(directory, input_files) {
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  if (!dir.exists(directory) || !length(input_files)) {
    stop("Source-file metadata requires a dataset directory and input paths.")
  }
  input_files <- vapply(input_files, function(path) {
    file.path(
      normalizePath(dirname(path), winslash = "/", mustWork = FALSE),
      basename(path)
    )
  }, character(1))
  prefix <- paste0(sub("/+$", "", directory), "/")
  if (!all(startsWith(input_files, prefix))) {
    stop("Source-file metadata paths must be within the dataset directory.")
  }
  paths <- sort(unique(substring(input_files, nchar(prefix) + 1L)))
  details <- file.info(file.path(directory, paths))
  if (any(details$isdir %in% TRUE)) {
    stop("Source-file metadata requires files rather than directories.")
  }
  list(
    version = 1L,
    files = data.frame(
      path = paths, size = unname(details$size),
      mtime = as.numeric(details$mtime), stringsAsFactors = FALSE
    ),
    candidates = .camdata_source_candidates(directory, paths)
  )
}

.valid_camdata_source_metadata <- function(snapshot) {
  if (!is.list(snapshot) || !identical(snapshot$version, 1L)) return(FALSE)
  files <- snapshot$files
  if (
    !is.data.frame(files) ||
      !identical(names(files), c("path", "size", "mtime")) ||
      !is.character(files$path) || !is.numeric(files$size) ||
      !is.numeric(files$mtime) || !nrow(files) || nrow(files) > 4L ||
      anyNA(files) || !all(is.finite(files$size)) ||
      !all(is.finite(files$mtime)) || any(files$size < 0)
  ) return(FALSE)
  paths <- files$path
  if (
    !all(nzchar(paths)) || any(grepl("^(/|[A-Za-z]:)", paths)) ||
      any(grepl("(^|/)\\.\\.(/|$)", paths)) ||
      !identical(paths, sort(unique(paths)))
  ) return(FALSE)
  candidates <- snapshot$candidates
  is.character(candidates) && !anyNA(candidates) &&
    identical(candidates, sort(unique(candidates))) &&
    all(paths %in% candidates)
}

.capture_camdata_source_metadata <- function(directory, input_files) {
  snapshot <- tryCatch(
    .camdata_source_metadata(directory, input_files),
    error = function(e) NULL, warning = function(w) NULL
  )
  if (!.valid_camdata_source_metadata(snapshot)) {
    warning(
      "Source-file metadata could not be recorded; ",
      "freshness of this camReport cache will not be verifiable. ",
      "Use camData(..., update = TRUE) to rebuild it when needed.",
      call. = FALSE
    )
    return(NULL)
  }
  snapshot
}

.check_camdata_cache_metadata <- function(object, directory) {
  snapshot <- object$info$source_file_metadata
  current <- NULL
  if (.valid_camdata_source_metadata(snapshot)) {
    current <- tryCatch(
      {
        source_directory <- normalizePath(
          directory, winslash = "/", mustWork = TRUE
        )
        .camdata_source_metadata(
          source_directory, file.path(source_directory, snapshot$files$path)
        )
      },
      error = function(e) NULL, warning = function(w) NULL
    )
  }
  changed <- !is.null(current) && (
    !identical(snapshot$candidates, current$candidates) ||
      (.valid_camdata_source_metadata(current) &&
         !identical(snapshot$files, current$files))
  )
  if (changed) {
    warning(
      "Source data have changed since this camReport object was cached. ",
      "The existing cached object is being reused. ",
      "Run camData(..., update = TRUE) to rebuild it.",
      call. = FALSE
    )
  } else if (is.null(current) || !.valid_camdata_source_metadata(current)) {
    warning(
      "Freshness of this camReport cache cannot be verified because ",
      "source-file metadata is missing or unavailable. ",
      "The saved object is still being reused; ",
      "use camData(..., update = TRUE) to rebuild it.",
      call. = FALSE
    )
  }
  invisible(!changed && .valid_camdata_source_metadata(current))
}
