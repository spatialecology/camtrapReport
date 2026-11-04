# Explicit output destinations and saved analysis objects.

.is_package_directory <- function(path) {
  path <- tolower(normalizePath(path, winslash = "/", mustWork = TRUE))
  roots <- unique(c(.libPaths(), system.file(package = "camtrapReport")))
  roots <- roots[nzchar(roots) & dir.exists(roots)]
  roots <- tolower(normalizePath(roots, winslash = "/", mustWork = TRUE))
  any(vapply(roots, function(root) {
    identical(path, root) || startsWith(path, paste0(root, "/"))
  }, logical(1)))
}

.check_output_directory <- function(path) {
  if (!dir.exists(path)) {
    stop("The output directory does not exist: ", path, call. = FALSE)
  }
  if (.is_package_directory(path)) {
    stop("Output inside an installed package or R library is not allowed. ",
         "Choose a separate output directory.", call. = FALSE)
  }
  if (file.access(path, 2L) != 0L) {
    stop("The output directory is not writable: ", path, call. = FALSE)
  }
  invisible(path)
}

.check_cache_file <- function(file, overwrite = FALSE, read_only = FALSE) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !grepl("\\.rds$", file, ignore.case = TRUE)) {
    stop("'cache_file' must be a single .rds filename.", call. = FALSE)
  }
  file <- path.expand(file)
  if (!read_only) .check_output_directory(dirname(file))
  if (dir.exists(file)) stop("The cache path is a directory.", call. = FALSE)
  if (file.exists(file) && !isTRUE(overwrite)) {
    stop("The cache file already exists; use overwrite = TRUE to replace it.",
         call. = FALSE)
  }
  file
}

.save_camreport_cache <- function(object, file, overwrite = FALSE) {
  file <- .check_cache_file(file, overwrite)
  saved <- object$copy(shallow = FALSE)
  if (inherits(saved$study_area$object, "SpatVector")) {
    saved$study_area$object <- terra::wrap(saved$study_area$object)
  }
  # Finish serialization before touching an existing, explicitly replaceable file.
  staging <- tempfile(".camtrapReport-cache-", tmpdir = dirname(file))
  on.exit(unlink(staging), add = TRUE)
  saveRDS(saved, staging)
  if (!file.rename(staging, file)) {
    stop("Could not save the analysis cache: ", file, call. = FALSE)
  }
  invisible(file)
}

.read_camreport_cache <- function(file) {
  object <- readRDS(file)
  if (!inherits(object, "camReport")) {
    stop("The cache does not contain a camReport object.", call. = FALSE)
  }
  if (inherits(object$study_area$object, "PackedSpatVector")) {
    object$study_area$object <- terra::unwrap(object$study_area$object)
  } else if (inherits(object$study_area$object, "SpatVector")) {
    # Old caches serialized a live terra pointer and saved the usable boundary
    # separately. Restore that boundary when reopening in another R session.
    path <- object$study_area$path
    if (is.character(path) && length(path) == 1L && !is.na(path) &&
        identical(basename(path), "study_area.map") && file.exists(path)) {
      object$study_area$object <- terra::readRDS(path)
    }
  }
  object
}
