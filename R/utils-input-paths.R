# Narrow input checks; these do not replace Camtrap DP schema validation.
.camdp_core_files <- function() {
  c("observations.csv", "deployments.csv", "media.csv", "datapackage.json")
}

.unsafe_windows_filename <- function(names) {
  stem <- tolower(sub("\\..*$", "", names))
  reserved <- c("con", "prn", "aux", "nul", "conin$", "conout$",
                paste0("com", c(1:9, "\u00b9", "\u00b2", "\u00b3")),
                paste0("lpt", c(1:9, "\u00b9", "\u00b2", "\u00b3")))
  grepl("[. ]$", names) | stem %in% reserved
}

.camdp_input_files <- function(paths) {
  vapply(.camdp_core_files(), function(name) {
    matches <- paths[tolower(basename(paths)) == name]
    if (!length(matches)) {
      stop(name, " is not available in the dataset.", call. = FALSE)
    }
    if (length(matches) != 1L) {
      stop("Ambiguous dataset: multiple files named ", name, ".",
           call. = FALSE)
    }
    matches
  }, character(1), USE.NAMES = FALSE)
}

.validate_camdp_zip_manifest <- function(manifest) {
  if (!is.data.frame(manifest) ||
      !all(c("Name", "Length") %in% names(manifest)) || !nrow(manifest) ||
      !is.character(manifest$Name) || anyNA(manifest$Name) ||
      !is.numeric(manifest$Length) || anyNA(manifest$Length) ||
      any(!is.finite(manifest$Length) | manifest$Length < 0)) {
    stop("The ZIP archive has an empty or invalid file inventory.",
         call. = FALSE)
  }
  limits <- c(
    camtrapReport.max_zip_entries = 100000,
    camtrapReport.max_zip_bytes = 20 * 1024^3
  )
  for (name in names(limits)) {
    value <- getOption(name, limits[[name]])
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value <= 0) {
      stop("Option '", name, "' must be a positive finite number.",
           call. = FALSE)
    }
    limits[[name]] <- value
  }
  if (nrow(manifest) > limits[["camtrapReport.max_zip_entries"]]) {
    stop("The ZIP archive exceeds the entry limit; see option ",
         "'camtrapReport.max_zip_entries'.", call. = FALSE)
  }
  if (sum(manifest$Length) > limits[["camtrapReport.max_zip_bytes"]]) {
    stop("The ZIP archive exceeds the declared uncompressed-size limit; ",
         "see option 'camtrapReport.max_zip_bytes'.", call. = FALSE)
  }
  paths <- manifest$Name
  unsafe <- !nzchar(paths) | grepl("^(/|[A-Za-z]:)", paths) |
    grepl("\\", paths, fixed = TRUE) |
    grepl("(^|/)\\.\\.(/|$)", paths) | grepl("[[:cntrl:]:]", paths)
  unsafe <- unsafe | vapply(strsplit(paths, "/", fixed = TRUE), function(parts) {
    any(.unsafe_windows_filename(parts[!parts %in% c("", ".")]))
  }, logical(1))
  if (any(unsafe)) {
    stop("The ZIP archive contains an unsafe file path: ",
         toString(paths[unsafe]), call. = FALSE)
  }
  canonical <- vapply(strsplit(paths, "/", fixed = TRUE), function(parts) {
    paste(parts[nzchar(parts) & parts != "."], collapse = "/")
  }, character(1))
  root_entry <- !nzchar(canonical) & paths == "./"
  if (any(!nzchar(canonical) & !root_entry)) {
    stop("The ZIP archive contains an invalid root entry.", call. = FALSE)
  }
  canonical <- tolower(canonical[!root_entry])
  files <- !grepl("/$", paths[!root_entry])
  if (anyDuplicated(canonical)) {
    stop("The ZIP archive contains duplicate file or directory paths.",
         call. = FALSE)
  }
  parents <- unlist(lapply(strsplit(canonical, "/", fixed = TRUE),
                          function(parts) {
    if (length(parts) < 2L) return(character())
    vapply(seq_len(length(parts) - 1L), function(i) {
      paste(parts[seq_len(i)], collapse = "/")
    }, character(1))
  }), use.names = FALSE)
  if (any(canonical[files] %in% parents)) {
    stop("The ZIP archive contains conflicting file and directory paths.",
         call. = FALSE)
  }
  .camdp_input_files(paths[!grepl("/$", paths)])
  invisible(manifest)
}

.check_camdp_zip_destination <- function(directory, paths, overwrite) {
  link <- Sys.readlink(directory)
  is_link <- !is.na(link) && nzchar(link)
  exists <- file.exists(directory) || dir.exists(directory) || is_link
  if (exists && !overwrite) {
    stop("The ZIP extraction target already exists: ", directory,
         ". Choose another ZIP filename/location or use overwrite = TRUE.",
         call. = FALSE)
  }
  if (exists && (is_link || !dir.exists(directory))) {
    stop("The ZIP extraction target must be a directory, not a file or ",
         "symbolic link: ", directory, call. = FALSE)
  }
  if (!exists) return(invisible(NULL))

  entries <- unique(unlist(lapply(strsplit(paths, "/", fixed = TRUE),
                                 function(parts) {
    parts <- parts[nzchar(parts) & parts != "."]
    vapply(seq_along(parts), function(i) {
      file.path(directory, paste(parts[seq_len(i)], collapse = "/"))
    }, character(1))
  }), use.names = FALSE))
  links <- Sys.readlink(entries)
  if (any(!is.na(links) & nzchar(links))) {
    stop("The ZIP extraction target contains a symbolic-link path.",
         call. = FALSE)
  }
  invisible(NULL)
}

.validate_gui_upload <- function(upload, extensions, multiple = FALSE) {
  if (!is.data.frame(upload) || !nrow(upload) ||
      !all(c("name", "datapath") %in% names(upload)) ||
      (!multiple && nrow(upload) != 1L)) {
    stop("Upload must contain the expected number of named files.",
         call. = FALSE)
  }
  names <- upload$name
  paths <- upload$datapath
  if (!is.character(names) || anyNA(names) || !all(nzchar(names)) ||
      any(names %in% c(".", "..")) ||
      any(grepl("[/\\\\[:cntrl:]:]", names)) ||
      any(.unsafe_windows_filename(names))) {
    stop("Uploaded filenames must be safe basenames without path components.",
         call. = FALSE)
  }
  if (anyDuplicated(tolower(names))) {
    stop("Uploaded filenames must be unique (ignoring case).", call. = FALSE)
  }
  pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")
  if (!all(grepl(pattern, names, ignore.case = TRUE))) {
    stop("Unsupported upload extension; expected ",
         toString(paste0(".", extensions)), ".", call. = FALSE)
  }
  if (!is.character(paths) || anyNA(paths) || !all(nzchar(paths)) ||
      any(!file.exists(paths) | dir.exists(paths))) {
    stop("Uploaded temporary files are missing or are not regular files.",
         call. = FALSE)
  }
  invisible(upload)
}
