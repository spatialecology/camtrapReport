.format_duration <- function(seconds) {
  seconds <- suppressWarnings(as.numeric(seconds))
  
  if (
    length(seconds) == 0 ||
      is.na(seconds) ||
      !is.finite(seconds) ||
      seconds < 0
  ) {
    return("unknown time")
  }
  
  seconds <- round(seconds)
  
  if (seconds < 60) {
    return(paste0(seconds, " sec"))
  }
  
  if (seconds < 3600) {
    minutes <- floor(seconds / 60)
    sec <- seconds %% 60
    return(paste0(minutes, " min ", sprintf("%02d", sec), " sec"))
  }
  
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  
  paste0(hours, " h ", minutes, " min")
}

#--------

.format_file_size <- function(bytes) {
  if (is.null(bytes) || length(bytes) == 0) {
    return("unknown size")
  }
  
  bytes <- suppressWarnings(as.numeric(bytes[1]))
  
  if (is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return("unknown size")
  }
  
  if (bytes < 1024) {
    return(paste0(round(bytes), " B"))
  }
  
  if (bytes < 1024^2) {
    return(paste0(round(bytes / 1024, 1), " KB"))
  }
  
  if (bytes < 1024^3) {
    return(paste0(round(bytes / 1024^2, 1), " MB"))
  }
  
  paste0(round(bytes / 1024^3, 2), " GB")
}

#--------

.estimate_camdata_size <- function(data) {
  if (
    is.null(data) ||
      length(data) == 0 ||
      is.na(data[1]) ||
      !file.exists(data[1])
  ) {
    return(list(
      file_size = NA_real_,
      file_size_label = "unknown size",
      zip_uncompressed_size = NA_real_,
      zip_uncompressed_label = "unknown size",
      effective_size = NA_real_,
      effective_size_label = "unknown size",
      size_class = "unknown"
    ))
  }
  
  data <- as.character(data[1])
  
  file_size <- NA_real_
  zip_uncompressed_size <- NA_real_
  
  if (dir.exists(data)) {
    all_files <- list.files(data, recursive = TRUE, full.names = TRUE)
    all_files <- all_files[file.exists(all_files)]
    
    if (length(all_files) > 0) {
      file_size <- sum(file.info(all_files)$size, na.rm = TRUE)
    }
  } else {
    file_size <- file.info(data)$size
  }
  
  if (grepl("\\.[Zz][Ii][Pp]$", data)) {
    zip_info <- try(utils::unzip(data, list = TRUE), silent = TRUE)
    
    if (!inherits(zip_info, "try-error") && "Length" %in% names(zip_info)) {
      zip_uncompressed_size <- sum(zip_info$Length, na.rm = TRUE)
    }
  }
  
  effective_size <- suppressWarnings(
    max(
      c(file_size, zip_uncompressed_size),
      na.rm = TRUE
    )
  )
  
  if (!is.finite(effective_size)) {
    effective_size <- NA_real_
  }
  
  size_class <- if (is.na(effective_size)) {
    "unknown"
  } else if (effective_size < 200 * 1024^2) {
    "small"
  } else if (effective_size < 1024^3) {
    "medium"
  } else if (effective_size < 5 * 1024^3) {
    "large"
  } else {
    "very_large"
  }
  
  list(
    file_size = file_size,
    file_size_label = .format_file_size(file_size),
    zip_uncompressed_size = zip_uncompressed_size,
    zip_uncompressed_label = .format_file_size(zip_uncompressed_size),
    effective_size = effective_size,
    effective_size_label = .format_file_size(effective_size),
    size_class = size_class
  )
}

#--------

.camdata_start_message <- function(data) {
  size_info <- .estimate_camdata_size(data)
  
  message("The camReport object is being created...")
  
  if (!is.na(size_info$zip_uncompressed_size)) {
    message(
      "Dataset size: ",
      size_info$file_size_label,
      " compressed; about ",
      size_info$zip_uncompressed_label,
      " after unzip."
    )
  } else {
    message("Dataset size: ", size_info$file_size_label, ".")
  }
  
  if (identical(size_info$size_class, "small")) {
    message(
      "File size looks modest, but full object creation may still take ",
      "several minutes depending on the number of records."
    )
  } else if (identical(size_info$size_class, "medium")) {
    message(
      "This may take several minutes. ",
      "Progress updates will be shown below."
    )
  } else if (identical(size_info$size_class, "large")) {
    message(
      "This is a large dataset. Object creation may take some time. ",
      "Progress updates will be shown below."
    )
  } else if (identical(size_info$size_class, "very_large")) {
    message(
      "This is a very large dataset. Please keep R running; ",
      "creating the camReport object may take some time. ",
      "Progress updates will be shown below."
    )
  } else {
    message(
      "Creating the camReport object may take some time, depending ",
      "on file size, number of records, and enabled analyses. ",
      "Progress updates will be shown below."
    )
  }
  
  invisible(size_info)
}

#--------

.camdata_done_message <- function(start_time, site_name = NULL) {
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  
  if (
    is.null(site_name) ||
      length(site_name) == 0 ||
      is.na(site_name[1]) ||
      !nzchar(site_name[1])
  ) {
    site_name <- "your study site"
  }
  
  message("Data loaded successfully in ", .format_duration(elapsed), ".")
  message("camReport object is ready for ", site_name, ".")
  
  invisible(TRUE)
}

#--------


#--------
# Safe module rendering helpers
#--------

