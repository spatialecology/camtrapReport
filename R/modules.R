# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  March 2026
# Version 0.2.22
# Licence MIT
#--------

#----------------------------
# helpers
#----------------------------

.section_dir <- function(package = "camtrapReport", dir = NULL) {
  if (!is.null(dir)) {
    return(normalizePath(dir, winslash = "/", mustWork = TRUE))
  }
  
  d <- system.file("reportSections", package = package)
  if (!nzchar(d)) {
    stop("Could not locate 'reportSections' for package: ", package)
  }
  d
}

.modules_info_path <- function(dir) {
  p <- list.files(dir, pattern = "__modulesList\\.csv$", full.names = TRUE)
  if (length(p) == 0L) {
    stop("Could not find '__modulesList.csv' in: ", dir)
  }
  p[1]
}

.norm_parent <- function(x) {
  if (is.null(x) || length(x) == 0L) return(".root")
  x <- trimws(as.character(x)[1])
  
  if (is.na(x) || !nzchar(x) || tolower(x) %in% c(".root", "root")) {
    ".root"
  } else {
    x
  }
}

.empty_info <- function() {
  data.frame(
    ID = integer(),
    name = character(),
    parent = character(),
    stringsAsFactors = FALSE
  )
}

.resequence_info <- function(info) {
  if (is.null(info) || nrow(info) == 0L) return(.empty_info())
  
  info <- as.data.frame(info, stringsAsFactors = FALSE)
  
  if (!all(c("name", "parent") %in% names(info))) {
    stop("'.info' must contain columns: 'name' and 'parent'")
  }
  
  info$name   <- trimws(as.character(info$name))
  info$parent <- vapply(info$parent, .norm_parent, character(1))
  
  info <- info[nzchar(info$name), c("name", "parent"), drop = FALSE]
  info <- info[!duplicated(info$name), , drop = FALSE]
  
  info$ID <- seq_len(nrow(info))
  info[, c("ID", "name", "parent"), drop = FALSE]
}

.read_modules_info <- function(dir, level0 = NULL, create_if_missing = FALSE) {
  path <- file.path(dir, "__modulesList.csv")
  
  if (!file.exists(path)) {
    if (!create_if_missing) {
      stop("Could not find '__modulesList.csv' in: ", dir)
    }
    
    info <- data.frame(
      ID = seq_along(level0),
      name = level0,
      parent = ".root",
      stringsAsFactors = FALSE
    )
    utils::write.csv(info, path, row.names = FALSE)
    return(info)
  }
  
  info <- utils::read.csv(path, stringsAsFactors = FALSE)
  if ("ID" %in% names(info)) {
    info <- info[order(info$ID), , drop = FALSE]
  }
  
  .resequence_info(info)
}

.ancestor_chain <- function(name, parent_lookup) {
  out <- character()
  cur <- name
  
  repeat {
    p <- parent_lookup[[cur]]
    if (is.null(p) || identical(p, ".root")) break
    out <- c(out, p)
    cur <- p
  }
  
  out
}

.subtree_end <- function(info, parent_name) {
  info <- .resequence_info(info)
  parent_name <- .norm_parent(parent_name)
  
  if (nrow(info) == 0L) return(0L)
  if (identical(parent_name, ".root")) return(nrow(info))
  
  i <- match(parent_name, info$name)
  if (is.na(i)) {
    stop("Unknown parent: ", parent_name)
  }
  
  if (i == nrow(info)) return(i)
  
  parent_lookup <- setNames(info$parent, info$name)
  end_i <- i
  
  for (j in seq.int(i + 1L, nrow(info))) {
    anc <- .ancestor_chain(info$name[j], parent_lookup)
    if (parent_name %in% anc) {
      end_i <- j
    } else {
      break
    }
  }
  
  end_i
}

.insert_row <- function(df, row, pos) {
  if (nrow(df) == 0L) return(row)
  
  if (pos <= 1L) {
    return(rbind(row, df))
  }
  
  if (pos > nrow(df)) {
    return(rbind(df, row))
  }
  
  rbind(
    df[seq_len(pos - 1L), , drop = FALSE],
    row,
    df[pos:nrow(df), , drop = FALSE]
  )
}

.guess_root_insert_pos <- function(info, name, level0) {
  if (nrow(info) == 0L) return(1L)
  
  # If the module name matches one of the canonical root sections,
  # place it according to level0; otherwise append at end.
  idx <- match(name, level0)
  if (is.na(idx)) {
    return(nrow(info) + 1L)
  }
  
  root_names <- info$name[info$parent == ".root"]
  
  prev_roots <- rev(level0[seq_len(max(0, idx - 1L))])
  prev_roots <- prev_roots[prev_roots %in% root_names]
  
  if (length(prev_roots) > 0L) {
    return(.subtree_end(info, prev_roots[1]) + 1L)
  }
  
  next_roots <- level0[seq.int(idx + 1L, length(level0))]
  next_roots <- next_roots[next_roots %in% root_names]
  
  if (length(next_roots) > 0L) {
    return(match(next_roots[1], info$name))
  }
  
  nrow(info) + 1L
}

.insert_module_info <- function(info,
                                name,
                                parent = ".root",
                                before = NULL,
                                after = NULL,
                                level0 = c("introduction", "methods", "results",
                                           "acknowledgements", "appendix")) {
  info <- .resequence_info(info)
  name <- trimws(as.character(name)[1])
  parent <- .norm_parent(parent)
  
  if (!nzchar(name)) stop("Module name is empty.")
  if (name %in% info$name) stop("Module already exists in .info: ", name)
  if (!is.null(before) && !is.null(after)) {
    stop("Use only one of 'before' or 'after'.")
  }
  
  if (!(identical(parent, ".root") || parent %in% info$name)) {
    stop("Parent not found in .info: ", parent)
  }
  
  if (nrow(info) == 0L) {
    out <- data.frame(name = name, parent = parent, stringsAsFactors = FALSE)
    return(.resequence_info(out))
  }
  
  # Allowed insertion range for a child of `parent`
  start_pos <- if (identical(parent, ".root")) 1L else match(parent, info$name) + 1L
  end_pos <- .subtree_end(info, parent)
  max_pos <- end_pos + 1L
  
  if (!is.null(before)) {
    before <- as.character(before)[1]
    if (!before %in% info$name) stop("'before' was not found in .info: ", before)
    
    insert_pos <- match(before, info$name)
    
    if (insert_pos < start_pos || insert_pos > max_pos) {
      stop("'before' is outside the subtree allowed by parent = '", parent, "'.")
    }
    
  } else if (!is.null(after)) {
    after <- as.character(after)[1]
    if (!after %in% info$name) stop("'after' was not found in .info: ", after)
    
    if (identical(after, parent)) {
      insert_pos <- match(parent, info$name) + 1L
    } else {
      insert_pos <- .subtree_end(info, after) + 1L
    }
    
    if (insert_pos < start_pos || insert_pos > max_pos) {
      stop("'after' is outside the subtree allowed by parent = '", parent, "'.")
    }
    
  } else {
    if (identical(parent, ".root")) {
      insert_pos <- .guess_root_insert_pos(info, name, level0)
    } else {
      insert_pos <- end_pos + 1L
    }
  }
  
  out <- .insert_row(
    df  = info[, c("name", "parent"), drop = FALSE],
    row = data.frame(name = name, parent = parent, stringsAsFactors = FALSE),
    pos = insert_pos
  )
  
  .resequence_info(out)
}

#----------------------------
# read all modules in order
#----------------------------

.read_modules <- function(level0 = c("introduction", "methods", "results",
                                     "acknowledgements", "appendix"),
                          package = "camtrapReport",
                          dir = NULL,
                          write_info = TRUE) {
  module_dir <- .section_dir(package = package, dir = dir)
  info_path <- .modules_info_path(module_dir)
  info <- .read_modules_info(module_dir, level0 = level0)
  
  files <- list.files(
    module_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )
  
  modules_by_name <- list()
  meta_by_name <- list()
  err  <- list()
  
  for (f in files) {
    m <- try(.read_yml(f), silent = TRUE)
    
    if (inherits(m, "try-error")) {
      err[[basename(f)]] <- m
      next
    }
    
    vv <- try(methods::validObject(m, test = TRUE), silent = TRUE)
    if (!inherits(vv, "try-error") && !isTRUE(vv)) {
      err[[basename(f)]] <- vv
      next
    }
    
    modules_by_name[[m@name]] <- m
    meta_by_name[[m@name]] <- list(
      name = m@name,
      parent = .norm_parent(m@parent)
    )
  }
  
  # Add modules missing from .info, iteratively so parent modules can be added
  # before their children.
  missing_in_info <- setdiff(names(modules_by_name), info$name)
  added_to_info <- character()
  
  while (length(missing_in_info) > 0L) {
    progress <- FALSE
    
    for (nm in missing_in_info) {
      p <- meta_by_name[[nm]]$parent
      
      if (identical(p, ".root") || p %in% info$name) {
        info <- .insert_module_info(
          info = info,
          name = nm,
          parent = p,
          level0 = level0
        )
        added_to_info <- c(added_to_info, nm)
        progress <- TRUE
      }
    }
    
    missing_in_info <- setdiff(missing_in_info, added_to_info)
    
    if (!progress && length(missing_in_info) > 0L) {
      stop(
        "Could not place these modules because their parent is missing ",
        "or circular: ",
        paste(missing_in_info, collapse = ", ")
      )
    }
  }
  
  if (write_info && length(added_to_info) > 0L) {
    utils::write.csv(info, info_path, row.names = FALSE)
  }
  
  ordered_names <- info$name[info$name %in% names(modules_by_name)]
  modules <- unname(modules_by_name[ordered_names])
  names(modules) <- ordered_names
  
  attr(modules, "info") <- info
  attr(modules, "added_to_info") <- added_to_info
  attr(modules, "errors") <- err
  attr(modules, "missing_files") <- setdiff(info$name, names(modules_by_name))
  
  modules
}

#----------------------------
# add one new module
#----------------------------
.add_Module <- function(x,
                        before = NULL,
                        after = NULL,
                        test = TRUE,
                        level0 = c("introduction", "methods", "results",
                                   "acknowledgements", "appendix"),
                        package = "camtrapReport",
                        dir = NULL) {
  module_dir <- .section_dir(package = package, dir = dir)
  .trash_dir(module_dir, create = TRUE)
  info_path <- .modules_info_path(module_dir)
  info <- .read_modules_info(module_dir, level0 = level0)
  
  m <- try(.read_yml(x), silent = TRUE)
  if (inherits(m, "try-error")) {
    stop("The module could not be read:\n", as.character(m))
  }
  
  if (isTRUE(test)) {
    vv <- methods::validObject(m, test = TRUE)
    if (!isTRUE(vv)) {
      stop("Invalid .textSection object:\n", paste(vv, collapse = "\n"))
    }
  }
  
  # active file scan by internal module name
  inv <- .scan_module_files_from_dir(module_dir, include_trash = FALSE, validate = FALSE)
  active_names <- unique(inv$module_name[!is.na(inv$module_name) & nzchar(inv$module_name)])
  
  if (m@name %in% info$name || m@name %in% active_names) {
    stop("A module with the same internal name already exists: ", m@name)
  }
  
  parent <- .norm_parent(m@parent)
  if (!(identical(parent, ".root") || parent %in% info$name)) {
    stop("Parent module not found in .info: ", parent)
  }
  
  info2 <- .insert_module_info(
    info  = info,
    name = m@name,
    parent = parent,
    before = before,
    after = after,
    level0 = level0
  )
  
  # keep the filename the user supplied
  dest <- file.path(module_dir, basename(x))
  if (file.exists(dest)) {
    stop("Destination file already exists: ", dest)
  }
  
  ok <- file.copy(from = x, to = dest, overwrite = FALSE)
  if (!ok) stop("Could not copy module file to: ", dest)
  
  utils::write.csv(info2, info_path, row.names = FALSE)
  
  invisible(list(
    module = m,
    info = info2,
    id  = info2$ID[match(m@name, info2$name)],
    file = dest
  ))
}
##########

#---------------------------------
# Trash helpers
#---------------------------------

.trash_dir <- function(module_dir, create = TRUE) {
  td <- file.path(module_dir, "Trash")
  if (create && !dir.exists(td)) {
    dir.create(td, recursive = TRUE, showWarnings = FALSE)
  }
  td
}

.trash_index_path <- function(trash_dir) {
  file.path(trash_dir, "__trashIndex.csv")
}

.empty_trash_index <- function() {
  data.frame(
    batch_id  = character(),
    name  = character(),
    parent = character(),
    original_id = integer(),
    before_anchor = character(),
    after_anchor = character(),
    original_file = character(),
    trash_file = character(),
    deleted_at = character(),
    recovered = logical(),
    recovered_at = character(),
    stringsAsFactors = FALSE
  )
}

.read_trash_index <- function(module_dir) {
  td <- .trash_dir(module_dir, create = TRUE)
  p <- .trash_index_path(td)
  
  if (!file.exists(p)) {
    idx <- .empty_trash_index()
    utils::write.csv(idx, p, row.names = FALSE)
    return(idx)
  }
  
  out <- utils::read.csv(p, stringsAsFactors = FALSE)
  needed <- names(.empty_trash_index())
  for (nm in setdiff(needed, names(out))) {
    out[[nm]] <- .empty_trash_index()[[nm]]
  }
  out[, needed, drop = FALSE]
}

.write_trash_index <- function(module_dir, idx) {
  td <- .trash_dir(module_dir, create = TRUE)
  utils::write.csv(idx, .trash_index_path(td), row.names = FALSE)
  invisible(idx)
}

#---------------------------------
# File helpers
#---------------------------------

.find_module_file <- function(module_dir, name) {
  cand <- list.files(
    module_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE,
    recursive = FALSE
  )
  
  if (length(cand) == 0L) return(NULL)
  
  base_no_ext <- tools::file_path_sans_ext(basename(cand))
  w <- which(base_no_ext == name)
  
  if (length(w) == 0L) return(NULL)
  cand[w[1]]
}

.safe_file_move <- function(from, to) {
  if (!file.exists(from)) {
    stop("Source file does not exist: ", from)
  }
  
  ok <- file.rename(from, to)
  if (isTRUE(ok)) return(invisible(TRUE))
  
  ok2 <- file.copy(from, to, overwrite = FALSE)
  if (!isTRUE(ok2)) {
    stop("Could not move file from '", from, "' to '", to, "'.")
  }
  
  unlink(from, force = TRUE)
  invisible(TRUE)
}

.unique_trash_file <- function(trash_dir, original_basename, name = NULL) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  base <- if (!is.null(name)) paste0(name, "__", stamp, "__", original_basename) else
    paste0(stamp, "__", original_basename)
  
  out <- file.path(trash_dir, base)
  i <- 1L
  while (file.exists(out)) {
    out <- file.path(trash_dir, paste0(i, "__", base))
    i <- i + 1L
  }
  out
}
#----
#---------------------------------
# Tree helpers
#---------------------------------

.get_descendants <- function(info, parent_name, include_self = FALSE) {
  info <- .resequence_info(info)
  
  if (!parent_name %in% info$name) {
    stop("Unknown module: ", parent_name)
  }
  
  parent_lookup <- stats::setNames(info$parent, info$name)
  
  is_desc <- function(x, target) {
    cur <- x
    repeat {
      p <- parent_lookup[[cur]]
      if (is.null(p) || identical(p, ".root")) return(FALSE)
      if (identical(p, target)) return(TRUE)
      cur <- p
    }
  }
  
  out <- info$name[vapply(info$name, is_desc, logical(1), target = parent_name)]
  
  if (include_self) c(parent_name, out) else out
}

.order_names_by_info <- function(info, x) {
  info <- .resequence_info(info)
  x[x %in% info$name][order(match(x[x %in% info$name], info$name))]
}

.nearest_surviving_anchors <- function(info, target_names, nm) {
  info <- .resequence_info(info)
  target_names <- unique(target_names)
  
  i <- match(nm, info$name)
  if (is.na(i)) stop("Unknown module: ", nm)
  
  keep <- !(info$name %in% target_names)
  
  before_anchor <- NA_character_
  after_anchor  <- NA_character_
  
  if (any(keep[seq_len(i - 1L)])) {
    before_anchor <- tail(info$name[keep & seq_len(nrow(info)) < i], 1)
  }
  
  if (i < nrow(info) && any(keep[seq.int(i + 1L, nrow(info))])) {
    after_anchor <- info$name[keep & seq_len(nrow(info)) > i][1]
  }
  
  list(before_anchor = before_anchor, after_anchor = after_anchor)
}

#----------------------------------
.delete_Module <- function(x,
                           recursive = TRUE,
                           package = "camtrapReport",
                           dir = NULL) {
  module_dir <- .section_dir(package = package, dir = dir)
  info <- .read_modules_info(module_dir)
  trash_idx <- .read_trash_index(module_dir)
  trash_dir <- .trash_dir(module_dir, create = TRUE)
  
  x <- unique(trimws(as.character(x)))
  x <- x[nzchar(x)]
  
  if (length(x) == 0L) stop("No module names were supplied.")
  missing_names <- setdiff(x, info$name)
  if (length(missing_names) > 0L) {
    stop("These module(s) are not in .info: ", paste(missing_names, collapse = ", "))
  }
  
  to_delete <- character()
  
  for (nm in x) {
    kids <- .get_descendants(info, nm, include_self = FALSE)
    
    if (length(kids) > 0L && !isTRUE(recursive)) {
      stop(
        "Module '", nm, "' has child module(s): ",
        paste(kids, collapse = ", "),
        ". Use recursive = TRUE to delete the whole subtree."
      )
    }
    
    to_delete <- c(to_delete, nm, if (recursive) kids else character())
  }
  
  to_delete <- unique(.order_names_by_info(info, to_delete))
  batch_id <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(10000:99999, 1))
  deleted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  rows <- vector("list", length(to_delete))
  
  for (i in seq_along(to_delete)) {
    nm <- to_delete[i]
    
    anchors <- .nearest_surviving_anchors(info, to_delete, nm)
    
    file <- .find_module_file(module_dir, nm)
    
    if (is.null(file)) {
      original_file <- NA_character_
      trash_file <- NA_character_
    } else {
      original_file <- basename(file)
      dest <- .unique_trash_file(
        trash_dir,
        original_basename = basename(file),
        name = nm
      )
      .safe_file_move(file, dest)
      trash_file <- basename(dest)
    }
    
    rows[[i]] <- data.frame(
      batch_id = batch_id,
      name = nm,
      parent = info$parent[match(nm, info$name)],
      original_id = info$ID[match(nm, info$name)],
      before_anchor = ifelse(is.na(anchors$before_anchor), "", anchors$before_anchor),
      after_anchor = ifelse(is.na(anchors$after_anchor),  "", anchors$after_anchor),
      original_file = ifelse(is.na(original_file), "", original_file),
      trash_file = ifelse(is.na(trash_file), "", trash_file),
      deleted_at = deleted_at,
      recovered = FALSE,
      recovered_at = "",
      stringsAsFactors = FALSE
    )
  }
  
  trash_idx <- rbind(trash_idx, do.call(rbind, rows))
  .write_trash_index(module_dir, trash_idx)
  
  info2 <- info[!(info$name %in% to_delete), c("name", "parent"), drop = FALSE]
  info2 <- .resequence_info(info2)
  utils::write.csv(info2, .modules_info_path(module_dir), row.names = FALSE)
  
  invisible(list(
    batch_id = batch_id,
    deleted = to_delete,
    info = info2
  ))
}
#----
.recover_Module <- function(x = NULL,
                            batch_id = NULL,
                            package = "camtrapReport",
                            dir = NULL,
                            level0 = c("introduction", "methods", "results",
                                       "acknowledgements", "appendix"),
                            test = TRUE) {
  module_dir <- .section_dir(package = package, dir = dir)
  info <- .read_modules_info(module_dir, level0 = level0)
  trash_idx <- .read_trash_index(module_dir)
  trash_dir <- .trash_dir(module_dir, create = TRUE)
  
  active <- trash_idx[!isTRUE(trash_idx$recovered), , drop = FALSE]
  if (nrow(active) == 0L) stop("Trash is empty.")
  
  if (is.null(x) && is.null(batch_id)) {
    # recover latest batch by default
    batch_id <- tail(unique(active$batch_id), 1)
  }
  
  sel <- rep(FALSE, nrow(active))
  
  if (!is.null(batch_id)) {
    sel <- sel | active$batch_id %in% as.character(batch_id)
  }
  
  if (!is.null(x)) {
    sel <- sel | active$name %in% as.character(x)
  }
  
  rec <- active[sel, , drop = FALSE]
  if (nrow(rec) == 0L) {
    stop("No matching deleted modules found in Trash.")
  }
  
  rec <- rec[order(rec$original_id), , drop = FALSE]
  
  recovered_names <- character()
  skipped <- list()
  
  for (i in seq_len(nrow(rec))) {
    row <- rec[i, , drop = FALSE]
    nm <- row$name
    
    if (nm %in% info$name) {
      skipped[[nm]] <- "A module with the same name already exists in .info."
      next
    }
    
    if (!nzchar(row$parent)) row$parent <- ".root"
    parent <- .norm_parent(row$parent)
    
    if (!(identical(parent, ".root") || parent %in% info$name || parent %in% recovered_names)) {
      skipped[[nm]] <- paste0("Parent not available during recovery: ", parent)
      next
    }
    
    trash_file <- if (nzchar(row$trash_file)) file.path(trash_dir, row$trash_file) else NULL
    if (!is.null(trash_file) && !file.exists(trash_file)) {
      skipped[[nm]] <- paste0("Trash file is missing for module: ", nm)
      next
    }
    
    dest_basename <- if (nzchar(row$original_file)) row$original_file else paste0(nm, ".yml")
    dest <- file.path(module_dir, dest_basename)
    
    if (!is.null(trash_file)) {
      if (file.exists(dest)) {
        skipped[[nm]] <- paste0("Destination file already exists: ", dest_basename)
        next
      }
      .safe_file_move(trash_file, dest)
      
      if (test) {
        m <- try(.read_yml(dest), silent = TRUE)
        if (inherits(m, "try-error")) {
          # move back to trash if validation fails
          .safe_file_move(dest, trash_file)
          skipped[[nm]] <- "Recovered file failed .read_yml()."
          next
        }
        
        vv <- methods::validObject(m, test = TRUE)
        if (!isTRUE(vv)) {
          .safe_file_move(dest, trash_file)
          skipped[[nm]] <- paste(vv, collapse = "; ")
          next
        }
      }
    }
    
    before_anchor <- trimws(row$before_anchor)
    after_anchor <- trimws(row$after_anchor)
    
    info_try <- NULL
    
    if (nzchar(after_anchor) && after_anchor %in% info$name) {
      info_try <- try(
        .insert_module_info(
          info = info, name = nm, parent = parent,
          after = after_anchor, level0 = level0
        ),
        silent = TRUE
      )
      if (!inherits(info_try, "try-error")) info <- info_try
    }
    
    if (is.null(info_try) || inherits(info_try, "try-error")) {
      if (nzchar(before_anchor) && before_anchor %in% info$name) {
        info_try <- try(
          .insert_module_info(
            info = info, name = nm, parent = parent,
            before = before_anchor, level0 = level0
          ),
          silent = TRUE
        )
        if (!inherits(info_try, "try-error")) info <- info_try
      }
    }
    
    if (is.null(info_try) || inherits(info_try, "try-error")) {
      info <- .insert_module_info(
        info = info, name = nm, parent = parent, level0 = level0
      )
    }
    
    recovered_names <- c(recovered_names, nm)
    
    w <- which(trash_idx$batch_id == row$batch_id &
                 trash_idx$name == nm &
                 trash_idx$deleted_at == row$deleted_at &
                 !trash_idx$recovered)
    
    if (length(w) > 0L) {
      trash_idx$recovered[w[1]]    <- TRUE
      trash_idx$recovered_at[w[1]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    }
  }
  
  utils::write.csv(info, .modules_info_path(module_dir), row.names = FALSE)
  .write_trash_index(module_dir, trash_idx)
  
  invisible(list(
    recovered = recovered_names,
    skipped = skipped,
    info = info
  ))
}
#------
.list_Modules <- function(package = "camtrapReport",
                          dir = NULL,
                          include_unlisted = TRUE,
                          include_invalid = TRUE,
                          validate = FALSE) {
  module_dir <- .section_dir(package = package, dir = dir)
  info <- .read_modules_info(module_dir)
  inv  <- .scan_module_files_from_dir(
    module_dir = module_dir,
    include_trash = FALSE,
    validate = validate
  )
  
  valid_named <- inv[
    !is.na(inv$module_name) & nzchar(inv$module_name),
    ,
    drop = FALSE
  ]
  
  # rows for modules declared in __modulesList.csv
  out <- data.frame(
    ID = info$ID,
    name = info$name,
    parent = info$parent,
    filename = "",
    path = "",
    yml_parent = "",
    title = "",
    exists = FALSE,
    parse_ok = FALSE,
    valid = FALSE,
    file_matches_name = FALSE,
    duplicate_module_name = FALSE,
    status = "missing_file",
    stringsAsFactors = FALSE
  )
  
  m <- match(out$name, valid_named$module_name)
  
  hit <- !is.na(m)
  out$filename[hit] <- valid_named$filename[m[hit]]
  out$path[hit] <- valid_named$path[m[hit]]
  out$yml_parent[hit] <- valid_named$parent[m[hit]]
  out$title[hit] <- valid_named$title[m[hit]]
  out$exists[hit] <- TRUE
  out$parse_ok[hit] <- valid_named$parse_ok[m[hit]]
  out$valid[hit] <- valid_named$valid[m[hit]]
  out$file_matches_name[hit] <- valid_named$file_stem[m[hit]] == out$name[hit]
  out$duplicate_module_name[hit] <- valid_named$duplicate_module_name[m[hit]]
  
  out$status[hit] <- ifelse(
    out$duplicate_module_name[hit],
    "duplicate_module_name",
    ifelse(out$valid[hit], "ok", "invalid_yml")
  )
  
  # unlisted but readable files
  if (isTRUE(include_unlisted)) {
    extra <- valid_named[
      !(valid_named$module_name %in% info$name),
      ,
      drop = FALSE
    ]
    
    if (nrow(extra) > 0L) {
      extra_out <- data.frame(
        ID = NA_integer_,
        name = extra$module_name,
        parent = extra$parent,
        filename = extra$filename,
        path = extra$path,
        yml_parent = extra$parent,
        title = extra$title,
        exists = TRUE,
        parse_ok = extra$parse_ok,
        valid = extra$valid,
        file_matches_name = extra$file_stem == extra$module_name,
        duplicate_module_name = extra$duplicate_module_name,
        status = ifelse(extra$duplicate_module_name,
                        "duplicate_module_name",
                        "unlisted_file"),
        stringsAsFactors = FALSE
      )
      
      out <- rbind(out, extra_out)
    }
  }
  
  # unreadable / invalid files
  if (isTRUE(include_invalid)) {
    bad <- inv[
      is.na(inv$module_name) | !nzchar(inv$module_name) | !inv$parse_ok,
      ,
      drop = FALSE
    ]
    
    if (nrow(bad) > 0L) {
      bad_out <- data.frame(
        ID = NA_integer_,
        name = NA_character_,
        parent = NA_character_,
        filename = bad$filename,
        path = bad$path,
        yml_parent = NA_character_,
        title = NA_character_,
        exists = TRUE,
        parse_ok = bad$parse_ok,
        valid = FALSE,
        file_matches_name = NA,
        duplicate_module_name = FALSE,
        status = "parse_error",
        stringsAsFactors = FALSE
      )
      
      out <- rbind(out, bad_out)
    }
  }
  
  rownames(out) <- NULL
  out
}
#----
.list_Trash <- function(package = "camtrapReport", dir = NULL, active_only = TRUE) {
  module_dir <- .section_dir(package = package, dir = dir)
  idx <- .read_trash_index(module_dir)
  
  if (isTRUE(active_only)) {
    idx <- idx[!idx$recovered, , drop = FALSE]
  }
  
  idx[order(idx$deleted_at, idx$original_id), , drop = FALSE]
}
#----
.audit_Modules <- function(package = "camtrapReport",
                           dir = NULL,
                           validate = FALSE) {
  module_dir <- .section_dir(package = package, dir = dir)
  info <- .read_modules_info(module_dir)
  inv  <- .scan_module_files_from_dir(
    module_dir = module_dir,
    include_trash = FALSE,
    validate = validate
  )
  idx  <- .read_trash_index(module_dir)
  
  valid_named <- inv[
    inv$parse_ok &
      inv$valid &
      !is.na(inv$module_name) &
      nzchar(inv$module_name),
    ,
    drop = FALSE
  ]
  
  list(
    info = info,
    inventory = inv,
    in_info_not_file = setdiff(info$name, valid_named$module_name),
    in_file_not_info = setdiff(valid_named$module_name, info$name),
    duplicate_module_names = unique(valid_named$module_name[valid_named$duplicate_module_name]),
    parse_errors = inv[!inv$parse_ok, c("filename", "path", "error"), drop = FALSE],
    filename_mismatch = valid_named[
      valid_named$file_stem != valid_named$module_name,
      c("module_name", "filename", "parent", "path"),
      drop = FALSE
    ],
    active_trash = idx[idx$recovered == FALSE, , drop = FALSE]
  )
}
#-----
.purge_Trash <- function(x = NULL,
                         batch_id = NULL,
                         recovered_only = FALSE,
                         package = "camtrapReport",
                         dir = NULL) {
  module_dir <- .section_dir(package = package, dir = dir)
  idx <- .read_trash_index(module_dir)
  trash_dir <- .trash_dir(module_dir, create = TRUE)
  
  if (nrow(idx) == 0L) return(invisible(idx))
  
  sel <- rep(TRUE, nrow(idx))
  
  if (!is.null(x)) {
    sel <- sel & idx$name %in% as.character(x)
  }
  
  if (!is.null(batch_id)) {
    sel <- sel & idx$batch_id %in% as.character(batch_id)
  }
  
  if (isTRUE(recovered_only)) {
    sel <- sel & idx$recovered
  }
  
  drop_rows <- idx[sel, , drop = FALSE]
  
  for (f in drop_rows$trash_file) {
    if (nzchar(f)) {
      p <- file.path(trash_dir, f)
      if (file.exists(p)) unlink(p, force = TRUE)
    }
  }
  
  idx2 <- idx[!sel, , drop = FALSE]
  .write_trash_index(module_dir, idx2)
  invisible(idx2)
}
#-------
#---------------------------------
# scan module files by reading YML
#---------------------------------

.scan_module_files_from_dir <- function(module_dir,
                                        include_trash = FALSE,
                                        validate = FALSE) {
  td <- file.path(module_dir, "Trash")
  
  active <- list.files(
    module_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE,
    recursive = FALSE
  )
  
  paths <- active
  
  if (isTRUE(include_trash) && dir.exists(td)) {
    trash_files <- list.files(
      td,
      pattern = "\\.ya?ml$",
      full.names = TRUE,
      recursive = FALSE
    )
    paths <- c(paths, trash_files)
  }
  
  out0 <- data.frame(
    path = character(),
    filename = character(),
    file_stem = character(),
    module_name = character(),
    parent = character(),
    title = character(),
    parse_ok = logical(),
    valid = logical(),
    duplicate_module_name = logical(),
    error = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
  
  if (length(paths) == 0L) return(out0)
  
  rows <- lapply(paths, function(p) {
    m <- try(.read_yml(p), silent = TRUE)
    
    parse_ok <- !inherits(m, "try-error")
    valid <- FALSE
    module_name <- NA_character_
    parent <- NA_character_
    title <- NA_character_
    err <- ""
    
    if (parse_ok) {
      module_name <- trimws(as.character(m@name))
      parent <- .norm_parent(m@parent)
      title <- as.character(m@title)
      valid <- nzchar(module_name)
      
      if (isTRUE(validate) && valid) {
        vv <- methods::validObject(m, test = TRUE)
        if (!isTRUE(vv)) {
          valid <- FALSE
          err <- paste(vv, collapse = "; ")
        }
      }
      
      if (!nzchar(module_name)) {
        err <- "Empty module name in YML."
      }
    } else {
      err <- as.character(m)
    }
    
    data.frame(
      path = normalizePath(p, winslash = "/", mustWork = FALSE),
      filename = basename(p),
      file_stem = tools::file_path_sans_ext(basename(p)),
      module_name = module_name,
      parent = parent,
      title = title,
      parse_ok = parse_ok,
      valid = valid,
      duplicate_module_name = FALSE,
      error = err,
      source = if (normalizePath(dirname(p), winslash = "/", mustWork = FALSE) ==
                   normalizePath(td, winslash = "/", mustWork = FALSE)) "trash" else "active",
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, rows)
  
  named <- !is.na(out$module_name) & nzchar(out$module_name)
  dup <- duplicated(out$module_name) | duplicated(out$module_name, fromLast = TRUE)
  out$duplicate_module_name <- named & dup
  
  out
}

.module_inventory <- function(package = "camtrapReport",
                              dir = NULL,
                              include_trash = FALSE,
                              validate = FALSE) {
  module_dir <- .section_dir(package = package, dir = dir)
  .scan_module_files_from_dir(
    module_dir = module_dir,
    include_trash = include_trash,
    validate = validate
  )
}

.module_file_map <- function(module_dir, validate = FALSE) {
  inv <- .scan_module_files_from_dir(
    module_dir = module_dir,
    include_trash = FALSE,
    validate = validate
  )
  
  bad_dup <- unique(inv$module_name[inv$duplicate_module_name])
  bad_dup <- bad_dup[!is.na(bad_dup) & nzchar(bad_dup)]
  
  if (length(bad_dup) > 0L) {
    stop(
      "Duplicate module names found across YML files: ",
      paste(bad_dup, collapse = ", ")
    )
  }
  
  inv <- inv[
    inv$source == "active" &
      inv$parse_ok &
      inv$valid &
      !is.na(inv$module_name) &
      nzchar(inv$module_name),
    ,
    drop = FALSE
  ]
  
  stats::setNames(inv$path, inv$module_name)
}

.find_module_file <- function(module_dir, name, validate = FALSE) {
  mp <- .module_file_map(module_dir = module_dir, validate = validate)
  if (!name %in% names(mp)) return(NULL)
  unname(mp[[name]])
}
#----
.locate_Module <- function(x,
                           package = "camtrapReport",
                           dir = NULL,
                           include_trash = TRUE,
                           validate = FALSE) {
  inv <- .module_inventory(
    package = package,
    dir = dir,
    include_trash = include_trash,
    validate = validate
  )
  
  inv[
    inv$module_name %in% x |
      inv$filename %in% x |
      inv$file_stem %in% x,
    ,
    drop = FALSE
  ]
}
#-------

`%||%` <- function(x, y) if (is.null(x)) y else x

.module_tree_df <- function(info) {
  info <- .resequence_info(info)
  
  level_of <- function(nm, info) {
    lvl <- 0L
    cur <- nm
    repeat {
      p <- info$parent[match(cur, info$name)]
      if (is.na(p) || identical(p, ".root")) break
      lvl <- lvl + 1L
      cur <- p
    }
    lvl
  }
  
  data.frame(
    ID = info$ID,
    name = info$name,
    parent = info$parent,
    level = vapply(info$name, level_of, integer(1), info = info),
    label = vapply(
      info$name,
      function(nm) {
        lvl <- level_of(nm, info)
        paste0(strrep("  ", lvl), "- ", nm)
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}
#------

#################################
#################################
#################################
#################################






if (!isGeneric("add_Module")) {
  setGeneric("add_Module", function(x,before,after,test)
    standardGeneric("add_Module"))
}


setMethod('add_Module', signature(x='character'), 
          function(x,before,after,test) {
            if (missing(before)) before <- NULL
            if (missing(after)) after <- NULL
            if (missing(test) || !is.logical(test)) test <- TRUE
            #----------
            .module_dir <- .section_dir(package = "camtrapReport")
            if (isTRUE(test)) {
              v <- validate_module(path, render = "parse", view = FALSE)
              if (!isTRUE(v$parse_ok && v$valid_s4)) {
                stop("Module validation failed:\n", paste(v$messages, collapse = "\n"))
              }
            }
            #----
            .add_Module(
              x = x,
              before = before,
              after = after,
              test = test,
              package = "camtrapReport",
              dir = .module_dir
            )
            
          }
)
#--------


if (!isGeneric("move_Module")) {
  setGeneric("move_Module", function(name,before,after,parent,level0)
    standardGeneric("move_Module"))
}


setMethod('move_Module', signature(name='character'), 
          function(name,before,after,parent,level0) {
            if (missing(before)) before <- NULL
            if (missing(after)) after <- NULL
            if (missing(parent)) parent <- NULL
            if (missing(level0)) level0 <- c("introduction", "methods", "results",
                                             "acknowledgements", "appendix")
            #----------
            .module_dir <- .section_dir(package = "camtrapReport")
            info_path  <- .modules_info_path(.module_dir)
            info       <- .read_modules_info(.module_dir, level0 = level0)
            
            if (!name %in% info$name) {
              stop("Unknown module: ", name)
            }
            
            current_parent <- info$parent[match(name, info$name)]
            if (is.null(parent)) parent <- current_parent
            parent <- .norm_parent(parent)
            
            info2 <- info[info$name != name, c("name", "parent"), drop = FALSE]
            info2 <- .resequence_info(info2)
            
            info2 <- .insert_module_info(
              info = info2,
              name = name,
              parent = parent,
              before = before,
              after = after,
              level0 = level0
            )
            
            utils::write.csv(info2, info_path, row.names = FALSE)
            invisible(info2)
            
          }
)
#--------


if (!isGeneric("remove_Module")) {
  setGeneric("remove_Module", function(name,recursive)
    standardGeneric("remove_Module"))
}


setMethod('remove_Module', signature(name='character'), 
          function(name,recursive) {
            if (missing(recursive)) recursive <- TRUE
            #----------
            .module_dir <- .section_dir(package = "camtrapReport")
            
            .delete_Module(
              x = name,
              recursive = recursive,
              package = "camtrapReport",
              dir = .module_dir
            )
          }
)
#--------

if (!isGeneric("empty_trash")) {
  setGeneric("empty_trash", function(name,id)
    standardGeneric("empty_trash"))
}


setMethod('empty_trash', signature(), 
          function(name,id) {
            if (missing(name)) name <- NULL
            if (missing(id)) id <- NULL
            #----------
            .module_dir <- .section_dir(package = "camtrapReport")
            .purge_Trash(x = name,batch_id = id,recovered_only = FALSE,
                         package = "camtrapReport",dir = .module_dir)
          }
)
#--------

if (!isGeneric("list_Modules")) {
  setGeneric("list_Modules", function(tree,brief,include_trash,validate)
    standardGeneric("list_Modules"))
}


setMethod('list_Modules', signature(), 
          function(tree,brief,include_trash,validate) {
            if (missing(tree)) tree <- TRUE
            if (missing(brief)) brief <- TRUE
            if (missing(include_trash)) include_trash <- FALSE
            if (missing(validate)) validate <- FALSE
            #----------
            .module_dir <- .section_dir(package = "camtrapReport")
            
            if (tree) {
              info <- .read_modules_info(.module_dir)
              return(.module_tree_df(info))
            }
            
            out <- .list_Modules(
              package = "camtrapReport",
              dir = .module_dir,
              include_unlisted = TRUE,
              include_invalid = TRUE,
              validate = validate
            )
            
            if (include_trash) {
              .trash <- .list_Trash(package = "camtrapReport", dir = .module_dir, active_only = TRUE)
            }
            
            if (brief) {
              out <- out[,c(1,2,3,10,13)]
            }
            
            #----
            if (include_trash && nrow(.trash) > 0) return(list(modules=out,trash=.trash))
            else return(out)
            
          }
)
#--------


if (!isGeneric("restore_Module")) {
  setGeneric("restore_Module", function(name,batch_id,test)
    standardGeneric("restore_Module"))
}


setMethod('restore_Module', signature(name='character'), 
          function(name,batch_id,test) {
            if (missing(batch_id)) batch_id <- NULL
            if (missing(test)) test <- TRUE
            
            #----------
            .module_dir <- .section_dir(package = "camtrapReport")
            
            .recover_Module(
              x = name,
              batch_id = batch_id,
              package = "camtrapReport",
              dir = .module_dir,
              test = test
            )
            
          }
)
#--------

