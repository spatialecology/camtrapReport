# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  Jan 2025
# Version 1.1
# Licence GPL v3
#--------

# a customised version of the calc_effort in the ctdp package (developed by: Henjo de Knegt)
.calc_effort <- function (x, by = NULL,unit=c('day','hour','minute','second','week')) {
  unit <- match.arg(unit)
  
  if (unit == 'day') unit <- 'days'
  else if (unit == 'hour') unit <- 'hours'
  else if (unit == 'minute') unit <- 'mins'
  else if (unit == 'second') unit <- 'secs'
  else if (unit == 'week') unit <- 'weeks'
  
  #------
  
  xdep <- x$deployments %>% left_join(x$locations, by = "locationID") %>% 
    mutate(dep_length = (deploymentEnd - deploymentStart)) 
  
  units(xdep$dep_length) <- unit
  
  if (!is.null(by)) {
    xdep <- xdep %>% group_by(pick(all_of(by)))
  }
  xdepEffort <- xdep %>% 
    summarise(effort = sum(dep_length,na.rm=TRUE), .groups = "drop")
  return(xdepEffort)
}
#-------

# a customised version of the merige_tibbles in the ctdp package (developed by: Henjo de Knegt)
.merge_data <- function (x, dropMedia = TRUE) {
  
  keys <- list(locations = "locationID", deployments = "deploymentID", 
               sequences = "sequenceID", observations = "observationID", 
               taxonomy = "taxonID", media = "mediaID")
  if ('scientificName' %in% colnames(x$observations)) {
    x$observations <- x$observations %>% 
      dplyr::select(-scientificName)
  }
  
  
  linkStructure <- list(locations = c("locations"), deployments = c("locations", "deployments"), 
                        sequences = c("deployments", "sequences"), 
                        observations = c("sequences", "observations", "taxonomy"), 
                        media = c("sequences", "media"), taxonomy = c("taxonomy"))
  {
    omitKeys <- keys[-which(names(keys) %in% linkStructure$media)] %>% 
      unlist() %>% as.character()
    y1 <- x$media %>% select(-any_of(omitKeys))
    omitKeys <- keys[-which(names(keys) %in% linkStructure$sequences)] %>% 
      unlist() %>% as.character()
    y2 <- x$sequences %>% select(-any_of(omitKeys))
    omitKeys <- keys[-which(names(keys) %in% linkStructure$observations)] %>% 
      unlist() %>% as.character()
    y3 <- x$observations %>% select(-any_of(omitKeys))
    omitKeys <- keys[-which(names(keys) %in% linkStructure$deployment)] %>% 
      unlist() %>% as.character()
    y4 <- x$deployment %>% select(-any_of(omitKeys))
    omitKeys <- keys[-which(names(keys) %in% linkStructure$locations)] %>% 
      unlist() %>% as.character()
    y5 <- x$locations %>% select(-any_of(omitKeys))
    omitKeys <- keys[-which(names(keys) %in% linkStructure$taxonomy)] %>% 
      unlist() %>% as.character()
    y6 <- x$taxonomy %>% select(-any_of(omitKeys))
  }
  if (!dropMedia) {
    omitKeys <- keys[-which(names(keys) %in% linkStructure$media)] %>% 
      unlist() %>% as.character()
    lc_media <- x$media %>% select(-any_of(omitKeys)) %>% 
      select("sequenceID", everything()) %>% data.table(key = "sequenceID")
    lc_media <- lc_media %>% dt_nest(sequenceID, .key = "media")
  }
  {
    y <- y2 %>% left_join(y3, by = keys$sequences, multiple = "all") %>% 
      left_join(y4, by = keys$deployments) %>% 
      left_join(y5, by = keys$locations) %>% 
      left_join(y6, by = keys$taxonomy)
    y <- y %>% select(all_of(c(as.character(unlist(keys[which(names(keys) != "media")])))), everything())
  }
  if (!dropMedia) {
    y <- y %>% left_join(lc_media, by = keys$sequences) %>% 
      relocate(media, .after = nrphotos)
  }
  return(y)
}
#-----------
# x: list of camera-trap data.frames
.get_classes <- function(x,count=TRUE) {
  .x <- table(x$taxonomy$class)
  if (count) {
    .x <- sort(.x,decreasing = TRUE)
    data.frame(class=names(.x),count=as.numeric(.x))
  } else names(.x)
}
#-----------
# x: list of camera-trap data.frames
# only_species -> T (exclude families, etc., keeps only species with names at the species level)
# exlude_sp. -> T (exclude names with pattern "genus_name sp.")
.get_species <- function(x,count=TRUE,only_species=TRUE,exclude_sp.=TRUE) {
  .x <- table(x$observations$scientificName)
  .x <- .x[names(.x) != '']
  
  if (only_species) {
    .x <- .x[grepl('\\s',names(.x))]
    
  }
  
  if (exclude_sp.) {
    .x <- .x[!grepl(' sp.$',names(.x))]
  }
  
  if (count) {
    .x <- sort(.x,decreasing = TRUE)
    data.frame(scientificName=names(.x),count=as.numeric(.x))
  } else names(.x)
}
#-----------


# a customised version of the captures in the ctdp package (developed by: Henjo de Knegt)
.captures <- function (x, onlyAnimal = TRUE, class = NULL, species = NULL, by = NULL) {
  
  
  effortTable <- .calc_effort(x, by = by)
  
  y <- .merge_data(x, dropMedia = TRUE)
  
  if (onlyAnimal) {
    y <- y %>% dplyr::filter(observationType == "animal")
  }
  if (!is.null(class)) {
    .cls <- .get_classes(x,count = FALSE)
    .cls <- .get_match(class,.cls)
    if (!is.na(.cls)) y <- y %>% dplyr::filter(class %in% .cls)
    else message('The specified class(es) is/are not recognised (or not available) in the dataset, so ignored!')
  }
  #-----
  if (!is.null(species)) {
    .sp <- .get_species(x,count = F)
    species <- .get_match(species,.sp)
    species <- species[!is.na(species)]
    if (length(species) > 0) {
      y <- y %>% dplyr::filter(scientificName %in% species)
    } else messge('The specified species are not recognised (or not available) in the dataset, so ignored!')
    
  }
  #-------
  keepCols <- c("taxonID", "captureMethod", "observationType", 
                "count", "classificationMethod", "scientificName", "vernacularNames" ,"vernacularNames.eng", 
                "class", "order", by) %>% unique()
  y <- y %>% select(any_of(keepCols))
  y <- y %>% group_by(pick(all_of(unique(c("observationType", "class", "order", by)))))
  if ("vernacularNames.eng" %in% colnames(y)) {
    z <- y %>% count(taxonID, vernacularNames.eng) %>% ungroup() %>% 
      rename(captures = n)
  } else if ("vernacularNames" %in% colnames(y)) {
    z <- y %>% count(taxonID, vernacularNames) %>% ungroup() %>% 
      rename(captures = n)
  }
  
  
  if (!is.null(by)) {
    z <- z %>% left_join(effortTable, by = by)
  } else {
    z <- z %>% mutate(effort = as.numeric(effortTable$effort[1]))
  }
  z <- z %>% mutate(capture_rate = captures/as.numeric(effort))
  return(z)
}


#---------
# adjusted from the package camtrapDensity:
# copied (and adjusted) from the fit_detmodel function in the camtrapDensity package:
.fit_detmodel <- function (formula, dat, species = NULL, newdata = NULL, unit = c("m", "km", "cm", "degree", "radian"), ...) {
  unit <- match.arg(unit)
  allvars <- all.vars(formula)
  depvar <- allvars[1]
  covars <- tail(allvars, -1)
  
  if (is.null(species)) stop('species is not specified!')
  else if (!species %in% dat$scientificName) stop('species is not available in the dataset!')
  
  #dat <- package$data$observations
  if (!all(allvars %in% names(dat))) stop("Can't find all model variables in data")
  
  if ("useDeployment" %in% names(dat)) dat <- subset(dat, useDeployment)
  
  dat <- dat[dat$scientificName %in% species,allvars,drop=FALSE]
  dat <- as.data.frame(na.omit(dat))
  
  if (nrow(dat) == 0) stop("There are no usable position data")
  
  if (class(dat[[depvar]]) == "numeric") {
    colnames(dat)[which(colnames(dat) == depvar)] <- 'distance'
    dat$distance <- abs(dat$distance)
  } else {
    cats <- strsplit(as.character(dat[,depvar]),"-")
    dat$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    dat$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    dat$distance <- (dat$distbegin + dat$distend)/2
  }
  type <- if (unit %in% c("m", "km", "cm")) "point" else "line"
  args <- c(list(data = dat, formula = formula[-2], transect = type), list(...))
  
  mod <- suppressWarnings(suppressMessages(.eval('do.call(Distance::ds,args)$ddf',environment())))
  
  if (length(covars) == 0) newdata <- data.frame(x = 0)
  else {
    if (is.null(newdata)) {
      newdata <- dat %>% dplyr::select(dplyr::all_of(covars)) %>% 
        lapply(function(x) if (is.numeric(x)) 
          mean(x, na.rm = T)
          else sort(unique(x))) %>% expand.grid()
    } else {
      if (!all(covars %in% names(newdata))) 
        stop("Can't find all model covariates in newdata")
    }
  }
  prdn <- predict(mod, newdata, esw = TRUE, se.fit = TRUE)
  if (mod$meta.data$point) {
    prdn$se.fit <- 0.5 * prdn$se.fit/(pi * prdn$fitted)^0.5
    prdn$fitted <- sqrt(prdn$fitted/pi)
  }
  ed <- cbind(estimate = prdn$fitted, se = prdn$se.fit)
  if (length(covars) >= 1) ed <- cbind(newdata, ed)
  mod$edd <- ed
  mod$unit <- unit
  mod$proportion_used <- nrow(mod$data)/nrow(dat)
  mod
}

#-----------------
# adjusted from the package camtrapDensity:
.fit_speedmodel <- function (dat, species = NULL, newdata = NULL, formula=speed~1,
                             reps = 1000, distUnit = c("m", "km", "cm"), timeUnit = c("second", "minute", "hour", "day"), ...) {
  distUnit <- match.arg(distUnit)
  timeUnit <- match.arg(timeUnit)
  varnms <- 'speed'
  
  dat <- dat[dat$scientificName %in% species & dat$speed > 0.01 & dat$speed < 10,varnms,drop=FALSE]
  dat <- as.data.frame(na.omit(dat))
  if (nrow(dat) == 0) stop("There are no usable speed data")
  
  args <- c(list(formula = formula,data=dat),list(...))
  res <- .eval('do.call(sbd::sbm,args)',environment())
  
  res$unit <- paste(distUnit, timeUnit, sep = "/")
  res
}
#-------------
# adjusted from the package camtrapDensity:
.fit_actmodel <- function (dat, species = NULL, reps = 999, obsdef = c("individual","sequence")) {
  obsdef <- match.arg(obsdef)
  
  obs <- dat[dat$scientificName %in% species,c("deploymentID","sequenceID", "timestamp",  "latitude", "longitude","count")]
  
  i <- switch(obsdef, individual = rep(1:nrow(obs), obs$count), sequence = !duplicated(obs$sequenceID))
  obs <- obs[i, ]
  
  if (nrow(obs) > 1) {
    suntimes <- .eval('activity::get_suntimes(obs$timestamp, obs$latitude, obs$longitude, 0)',environment())
    timeshift <- pi - mean(suntimes[, 1] + suntimes[, 3]/2) * pi/12
    
    obs$solartime <- .eval('obs %>% with(activity::solartime(timestamp,latitude, longitude, 0)) %>% .$solar %>% + timeshift %>% activity::wrap()',environment())
    .eval('activity::fitact(obs$solartime, adj = 1.5, sample = "data", reps = reps)',environment())
  }
  else NULL
}
#---------
# adjusted from the package camtrapDensity:
.get_traprate_data <- function (dat, species = NULL, unit = c("day", "hour", "minute","second")) {
  unit <- match.arg(unit)
  #species <- select_species(package, species)
  dep <- dat$deployments %>% left_join(dat$locations,by='locationID')
  .eff <- .calc_effort(dat,by='deploymentID',unit=unit)
  .eff$effort <- as.numeric(.eff$effort)
  
  a <- dat$observations %>% group_by(deploymentID,scientificName) %>% count(scientificName)
  if (!is.null(species)) a <- a %>% filter(scientificName == species)
  else a <- a %>% dplyr::filter(scientificName != species & grepl('\\s',scientificName)) 
  res <- a %>% dplyr::left_join(dep, by = "deploymentID") %>% 
    dplyr::left_join(.eff, by = "deploymentID") %>% dplyr::group_by(locationName) %>% 
    dplyr::summarise(latitude = mean(latitude), longitude = mean(longitude), 
                     n = sum(n), effort = sum(effort)) %>% dplyr::mutate(effort_unit = unit, scientificName = paste(species, collapse = "|"))
  
  res
}
#-------
# adjusted from the package camtrapDensity:
.get_parameter_table <- function (traprate_data, radius_model, angle_model, speed_model, activity_model, reps = 999) {
  rad <- radius_model$edd
  ang <- angle_model$edd * 2
  spd <- dplyr::select(speed_model$estimate, dplyr::all_of(c("est", "se")))
  act <- activity_model@act[1:2]
  names(act) <- names(spd) <- dimnames(rad)[[2]]
  res <- data.frame(rbind(rad, ang, spd, act))
  ospd <- res[3, 1] * res[4, 1]
  se_ospd <- ospd * sqrt(sum((res[3:4, 2]/res[3:4, 1])^2))
  res <- rbind(res, c(ospd, se_ospd))
  res$cv <- res$se/res$estimate
  res$lcl95 <- res$estimate - 1.96 * res$se
  res$ucl95 <- res$estimate + 1.96 * res$se
  res$n <- c(nrow(radius_model$data), nrow(angle_model$data), 
             nrow(speed_model$data), length(activity_model@data), 
             NA)
  res$unit <- c(radius_model$unit, angle_model$unit, speed_model$unit, "none", speed_model$unit)
  rownames(res) <- c("radius", "angle", "active_speed", "activity_level", "overall_speed")
  traprate <- .eval('camtrapDensity::get_trap_rate(traprate_data, strata=NULL, reps)',environment())
  j <- c("estimate", "se", "lcl95", "ucl95")
  traprate[, j] <- traprate[, j] * radius_model$proportion_used
  res <- rbind(res, traprate)
  .eval('camtrapDensity::convert_units(res)',environment())
}
#-------------

# checks whether required data are available for REM analysis:
.any_data_for_rem <- function(dat,sp=NULL) {
  tab <- dat$observations
  
  if (!is.null(sp)) {
    tab <- dat$observations %>% 
      dplyr::filter(scientificName %in% sp)
  }
  #--------
  tab <- tab %>% 
    dplyr::group_by(scientificName) %>% 
    dplyr::summarise(n_sequences = sum(!duplicated(sequenceID)),
                     n_individuals = sum(count), 
                     n_speeds = sum(speed > 0.01 & speed < 10 & !is.na(speed)), n_radii = sum(!is.na(radius)), 
                     n_angles = sum(!is.na(angle))) %>% 
    dplyr::filter(!is.na(scientificName) & scientificName != "" & grepl('\\s',scientificName))
  
  if (nrow(tab) > 0) {
    sp <- unique(tab$scientificName)
    
    o <- rep(NA,length(sp))
    names(o) <- sp
    o[sp] <- apply(tab[,c('n_speeds','n_radii','n_angles')],1,function(x) all(x > 0))
    
    o
  } else NA
}
#-------
.rem <- function (parameters) {
  required_rows <- c("trap_rate", "overall_speed", "radius", "angle")
  required_cols <- c("estimate", "se", "unit")
  if (!all(required_rows %in% rownames(parameters)) | !all(required_cols %in% colnames(parameters))) 
    stop(paste("parameters must have (at least) row names:", 
               paste(required_rows, collapse = ", "), ";\nand (at least) column names:", 
               paste(required_cols, collapse = ", ")))
  param <- .eval("camtrapDensity::convert_units(parameters[required_rows, ])",environment())
  wtd_est <- param$estimate + c(0, 0, 0, 2)
  pwr_est <- wtd_est^c(1, -1, -1, -1)
  CVs <- param$se/wtd_est
  density <- pi * prod(pwr_est)
  cv <- sqrt(sum(CVs^2))
  se <- density * cv
  ci <- unname(lnorm_confint(density, se))
  parameters["density", "estimate"] <- density
  parameters["density", "se"] <- se
  if ("cv" %in% names(parameters)) parameters["density", "cv"] <- cv
  if ("lcl95" %in% names(parameters)) parameters["density", "lcl95"] <- ci[1]
  if ("ucl95" %in% names(parameters)) parameters["density", "ucl95"] <- ci[2]
  parameters["density", "unit"] <- "n/km2"
  parameters
}
############


#########################################
#    functions to read report modules:  #
#########################################


.read_section_module <- function(path) {
  m <- rmarkdown::yaml_front_matter(path)  # returns a list
  # yaml_front_matter() reads YAML from Rmd/MD files.
  
  # basic validation
  if (is.null(m$name)) {
    stop("Module must define at least `name:` in YAML: ", path)
  }
  
  if (is.null(m$title)) {
    m$title <- m$name
  }
  
  if (!is.null(m$parent) && identical(tolower(as.character(m$parent)), "null")) m$parent <- NULL
  
  m
}
#------


# the following, reads the setting lines (setting, name, packages, etc.) in the code
# the setting lines in the template can be started with #|
#   #| setting: echo=FALSE, results='asis'
#   #| packages: echo=FALSE, results='asis'


.parse_setting_lines <- function(code_text,key=NULL) {
  
  if (is.null(key)) stop('key is not provided...!')
  if (!is.character(key)) stop('key should be character...!')
  #-----------------
  out <- list()
  for (k in key) {
    lines <- strsplit(code_text %||% "", "\n", fixed = TRUE)[[1]]
    opt <- grep(paste0("^\\s*#\\|\\s*",k,"\\s*:"), lines, value = TRUE)
    
    if (length(opt)) {
      rhs <- sub(paste0("^\\s*#\\|\\s*",k,"\\s*:\\s*"), "", opt[1])
      parts <- trimws(strsplit(rhs, ",", fixed = TRUE)[[1]])
      parts <- parts[nzchar(parts)]
      if (length(parts)) out[[k]] <- parts
      # remove that option line from code
      lines <- lines[!grepl(paste0("^\\s*#\\|\\s*",k,"\\s*:"), lines)]
    }
    code_text <- paste(lines, collapse = "\n")
  }
  
  list(setting = out, code = code_text)
}

#-------

.read_yml <- function(x) {
  f <- .read_section_module(x)
  code <- NULL
  
  f$title <- .trim(f$title)
  
  if (substr(f$title,1,1) == "#") {
    .h <- 0
    .w <- strsplit(f$title ,"")[[1]]
    for (i in 1:4) {
      if (.w[i] == '#') {
        .h <- .h + 1
      } else {
        .w <- .w[-c(1:.h)]
        .w <- .trim(paste(.w,collapse = ''))
        break
      }
    }
    #------
    if (length(.w) > 1) {
      .w <- .w[-c(1:.h)]
      .w <- .trim(paste(.w,collapse = ''))
      .h <- 3
    }
    #---
    f$title <- .w
  } else .h <- 1
  #-----
  .txt <- .getTextObj(name=f$name,title = f$title,parent = f$parent,headLevel = .h,txt = f$text)
  
  if (length(which(grepl('code', names(f)))) == 1 && is.null(f[[which(grepl('code', names(f)))]])) {
    return(.txt)
  } else if (length(which(grepl('code', names(f)))) > 0) {
    codeList <- list()
    .w <- which(grepl('code', names(f)))
    for (i in .w) {
      
      code <- .parse_setting_lines(f[[i]],key=c('name','packages','setting'))
      #---
      if (!is.null(code)) {
        if (is.null(code$setting$name)) code$setting$name <- paste0(f$name,'__code')
        else code$setting$name <- paste0(f$name,'__',code$setting$name)
        #---
        if (!is.null(code$setting$setting)) code$setting$setting <- paste(code$setting$setting,collapse = ', ')
        else code$setting$setting <- NULL
        
        codeList[[code$setting$name]] <- new('.Rchunk',parent = f$name,name = code$setting$name,setting = code$setting$setting,packages=code$setting$packages,code=code$code)
      }
    }
    #---
    if (length(codeList) > 0) {
      if (length(codeList) > 1) .txt@Rchunk <- codeList
      else .txt@Rchunk <- codeList[[1]]
    }
  }
  
  .txt
  
}
#----------
.effort_table <- function (x, startend = FALSE) {
  x_start <- x$data$deployments$deploymentStart
  x_end <- x$data$deployments$deploymentEnd
  
  dt <- as.numeric(max(x_end)) - as.numeric(min(x_start))
  effort <- bind_rows(dplyr::tibble(time = min(x_start) - 0.025 * dt, add = 0L), 
                      dplyr::tibble(time = x_start, add = +1L), 
                      dplyr::tibble(time = x_end, add = -1L), 
                      dplyr::tibble(time = max(x_end) + 0.025 * dt, add = 0L))
  effort <- effort %>% group_by(time) %>% 
    summarize(add = sum(add)) %>% 
    ungroup()
  effort <- effort %>% arrange(time) %>% mutate(nrCams = cumsum(add))
  effort <- effort %>% select(-add)
  if (startend) {
    n <- nrow(effort)
    effort <- dplyr::tibble(time = c(effort$time[1], rep(effort$time[-1],each = 2), 
                                     effort$time[n]), nrCams = rep(effort$nrCams, each = 2))
  }
  return(effort)
}

.plot_effort <- function (x, dynamic = TRUE, main = "Effort", xlab = "time", 
                          ylab = "nr of active cams", ...) {
  z <- .effort_table(x, startend = TRUE)
  
  z <- na.omit(z)
  if (dynamic) {
    series <- .eval('xts(z$nrCams, order.by = z$time, tz = "GMT")',env=environment())
    .eval("dygraph(series, main = main, xlab = xlab, ylab = ylab, ...) %>% 
      dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>% 
      dyRangeSelector()",env=environment())
  } else {
    plot(z$time, z$nrCams, type = "n", main = main, xlab = xlab, 
         ylab = ylab, ...)
    xx <- c(z$time, rev(z$time))
    yy <- c(z$nrCams, rep(0, length(z$nrCams)))
    polygon(xx, yy, border = NA, col = 8)
    lines(z$time, z$nrCams, type = "s")
  }
}
#--------
.left_join <- function(d1,d2,by) {
  if (length(by) == 1) {
    if(!by %in% colnames(d1) & by %in% colnames(d2)) stop('the "by" column does not exist in both data!')
    merge(d1,d2,by=by,all.x=TRUE)
  } else if (length(by) == 2) {
    if(!by[1] %in% colnames(d1) & by[2] %in% colnames(d2)) stop('the "by" columns do not exist in the data!')
    merge(d1,d2,by.x=by[1],by.y=by[2],all.x=TRUE)
  }
}


#----------
.pivot_wider <- function(data,
                         id_cols,
                         names_from,
                         values_from,
                         fill = 0,
                         agg_fun = sum) {
  stopifnot(is.data.frame(data))
  stopifnot(length(id_cols) == 1L) 
  id <- id_cols
  
  # Keep only needed columns
  x <- data[, c(id, names_from, values_from)]
  names(x) <- c("..id", "..name", "..value")
  
  # Aggregate duplicates (if any) to ensure one cell per (id, name)
  x <- aggregate(..value ~ ..id + ..name, data = x, FUN = agg_fun)
  
  # Pivot: LHS is the value column (already tabulated), RHS are dimensions
  tab <- xtabs(..value ~ ..id + ..name, data = x) 
  
  # Convert to data.frame with explicit id column; keep original column names
  out <- data.frame(
    setNames(list(rownames(tab)), id),
    as.data.frame.matrix(tab, stringsAsFactors = FALSE, optional = TRUE),
    row.names = NULL,
    check.names = FALSE
  )
  
  out
}