# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  July 2025
# Version 1.4
# Licence GPL v3
#--------

.paste_comma_and <- function(x) {
  if (length(x) == 1) {
    return(as.character(x))
  } else {
    return(paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)]))
  }
}
#-----------
.trim <- function(x) {
  x <- strsplit(x,'')[[1]]
  if (x[1] == ' ') {
    i <- 1
    while(x[i] == ' ') {
      i <- i+1
    }
    x <- x[i:length(x)]
  }
  #-----
  if (x[length(x)] == ' ') {
    i <- length(x)
    while(x[i] == ' ') {
      i <- i-1
    }
    x <- x[1:i]
  }
  #----
  paste(x,collapse='')
}
#------------

# copied from the sdm package:
.require <-function(x) {
  x <- as.character(x)
  xx <- unlist(lapply(.libPaths(), function(lib) find.package(x, lib, quiet=TRUE, verbose=FALSE)))
  if (length(xx) > 0) {
    .loaded <- eval(parse(text=paste0('require(',x,')')))
    return (.loaded)
  } else FALSE
}
#-------
.eval <- function(x,env) {
  eval(parse(text=x),envir=env)
}
#-----------
.rmChar <- function(x,rm,rmLast=FALSE) {
  # rm: the character index to be removed!
  # rmLast: should the last character be removed?
  x <- strsplit(x,'')[[1]]
  x <- x[-rm]
  if (rmLast) x <- x[-length(x)]
  paste(x,collapse='')
  
}
#--------
.getRchunk <- function(name=NULL,setting=NULL,code) {
  
  if (as.character(substitute(setting))[1] == '{' ) {
    setting <- substitute(setting)
    setting <- as.character(setting)[-1]
    setting <- .trim(setting)
    setting <- .rmChar(setting,rm=c(1,2),rmLast = TRUE)
  }
  
  
  #----
  code <- substitute(code)
  if (as.character(code)[1] != '{' ) stop('code should be placed within { } ')
  if (is.null(setting)) {
    p1 <- paste('```{r',name,'}')
  } else {
    p1 <- paste('```{r',name,',',paste(setting,collapse = ','),'}')
  }
  #--
  p2 <- paste(as.character(code)[-1],collapse = '\n')
  #---
  paste0(p1,'\n',p2,'\n','```')
  
}
#-------
.findParent <- function(x,n) {
  if (length(x) == 0) return(NA)
  else {
    for (i in seq_along(x)) {
      if (is.list(x[[i]])) {
        .findParent(x[[i]],n)
      } else {
        if (x[[i]]@parent == n) {
          return(c(index=i,name=x[[i]]@name,parent=x[[i]]@parent))
        }
      }
    }
  }
}
#------
# get year based on the first 4 character!
# if interval=T, then it assumes year items in the interval field has 4 digits,
# so it identifies the components with 4 digits and extract them!
.getYear <- function(x,.interval=FALSE) {
  if (.interval) {
    .x <- strsplit(as.character(x),'-')
    lapply(.x,function(x) {
      .j <- sapply(x,function(k) length(strsplit(k,'')[[1]]))
      unique(as.numeric(names(.j[.j == 4])))
    })
  } else {
    as.numeric(substr(as.character(x),1,4))
  }
}
#----

.ctdp_Filter_by_year <- function(x,y) {
  .y <- .getYear(x$sequences$sequence_interval,.interval=TRUE)
  if (length(.y) == length(unlist(.y))) {
    .y <- unlist(.y)
    .w <- which(.y %in% y)
  } else {
    .w <- which(sapply(.y, function(k) any(k %in% y)))
  }
  x$sequences <- x$sequences[.w,]
  x$sequences <- x$sequences %>% filter(int_start(sequence_interval) > 
                                          start, int_start(sequence_interval) < end)
  x$deployments <- x$deployments[x$deployments$deploymentID %in% x$sequences$deploymentID,]
  x$locations <- x$locations[x$locations$locationID %in% x$deployments$locationID,]
  
  x$observations <- x$observations[x$observations$sequenceID %in% x$sequences$sequenceID,] 
  x$media <- x$media[x$media$sequenceID %in% x$sequences$sequenceID,]
  
  x
  
}
#====

.getTextObj <- function(name=NULL,title=NULL,parent=NULL,txt=NULL) {
  new('.textSection',name=name,title=title,parent=parent,txt=txt)
}
#---


.getRchunk <- function(parent=NULL,name=NULL,setting=NULL,code) {
  
  if (!is.null(setting) && as.character(substitute(setting))[1] == '{' ) {
    setting <- substitute(setting)
    setting <- as.character(setting)[-1]
    setting <- .trim(setting)
    setting <- .rmChar(setting,rm=c(1,2),rmLast = TRUE)
  }
  
  
  #----
  code <- substitute(code)
  
  if (as.character(code)[1] != '{' ) stop('code should be placed within { } ')
  
  code <- paste(as.character(code)[-1],collapse = '\n')
  
  new('.Rchunk',parent=parent,name=name,setting=setting,code=code)
  
  # if (is.null(setting)) {
  #   p1 <- paste('```{r',name,'}')
  # } else {
  #   p1 <- paste('```{r',name,',',paste(setting,collapse = ','),'}')
  # }
  # #--
  # 
  # #---
  # paste0(p1,'\n',code,'\n','```')
  
}
#----

.glueTextSection <- function(x,.envir) {
  #if (!is.null(x@parent)) headLevel <- 1
  #----
  .title <- paste0(paste(rep('#',x@headLevel),collapse = ''),' ',x@title)
  
  if (!is.null(x@txt)) .out <- paste0(c(.title,sapply(x@txt,glue::glue,.envir=.envir)),collapse = '\n\n')
  else .out <- paste0(.title,'\n\n')
  #---
  if (!is.null(x@Rchunk)) {
    
    if (is.list(x@Rchunk)) {
      for (i in seq_along(x@Rchunk)) {
        if (is.null(x@Rchunk[[i]]@setting)) {
          .p1 <- paste0('```{r ',x@Rchunk[[i]]@name,'}')
        } else {
          .p1 <- paste0('```{r ',x@Rchunk[[i]]@name,',',paste(x@Rchunk[[i]]@setting,collapse = ','),'}')
        }
        .p1 <- paste0(.p1,'\n',x@Rchunk[[i]]@code,'\n','```')
        .out <- paste0(.out,'\n\n',.p1,'\n\n')
      }
      
      
    } else {
      if (is.null(x@Rchunk@setting)) {
        p1 <- paste0('```{r ',x@Rchunk@name,'}')
      } else {
        p1 <- paste0('```{r ',x@Rchunk@name,',',paste(x@Rchunk@setting,collapse = ','),'}')
      }
      p1 <- paste0(p1,'\n',x@Rchunk@code,'\n','```')
      .out <- paste0(.out,'\n\n',p1,'\n\n')
    }
    
  }
  .out
}
#--------


.getMissingTaxon_GBIF <- function(x) {
  # this retrieves the class and order taxonomic information for each scientific name from GBIF
  # it uses the taxize package for the job:
  
  if (.require('taxize')) {
    .id <- "as.data.frame(get_gbifid(x,rows=1,ask=FALSE,messages=FALSE))"
    .id <- .eval(.id,env=environment())
    #----
    .x <- "classification(.id$ids,db='gbif')"
    .x <- .eval(.x,env=environment())
    #----
    .class <- sapply(.x, function(x) x$name[3])
    .order <- sapply(.x, function(x) x$name[4])
    names(.class) <- names(.order) <- NULL
    
    data.frame(scientificName=x,class=.class,order=.order)
    
  } else stop('You need to install the taxize package first before executing this function...!')
  
  
  
}
#----
.getMissingTaxon_NCBI <- function(x) {
  # this retrieves the class and order taxonomic information for each scientific name from NCBI
  # it uses the taxize package for the job:
  
  if (.require('taxize')) {
    .id <- "as.data.frame(get_uid(x,rows=1,ask=FALSE,messages=FALSE))"
    .id <- .eval(.id,env=environment())
    #----
    .x <- "classification(.id$ids,db='ncbi')"
    .x <- .eval(.x,env=environment())
    #----
    .class <- .order <- rep(NA,length(.x))
    
    .class <- sapply(.x, function(x) if (length(x) > 1 && 'class' %in% x$rank) x$name[x$rank == 'class'] else NA)
    .order <- sapply(.x, function(x) if (length(x) > 1 && 'order' %in% x$rank) x$name[x$rank == 'order'] else NA)
    names(.class) <- names(.order) <- NULL
    
    data.frame(scientificName=x,class=unlist(.class),order=unlist(.order))
    
  } else stop('You need to install the taxize package first before executing this function...!')
  
  
}

#----------
# copied (and adjusted) from the fit_detmodel function in the camtrapDensity package:
.fit_rem <- function (formula, dat, species = NULL, newdata = NULL, unit = c("m", "km", "cm", "degree", "radian"), ...) {
  unit <- match.arg(unit)
  allvars <- all.vars(formula)
  depvar <- allvars[1]
  covars <- tail(allvars, -1)
  #dat <- package$data$observations
  if (!all(allvars %in% names(dat))) 
    stop("Can't find all model variables in data")
  if ("distance" %in% covars) 
    stop("Cannot use \"distance\" as a covariate name - rename and try again")
  #species <- select_species(package, species)
  if ("useDeployment" %in% names(dat)) dat <- subset(dat, useDeployment)
  
  dat <- dat[dat$scientificName %in% species,allvars,drop=FALSE]
  dat <- as.data.frame(na.omit(dat))
  
  if (nrow(dat) == 0) 
    stop("There are no usable position data")
  classes <- apply(dat,2,class)
  if (classes[depvar] == "numeric") {
    colnames(dat)[which(colnames(dat) == depvar)] <- 'distance'
    dat$distance <- abs(dat$distance)
  } else {
    cats <- strsplit(as.character(dat[,depvar]),"-")
    dat$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    dat$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    dat$distance <- (dat$distbegin + dat$distend)/2
  }
  type <- if (unit %in% c("m", "km", "cm")) 
    "point" else "line"
  args <- c(data = list(dat), formula = formula[-2], transect = type, 
            list(...))
  mod <- suppressWarnings(suppressMessages(do.call(Distance::ds, 
                                                   args)$ddf))
  if (length(covars) == 0) 
    newdata <- data.frame(x = 0)
  else {
    if (is.null(newdata)) {
      newdata <- dat %>% dplyr::select(dplyr::all_of(covars)) %>% 
        lapply(function(x) if (is.numeric(x)) 
          mean(x, na.rm = T)
          else sort(unique(x))) %>% expand.grid()
    }
    else {
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
  if (length(covars) >= 1) 
    ed <- cbind(newdata, ed)
  mod$edd <- ed
  mod$unit <- unit
  mod$proportion_used <- nrow(mod$data)/nrow(dat)
  mod
}
#-----
.fit_speedmodel <- function (dat, species = NULL, newdata = NULL, formula=speed~1,
                             reps = 1000, distUnit = c("m", "km", "cm"), timeUnit = c("second", "minute", "hour", "day"), ...) {
  distUnit <- match.arg(distUnit)
  timeUnit <- match.arg(timeUnit)
  varnms <- 'speed'
  
  dat <- dat[dat$scientificName %in% species & dat$speed > 0.01 & dat$speed < 10,varnms,drop=FALSE]
  dat <- as.data.frame(na.omit(dat))
  
  if (nrow(dat) == 0) 
    stop("There are no usable speed data")
  res <- sbd::sbm(formula, dat, ...)
  res$unit <- paste(distUnit, timeUnit, sep = "/")
  res
}
#---



.fit_actmodel <- function (dat, species = NULL, reps = 999, obsdef = c("individual","sequence"), ...) {
  obsdef <- match.arg(obsdef)
  
  obs <- dat[dat$scientificName %in% species,c("deploymentID","sequenceID", "observation_timestamp",  "latitude", "longitude","count")]
  
  i <- switch(obsdef, individual = rep(1:nrow(obs), obs$count), sequence = !duplicated(obs$sequenceID))
  obs <- obs[i, ]
  
  if (nrow(obs) > 1) {
    suntimes <- activity::get_suntimes(obs$observation_timestamp, obs$latitude, obs$longitude, 0)
    timeshift <- pi - mean(suntimes[, 1] + suntimes[, 3]/2) * pi/12
    obs$solartime <- obs %>% with(activity::solartime(observation_timestamp, 
                                                      latitude, longitude, 0)) %>% .$solar %>% +timeshift %>% 
      activity::wrap()
    activity::fitact(obs$solartime, adj = 1.5, sample = "data", reps = reps, ...)
  }
  else NULL
}
#---------

# convert hms to hours:
.get_hour <- function(x) {
  as.numeric(format(as.POSIXct(x,format="%H:%M:%S"),"%H")) +
    as.numeric(format(as.POSIXct(x,format="%H:%M:%S"),"%M"))/60
}
#---------

.basic_corrplot <- function(x,main='Species Co-occurrence') {
  # a correlation plot using image function (used when corrplot is not available)!
  # x is a correlation matrix
  
  
  # Set upper triangle and diagonal to NA
  x[upper.tri(x, diag = TRUE)] <- NA
  
  # Define color palette
  .colors <- colorRampPalette(c("red", "white", "blue"))(100)
  
  # Plot the heatmap
  image(
    1:ncol(x), 1:nrow(x), t(x[nrow(x):1, ]),
    col = .colors, axes = FALSE, xlab = "", ylab = "", main = main
  )
  
  # Add axis labels with 45-degree rotation
  labels <- colnames(x)
  n <- length(labels)
  x_pos <- 1:n
  y_pos <- 1:n
  
  # X-axis labels
  text(x = x_pos, y = par("usr")[3] - 0.5, labels = labels, srt = 45, adj = 1, xpd = TRUE)
  
  # Y-axis labels
  text(x = par("usr")[1] - 0.5, y = y_pos, labels = rev(labels), srt = 45, adj = 1, xpd = TRUE)
  
  # Add color legend
  legend(
    x = n/1.2 , y = n,
    legend = round(seq(-1, 1, length.out = 10), 2),
    fill = colorRampPalette(c("red", "white", "blue"))(10),
    border = NA, bty = "n", y.intersp = 1, cex = 0.8
  )
  
}
#######


.get_projected_sf <- function(x) {
  # automatically project a spatial data with a geographic CRS
  # identify the best metric CRS given the size and location of data
  # x is an sf object!
  
  # Ensure data is geographic
  if (sf::st_crs(x)$epsg != 4326) {
    x <- sf::st_transform(x, 4326)
  }
  # Centroid longitude & latitude
  cen <- sf::st_coordinates(sf::st_centroid(sf::st_union(.xxs)))
  
  cen <- colMeans(crds(.xs))
  
  lon <- cen[1]; lat <- cen[2]
  if (abs(lat) <= 84) {
    # UTM zone calculation
    .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
    .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
    return(sf::st_transform(x, .epsg))
  } else {
    proj4 <- sprintf(
      "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
      lat, lon
    )
    return(sf::st_transform(x, proj4))
  }
  
  
}

.is.projected <- function(x) {
  e <- as.vector(ext(x))
  !all(e >= -180 & e <= 180)
}
#----

.get_projected_vect <- function(x) {
  # automatically project a spatial data with a geographic CRS
  # identify the best metric CRS given the size and location of data
  # x is an SpatVector (terra)
  
  if (!.is.projected(x)) {
    cen <- colMeans(crds(x))
    lon <- cen[1]; lat <- cen[2]
    if (abs(lat) <= 84) {
      # UTM zone calculation
      .zone <- ((floor((lon + 180) / 6) %% 60) + 1)
      .epsg <- if (lat >= 0) 32600 + .zone else 32700 + .zone
      return(project(x, paste0("EPSG:",.epsg)))
    } else {
      proj4 <- sprintf(
        "+proj=laea +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
        lat, lon
      )
      return(project(x, proj4))
    }
  } else {
    warning('The input dataset seems projected, so no projection is applied...!')
    x
  }
  
  
  
}
#----------

.get_Time_length <- function(x,y=NULL,unit='days') {
  # if y is NULL --> it is assumed that x is time-interval
  #------
  if (is.null(y)) {
    .s <- sapply(as.character(x),function(x) strsplit(x,'--')[[1]][1])
    .e <- sapply(as.character(x),function(x) strsplit(x,'--')[[1]][2])
    names(.s) <- names(.e) <-NULL
    as.numeric(difftime(as.POSIXct(.e),as.POSIXct(.s),units=unit))
  } else {
    as.numeric(difftime(as.POSIXct(x),as.POSIXct(y),units=unit))
  }
}
#----


.isZip <- function(x) {
  ex <- strsplit(basename(x),'\\.')[[1]]
  
  tolower(ex[length(ex)]) == 'zip'
}

#---
.isJson <- function(x) {
  # is the x a json filename?
  ex <- strsplit(basename(x),'\\.')[[1]]
  
  tolower(ex[length(ex)]) == 'json'
}
#---
.firstUpper <- function(x) {
  paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
}

############

# temporary function:
.summarize_spatial_info <- function(cm) {
  
  location_df <- cm$data$locations
  # 1. Record counts & initial cleaning
  total_locationsrow <- nrow(location_df)
  
  # Add a 'row' index and coerce empty strings to NA
  location_df <- location_df %>%
    mutate(row = row_number(),
           locationID   = na_if(trimws(locationID), ""),
           locationName = na_if(trimws(locationName), ""),
           longitude    = as.numeric(na_if(trimws(longitude), "")),
           latitude     = as.numeric(na_if(trimws(latitude), "")))
  
  # 2. Identify missing‐value rows
  location_cleaned <- location_df %>%
    filter(complete.cases(locationID, locationName, longitude, latitude))
  missing_rows <- setdiff(location_df$row, location_cleaned$row)
  
  message_missing <- if (length(missing_rows) == 0) {
    "🟢 No missing data found"
  } else {
    paste0("🔴 ", length(missing_rows), " rows with missing data: [",
           paste(missing_rows, collapse = ", "), "]")
  }
  
  # 3. Detect & remove duplicate coordinates (rounded to 6 decimals)
  location_cleaned <- location_cleaned %>%
    mutate(lon_round = round(longitude, 6),
           lat_round = round(latitude, 6))
  
  coord_dups <- location_cleaned %>%
    group_by(lon_round, lat_round) %>%
    mutate(dup_coord = n() > 1) %>%
    ungroup()
  
  num_duplicated_coordinate <- sum(coord_dups$dup_coord)
  status_duplicated_coordinate <- if (num_duplicated_coordinate > 0) {
    "🔴 Duplicate coordinates found"
  } else {
    "🟢 No duplicated coordinates"
  }
  if (num_duplicated_coordinate > 0) {
    # Keep only the first row in each duplicate‐coordinate group
    location_cleaned <- coord_dups %>%
      group_by(lon_round, lat_round) %>%
      slice(1) %>%
      ungroup()
    #----------
    
  }
  
  
  # 4. Detect & remove duplicate locationID
  num_dup_locationID <- length(which(table(location_cleaned$locationID) > 1))
  
  if (num_dup_locationID > 0) {
    status_dup_locationID <- "🔴 Duplicate IDs found"
    
    # Keep only the first row for each duplicated ID
    location_cleaned <- location_cleaned %>%
      group_by(locationID) %>%
      slice(1) %>%
      ungroup()
    
  } else {
    status_dup_locationID <- "🟢 No duplicated locationIDs"
  }
  # 
  
  
  # 5. Detect & remove duplicate locationName
  
  num_dup_locationName <- length(which(table(location_cleaned$locationName) > 1))
  
  if (num_dup_locationName > 0) {
    status_dup_locationName <- "🔴 Duplicate IDs found"
    
    # Keep only the first row for each duplicated name
    location_cleaned <- location_cleaned %>%
      group_by(locationName) %>%
      slice(1) %>%
      ungroup()
    
  } else {
    status_dup_locationName <- "🟢 No duplicated locationIDs"
  }
  
  # 
  
  # 6. Final set of unique, cleaned locations
  total_unique_locations_df <- location_cleaned %>%
    distinct(longitude, latitude, .keep_all = TRUE)
  total_unique_locations <- nrow(total_unique_locations_df)
  
  
  # Early exit if only one unique location remains
  if (total_unique_locations == 1) {
    note <- "🟡 Only one unique location — spatial analysis skipped."
    return(list(
      total_locationsrow     = total_locationsrow,
      total_unique_locations = total_unique_locations,
      message_missing        = message_missing,
      status_duplicated_coordinate = status_duplicated_coordinate,
      status_dup_locationID  = status_dup_locationID,
      status_dup_locationName= status_dup_locationName,
      note                   = note
    ))
  }
  
  note <- "🟢 Proceeding with spatial analysis"
  cat(note, "\n")
  
  # 7. Outlier Detection (nearest‐neighbor based)
  .getOutlier <- function(df, minD = 2, prob = 0.99) {
    stopifnot(is.data.frame(df),
              all(c("longitude", "latitude", "locationName") %in% names(df)))
    coords_mat <- as.matrix(df[, c("longitude", "latitude")])
    names_vec <- df$locationName
    
    # Pairwise distance matrix (in meters)
    
    dist_matrix <- as.matrix(distance(coords_mat,lonlat=TRUE))
    diag(dist_matrix) <- NA
    
    
    # For each point, the distance to its nearest neighbor
    nn_dist <- apply(dist_matrix, 1, min, na.rm = TRUE)
    
    # Compute threshold based on the (prob) quantile
    q_threshold <- quantile(nn_dist, prob = prob, na.rm = TRUE)
    mean_within <- mean(nn_dist[nn_dist < q_threshold], na.rm = TRUE)
    
    # Identify outlier tiers
    w3 <- which(nn_dist > q_threshold + ((minD + 2) * mean_within))  # high‐risk
    w2 <- setdiff(which(nn_dist > q_threshold + ((minD + 1) * mean_within)), w3)  # med‐risk
    w1 <- setdiff(which(nn_dist > q_threshold + (minD * mean_within)), c(w2, w3))  # low‐risk
    
    # Closest and farthest overall pairs
    min_idx <- which(dist_matrix == min(dist_matrix, na.rm = TRUE), arr.ind = TRUE)[1, ]
    max_idx <- which(dist_matrix == max(dist_matrix, na.rm = TRUE), arr.ind = TRUE)[1, ]
    min_pair <- paste(names_vec[min_idx], collapse = " and ")
    max_pair <- paste(names_vec[max_idx], collapse = " and ")
    
    list(
      low_prob           = w1,
      medium             = w2,
      high_prob          = w3,
      mean_distance      = mean(nn_dist, na.rm = TRUE),
      min_distance       = min(nn_dist, na.rm = TRUE),
      max_distance       = max(nn_dist, na.rm = TRUE),
      min_distance_names = min_pair,
      max_distance_names = max_pair
    )
  }
  
  outlier_res <- .getOutlier(total_unique_locations_df, minD = 2, prob = 0.99)
  
  mean_distance_cam       <- round(outlier_res$mean_distance, 2)
  min_distance_cam        <- round(outlier_res$min_distance, 2)
  max_distance_cam        <- round(outlier_res$max_distance, 2)
  min_distance_camNames   <- outlier_res$min_distance_names
  max_distance_camNames   <- outlier_res$max_distance_names
  
  num_lowrisk_outliers    <- length(outlier_res$low_prob)
  num_mediumrisk_outliers <- length(outlier_res$medium)
  num_highrisk_outliers   <- length(outlier_res$high_prob)
  
  safe_get_names <- function(idxs) {
    if (length(idxs) > 0) {
      sort(unique(na.omit(total_unique_locations_df$locationName[idxs])))
    } else {
      character(0)
    }
  }
  
  low_names  <- safe_get_names(outlier_res$low_prob)
  med_names  <- safe_get_names(outlier_res$medium)
  high_names <- safe_get_names(outlier_res$high_prob)
  
  # Build a summary string for outliers, ensuring it’s always a character
  distance_outlier_summary <- ""
  if (num_highrisk_outliers > 0) {
    distance_outlier_summary <- paste0(distance_outlier_summary,
                                       "🔴 High‐risk (", num_highrisk_outliers, "): ",
                                       paste(high_names, collapse = ", ")
    )
  }
  if (num_mediumrisk_outliers > 0) {
    distance_outlier_summary <- paste0(distance_outlier_summary, 
                                       if (distance_outlier_summary != "") " | ",
                                       "🟠 Medium‐risk (", num_mediumrisk_outliers, "): ",
                                       paste(med_names, collapse = ", ")
    )
  }
  if (num_lowrisk_outliers > 0) {
    distance_outlier_summary <- paste0(distance_outlier_summary, 
                                       if (distance_outlier_summary != "") " | ",
                                       "🟡 Low‐risk (", num_lowrisk_outliers, "): ",
                                       paste(low_names, collapse = ", ")
    )
  }
  if (distance_outlier_summary == "") {
    distance_outlier_summary <- "🟢 No spatial outliers detected"
  }
  
  # 8. Sea‐vs‐land check
  loc   <- vect(total_unique_locations_df, geom = c("longitude", "latitude"), crs = "epsg:4326")
  wrld  <- readRDS(system.file("external/world.map", package="camtrapReport"))
  loc$on_land <- !is.na(extract( wrld[,'name'],loc)$name)
  num_sea_outliers <- sum(!loc$on_land)
  
  sea_outlier_status <- if (num_sea_outliers > 0) {
    paste0("🌊 ", num_sea_outliers, " location(s) fall in the sea.")
  } else {
    "🟢 All locations are on land."
  }
  
  outliers_status <- paste(distance_outlier_summary, sea_outlier_status, sep = " | ")
  
  # 9. Minimum Convex Polygon (MCP) & area
  
  center_lon <- mean(total_unique_locations_df$longitude, na.rm = TRUE)
  is_northern <- mean(total_unique_locations_df$latitude, na.rm = TRUE) >= 0
  #zone_number <- floor((center_lon + 180) / 6) + 1
  
  #epsg_code <- if (is_northern) 32600 + zone_number else 32700 + zone_number
  #pts_proj <- project(loc, crs = epsg_code)
  #mcp_poly <- st_convex_hull(st_union(pts_proj))
  mcp_poly <- hull(.get_projected_vect(loc))
  area_sqkm <- expanse(mcp_poly,unit='km')
  #as.numeric(size(mcp_poly)) / 1e6
  #.get_projected_vect(loc)
  
  # cat("Auto‐selected EPSG:", epsg_code, "\n")
  # cat("Convex hull area:", round(area_sqkm, 2), "km²\n")
  # 
  status_MCArea <- paste0(
    "MCP covers ", round(area_sqkm, 2), " km² over ", total_unique_locations, " points."
  )
  
  # 10. Country / Region / Timezone summary
  
  Country    <- .paste_comma_and(unique(extract(wrld, loc)$name))
  
  time_zone  <- cm$data$settings$tz
  
  summary_country_timezone <- glue(
    "Dataset spans <b>{Country}</b> with time zone <b>{time_zone}</b>."
  )
  
  # 11. Spatial Pattern Detection (Clark‐Evans/K‐function)
  coords_xy <- total_unique_locations_df %>% distinct(longitude, latitude)
  
  if (nrow(coords_xy) >= 9) {
    buffer_ratio <- 0.01
    xrange <- range(coords_xy$longitude) + diff(range(coords_xy$longitude)) * c(-buffer_ratio, buffer_ratio)
    yrange <- range(coords_xy$latitude) + diff(range(coords_xy$latitude)) * c(-buffer_ratio, buffer_ratio)
    if (.require('spatstat')) {
      win <- .eval("owin(xrange = xrange, yrange = yrange)",env=environment())
      ppp_obj <- .eval("ppp(x = coords_xy$longitude, y = coords_xy$latitude, window = win)",env=environment())
      
      qtest <- .eval("quadrat.test(ppp_obj, nx = 3, ny = 3)",env=environment())
      kres  <- .eval("Kest(ppp_obj, correction = 'iso')",env=environment())
      is_clustered <- qtest$p.value < 0.05
      is_regular   <- any(kres$iso < kres$theo)
      is_random    <- !is_clustered && !is_regular
    } else {
      is_clustered <- NULL
      is_regular   <- NULL
      is_random    <- NULL
    }
    
    
    spatial_pattern <- if (is_clustered && is_regular && is_random) {
      "Mixed: Clustered + Regular + Random"
    } else if (is_clustered && is_regular) {
      "Mixed: Clustered + Regular"
    } else if (is_clustered && is_random) {
      "Mixed: Clustered + Random"
    } else if (is_regular && is_random) {
      "Mixed: Regular + Random"
    } else if (is_clustered) {
      "Clustered"
    } else if (is_regular) {
      "Regular / Possibly Linear"
    } else if (is_random) {
      "Random (matches CSR)"
    } else {
      "Ambiguous / Inconclusive"
    }
  } else {
    spatial_pattern <- "⚠️ Too few locations to detect a spatial pattern"
  }
  
  status_spatial <- cm$pkg$project$samplingDesign
  
  status_spatial <- if (!is.null(status_spatial) && nzchar(trimws(status_spatial))) {
    status_spatial
  } else {
    "Not specified"
  }
  
  
  # FINAL RETURN — all collected summary pieces
  return(list(
    total_locationsrow           = total_locationsrow,
    total_unique_locations       = total_unique_locations,
    number_missing_rows          = length(missing_rows),
    message_missing              = message_missing,
    num_duplicated_coordinate    = num_duplicated_coordinate,
    status_duplicated_coordinate = status_duplicated_coordinate,
    num_dup_locationID           = num_dup_locationID,
    status_dup_locationID        = status_dup_locationID,
    num_dup_locationName         = num_dup_locationName,
    status_dup_locationName      = status_dup_locationName,
    mean_distance_cam            = mean_distance_cam,
    min_distance_cam             = min_distance_cam,
    max_distance_cam             = max_distance_cam,
    min_distance_camNames        = min_distance_camNames,
    max_distance_camNames        = max_distance_camNames,
    num_lowrisk_outliers         = num_lowrisk_outliers,
    num_mediumrisk_outliers      = num_mediumrisk_outliers,
    num_highrisk_outliers        = num_highrisk_outliers,
    num_sea_outliers             = num_sea_outliers,
    outliers_status              = outliers_status,
    spatial_pattern              = spatial_pattern,
    status_spatial               = status_spatial,
    MCArea                       = area_sqkm,
    status_MCArea                = status_MCArea,
    country                       = Country,
    TimeZone                      = time_zone,
    summary_country_timezone     = summary_country_timezone,
    note                          = note
  ))
}

#-----------
.loadPKG <- function(pkgs) {
  options(warn=-1)
  all(unlist(lapply(pkgs,function(p) {.require(p)})))
  options(warn=0)
}
#---------


.plot_effort <- function (x, dynamic = TRUE, main = "Effort", xlab = "time", 
                          ylab = "nr of active cams", ...) {
  z <- effort_table(x, startend = TRUE)
  
  z <- na.omit(z)
  if (dynamic) {
    series <- xts(z$nrCams, order.by = z$time, tz = "GMT")
    dygraph(series, main = main, xlab = xlab, ylab = ylab, 
            ...) %>% dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>% 
      dyRangeSelector()
  }
  else {
    plot(z$time, z$nrCams, type = "n", main = main, xlab = xlab, 
         ylab = ylab, ...)
    xx <- c(z$time, rev(z$time))
    yy <- c(z$nrCams, rep(0, length(z$nrCams)))
    polygon(xx, yy, border = NA, col = 8)
    lines(z$time, z$nrCams, type = "s")
  }
}
