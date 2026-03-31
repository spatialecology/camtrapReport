# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  March 2026
# Version 0.2.20
# Licence MIT
#--------


# get the sequences data.frame from media (the code copied from the ctdp package):
.getSequences <- function(media) {
  if (!.require('data.table')) stop('The data.table package is not installed...!')
  
  sequences <- .eval('media |> 
    dplyr::distinct() |> 
    dplyr::select(deploymentID, sequenceID, timestamp, captureMethod) |> 
    data.table::data.table(key = "sequenceID")',environment())
  
  # summarize per key
  sequences <- sequences[, list(deploymentID = unique(deploymentID),
                                captureMethod = unique(captureMethod),
                                start = min(timestamp),
                                end = max(timestamp),
                                nrphotos = length(timestamp)),
                         by = sequenceID]
  
  
  # convert to tibble, arrange, and convert start/end to interval object
  sequences <- .eval("sequences |>
    dplyr::as_tibble() |>
    dplyr::arrange(deploymentID, sequenceID) |> 
    dplyr::mutate(sequence_interval = lubridate::interval(start, end)) |> 
    dplyr::relocate(sequence_interval, .before =  start) |> 
    dplyr::select(-start, -end)",environment())
  
  as.data.frame(sequences)
}



.get_Taxonomic_DF <- function(x) {
  w <- sapply(x,function(x) {
    length(names(x$vernacularNames))
  })
  #-----
  if (all(w == 0)) {
    .bind_rows(lapply(x,function(x) {
      .x <- strsplit(x$taxonID,'/')[[1]]
      .x <- data.frame(taxonID=.x[length(.x)],scientificName=x$scientificName,family=x$family,order=x$order,class=NA,taxonRank=x$taxonRank)
      if (length(x$vernacularNames) > 0) .x[["vernacularNames"]] <- x$vernacularNames
      else .x[["vernacularNames"]] <- NA
      .x
    }))
  } else if (any(w > 0)) {
    .w <- max(w,na.rm=TRUE)
    .tmp <- unlist(lapply(x,function(x) {
      names(x$vernacularNames)
    }))
    if (length(unique(.tmp[!is.na(.tmp)])) > .w) {
      # sign of inconsistency in the names!
      # if inconsistent, then the most frequent name is used:
      n <- rep(NA,.w)
      for (i in 1:.w) {
        n[i] <- names(sort(table(sapply(x,function(x) {
          names(x$vernacularNames)[i]
        })),decreasing = TRUE))[1]
      }
    } else {
      w <- which.max(w)
      n <- names(x[[w]]$vernacularNames)
    }
    #----------
    
    .xx <- data.frame(taxonID="",scientificName="",family="",order="",class=NA,taxonRank="")
    if (length(n) > 0) {
      .n <- paste0("vernacularNames.",n)
      for (i in seq_along(.n)) {
        .xx[[.n[i]]] <- ""
      }
    }
    
    
    bind_rows(lapply(x,function(x) {
      .x <- .xx
      
      .tmp <- strsplit(x$taxonID,'/')[[1]]
      .x$taxonID <- .tmp[length(.tmp)]
      .x$scientificName <- x$scientificName
      .x$family <- x$family
      .x$order <- x$order
      .x$taxonRank <- x$taxonRank
      
      #.x <- strsplit(x$taxonID,'/')[[1]]
      #.x <- data.frame(taxonID=.x[length(.x)],scientificName=x$scientificName,family=x$family,order=x$order,class=NA,taxonRank=x$taxonRank)
      
      if (length(x$vernacularNames) > 0) {
        if (!is.null(names(x$vernacularNames))) {
          .n <- names(x$vernacularNames)
          for (i in seq_along(.n)) {
            .x[[.n[i]]] <- x$vernacularNames[[i]]
          }
        } else {
          for (i in 1:length(x$vernacularNames)) {
            .x[[6+i]] <- x$vernacularNames[[i]]
          }
        }
      } 
      #---
      .x
    }))
  }
}


# file is filename (.zip file OR unzipped folder with the contents)
.read_camdp <- function(file,path=NULL,tz="") {
  
  if (!.require('jsonlite')) stop('package jsonlite is not installed; please first install the package...!')
  
  .d <- list()
  
  
  if (.isZip(file)) {
    if (!is.null(path) && is.character(path)) {
      .path <- paste0(path.expand(path),'/',gsub(basename(file), pattern = ".zip", replacement = "",ignore.case = TRUE))
    } else {
      .path <- gsub(basename(file), pattern = ".zip", replacement = "",ignore.case = TRUE)
    }
    #----
    
    file <- unzip(file,exdir = .path)
    
    
    
  } else if (dir.exists(file)) {
    if (all(c("datapackage.json","deployments.csv","observations.csv") %in% tolower(dir(file)))) {
      
      .path <- file
      
      file <- dir(file,full.names = TRUE)
      
    } else {
      if (any(c("datapackage.json","deployments.csv","observations.csv") %in% tolower(dir(file)))) {
        .w <- !c("datapackage.json","deployments.csv","observations.csv","media.csv") %in% tolower(dir(file))
        stop(paste0('The standard data files (',paste(c("datapackage.json","deployments.csv","observations.csv","media.csv")[.w],collapse = ', '),') are not available in the specified folder!'))
      } else  stop('The specified folder does not have the standard data files (e.g., "datapackage.json","deployments.csv","observations.csv")...!')
    }
  } else stop('The specified input is not a zip file or a directory...!')
  #-----------------------
  
  
  .w <- grepl('observations.csv',file,ignore.case = TRUE)
  if (any(.w)) .d$observations <- .eval("as.data.frame(data.table::fread(file[.w],tz=tz))",environment())
  else stop('observations.csv IS NOT available in the dataset...!')
  #----
  .w <- grepl('deployments.csv',file,ignore.case = TRUE)
  if (any(.w)) .d$deployments <- .eval("as.data.frame(data.table::fread(file[.w],tz=tz))",environment())
  else stop('deployments.csv IS NOT available in the dataset...!')
  #----
  .w <- grepl('media.csv',file,ignore.case = TRUE)
  if (any(.w)) .d$media <- .eval("as.data.frame(data.table::fread(file[.w],tz=tz))",environment())
  else stop('media.csv IS NOT available in the dataset...!')
  #----
  .w <- grepl('datapackage.json',file,ignore.case = TRUE)
  if (any(.w)) .js <- .eval("jsonlite::read_json(file[.w])",env = environment())
  else stop('datapackage.json IS NOT available in the dataset...!')
  #--------------------------
  ##############################
  
  .d$locations <- unique(.d$deployments[,c("locationID","locationName","longitude","latitude")])
  .d$deployments <- .d$deployments[,-which(colnames(.d$deployments) %in% c("locationName","longitude","latitude"))]
  #-----
  .d$deployments$Year <- .getYear(.d$deployments$deploymentStart)
  #----
  .d$deployments <- .eval(".d$deployments |> 
  dplyr::mutate(deployment_interval = lubridate::interval(deploymentStart, deploymentEnd),
         deployment_interval = lubridate::int_standardize(deployment_interval)) |> 
  dplyr::relocate(deployment_interval, .before = deploymentStart)",environment())
  #--------------
  .media.obs <- .d$observations[.d$observations$observationLevel == 'media',]
  
  obs_first_radius_angle <- .eval('.media.obs |>
  dplyr::filter(!is.na(.data$individualPositionRadius),
                !is.na(.data$individualPositionAngle)) |>
  dplyr::group_by(.data$eventID, .data$individualID) |>
  dplyr::slice_min(.data$eventStart, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(c("eventID",
                  "individualID",
                  "individualPositionRadius", 
                  "individualPositionAngle")) |>
  dplyr::rename_with(~ paste0("media_", .x),
                     dplyr::starts_with("individualPosition"))',environment())
  
  # Get event-based observations
  .obs <- .d$observations[.d$observations$observationLevel == 'event',]
  
  
  # Add angle/radius to event based observations if missing
  .obs <- .eval('.obs |>
    dplyr::left_join(obs_first_radius_angle,by = c("eventID", "individualID")) |>
    dplyr::mutate(
      individualPositionAngle = dplyr::if_else(
        condition = is.na(.data$individualPositionAngle),
        true = .data$media_individualPositionAngle,
        false = .data$individualPositionAngle),
      individualPositionRadius = dplyr::if_else(
        condition = is.na(.data$individualPositionRadius),
        true = .data$media_individualPositionRadius,
        false = .data$individualPositionRadius)) |>
    dplyr::select(-c("media_individualPositionAngle",
                     "media_individualPositionRadius"))',environment())
  
  
  
  # only events!
  .d$observations <- .obs
  
  rm(.obs,obs_first_radius_angle)
  
  if (!.is.POSIXct(.d$observations$classificationTimestam)) {
    .d$observations$classificationTimestamp[.d$observations$classificationTimestamp == ""] <- NA_character_
    #.d$observations <- .left_join(.d$observations,.d$media[,c("mediaID","timestamp")],by = "mediaID")
    .w <- which(!is.na(.d$observations$classificationTimestamp))
    if (length(.w) > 0) {
      .w <- .w[1:min(3,length(.w))]
      .f <- .getFormat(.d$observations$classificationTimestamp[.w])
      .d$observations$observation_timestamp <- as.POSIXct(.d$observations$classificationTimestamp,tz=tz,format = .f)
    } else .d$observations$observation_timestamp <- NA
  }
  
  
  .d$observations <- .d$observations[,-which(colnames(.d$observations) == "classificationTimestamp")]
  
  if (!.is.POSIXct(.d$observations$eventStart)) {
    .w <- which(!is.na(.d$observations$eventStart))
    if (length(.w) > 0) {
      .w <- .w[1:min(3,length(.w))]
      .f <- .getFormat(.d$observations$eventStart[.w])
      .d$observations$timestamp <- as.POSIXct(.d$observations$eventStart,tz=tz,format = .f)
    } else .d$observations$timestamp <- NA
  }
  #------
  if ("cameraSetupType" %in% names(.d$observations)) {
    colnames(.d$observations)[which(colnames(.d$observations) == "cameraSetupType")] <- "cameraSetup"
  } else {
    .d$observations$cameraSetup <- NA
  }
  #---
  if ("individualSpeed" %in% names(.d$observations)) {
    colnames(.d$observations)[which(colnames(.d$observations) == "individualSpeed")] <- "speed"
  }
  
  if ("individualPositionRadius" %in% names(.d$observations)) {
    colnames(.d$observations)[which(colnames(.d$observations) == "individualPositionRadius")] <- "radius"
  }
  
  if ("individualPositionAngle" %in% names(.d$observations)) {
    colnames(.d$observations)[which(colnames(.d$observations) == "individualPositionAngle")] <- "angle"
  }
  
  # remove bounding box related cols if present
  .w <- which(grepl('^bbox',colnames(.d$observations)))
  if (length(.w) > 0) .d$observations <- .d$observations[,-.w]
  
  # add taxonID if missing
  if(!"taxonID" %in% colnames(.d$observations)){
    .d$observations$taxonID <- NA_character_
  }
  # add taxonIDReference if missing
  if(!"taxonIDReference" %in% colnames(.d$observations)){
    .d$observations$taxonIDReference <- NA_character_
  }
  #-----
  colnames(.d$observations)[which(colnames(.d$observations) == "classificationProbability")] <- "classificationConfidence"
  
  
  .d$observations$mediaID <- ifelse(.d$observations$mediaID == "", NA,.d$observations$mediaID)
  .event_obs <-.d$observations[is.na(.d$observations$mediaID) & !is.na(.d$observations$eventID),c("eventID", "deploymentID", "eventStart", "eventEnd")]
  
  if ("eventID" %in% names(.d$observations)) {
    colnames(.d$observations)[which(colnames(.d$observations) == "eventID")] <- "sequenceID"
  } else {
    .d$observations$sequenceID <- NA
  }
  #-------
  colnames(.d$observations)[which(colnames(.d$observations) == "eventStart")] <- "timestamp"
  
  # Join on deploymentID and timestamp between eventStart and eventEnd
  by <- .eval('dplyr::join_by("deploymentID", dplyr::between(x$timestamp, y$eventStart, y$eventEnd))',env = environment())
  #.a <- merge(.d$media,.event_obs,by.x='timestamp',by.y=c('eventStart'))
  
  # Join media with event-based observations (obs without mediaID)
  .media <- .eval('.d$media |>
  dplyr::full_join(.event_obs, by) |>
  dplyr::rename(sequenceID = "eventID") |>
  dplyr::select(-c("eventStart", "eventEnd")) |>
  dplyr::relocate("sequenceID", .after = "deploymentID")',env = environment())
  
  
  if ("filePublic" %in% names(.media))  {
    .media$filePublic <- NULL
  }
  
  if ("favorite" %in% names(.media)) {
    colnames(.media)[colnames(.media) == "favorite"] <- 'favourite'
  }
  
  if ("mediaComments" %in% names(.media)) {
    colnames(.media)[colnames(.media) == "mediaComments"] <- 'comments'
  }
  
  if (!"_id" %in% names(.media)) {
    .media$`_id` <- NA
  }
  
  
  .media <- .eval('.media |> 
  dplyr::mutate(
    captureMethod = factor(
      ifelse(.data$captureMethod == "activityDetection",
             "motionDetection",
             as.character(.data$captureMethod))
    )
  )',environment())
  
  
  if (!.is.POSIXct(.media$timestamp)) {
    .w <- which(!is.na(.media$timestamp))
    if (length(.w) > 0) {
      .w <- .w[1:min(30,length(.w))]
      .f <- .getFormat(.media$timestamp[.w])
      .media$timestamp <- as.POSIXct(.media$timestamp,tz=tz,format = .f)
    } else .media$timestamp <- NA
  }
  
  .d$media <- .media
  rm(.media)
  #--------------
  
  .d$sequences <- .getSequences(.d$media)
  
  
  .d$taxonomy <- .get_Taxonomic_DF(.js$taxonomic)
  #-------
  .d$taxonomy$order[.d$taxonomy$order == ''] <- NA
  #--------
  
  if (.require('taxize')) {
    .w <- .getMissingTaxon_GBIF(.d$taxonomy$scientificName[!is.na(.d$taxonomy$scientificName)])
    
    #-----
    # assign the retrieved info to the main CameraTrap database:
    for (i in 1:nrow(.w)) {
      w <- which(.d$taxonomy$scientificName == .w$scientificName[i])
      .d$taxonomy[w,'class'] <- .w$class[i]
      if (is.na(.d$taxonomy[w,'order'])) .d$taxonomy[w,'order'] <- .w$order[i]
    }
    
    rm(.w,w)
  }
  #------
  .d$observations <- .d$observations[,-which(colnames(.d$observations) == 'taxonID')]
  
  .d$observations$taxonID <- left_join(.d$observations,.d$taxonomy,by='scientificName')$taxonID
  
  list(data=.d,json=.js,directory=.path)
  
}

#---------


if (!isGeneric("camData")) {
  setGeneric("camData", function(data,habitat,study_area,...)
    standardGeneric("camData"))
}


methods::setGeneric(
  "camData",
  function(data, habitat, study_area = NULL, ...) {
    methods::standardGeneric("camData")
  }
)

methods::setMethod(
  "camData",
  signature(data = "character"),
  function(data, habitat, study_area = NULL, ...) {
    
    if (missing(habitat) || !is.data.frame(habitat)) habitat <- NULL
    if (missing(study_area)) study_area <- NULL
    
    .d <- .read_camdp(data)
    cm <- camR$new()
    cm$setting$locationLegend <- TRUE
    
    cm$data <- .d$data
    cm$info$json <- .d$json
    cm$info$directory <- .d$directory
    
    if (!is.null(habitat)) cm$habitat <- habitat
    
    if (!is.null(study_area)) {
      if (is.character(study_area)) {
        if (file.exists(study_area)) {
          .v <- try(terra::vect(study_area), silent = TRUE)
          if (!inherits(.v, "try-error")) {
            saveRDS(.v, paste0(cm$info$directory, "/study_area.map"))
            cm$study_area$path <- paste0(cm$info$directory, "/study_area.map")
            cm$study_area$object <- .v
            rm(.v)
          } else warning("the specified study_area file could not be read (is it a spatial dataset?)")
        } else {
          warning("study_area filename is not available and ignored...!")
        }
      } else if (inherits(study_area, "SpatVector")) {
        cm$study_area$object <- study_area
        saveRDS(study_area, paste0(cm$info$directory, "/study_area.map"))
        cm$study_area$path <- paste0(cm$info$directory, "/study_area.map")
      } else if (.eval("inherits(study_area,'sf')", env = environment())) {
        cm$study_area$object <- terra::vect(study_area)
        saveRDS(cm$study_area$object, paste0(cm$info$directory, "/study_area.map"))
        cm$study_area$path <- paste0(cm$info$directory, "/study_area.map")
      } else {
        warning("study_area is ignored (should be a filename or a spatial dataset)...!")
      }
    }
    
    cm$filterExclude <- list(
      scientificName = c(
        "Homo sapiens", "Canis lupus familiaris", "Felis catus",
        "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
        "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
        "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus"
      )
    )
    cm$filterKeep <- list(observationType = c("animal"), class = NULL)
    
    cm$add_group("large_mammals", list(order = c("Artiodactyla", "Carnivora")))
    cm$filterCount <- 25
    
    cm$add_group("domestic", list(
      scientificName = c(
        "Homo sapiens", "Canis lupus familiaris", "Felis catus",
        "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
        "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
        "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus"
      )
    ))
    
    cm$setting$focus_groups <- "large_mammals"
    
    if (!is.null(.d$json$project$title) && .d$json$project$title != "") {
      cm$siteName <- .d$json$project$title
    } else {
      cm$siteName <- "Unnamed Site"
    }
    
    .summarize_spatial(cm)
    .Temporal(cm)
    .Essentials(cm)
    .Annotation(cm)
    .Validation(cm)
    .Species(cm)
    .Visuals_capture_method(cm)
    
    country <- cm$data_status$country
    fg <- .firstUpper(.paste_comma_and(cm$setting$focus_groups))
    site_Name <- cm$siteName
    cm$title <- glue("Camera-Trap Report: {fg} at {site_Name}, {country}")
    cm$subtitle <- "Ecological report based on camera-trap monitoring"
    
    rm(.d); gc()
    
    cm$setup()
    .module_dir <- .section_dir("camtrapReport")
    
    mods <- .read_modules(
      level0 = c("introduction", "methods", "results", "acknowledgements", "appendix"),
      package = "camtrapReport",
      dir = .module_dir,
      write_info = TRUE
    )
    
    for (i in seq_along(mods)) {
      cm$addReportObject(mods[[i]])
    }
    
    cm
  }
)