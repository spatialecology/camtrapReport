# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  Dec. 2025
# Version 2.0
# Licence GPL v3
#--------

#-----------


# get the sequences data.frame from media (the code copied from the ctdp package):
.getSequences <- function(media) {
  if (!.require('data.table')) stop('The data.table package is not installed...!')
  
  sequences <- .eval('media %>% 
    dplyr::distinct() %>% 
    dplyr::select(deploymentID, sequenceID, timestamp, captureMethod) %>% 
    data.table::data.table(key = "sequenceID")',environment())
  
  # summarize per key
  sequences <- sequences[, list(deploymentID = unique(deploymentID),
                                captureMethod = unique(captureMethod),
                                start = min(timestamp),
                                end = max(timestamp),
                                nrphotos = length(timestamp)),
                         by = sequenceID]
  
  
  # convert to tibble, arrange, and convert start/end to interval object
  sequences <- .eval("sequences %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(deploymentID, sequenceID) %>% 
    dplyr::mutate(sequence_interval = lubridate::interval(start, end)) %>% 
    dplyr::relocate(sequence_interval, .before =  start) %>% 
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
  
  if (.isZip(file)) {
    if (!is.null(path) && is.character(path)) {
      .path <- paste0(path.expand(path),'/',gsub(basename(file), pattern = ".zip", replacement = "",ignore.case = TRUE))
    } else {
      .path <- gsub(basename(file), pattern = ".zip", replacement = "",ignore.case = TRUE)
    }
    #----
    
    file <- unzip(file,exdir = .path)
    
    #.dep = read.csv(unz(file,filename = 'deployments.csv')) # read specific file in zip
    
  } else if (dir.exists(file)) {
    if (all(c("datapackage.json","deployments.csv","observations.csv") %in% tolower(dir(file)))) {
      file <- dir(file,full.names = TRUE)
      
    } else {
      if (any(c("datapackage.json","deployments.csv","observations.csv") %in% tolower(dir(file)))) {
        .w <- !c("datapackage.json","deployments.csv","observations.csv","media.csv") %in% tolower(dir(file))
        stop(paste0('The standard data files (',paste(c("datapackage.json","deployments.csv","observations.csv","media.csv")[.w],collapse = ', '),') are not available in the specified folder!'))
      } else  stop('The specified folder does not have the standard data files (e.g., "datapackage.json","deployments.csv","observations.csv")...!')
    }
  } else stop('The specified input is not a zip file or a directory...!')
  #-----------------------
  .d <- list()
  
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
  .d$deployments <- .eval(".d$deployments %>% 
  dplyr::mutate(deployment_interval = lubridate::interval(deploymentStart, deploymentEnd),
         deployment_interval = lubridate::int_standardize(deployment_interval)) %>% 
  dplyr::relocate(deployment_interval, .before = deploymentStart)",environment())
  #--------------
  .media.obs <- .d$observations[.d$observations$observationLevel == 'media',]
  
  obs_first_radius_angle <- .eval('.media.obs %>%
  dplyr::filter(!is.na(.data$individualPositionRadius),
                !is.na(.data$individualPositionAngle)) %>%
  dplyr::group_by(.data$eventID, .data$individualID) %>%
  dplyr::slice_min(.data$eventStart, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(c("eventID",
                  "individualID",
                  "individualPositionRadius", 
                  "individualPositionAngle")) %>%
  dplyr::rename_with(~ paste0("media_", .x),
                     dplyr::starts_with("individualPosition"))',environment())
  
  # Get event-based observations
  .obs <- .d$observations[.d$observations$observationLevel == 'event',]
  
  
  # Add angle/radius to event based observations if missing
  .obs <- .eval('.obs %>%
    dplyr::left_join(obs_first_radius_angle,by = c("eventID", "individualID")) %>%
    dplyr::mutate(
      individualPositionAngle = dplyr::if_else(
        condition = is.na(.data$individualPositionAngle),
        true = .data$media_individualPositionAngle,
        false = .data$individualPositionAngle),
      individualPositionRadius = dplyr::if_else(
        condition = is.na(.data$individualPositionRadius),
        true = .data$media_individualPositionRadius,
        false = .data$individualPositionRadius)) %>%
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
  .media <- .eval('.d$media %>%
  dplyr::full_join(.event_obs, by) %>%
  dplyr::rename(sequenceID = "eventID") %>%
  dplyr::select(-c("eventStart", "eventEnd")) %>%
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
  
  
  .media <- .eval('.media %>% 
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
  
  list(data=.d,json=.js)
  
}

# read the content of camtrap data (as zip file OR as a folder):
# default is to create a folder in the working directory, unless the new path is specified (path)
# if ctdp -> TRUE, a list with both camtrapDP package and ctdp object will be returned!
# ... (additional arguments): tz = "Etc/GMT-2", verbose = TRUE, rmEmpty = FALSE, dropMedia = FALSE

.read_camtrapDATA <- function (file,path=NULL,ctdp=FALSE,...) {
  if (.isZip(file)) {
    if (!is.null(path) && is.character(path)) {
      #if (!dir.exists(path.expand(path))) dir.create(path.expand(path))
      .path <- paste0(path.expand(path),'/',gsub(basename(file), pattern = ".zip", replacement = "",ignore.case = TRUE))
    } else {
      .path <- gsub(basename(file), pattern = ".zip", replacement = "",ignore.case = TRUE)
    }
    #----
    unzip(file,exdir = .path)
    #----
    if (.require('camtraptor')) {
      pkg <- .eval('camtraptor::read_camtrap_dp(file = file.path(.path, "datapackage.json"), media = TRUE)',env = environment())
    } else stop('The package camtraptor is needed but not installed...!')
    
  } else if (dir.exists(file)) {
    if ("datapackage.json" %in% tolower(dir(file))) {
      file <- dir(file,full.names = TRUE)
      file <- file[grepl('.json',file,ignore.case = TRUE)]
      if (.require('camtraptor')) {
        pkg <- .eval('camtraptor::read_camtrap_dp(file = file, media = TRUE)',env = environment())
      } else stop('The package camtraptor is needed but not installed...!')
    } else stop('The datapackage.json is not available in the input directory...!')
    
  } else if (.isJson(file)) {
    if (.require('camtraptor')) {
      pkg <- .eval('camtraptor::read_camtrap_dp(file = file, media = TRUE)',env = environment())
      
    } else stop('The package camtraptor is needed but not installed...!')
  } else stop('The input file not recognized (should be a zip or json file, or a directory with camtrapDP content...)!')
  
  if (ctdp) {
    if (.require('ctdp')) {
      x <- .eval("as_ctdp(pkg, verbose = FALSE,...)",env = environment())
      return(list(pkg=pkg,ctdp=x))
    } else {
      warning('The ctdp package is not installed...!')
      
      pkg
    }
  } else {
    pkg
  }
}

#------

.addReportSections <- function(cm) {
  .p <- c("ctdp", "camtrapDensity","camtrapdp", "camtraptor", "leaflet", "tidyverse", "spatstat","ggplot2", "corrplot", "suncalc",
         "dplyr","gt", "camtrapR", "spOccupancy", "leaflet.extras",'frictionless','htmltools')
  .loadPKG(.p)
  # Introduction:
  .txx <- .getTextObj(name='introduction',title='Introduction',
                      txt=list(p1="Mammals play vital roles in terrestrial ecosystems. Ungulates and carnivores in particular influence vegetation through herbivory and predation, contribute to nutrient cycling, energy flows, and seed dispersal, and act as ecosystem engineers through trampling, digging, and rooting (*Augustine & McNaughton 1998; Lacher et al., 2019*). Mammals are also central to public nature experiences and conservation goals. However, they often come into conflict with humans, causing crop damage, traffic collisions, and disease spread, prompting population control measures (*Torres et al., 2018*). Understanding their distribution, abundance and behavior is therefore crucial for protected area management.",
                               p2 = "Camera trapping has become an affordable, reliable, and non-invasive method for monitoring mammal populations in protected areas (*Kays et al., 2009*). It provides insights into habitat use and activity patterns by capturing images or videos when warm-bodied animals trigger infrared sensors, day and night, with minimal disturbance. Scientists use these data to analyze species distribution, activity, and abundance, supporting population monitoring and ecological research.",
                               p3 = "The [European Observatory of Wildlife (EOW)](https://wildlifeobservatory.org) is a standardized camera-trapping network across [73 European protected areas](https://eow.wildlifeobservatory.org/), organized by the [ENETWILD consortium](https://enetwild.com/) and funded by the[European Food Safety Authority (EFSA)](https://www.efsa.europa.eu/en). Images are processed and archived on [Agouti](https://www.agouti.eu), with annual reports submitted to EFSA.",
                               p4 = "A unique feature of the EOW protocol is its ability to estimate population density using the [Random Encounter Model (REM)] (https://github.com/MarcusRowcliffe/camtrapDensity) (*Rowcliffe et al., 2008*). The REM is based on the assumption that animals move randomly in relation to camera placements, and that the total number of detections across locations is proportional to the population density. In other words, more captures indicate a larger number of individuals in the population. This model allows for the estimation of animal population density without the need for individual identification by correcting capture rates for species-specific differences in detectability, which may additionally vary by habitat. Detectability itself depends on two factors: (1) the effective size of the wedge-shaped area over which the camera's sensor detects passing animals, and (2) the average daily distance travelled by the species. The average daily distance travelled is in turn a product of two factors: the daily activity level, which refers to the average number of hours per day that the species spends moving, and the movement speed. The daily activity level can be calculated from the frequency distribution of captures across the 24-hour day (*Rowcliffe et al., 2014*). The detection area and movement speed can be estimated from movement paths of animals captured by the cameras (*Rowcliffe et al., 2011*).",
                               p5 = "Here, we report on camera-trap surveys in {siteName} as part of EOW. The goal was to estimate and monitor ungulate and carnivore populations over time and across sites, providing data on species distribution and activity patterns."
                      ))
  #---------------------
  # OR Alternatively, if the paragraphs are read from a text file, you can use:
  
  #.tmpText <- readLines(introduction_file, warn = FALSE, encoding = "UTF-8")
  #.tmpText <- as.list(.tmpText[.tmpText != ""])
  #.txx <- .getTextObj(name='introduction',title='Introduction',txt=.tmpText)
  #-------
  # adding the introduction text to the report object:
  cm$addReportObject(.txx) 
  
  #---------
  # Methods:
  
  # Let's create the other text sections one by one (all through the temporary .txx object):
  
  .txx <- .getTextObj(name='methods',title='Methods')
  cm$addReportObject(.txx) 
  #----
  # Methods -> Study Area:
  .txx <- .getTextObj(name='study_area',title="Study Area",parent='methods',txt='{description}')
  cm$addReportObject(.txx) 
  #----
  # Methods -> Sampling:
  .txx <- .getTextObj(name='sampling',title="Sampling",parent='methods',txt='{sampling}')
  cm$addReportObject(.txx) 
  #----
  
  
  .rxx <- .getRchunk(parent='sampling',name = 'sampling_table',setting={c(echo=F,results="asis",message=FALSE, warning=FALSE)},code = {
    object$camera_setup %>%
      gt() %>%
      tab_header(
        title = md("**📷 Camera Deployment Summary**"),
        subtitle = md("**Table 1.** Details of camera deployments per year")
      ) %>%
      cols_label(
        year = "Year",
        number_camtraps = "Camera Traps",
        deployment_period = "Deployment Period",
        setup_period = "Setup Period",
        pickup_period = "Pickup Period"
      ) %>%
      tab_options(
        table.font.size = px(14),
        heading.title.font.size = px(16),
        heading.subtitle.font.size = px(12),
        row.striping.include_table_body = TRUE
      ) %>%
      opt_row_striping() %>%
      tab_style(
        style = list(
          cell_borders(sides = "bottom", color = "gray", weight = px(1))
        ),
        locations = cells_body()
      ) %>%
      tab_style(
        style = list(
          cell_text(weight = "bold")
        ),
        locations = cells_column_labels()
      )
  })
  
  cm$addReportObject(.rxx) # adding R chunk!
  
  #-------
  # # Methods -> Locations:
  
  
  # # Define Function to Plot Camera Locations
  # plot_locations <- function(data, color_palette, add_legend = FALSE) {
  #   if (is.null(data) || nrow(data) == 0) return(NULL)
  #   
  #   data <- data %>%
  #     mutate(
  #       popup_text = paste0(
  #         "<b>Location:</b> ", ifelse(!is.na(locationName), locationName, "N/A"), "<br>",
  #         "<b>Habitat Type:</b> ", ifelse(!is.na(Habitat_Type), Habitat_Type, "N/A"), "<br>",
  #         "<b>Bait Use:</b> ", ifelse(!is.na(BaitUse_List) & BaitUse_List != "", BaitUse_List, "No Data"), "<br>",
  #         "<b>Camera Height:</b> ", ifelse(!is.na(cameraHeight) & cameraHeight != "", cameraHeight, "No Data"), "<br>",
  #         "<b>Species Observed:</b> ", ifelse(!is.na(Species_List) & Species_List != "", Species_List, "No Data"), "<br>",
  #         "<b>Capture Method:</b> ", ifelse(!is.na(CaptureMethod_List) & CaptureMethod_List != "", CaptureMethod_List, "No Data"), "<br>",
  #         "<b>Number of Photos:</b> ", ifelse(!is.na(Total_Photos), Total_Photos, "No Data"), "<br>",
  #         "<b>Classified By:</b> ", ifelse(!is.na(Classify_By_List) & Classify_By_List != "", Classify_By_List, "No Data"), "<br>",
  #         "<b>Setup By:</b> ", ifelse(!is.na(Setup_By_List) & Setup_By_List != "", Setup_By_List, "No Data")
  #       )
  #     )
  #   
  #   map <- leaflet(data) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       data = study_areaSHP,
  #       fillColor = "transparent",
  #       fillOpacity = 0.3,
  #       color = "black",
  #       weight = 2
  #     ) %>%
  #     addCircleMarkers(
  #       lng = ~longitude, lat = ~latitude,
  #       radius = 6,
  #       color = ~color_palette(Habitat_Type),
  #       fillOpacity = 0.85, stroke = TRUE, weight = 1,
  #       popup = ~popup_text
  #     ) %>%
  #     fitBounds(
  #       lng1 = min(data$longitude, na.rm = TRUE),
  #       lat1 = min(data$latitude, na.rm = TRUE),
  #       lng2 = max(data$longitude, na.rm = TRUE),
  #       lat2 = max(data$latitude, na.rm = TRUE)
  #     )
  #   
  #   if (add_legend) {
  #     map <- map %>%
  #       leaflet::addLegend(
  #         position = "bottomright",
  #         pal = color_palette,
  #         values = data$Habitat_Type,
  #         title = "Habitat Type",
  #         opacity = 1)
  #   }
  #   
  #   return(map)
  # }
  
  
  .txx <- .getTextObj(name='location',title="Camera Locations {.tabset}",parent='methods',
                      txt='The maps below display the locations of camera traps deployed during different years. Use the tabs above to explore data for each sampling year. The last tab shows the study area with all camera locations in this site. Locations are color-coded by habitat type. Click on the points for additional deployment information.')
  
  cm$addReportObject(.txx)
  
  #---------------
  .rxx <- .getRchunk(parent='location',name = 'camera_locations_leaflet',setting={c(echo=FALSE,results="asis",fig.show="hold")},code = {
    
    
    
    # Function to plot camera locations with popups
    plot_locations <- function(data, color_palette,add_legend=object$setting$locationLegend,.all_habitat) {  
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)  # Return NULL if data is missing or empty
      }
      
      
      # Create color mapping
      habitat_colors <- colorFactor(
        palette = color_palette,
        domain = .all_habitat,
        na.color = "#EDAC8C"
      )
      
      
      data <- data %>%
        mutate(
          popup_text = paste0(
            "<b>Location:</b> ", ifelse(!is.na(locationName), locationName, "N/A"), "<br>",
            "<b>Habitat Type:</b> ", ifelse(!is.na(Habitat_Type), Habitat_Type, "N/A"), "<br>",
            "<b>Bait Use:</b> ", ifelse(!is.na(BaitUse_List) & BaitUse_List != "", BaitUse_List, "No Data"), "<br>",
            "<b>Camera Height:</b> ", ifelse(!is.na(cameraHeight) & cameraHeight != "", cameraHeight, "No Data"), "<br>",
            "<b>Species Observed:</b> ", ifelse(!is.na(Species_List) & Species_List != "", Species_List, "No Data"), "<br>",
            "<b>Capture Method:</b> ", ifelse(!is.na(CaptureMethod_List) & CaptureMethod_List != "", CaptureMethod_List, "No Data"), "<br>",
            "<b>Number of Photos:</b> ", ifelse(!is.na(Total_Photos), Total_Photos, "No Data"), "<br>",
            "<b>Classified By:</b> ", ifelse(!is.na(Classify_By_List) & Classify_By_List != "", Classify_By_List, "No Data"), "<br>",
            "<b>Setup By:</b> ", ifelse(!is.na(Setup_By_List) & Setup_By_List != "", Setup_By_List, "No Data")
          )
        )
      
      map <- leaflet(data) %>%
        addTiles()
      
      if (nrow(object$study_area@attributes) > 0) {
        .tmpStudyArea <- project(unwrap(object$study_area),"epsg:4326")
        
        map <- map %>%
          addPolygons(
            data = .tmpStudyArea,
            fillColor = "transparent",
            fillOpacity = 0.3,
            color = "black",
            weight = 2
          )
      }
      #---
      
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = 6,
          color = ~habitat_colors(Habitat_Type),
          fillOpacity = 0.85, stroke = TRUE, weight = 1,
          popup = ~popup_text
        ) %>%
        fitBounds(
          lng1 = min(data$longitude, na.rm = TRUE),
          lat1 = min(data$latitude, na.rm = TRUE),
          lng2 = max(data$longitude, na.rm = TRUE),
          lat2 = max(data$latitude, na.rm = TRUE)
        )
      
      if (add_legend) {
        map <- map %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = habitat_colors,
            values = data$Habitat_Type,
            title = "Habitat Type",
            opacity = 1)
      }
      map
    }
    
    #----
    # **Automatically Create Tabs for Each Year**
    for (doyear in object$years) {
      cat(paste0("### ", doyear, " {.unnumbered}\n\n"))
      
      .all_habitat <- unique(object$data_merged$Habitat_Type)
      .all_habitat <- .all_habitat[!is.na(.all_habitat)]
      
      
      
      # Extract the correct dataset for the year
      
      .dat_year <- object$data_merged[object$data_merged$Year ==  doyear ,c('locationName','cameraHeight','BaitUse_List','Species_List','CaptureMethod_List','Total_Photos','Classify_By_List','Setup_By_List','longitude','latitude','Year','Habitat_Type')]
      .dat_year <- .dat_year[!is.na(.dat_year$longitude) & !is.na(.dat_year$longitude),]
      
      # nsure the data is valid before proceeding
      if (nrow(.dat_year) > 0) {
        p <- plot_locations(.dat_year, color = colorRampPalette(object$setting$color)(length(.all_habitat)),.all_habitat=.all_habitat)
        print(htmltools::tagList(p))
      } else {
        cat(paste0("⚠️ No location data available for ", doyear, ".\n"))
      }
      cat('\n\n')
    }
    
  })
  #-------
  cm$addReportObject(.rxx) # adding R chunk!
  
  
  .rxx <- .getRchunk(parent='location',name = 'camera_research_area',setting={c(echo=FALSE,results="asis",message=FALSE, warning=FALSE)},code={
    
    cat("\n### Total {.unnumbered} \n")
    
    # 📌 **Define File Paths**
    Data_ResearchArea <- object$data_merged
    
    .dat_year <- object$data_merged[,c('locationName','cameraHeight','longitude','latitude',
                                       'Year','Year_List','Habitat_Type','BaitUse_List','Species_List',
                                       'CaptureMethod_List','Total_Photos','Classify_By_List','Setup_By_List')]
    
    # step 3: Define Habitat-Based Color Palette
    .dat_year$Habitat_Type <- factor(.dat_year$Habitat_Type)
    unique_habitats <- unique(.dat_year$Habitat_Type)
    
    habitat_colors <- colorFactor(
      palette = object$setting$color,
      domain = unique_habitats, na.color = "#EDAC8C"
    )
    
    .dat_year <- .dat_year %>%
      mutate(
        Habitat_Type = as.character(Habitat_Type),  # Convert Habitat Type to character
        label = paste0(
          "<b>Location:</b> ", ifelse(!is.na(locationName), locationName, "N/A"), "<br>",
          "<b>List of Years:</b> ", ifelse(!is.na(Year_List), Year_List, "N/A"), "<br>",
          "<b>Habitat Type:</b> ", ifelse(!is.na(Habitat_Type), Habitat_Type, "N/A"), "<br>",
          "<b>Bait Use:</b> ", ifelse(!is.na(BaitUse_List) & BaitUse_List != "", BaitUse_List, "No Data"), "<br>",
          "<b>Species Observed:</b> ", ifelse(!is.na(Species_List) & Species_List != "", Species_List, "No Data"), "<br>",
          "<b>Capture Method:</b> ", ifelse(!is.na(CaptureMethod_List) & CaptureMethod_List != "", CaptureMethod_List, "No Data"), "<br>",
          "<b>Number of Photos:</b> ", ifelse(!is.na(Total_Photos), Total_Photos, "No Data"), "<br>",
          "<b>Classified By:</b> ", ifelse(!is.na(Classify_By_List) & Classify_By_List != "", Classify_By_List, "No Data"), "<br>",
          "<b>Setup By:</b> ", ifelse(!is.na(Setup_By_List) & Setup_By_List != "", Setup_By_List, "No Data")
        )
      )
    
    
    map <- leaflet(.dat_year, width = "100%", height = "400px") %>%
      addTiles()
    
    if (nrow(object$study_area@attributes) > 0) {
      .tmpStudyArea <- project(unwrap(object$study_area),"epsg:4326")
      
      map <- map %>%
        addPolygons(
          data = .tmpStudyArea,
          fillColor = "transparent",
          fillOpacity = 0.3,
          color = "black",
          weight = 2
        )
    }
    
    # ✅ **Step 5: Generate and Display Interactive Leaflet Map**
    map <- map %>%
      
      # 🔹 **Add Camera Locations with Color Mapping Based on Habitat**
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 6,
        color = ~habitat_colors(Habitat_Type),  # ✅ Apply Color Mapping
        fillOpacity = 0.85, stroke = TRUE, weight = 1,
        popup = ~label
      ) %>%
      
      # ✅ **Fix: Set Consistent Zoom Level**
      fitBounds(
        lng1 = min(.dat_year$longitude, na.rm = TRUE),
        lat1 = min(.dat_year$latitude, na.rm = TRUE),
        lng2 = max(.dat_year$longitude, na.rm = TRUE),
        lat2 = max(.dat_year$latitude, na.rm = TRUE)
      ) 
    if (object$setting$locationLegend) {
      map <- map %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = habitat_colors,
          values = .dat_year$Habitat_Type,
          title = "Habitat Type",
          opacity = 1
        )
    }
    map
    
    cat('\n\n')
    
    cat('##  {.unnumbered}\n')
    
    cat("**Fig. 1**: *Interactive map of camera trap locations. Click on a marker to view the corresponding location details and additional metadata.*\n")
    
  })
  
  cm$addReportObject(.rxx) # adding R chunk!
  
  #--------
  
  .txx <- .getTextObj(name='effort',title="Sampling Efforts",parent='methods',txt="Sampling effort refers to the total number of camera trap operational days, accounting for both the number of active cameras and their duration of deployment. It provides critical context for interpreting species detection rates and comparing data across years.")
  cm$addReportObject(.txx) 
  #----
  
  .rxx <- .getRchunk(parent='effort',name = 'sampling_effort',setting={c(echo=FALSE,results="asis",message=FALSE, warning=FALSE)},code={
    
    
    .camera_summary_text <- lapply(seq_len(nrow(object$camera_stats)), function(i) {
      year <- object$camera_stats$year[i]
      failed_cameras <- object$camera_stats$failed_cameras[i]
      average_runtime <- object$camera_stats$average_runtime[i]
      runtime_range <- object$camera_stats$runtime_range[i]
      total_runtime_days <- object$camera_stats$total_runtime_days[i]
      total_runtime_years <- object$camera_stats$total_runtime_years[i]
      
      if (failed_cameras > 0) {
        if (failed_cameras > 1) {
          paste0(
            "In ", year, ", ", failed_cameras, " cameras broke down during the study period. ",
            "The cameras ran on average ", average_runtime, " days (range ", runtime_range, " days). ",
            "The total sampling effort was ", total_runtime_days, " days, which equals ", total_runtime_years, " years."
          )
        } else {
          paste0(
            "In ", year, ", ", failed_cameras, " camera broke down during the study period. ",
            "The cameras ran on average ", average_runtime, " days (range ", runtime_range, " days). ",
            "The total sampling effort was ", total_runtime_days, " days, which equals ", total_runtime_years, " years."
          )
        }
      } else {
        paste0(
          "In ", year, ", NO cameras broke down during the study period. ",
          "The cameras ran on average ", average_runtime, " days (range ", runtime_range, " days). ",
          "The total sampling effort was ", total_runtime_days, " days, which equals ", total_runtime_years, " years."
        )
      }
      
    })
    
    #  Combine Text with Proper Formatting
    .camera_summary_text <- paste(.camera_summary_text, collapse = " ")
    
    # Print Output in RMarkdown HTML
    cat(.camera_summary_text, "See Figure 2 for detailed information on sampling effort over time.")
    #---------------
    
    
    # Generate Dygraph
    plot_test <- .plot_effort(object$data) %>%
      dyAxis("x", label = "Time") %>%
      dyAxis("y", label = "Active Camera Count") %>%
      dyOptions(fillGraph = TRUE, colors = "#033800FF") %>%
      dyRangeSelector()
    
    # Check if the plot is valid
    if (!is.null(plot_test)) {
      
      # Save the plot as an HTML widget
      temp_file <- file.path(tempdir(), "effort_plot.html")
      htmlwidgets::saveWidget(plot_test, file = temp_file, selfcontained = TRUE)
      
      # Embed iframe inside a **plain gray box (no shadow)**
      tags$div(
        style = "border: 1px solid #ccc; 
             background-color: #f0f0f0; 
             padding: 15px; 
             border-radius: 5px;",
        tags$iframe(
          srcdoc = paste(readLines(temp_file), collapse = "\n"),
          width = "100%", height = "500px",
          style = "border: none;"
        )
      )
    } else {
      cat("\n Warning: No data available for plotting!")
    }
    #---
    cat('\n')
    
    cat('##  {.unnumbered}\n')
    
    cat("**Fig. 2**: *Number of active camera traps per survey year. Adjust the slider to focus on specific time periods.*\n")
    
  })
  
  cm$addReportObject(.rxx) # adding R chunk!
  
  
  
  #----------
  .txx <- .getTextObj(name='image_processing',title="Image Processing",parent='methods',
                      txt = "The images were processed and archived in [Agouti](https://www.agouti.eu), an online platform managed by [Wageningen University (WUR)](https://www.wur.nl/en/) and the [Flemish Research Institute for Nature and Forest (INBO)](https://www.vlaanderen.be/inbo/en-gb/homepage/). Images captured within 120 seconds of each other were automatically grouped into sequences, typically representing a single event. These sequences were partially annotated using Artificial Intelligence (AI), with additional manual annotations by {annonator}. A subset of these annotations was validated by experienced observers to ensure accuracy. For each sampling location, calibration images were manually annotated in [Agouti](https://www.agouti.eu) by marking points along a barred stick, enabling photogrammetry-based position estimation (see Appendix). Additionally, for each species of interest, movement paths were digitized in [Agouti](https://www.agouti.eu) from at least 25 randomly selected sequences by tracking the front foot of the first-appearing individual across images. These annotations were analyzed using the photogrammetry tools in the R package [camtrapDensity](https://github.com/MarcusRowcliffe/camtrapDensity) (*Rowcliffe, 2024*), which links pixel coordinates to ground positions in a local coordinate system, providing a depth perspective to the images.")
  cm$addReportObject(.txx) 
  
  
  cm$annonator <- "volunteers from [*WUR-WEC*](https://www.wur.nl/en/Research-Results/Chair-groups/Environmental-Sciences/Wildlife-Ecology-and-Conservation-Group.htm)" 
  
  #-----
  .txx <- .getTextObj(name='data_processing',title="Data Processing",parent='methods',
                      txt = "The data were exported from Agouti in [camtrapDP](https://camtrap-dp.tdwg.org/) format and processed in R using the packages [camtraptor](https://github.com/inbo/camtraptor) and [ctdp](https://wec.wur.nl/r/ctdp/), among others. Descriptive information was calculated, and densities were estimated using the Random Encounter Model with the R package [camtrapDensity](https://github.com/MarcusRowcliffe/camtrapDensity). The parameters for effective detection area, detection angle, and movement speeds per species were calculated from the tagged pixels, while the daily activity level parameter was derived from the timestamps of observations.")
  cm$addReportObject(.txx) 
  
  
  
  #----------
  
  .txx <- .getTextObj(name='results',title='Results')
  cm$addReportObject(.txx) 
  #----
  # Methods -> Study Area:
  .txx <- .getTextObj(name='captures',title="Captures",parent='results',txt='A yearly summary of the observation data, including the total number of photos, observations, image sequences, and animal detections is presented in *Table 2*.')
  cm$addReportObject(.txx) 
  
  cm$setting$focus_groups <- 'large_mammals'
  ###########
  .rxx <- .getRchunk(parent='captures',name = 'focus_text',setting={c(echo=FALSE,results="asis",warning=FALSE)},code={
    
    .total_human <- sum(object$species_summary$human_observation$per_year$total_observations)
    
    # Summary Metrics
    total_species <- nrow(object$species_summary$wild_species$site_list)
    total_domestic <- nrow(object$species_summary$domestic$site_list)
    
    years_vector <- sort(object$species_summary$wild_animals$per_year$observation_Year)
    
    # Format years as range if consecutive
    if (length(years_vector) > 1 && all(diff(years_vector) == 1)) {
      year_text <- paste0(min(years_vector), " to ", max(years_vector))
    } else {
      year_text <- .paste_comma_and(years_vector)
    }
    
    # Main summary sentence
    cat(paste0(
      "Across the period from ", year_text, ", the observations recorded a total of ",
      nrow(object$species_summary$wild_animals$site_list), " different wild species and ", nrow(object$species_summary$domestic$site_list), " domestic species."
    ))
    
    #----
    
    .w <- which(object$species_summary$count$Group %in% c('wild_mammals','birds','amphibians','reptiles'))
    if (length(.w) > 0) {
      cat(paste0("Among the wild species, ", .paste_comma_and(paste(object$species_summary$count$Count[.w],'were',object$species_summary$count$Name[.w],'species')), ".\n"))
    }
    
    # Human observation
    
    if (.total_human > 0) {
      cat("Over the same period, camera traps recorded human presence", .total_human , 
          "occasions, which likely reflecting detections of the site monitoring group during fieldwork activities such as camera installation and maintenance.\n\n"
      )
    }
    #---
    
    if (!is.null(object$setting$focus_groups)) {
      .w <- .paste_comma_and(object$species_summary$count$Name[object$species_summary$count$Group %in% object$setting$focus_groups])
      #---
      cat(paste0(
        "This report focuses on ", .w,
        " that observed at least ", object$filterCount, " times during the study period.\n\n"
      ))
    }
    
    cat("A detailed list of these species, including their number of observations, locations, and total photos, is presented in *Table 2*.\n")
    
    #########
    if (!is.null(object$setting$focus_groups)) {
      .w <- which(names(object$species_summary) %in% object$setting$focus_groups)
      .w <- unlist(lapply(.w,function (i) object$species_summary[[i]]$site_list$scientificName))
    } else {
      if (any(c('wild_mammals','large_mammals','birds','amphibians','reptiles') %in% object$species_summary$count$Group)) {
        .w <- which(names(object$species_summary) %in% c('wild_mammals','large_mammals','birds','amphibians','reptiles'))
        .w <- unlist(lapply(.w,function (i) object$species_summary[[i]]$site_list$scientificName))
      }
    }
    
    
    
    .tax_obs <-object$data$observations %>%
      select(-scientificName) %>%
      left_join(object$data$taxonomy, by = "taxonID") %>%
      mutate(
        observation_date = as.Date(observation_timestamp),
        observation_Year = .getYear(observation_timestamp)
      )
    
    
    # Join with sequence & location data
    enriched_obs <- .tax_obs %>%
      filter(scientificName %in% .w) %>%
      left_join(object$data$sequences %>% select(sequenceID, deploymentID, nrphotos), by = "sequenceID") %>%
      left_join(object$data$deployments %>% select(deploymentID, locationID), by = "deploymentID") %>%
      left_join(object$data$locations %>% select(locationID, locationName), by = "locationID")
    # Summarize
    group_summary <- enriched_obs %>%
      group_by(scientificName, vernacularNames.eng, family) %>%
      summarise(
        TotalCaptures = n(),
        UniqueLocations = n_distinct(locationName),
        TotalPhotos = sum(nrphotos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(TotalCaptures >= object$filterCount) %>%
      arrange(desc(TotalCaptures))
    
    # Emoji for group
    emoji <- if ("large_mammals" %in% object$setting$focus_groups) "🦌" else if ("birds" %in% object$setting$focus_groups) "🕊️" else "🐾"
    group_labels <- .paste_comma_and(object$species_summary$count$Name[object$species_summary$count$Group %in% object$setting$focus_groups])
    
    # Output Table
    if (nrow(group_summary) == 0) {
      cat("\n! No species met the threshold of ", object$filterCount, " observations.\n")
    } else {
      group_summary %>%
        rename(
          `Scientific Name` = scientificName,
          `Common Name` = vernacularNames.eng,
          `Family` = family,
          `Observations` = TotalCaptures,
          `Capture Locations` = UniqueLocations,
          `Total Photos` = TotalPhotos
        ) %>%
        gt() %>%
        tab_header(
          title = md(paste0("**", emoji, " Table 3. Summary of Frequently Observed ", group_labels, "**")),
          subtitle = md(paste0("Species with ≥ ", object$filterCount, " observations"))
        ) %>%
        tab_options(
          table.font.size = px(14),
          heading.title.font.size = px(16),
          heading.subtitle.font.size = px(12)
        ) %>%
        opt_row_striping()
    }
    
    
  })
  
  
  cm$addReportObject(.rxx) # adding R chunk!
  
  ######################
  # 3.2- Abundance trends:
  .txx <- .getTextObj(name='abundance_trends',title="Abundance Trends {.tabset}",parent='results',txt="While camera trap capture rates do not reflect true population densities, they serve as a valuable proxy for assessing changes in relative species abundance over time. An increase in capture rate, such as twice as many captures per unit time, may indicate a proportional increase in the number of individuals present, assuming detection probability remains constant.")
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='abundance_trends',name = 'abundance',setting={c(echo=FALSE,results="asis",warning=FALSE,message=FALSE)},code={
    if (!.require('ggplot2') & .require('ggrepel')) stop('Both the packages of "ggplot2" and "ggrepel" need to be installed...!')
    group_label <- .paste_comma_and(object$setting$focus_groups)
    cat(paste0(
      "The graphs below illustrate temporal trends in the number of captures, the number of active camera locations, ",
      "and the resulting capture rates for ", group_label, " species observed more than ", object$filterCount,
      " times in ", object$siteName, " across the sampling period (*Figure 3, 4, 5 and 6*) .\n\n"
    ))
    #----
    
    cat("### Number of Captures {.unnumbered}\n\n")
    
    
    if (!is.null(object$setting$focus_groups)) {
      .w <- which(names(object$species_summary) %in% object$setting$focus_groups)
      .w <- unlist(lapply(.w,function (i) object$species_summary[[i]]$site_list$scientificName))
    } else {
      if (any(c('wild_mammals','large_mammals','birds','amphibians','reptiles') %in% object$species_summary$count$Group)) {
        .w <- which(names(object$species_summary) %in% c('wild_mammals','large_mammals','birds','amphibians','reptiles'))
        .w <- unlist(lapply(.w,function (i) object$species_summary[[i]]$site_list$scientificName))
      }
    }
    
    # Shared setup for species colors
    species_list <- unique(object$capture$scientificName[
      object$capture$scientificName %in% .w & 
        object$capture$Year != "total"
    ])
    
    color_palette <- c(
      "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
      "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C",
      "#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C",
      "#DAA51B", "#2F8AC4", "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99",
      "#6D904F", "#F6C85F", "#B276B2", "#DECF3F", "#FAA43A", "#60BD68",
      "#F15854", "#4D4D4D", "#B2912F", "#7B615C", "#1F77B4", "#FF7F0E",
      "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#17BECF"
    )
    
    species_colors <- setNames(color_palette[1:length(species_list)], species_list)
    
    
    # Filter and prep data
    #large_mammals_frequently <- group_summary$scientificName
    
    capture_total_filtered <- object$capture %>%
      filter(scientificName %in% .w, Year != "total") %>%
      mutate(
        Year = as.factor(Year),
        Year_numeric = as.numeric(as.character(Year))
      ) %>%
      arrange(scientificName, Year)
    
    #  Labels for most recent year
    label_data <- capture_total_filtered %>%
      group_by(scientificName) %>%
      dplyr::filter(Year_numeric == max(Year_numeric)) %>%
      ungroup()
    
    # Build the plot
    plot_obj <- ggplot(capture_total_filtered, 
                       aes(x = Year, y = Captures, group = scientificName, color = scientificName)) +
      geom_line(size = 1.1) +
      geom_point(size = 2.5, shape = 21, fill = "white") +
      geom_text_repel(
        data = label_data,
        aes(label = scientificName),
        size = 3.2,
        nudge_x = 0.3,
        direction = "y",
        hjust = 0,
        segment.color = "gray60",
        family = "",
        show.legend = FALSE
      ) +
      scale_color_manual(values = species_colors) +
      labs(
        x = "Survey Year",
        y = "Number of Captures"
      ) +
      theme_minimal(base_family = "", base_size = 12) +
      theme(
        axis.title.x = element_text(size = 12, face = "bold", color = "black"),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.margin = ggplot2::margin(20, 40, 20, 40)
      )
    
    #️ Print the plot
    print(plot_obj)
    
    cat(paste0(
      '<p style="font-size:16px; color:black; font-weight:normal; margin-top:10px;">',
      '<b>Figure 3.</b> <i>Trends in camera-trap detections over the survey years ',
      'for frequently observed ', group_label, ', based on the number of captures.</i>',
      '</p>'
    ))
    
    #######
    
    cat("### Capture Rate {.unnumbered} \n\n")
    
    # species_list <- unique(object$capture$scientificName[
    #   object$capture$scientificName %in% .w & 
    #     object$capture$Year != "total"
    # ])
    # species_colors <- setNames(color_palette[1:length(species_list)], species_list)
    
    df_capture_rate <- object$capture %>%
      filter(scientificName %in% .w, Year != "total") %>%
      mutate(
        Year = as.factor(Year),
        Year_numeric = as.numeric(as.character(Year)),
        CaptureRate_100 = 100 * Capture_Rate
      ) %>%
      arrange(scientificName, Year)
    
    label_data <- df_capture_rate %>%
      group_by(scientificName) %>%
      filter(Year_numeric == max(Year_numeric)) %>%
      ungroup()
    
    # Plot
    plot_rate <- ggplot(df_capture_rate,
                        aes(x = Year, y = CaptureRate_100, group = scientificName, color = scientificName)) +
      geom_line(size = 1.1) +
      geom_point(shape = 21, size = 2.5, fill = "white") +
      geom_text_repel(data = label_data, aes(label = scientificName),
                      size = 3.2, nudge_x = 0.3, hjust = 0,
                      direction = "y", segment.color = "gray60",
                      family = "", show.legend = FALSE) +
      scale_color_manual(values = species_colors) +
      scale_y_log10(
        breaks = c(0.1, 1, 10, 100),  
        labels = c("0.1", "1", "10", "100"),
        limits = c(0.1, 100)
      ) +
      labs(x = "Survey Year", y = "Capture Rate (per 100 trap/days)") +  
      theme_minimal(base_family = "", base_size = 12) +
      theme(axis.title = element_text(face = "bold", color = "black"),
            axis.text.x = element_text(angle = 0, hjust = 0.5),         
            axis.text.y = element_text(angle = 0, hjust = 0.5),         
            legend.position = "none",
            panel.grid.minor = element_blank(),
            plot.margin = ggplot2::margin(20, 40, 20, 40))
    
    print(plot_rate)
    
    
    HTML('<p style="font-size: 16px; color: black; font-weight: normal; margin-top: 10px;">
<b>Figure 4.</b> <i>Log-transformed capture rates per 100 trap-days for frequently observed ', 
         group_label, ' across survey years.</i></p>')
    
    #----------
    cat("### Number of Locations {.unnumbered} \n\n")
    
    plot_obj <- ggplot(capture_total_filtered,
                       aes(x = Year, y = Locations, group = scientificName, color = scientificName)) +
      geom_line(size = 1.2) +
      geom_point(shape = 21, size = 3, fill = "white") +
      geom_text_repel(
        data = label_data,
        aes(label = scientificName),
        size = 3.5, family = "",
        nudge_x = 0.3,
        hjust = 0,
        direction = "y",
        box.padding = 0.3,
        segment.color = "gray60",
        show.legend = FALSE
      ) +
      scale_color_manual(values = species_colors) +
      scale_y_continuous(
        breaks = seq(0, 50, by = 10),
        labels = c("0", "10", "20", "30", "40", "50"),
        limits = c(0, 50),
        expand = expansion(mult = c(0, 0))
      ) +
      labs(
        x = "Survey Year",
        y = "Number of Locations"
      ) +
      theme_minimal( base_size = 12) +
      theme(
        axis.title.x = element_text(face = "bold", color = "black"),
        axis.title.y = element_text(face = "bold", color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),  
        legend.position = "none",  
        panel.grid.minor = element_blank(),
        plot.margin = ggplot2::margin(30, 40, 30, 40)
      )
    # Print plot
    print(plot_obj)
    
    # Build caption text dynamically using paste0()
    caption_text <- paste0(
      '<p style="font-size: 16px; color: black; font-weight: normal; margin-top: 10px;">',
      '<b>Figure 5.</b> <i>Trends in the number of camera-trap locations where each species was recorded, showing spatial dynamics', 
      group_label, 
      ' across survey years.</i></p>'
    )
    # Output as HTML
    HTML(caption_text)
    #----------
    
    cat("### REM-Based Density {.unnumbered}\n\n")
    
    spn <- object$get_speciesNames(object$setting$focus_groups)
    
    estimates_rem <- bind_rows(object$get_REM(spn[1]))
    if (length(spn) > 1) {
      for (i in 2:length(spn)) {
        estimates_rem <- rbind(estimates_rem,bind_rows(object$get_REM(spn[i])))
      }
    }
    
    if (nrow(estimates_rem) > 0) {
      # Filter density-only data
      density_data <- estimates_rem %>%
        dplyr::filter(Metric == "density") %>%
        mutate(
          Year = as.factor(Year),
          scientificName = factor(scientificName, levels = unique(scientificName)),
          Year_numeric = as.numeric(as.character(Year))
        )
      
      # Reuse existing color palette from earlier chunk
      species_list_rem <- unique(density_data$scientificName)
      species_colors_rem <- species_colors[species_list_rem]
      
      # Prepare label data for latest year
      label_data <- density_data %>%
        group_by(scientificName) %>%
        filter(Year_numeric == max(Year_numeric)) %>%
        ungroup()
      
      # Plot
      plot_density <- ggplot(density_data, aes(x = Year, y = estimate, group = scientificName, color = scientificName)) +
        geom_line(size = 1.1) +
        geom_point(shape = 21, size = 2.5, fill = "white") +
        geom_text_repel(data = label_data, aes(label = scientificName),
                        size = 3.2, nudge_x = 0.3, hjust = 0,
                        direction = "y", segment.color = "gray60",
                        family = "", show.legend = FALSE) +
        scale_color_manual(values = species_colors_rem) +
        labs(
          x = "Survey Year",
          y = "Density (individuals / km²)"
        ) +
        theme_minimal(base_family = "", base_size = 12) +
        theme(
          axis.title.x = element_text(size = 12, face = "bold", color = "black"),
          axis.title.y = element_text(size = 12, face = "bold", color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5, face = "plain"),  
          legend.position = "none",
          panel.grid.minor = element_blank(),
          plot.margin = ggplot2::margin(20, 40, 20, 40)
        )
      
      # Render plot
      print(plot_density)
      
      # Styled caption
      HTML(paste0(
        '<p style="font-size: 16px; color: black; font-weight: normal; margin-top: 10px;">',
        '<b>Figure 6.</b> <i>Trends in estimated REM-based population density for ', 
        group_label, ' across survey years.</i></p>'
      ))
      
    }
    
    
  })
  
  
  
  cm$addReportObject(.rxx) # adding R chunk!
  
  ###########
  
  .txx <- .getTextObj(name='population_density',title="Population Density Estimation",parent='results',txt="Population densities were estimated using the [REM](https://doi.org/10.1002/rse2.269), applied to study species with sufficient data to support reliable parameter estimation. Detection probability varies across species, habitat types, and camera models. The estimated model parameters and resulting population density estimates are presented below. For consistency, densities were calculated using parameter values averaged across all sampling rounds. \n\n")
  cm$addReportObject(.txx) 
  
  
  .txx <- .getTextObj(name='model_parameters',title="Model Parameters {.tabset}",parent='population_density',txt="The REM relies on three main parameters: movement speed, activity level, and day range. Movement speed represents the average distance traveled by an individual over time. Activity level reflects the proportion of time individuals are active and available for detection by camera traps. Day range refers to the total distance an animal typically moves within a 24-hour period and is used to refine density estimates. Plots for each model parameter are presented below (*Figures 7, 8, and 9*).\n\n")
  cm$addReportObject(.txx) 
  
  .rxx <- .getRchunk(parent='model_parameters',name = 'movement',setting={c(echo=FALSE,results="asis")},code={
    
    
    if (nrow(estimates_rem) > 0) {
      cat('\n#### Movement Speed {.tabset .unnumbered}\n\n')
      
      
      # Filter for the desired metrics
      speed_activity_data <- estimates_rem %>%
        dplyr::filter(Metric %in% c("active_speed", "activity_level", "overall_speed")) %>%
        group_by(scientificName, Metric) %>%
        dplyr::filter(Year == min(Year)) %>%
        ungroup()
      
      # Match colors to scientific names
      unique_species <- unique(speed_activity_data$scientificName)
      species_colors <- setNames(color_palette[1:length(unique_species)], unique_species)
      
      # Plot active speed with error bars
      p <- ggplot(speed_activity_data %>% dplyr::filter(Metric == "active_speed"),
             aes(x = scientificName, y = estimate, fill = scientificName)) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin = lcl95, ymax = ucl95), width = 0.2) +
        labs(x = "Species", y = "Active speed (km/h)") +
        scale_fill_manual(values = species_colors) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(face = "bold", color = "black"),
          axis.title.y = element_text(face = "bold", color = "black"),
          plot.title = element_text(face = "bold"),
          legend.position = "none"
        )
      
      print(p)
      # Caption
      cat("\n\n**Figure 7.** *Estimated active movement speeds (km/h) across study species, with error bars showing 95% confidence intervals.*\n")
      
      
      cat('\n#### Activity Level {.tabset .unnumbered}\n\n')
      
      activity_data <- speed_activity_data %>%
        dplyr::filter(Metric == "activity_level") %>%
        group_by(scientificName) %>%
        filter(Year == min(Year)) %>%
        ungroup()
      
      # Generate color map by scientific name
      unique_species <- unique(activity_data$scientificName)
      species_colors <- setNames(color_palette[1:length(unique_species)], unique_species)
      
      # Plot
      p <- ggplot(activity_data, 
             aes(x = scientificName, y = estimate, fill = scientificName)) + 
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin = lcl95, ymax = ucl95), width = 0.2) +
        labs(x = "Species", y = "Proportion of Time Active") +
        scale_fill_manual(values = species_colors) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(face = "bold", color = "black"),
          axis.title.y = element_text(face = "bold", color = "black"),
          plot.title = element_text(face = "bold"),
          legend.position = "none"
        )
      
      print(p)
      
      # Caption
      cat("\n\n**Figure 8.** *Estimated activity levels (proportion of the day active) for each study species. Whiskers represent 95% confidence intervals.*\n")
      #---------
      
      cat('\n#### Day range {.tabset .unnumbered}\n\n')
      
      overall_speed_data <- speed_activity_data %>%
        dplyr::filter(Metric == "overall_speed") %>%
        group_by(scientificName) %>%
        filter(Year == min(Year)) %>%
        ungroup()
      
      # Define color mapping using scientific names
      unique_species <- unique(overall_speed_data$scientificName)
      species_colors <- setNames(color_palette[1:length(unique_species)], unique_species)
      
      # Plot
      p <- ggplot(overall_speed_data, 
             aes(x = scientificName, y = estimate, fill = scientificName)) + 
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin = lcl95, ymax = ucl95), width = 0.2) +
        labs(x = "Species", y = "Day Range (km)") +
        scale_fill_manual(values = species_colors) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_text(face = "bold", color = "black"),
          axis.title.y = element_text(face = "bold", color = "black"),
          plot.title = element_text(face = "bold"),
          legend.position = "none"
        )
      
      print(p)
      # Caption
      cat("\n\n**Figure 9.** *Estimated day ranges for each study species, calculated as the product of movement speed and activity level, scaled to a 24-hour period. Whiskers indicate 95% confidence intervals.*\n")
    }
    
    
  })
  
  cm$addReportObject(.rxx)
  ###############
  .txx <- .getTextObj(name='population_densities',title="Population densities {.tabset}",parent='population_density',txt='For each study species, population densities were estimated using the REM implemented via the *camtrapDensity* package. The resulting plots are organized into interactive tabs, allowing users to dynamically explore density estimates across survey years for each species (*Figure 10*).\n\n')
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='population_densities',name = 'pop_densities',setting={c(echo=FALSE,results="asis",warning=FALSE,message=FALSE)},code={
    if (nrow(estimates_rem) > 0) {
      density_data <- estimates_rem %>%
        dplyr::filter(Metric == "density")
      
      # Ensure data is not empty
      if (nrow(density_data) > 0) {
        # Get a list of unique species
        species_list <- unique(density_data$scientificName)
        
        # Assign Acadia colors to species (cycling if necessary)
        species_colors <- setNames(rep(color_palette, length.out = length(species_list)), species_list)
        
        # Loop through each species
        for (i in seq_along(species_list)) {
          species <- species_list[i]  # Define species inside the loop
          
          # Header
          cat(paste0("\n#### ", species, " {.unnumbered}\n\n"))
          
          # Filter for current species
          species_data <- density_data %>%
            filter(scientificName == species)
          
          # Select the corresponding color
          species_color <- species_colors[species]
          
          # Plot per species
          p <- ggplot(species_data, aes(x = as.factor(Year), y = estimate)) +
            geom_bar(stat = "identity", fill = species_color, width = 0.6) +  # Apply species-specific color
            geom_errorbar(aes(ymin = lcl95, ymax = ucl95), width = 0.2, color = "black") +
            labs(x = "Year", y = expression("Population density (km"^-2*")")) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold", size = 14)
            )
          
          # Print the plot
          print(p)
          
          # Caption (COMPLETED)
          cat(paste0(
            "\n\n**Figure 10.** ", "*Estimated population density per sampling year for ", 
            species, 
            ". Whiskers represent the 95% confidence intervals.*\n"
          ))
          
        }
      }
    }
    
    
  })
  
  cm$addReportObject(.rxx)
  ##############################
  # 3.4- Activity Patterns:
  
  .txx <- .getTextObj(name='activity_patterns',title="Activity Patterns {.tabset}",parent='results',txt="Activity patterns describe how animals distribute their behavior throughout the 24-hour day and are typically derived from the timing of detections recorded by camera traps. These patterns help us understand when species are most active, offering valuable insight into their daily routines, ecological roles, and potential interactions with other species. By examining activity curves, we can detect whether a species is diurnal, nocturnal, or crepuscular, and identify shifts in behavior that may result from environmental factors or human disturbance. Below, activity curves illustrate the daily activity patterns for each study species, based on camera trap detections across the monitoring period (*Figure 11*).")
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='activity_patterns',name = 'activity_patterns_code',setting={c(echo=FALSE,results="asis",warning=FALSE,message=FALSE)},code={
    
    if (!is.null(object$sun_times) && nrow(object$sun_times) > 0) {
      frequent_species_scientific <- object$get_speciesNames(object$setting$focus_groups)
      
      # Color palette
      species_colors <- setNames(color_palette[1:length(frequent_species_scientific)], frequent_species_scientific)
      
      ############
      
      # Compute total average sunrise/sunset
      overall_sunrise <- mean(object$sun_times$avg_sunrise, na.rm = TRUE)
      overall_sunset  <- mean(object$sun_times$avg_sunset, na.rm = TRUE)
      
      # Loop through species
      for (species in frequent_species_scientific) {
        
        cat("\n###", species, "{.unnumbered}\n\n")
        
        .rem_param <- object$.get_REM_Param(species)
        if (!is.null(.rem_param)) {
          res <- tryCatch({
            rem_estimate(object$pkg, check_deployments = FALSE, species = species, reps = 10,
                         radius_model = .rem_param$radius_model,angle_model = .rem_param$angle_model,
                         speed_model = .rem_param$speed_model,activity_model = .rem_param$activity_model)
          }, error = function(e) {
            cat("\n! Warning: REM estimation failed for", species, "\n")
            return(NULL)
          })
        } else cat("\n! Warning: REM estimation failed for", species, "\n")
        
        
        if (is.null(res) || is.null(res$activity_model)) {
          cat("\n! Skipping", species, "- missing models!\n")
          next
        }
        
        activity_pred <- data.frame(
          time = (res$activity_model@pdf[, "x"] / (2 * pi)) * 24,
          estimate = res$activity_model@pdf[, "y"],
          lcl = res$activity_model@pdf[, "lcl"],
          ucl = res$activity_model@pdf[, "ucl"]
        )
        
        p <- ggplot(activity_pred, aes(x = time, y = estimate)) +
          geom_line(color = species_colors[[species]], size = 1.2) +
          geom_ribbon(aes(ymin = lcl, ymax = ucl),
                      fill = species_colors[[species]], alpha = 0.3) +
          
          # Sunrise and sunset lines
          geom_vline(xintercept = overall_sunrise, color = "gold", linetype = "dashed", linewidth = 0.8) +
          geom_vline(xintercept = overall_sunset, color = "gray", linetype = "dashed", linewidth = 0.8) +
          
          scale_x_continuous(
            limits = c(0, 24),
            breaks = seq(0, 24, by = 6),
            labels = c("00:00", "06:00", "12:00", "18:00", "24:00")
          ) +
          labs(
            x = "Time of Day",
            y = "Estimated Activity Density"
          ) +
          theme_minimal(base_family = "", base_size = 13) +
          theme(
            axis.title.x = element_text(face = "bold", color = "black"),
            axis.title.y = element_text(face = "bold", color = "black"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            plot.margin = ggplot2::margin(20, 40, 20, 40)
          )
        
        print(p)
        
        cat(paste0(
          "\n\n**Figure 11.** *Estimated daily activity pattern for ", 
          species, 
          ", aggregated across all sampling rounds. Dashed lines indicate the average sunrise (yellow) and sunset (gray) times across survey years. The solid line shows the fitted activity model, and the shaded region shows the 95% CI.*\n\n"
        ))
      }
    } else cat('\n\n The sun times have not been calculated; it requires the suncalc package (run setup first...)!\n')
    
  })
  #------------
  
  cm$addReportObject(.rxx)
  #-----------------------
  # 3.5- Richness:
  .txx <- .getTextObj(name='richness',title="Richness {.tabset}",parent='results',txt="Species richness refers to the number of unique species recorded at each camera trap location. It serves as an important indicator of local biodiversity and helps evaluate how effectively the sampling captured the wildlife community within the study area. Figure 11 displays spatial patterns in species richness, where each point represents a camera trap location. The size of the circle reflects the total number of unique species detected at that site, while the color indicates richness intensity, with warmer colors denoting higher species counts. By clicking on any point, users can view detailed information about the detected species (community composition), the specific richness count, and the associated habitat type (*Figure 12*). This allows for exploration of biodiversity hotspots, detection gaps, and potential habitat-specific species assemblages. Understanding species richness across space helps conservationists assess survey completeness, identify ecologically valuable locations, and prioritize future conservation or monitoring efforts.")
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='richness',name = 'richness_code',setting={c(echo=FALSE,results="asis",warning=FALSE,fig.show='hold')},code={
    study_areaSHP <- NULL
    if (nrow(object$study_area@attributes) > 0) {
      # study_areaSHP <- object$study_area %>%
      #   st_zm() %>% st_transform(crs = 4326)
      
      study_areaSHP <- project(unwrap(object$study_area),"epsg:4326")
    }
    
    #------------
    
    plot_richness <- function(data) {
      if ('Habitat_Type' %in% colnames(data)) {
        data <- data %>%
          mutate(
            Habitat_Type = gsub("_", " ", Habitat_Type),
            popup = paste0(
              "<b>Location:</b> ", ifelse(!is.na(locationName), locationName, "N/A"), "<br>",
              "<b>Richness:</b> ", ifelse(!is.na(Richness), Richness, "N/A"), "<br>",
              "<b>Community:</b> ", ifelse(!is.na(Community_Composition), Community_Composition, "N/A"), "<br>",
              "<b>Habitat:</b> ", ifelse(!is.na(Habitat_Type), Habitat_Type, "N/A")
            )
          )
      } else {
        data <- data %>%
          mutate(
            popup = paste0(
              "<b>Location:</b> ", ifelse(!is.na(locationName), locationName, "N/A"), "<br>",
              "<b>Richness:</b> ", ifelse(!is.na(Richness), Richness, "N/A"), "<br>",
              "<b>Community:</b> ", ifelse(!is.na(Community_Composition), Community_Composition, "N/A"), "<br>"
              
            )
          )
      }
      
      #
      present_levels <- as.character(sort(unique(data$Richness)))
      
      full_palette <- c(
        "#2D8B3F", "#91FF00", "#FFDA00", "#FF9100", "#FF4800",
        "#FF0000", "#D33682", "#7F00FF", "#00CED1", "#FFA07A",
        "#1E90FF", "#8B4513", "#808000", "#FF69B4", "#00FA9A",
        "#A52A2A", "#9370DB", "#4682B4", "#FF1493", "#20B2AA"
      )
      
      richness_palette <- colorFactor(
        palette = full_palette[seq_along(present_levels)],
        domain = present_levels,
        na.color = "#bdbdbd"
      )
      
      #
      data$Richness <- factor(data$Richness, levels = present_levels)
      
      if (!is.null(study_areaSHP)) {
        leaflet(data) %>%
          addTiles() %>%
          addPolygons(
            data = study_areaSHP,
            color = "#3A3F44", weight = 2, opacity = 0.9,
            fillColor = "transparent", fillOpacity = 0
          ) %>%
          addCircleMarkers(
            lng = ~longitude, lat = ~latitude,
            radius = ~ifelse(as.numeric(as.character(Richness)) == 1, 2, as.numeric(as.character(Richness)) * 2),
            fillColor = ~richness_palette(Richness),
            fillOpacity = 0.7, stroke = TRUE, color = "black",
            popup = ~popup
          ) %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = richness_palette,
            values = present_levels,
            title = "Species Richness",
            opacity = 1,
            labFormat = labelFormat(transform = identity)
          )
      } else {
        leaflet(data) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~longitude, lat = ~latitude,
            radius = ~ifelse(as.numeric(as.character(Richness)) == 1, 2, as.numeric(as.character(Richness)) * 2),
            fillColor = ~richness_palette(Richness),
            fillOpacity = 0.7, stroke = TRUE, color = "black",
            popup = ~popup
          ) %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = richness_palette,
            values = present_levels,
            title = "Species Richness",
            opacity = 1,
            labFormat = labelFormat(transform = identity)
          )
      }
    }
    
    # Loop through each year
    for (.year in object$years) {
      
      cat(paste0("\n### ", .year, " {.unnumbered}\n\n"))
      
      .richness <- object$richness(year=.year,spList=object$get_speciesNames(group = object$setting$focus_groups))
      p <- plot_richness(.richness)
      print(tagList(p))
      cat("\n\n")
    }
    
    # Total summary map
    cat("\n### Total {.unnumbered}\n\n")
    
    .richness <- object$richness(year=NULL,spList=object$get_speciesNames(group = object$setting$focus_groups))
    p <- plot_richness(.richness)
    print(tagList(p))
    
    cat("\n\n")
    if (!is.null(object$setting$focus_groups) && is.character(object$setting$focus_groups) && length(object$setting$focus_groups) > 0) {
      cat(paste0("## {.unnumbered}\n**Figure 12.** *Species richness based on the species focus group: ",.paste_comma_and(object$setting$focus_groups),", across camera trap locations for each survey year, as well as the cumulative richness across all years.*\n"))
    } else cat("**Figure 12.** *Species richness across camera trap locations for each survey year, as well as the cumulative richness across all years.*\n")
    
  })
  cm$addReportObject(.rxx)
  #-----------
  ##############################################
  # 3.6 Species Co-occurrence:
  .txx <- .getTextObj(name='co_occurrence',title="Species Co-occurrence {.tabset}",parent='results',txt="Species co-occurrence analysis examines how different species share habitats and interact within the same locations. By using correlation matrices and clustering techniques, we identify species that tend to be found together and those that exhibit avoidance patterns. Understanding co-occurrence patterns helps assess interspecific relationships and habitat preferences (*Figure 13*).")
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='co_occurrence',name = 'co_occurrence_code',setting={c(echo=FALSE,results="asis",warning=FALSE,cache=FALSE)},code={
    .spn <- object$get_speciesNames(object$setting$focus_groups)
    .pa <- FALSE
    if (!is.null(object$setting$PA) && is.logical(object$setting$PA)) .pa <- TRUE
    
    if (.require('corrplot')) {
      for (.year in object$years) {
        
        .spo <- object$species_summary_by_location(year=.year,spList = .spn,cor_matrix = TRUE,PA=.pa)
        if (!all(is.na(.spo))) {
          cat(paste0("\n### ", .year, " {.unnumbered}\n\n"))
          
          
          
          par(mar = c(5, 5, 15, 5))
          
          .eval('corrplot(.spo, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.8, diag = FALSE,title= paste0("Species Co-occurrence (Year: ",.year,")"),mar=c(0,0,1,0))',env = environment())
          
          cat("\n\n")
        }
        
      }
      #---------
      .spo <- object$species_summary_by_location(spList = .spn,cor_matrix = TRUE,PA=.pa)
      
      if (!all(is.na(.spo))) {
        cat("\n### Total {.unnumbered}\n\n")
        
        .eval('corrplot(.spo, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.8, diag = FALSE,title="Species Co-occurrence (Total)",mar=c(0,0,1,0))',env = environment())
        
        
        cat("\n\n")
      }
      
      
      
    } else {
      for (.year in object$years) {
        
        
        .spo <- object$species_summary_by_location(year=.year,spList = .spn,cor_matrix = TRUE,PA=.pa)
        
        if (!all(is.na(.spo))) {
          cat(paste0("\n### ", .year, " {.unnumbered}\n\n"))
          par(mar = c(7, 8, 4, 4))
          .basic_corrplot(.spo,main = paste0("Species Co-occurrence (Year: ",.year,")"))
          
          cat("\n\n")
        }
        
      }
      #---------
      .spo <- object$species_summary_by_location(spList = .spn,cor_matrix = TRUE,PA=.pa)
      
      if (!all(is.na(.spo))) {
        cat("\n### Total {.unnumbered}\n\n")
        par(mar = c(7, 8, 4, 4))
        .basic_corrplot(.spo,main = "Species Co-occurrence (Total)")
        
        cat("\n\n")
      }
      
    }
    #---------
    if (!is.null(object$setting$focus_groups) && is.character(object$setting$focus_groups) && length(object$setting$focus_groups) > 0) {
      cat(paste0("## {.unnumbered}\n**Figure 13.** *Species co-occurrence patterns based on the species focus group: ",.paste_comma_and(object$setting$focus_groups),", afor each survey year, as well as the cumulative across all years.*\n"))
    } else cat("**Figure 13.** *Species co-occurrence patterns for each survey year, as well as the cumulative across all years.*\n")
    
  })
  cm$addReportObject(.rxx)
  #----------
  .txx <- .getTextObj(name='spatial_density',title="Spatial Density {.tabset}",parent='results',txt="Spatial density maps generated from point pattern analysis illustrate where species were most frequently detected across the study area. These visualizations highlight spatial variation in observation intensity, helping to identify hotspots of activity as well as areas with low detection probability. Each map shows the density distribution for a selected species, either by year or across the full monitoring period. Warmer colors represent areas of higher detection density, while cooler tones indicate less frequent observations. The interactive layout allows users to explore spatial patterns by toggling between species and years, enabling direct comparisons and revealing temporal shifts in distribution (*Figure 14*).")
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='spatial_density',name = 'spatial_density_code',setting={c(echo=FALSE,results="asis",warning=FALSE,message=FALSE)},code={
    .spn <- object$get_speciesNames(object$setting$focus_groups)
    
    .generate_density_maps <- function(.year=NULL) {
      df <- object$species_summary_by_location(year = .year,spList = .spn,cor_matrix = FALSE)
      df$lon <- df$longitude
      df$lat <- df$latitude
      if (is.null(df)) return(NULL)
      .dfs <- vect(df,geom=c('lon','lat'),crs=crs(rast()))
      .dfs <- .get_projected_vect(.dfs)
      .crs <- crs(.dfs)
      .dfs <- as.data.frame(.dfs,geom="XY")
      species_list <- unique(.dfs$scientificName)
      #---
      if (nrow(object$study_area@attributes) > 0) {
        .ext <- as.vector(ext(project(unwrap(object$study_area),.crs)))
      } else .ext <- NULL
      
      .label <- 'Total'
      if (!is.null(.year)) .label <- as.character(.year)
      
      cat("\n###", .label, "{.tabset .unnumbered}\n\n")
      
      for (sp in species_list) {
        df_sp <- .dfs %>% dplyr::filter(scientificName == sp)
        if (nrow(df_sp) < 3) next
        
        r <- try(object$spatial_density(df_sp,.crs=.crs,.ext=.ext),silent = TRUE)
        if (inherits(r,'try-error')) r <- NULL
        if (is.null(r)) next
        
        cat("\n####", sp, "{.unnumbered}\n\n")
        m <- leaflet(df_sp) %>%
          addTiles(group = "OSM") %>%
          addRasterImage(
            r, opacity = 0.7,
            colors = colorRampPalette(c("gray","orange","red","darkred"))(200),
            group = "Density"
          )
        if ("Habitat_type" %in% colnames(df_sp)) {
          m <- m %>%
            addCircleMarkers(
              ~longitude, ~latitude,
              popup = ~sprintf(
                "<b>%s</b><br/>%s<br/>Obs: %d<br/>Habitat: %s",
                scientificName, locationName, total_observations, Habitat_type
              ),
              radius = ~sqrt(total_observations)*1.2,
              color  = "blue", fillOpacity = 0.4,
              group  = "Points"
            )
        } else {
          m <- m %>%
            addCircleMarkers(
              ~longitude, ~latitude,
              popup = ~sprintf(
                "<b>%s</b><br/>%s<br/>Obs: %d",
                scientificName, locationName, total_observations
              ),
              radius = ~sqrt(total_observations)*1.2,
              color  = "blue", fillOpacity = 0.4,
              group  = "Points"
            )
        }
        #----
         m <- m %>%
          addLayersControl(
            baseGroups    = "OSM",
            overlayGroups = c("Density","Points"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          setView(
            lng = mean(df_sp$longitude, na.rm = TRUE),
            lat = mean(df_sp$latitude,  na.rm = TRUE),
            zoom = 11
          )
        print(tagList(m))
      }
    }
    
    cat("\n## Species Density Maps {.tabset .unnumbered}\n\n")
    
    for (.year in object$years) {
      .generate_density_maps(.year)
    }
    #---
    .generate_density_maps(.year=NULL) # for Total
    #----
    cat("\n\n")
    
    #---------
    if (!is.null(object$setting$focus_groups) && is.character(object$setting$focus_groups) && length(object$setting$focus_groups) > 0) {
      cat(paste0("## {.unnumbered}\n**Figure 14.** *Spatial point pattern analysis based on the species focus group: ",.paste_comma_and(object$setting$focus_groups),", for each survey year, as well as the cumulative (Total) across all years.*\n"))
    } else cat("**Figure 14.** *Spatial point pattern analysis for each year separately as well as  the cumulative (Total) across all years.*\n")
    
  })
  cm$addReportObject(.rxx)
  #----------
  #----------
  .txx <- .getTextObj(name='habitat_preferences',title="Habitat preferences {.tabset}",parent='results',txt='Differences in capture rate - the number of observations per unit effort - reflect the degree to which animals prefer or avoid certain points or habitat types.')
  cm$addReportObject(.txx) 
  
  
  .rxx <- .getRchunk(parent='habitat_preferences',name = 'habitat_preferences_code',setting={c(echo=FALSE,results="asis",warning=FALSE,message=FALSE)},code={
    .spn <- object$get_speciesNames(object$setting$focus_groups)
    
    capture_habitat_sel <- object$species_summary_by_habitat 
    if (nrow(capture_habitat_sel) > 0) {
      capture_habitat_sel <- capture_habitat_sel %>%
        dplyr::filter(scientificName %in% .spn)
      
      
      # 🎨 **Define Acadia color palette**
      acadia_colors <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", 
                         "#526A83", "#625377", "#68855C", "#9C9C5E", 
                         "#A06177", "#8C785D", "#467378", "#7C7C7C")
      
      # 🎨 **Create a color mapping for habitat types**
      habitat_types <- unique(capture_habitat_sel$Habitat_Type)
      habitat_colors <- setNames(rep(acadia_colors, length.out = length(habitat_types)), habitat_types)
      
      # 🎨 **Generate the plot with Acadia colors**
      print(
        ggplot(capture_habitat_sel, aes(x = scientificName, y = capture_rate, fill = Habitat_Type)) +
          geom_bar(stat = "identity", position = "fill") +  
          labs(
            x = "Species",
            y = "Capture Rate (Proportion)",
            fill = "Habitat Type"
          ) +
          scale_fill_manual(values = habitat_colors) +  # Apply Acadia colors
          scale_y_continuous(labels = scales::percent_format()) + 
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 14, face = "bold")
          )
      )
      cat("\n\n")
      
      #---------
      if (!is.null(object$setting$focus_groups) && is.character(object$setting$focus_groups) && length(object$setting$focus_groups) > 0) {
        cat(paste0("**Figure 15.**: _Capture rate distribution across habitat types based on the species focus group: ",.paste_comma_and(object$setting$focus_groups),". Each bar represents a species, with sections indicating the proportion of each habitat type_\n"))
      } else cat("**Figure 15.**: _Capture rate distribution across habitat types for all species. Each bar represents a species, with sections indicating the proportion of each habitat type_\n")
      
    }
    
  })
  cm$addReportObject(.rxx)
  #----------
  
  #readLines('_Dataset/input/Text/acknowledgements.txt')
  
  
  .txx <- .getTextObj(name='acknowledgements',title='Acknowledgements',
                      txt="This report was generated using the R package camtrapReport, developed by Elham Ebrahimi and Patrick Jansen from the Wildlife Ecology and Conservation Group, Wageningen University, and the Wildlife Ecology and Nature Restoration Group, Utrecht University. Users are kindly requested to cite the package using its DOI (https://doi.org/10.5281/zenodo.15721045) when using camtrapReport or publishing results based on it.")
  #---------------------
  cm$addReportObject(.txx)
  
  
  
  # .ref <- readLines('_Dataset/input/Text/References.txt')
  # .ref <- paste(paste("<p>",.ref,"</p>"),collapse = '\n')
  # 
  # 
  # .txx <- .getTextObj(name='references',title='References',
  #                     txt=list(p1='<div style="text-indent: -20px; padding-left: 20px; line-height: 1.5;">\n',
  #                              p2 = .ref,
  #                              p3 = "</div>\n"
  #                     ))
  # 
  # cm$addReportObject(.txx)
  #-------------------
  .txx <- .getTextObj(name='appendix',title='Appendix {.tabset .unnumbered}')
  cm$addReportObject(.txx)
  
  
  .rxx <- .getRchunk(parent='appendix',name = 'appendix_code',setting={c(echo=FALSE,results="asis",warning = FALSE)},code={
    
    # Filter media with favorites
    favoImgs <- object$data$media %>%
      dplyr::filter(favourite == TRUE) %>%
      left_join(dplyr::select(object$data$observations, c(sequenceID, taxonID)), by = "sequenceID",relationship = 'many-to-many') %>%
      left_join(dplyr::select(object$data$taxonomy, c(taxonID, vernacularNames.eng, scientificName)), by = "taxonID") %>%
      group_by(scientificName)
    
    .downloadable <- FALSE
    
    if (nrow(favoImgs) > 0) {
      if (!is.null(object$setting$focus_groups)) {
        favoImgs <- favoImgs %>%
          dplyr::filter(scientificName %in% object$get_speciesNames(object$setting$focus_groups)) %>%
          sample_n(1) %>%
          ungroup()
      } else {
        favoImgs <- favoImgs %>%
          sample_n(1) %>%
          ungroup()
      }
      
      #--------
      if (nrow(favoImgs) > 0) {
        
        #--------
        #------
        # CSS for Lightbox Effect 
        HTML('
        <style>
    /* Lightbox effect */
    .lightbox {
      position: fixed;
      z-index: 9999;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background: rgba(0, 0, 0, 0.8);
      display: none;
      justify-content: center;
      align-items: center;
    }
    .lightbox img {
      max-width: 40%;
      max-height: 40%;
    }
    .lightbox:target {
      display: flex;
    }
    
    /* Grid layout for images */
    .image-container {
      display: flex;
      flex-wrap: wrap;
      justify-content: center;
    }
    .image-box {
      width: 45%;
      text-align: center;
      margin: 10px;
    }
    .image-box img {
      width: 90%;
      cursor: pointer;
      transition: transform 0.2s;
    }
    .image-box img:hover {
      transform: scale(1.05);
    }
    </style>
    ')
        #-------
        if (!dir.exists(paste0(object$pkg$directory,"/_camtrap_pictures"))) {
          dir.create(paste0(object$pkg$directory,"/_camtrap_pictures"))
        }
        #-----
        .fn <- favoImgs$fileName
        html_output <- '<div class="image-container">'
        for (i in seq_along(.fn)) {
          .pic_fn <- paste0(object$pkg$directory,"/_camtrap_pictures/",.fn[i])
          if (!file.exists(.pic_fn)) {
            .pic_link <- favoImgs$filePath[i]
            if (grepl("^https?://", .pic_link)) {
              e <- try(download.file(.pic_link, .pic_fn, mode = "wb",quiet = TRUE),silent = TRUE)
              if (!inherits(e,'try-error')) {
                # Resize and save locally
                image_read(.pic_fn) %>%
                  image_scale("600") %>%
                  image_write(path = .pic_fn)
                
                .downloadable <- TRUE
              }
            }
          }
          #---
          if (file.exists(.pic_fn)) {
            .downloadable <- TRUE
            # Add image with lightbox effect
            html_output <- paste0(html_output, '
        <div class="image-box">
          <a href="#img', i, '"><img src="', .pic_fn, '" alt="Image ', i, '"></a>
        </div>')
          } # else {
          #html_output <- paste0(html_output, "<p style='color: red;'> Image not found: ", .pic_link, "</p>")
          #}
        }
        #------
        html_output <- paste0(html_output, '</div>')
        
        
        if (.downloadable) {
          cat(paste0(
            "\nThe following images showcase a selection of ", nrow(favoImgs), " species: ",
            paste(paste0("<i>", favoImgs$scientificName, "</i>"), collapse = ", "),
            ", captured at different camera locations within this study site.\n"
          ))
          
          HTML(html_output)
        } else {
          cat("As the user did not provide access to their favourite images, no images were selected for the appendix.")
        }
        
        # Render HTML correctly
        
        
        
      }
    }
    
  })
  
  
  #==========
  cm$addReportObject(.rxx)
}
#-----------

if (!isGeneric("camData")) {
  setGeneric("camData", function(data,habitat,study_area,...)
    standardGeneric("camData"))
}


setMethod('camData', signature(data='character'), 
          function(data,habitat,study_area=NULL,...) {
            
            if (missing(habitat) || !is.data.frame(habitat)) habitat <- NULL
            
            if (missing(study_area)) study_area <- NULL
            
            #.d <- .read_camtrapDATA(data,ctdp = TRUE)
            .d <- .read_camdp(data)
            cm <- camR$new()
            cm$setting$locationLegend <- TRUE
            
            cm$data <- .d$data
            
            #-------
            if (!is.null(habitat)) cm$habitat <- habitat
            
            if (!is.null(study_area)) {
              if (is.character(study_area)) {
                if (file.exists(study_area)) {
                  cm$study_area <- terra::wrap(vect(study_area))
                } else {
                  warning("study_area filename is not available and ignored...!")
                }
              } else if (inherits(study_area,'SpatVector')) {
                cm$study_area <- terra::wrap(study_area)
              } else if (inherits(study_area,'PackedSpatVector')) {
                cm$study_area <- study_area
              } else if (.eval("inherits(study_area,'sf')",env = environment())) {
                cm$study_area <- terra::wrap(vect(study_area))
              } else {
                warning("study_area is ignored (should be a filename or a spatial dataset)...!")
              }
            }
            #----------
            cm$filterExclude <- list(
              scientificName=c("Homo sapiens", "Canis lupus familiaris", "Felis catus",
                               "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
                               "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
                               "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus"))
            cm$filterKeep <- list(observationType=c("animal"),class=NULL)
          
            cm$add_group('large_mammals',list(order=c("Artiodactyla", "Carnivora")))
            #---------
            cm$filterCount <- 25
            #-------
            cm$add_group('domestic',list(
              scientificName=c("Homo sapiens", "Canis lupus familiaris", "Felis catus",
                               "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
                               "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
                               "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus")))
            
            cm$setting$focus_groups <- 'large_mammals'
            #-------
            if (!is.null(.d$json$project$title) && .d$json$project$title != "") {
              cm$siteName <- .d$json$project$title
            } else cm$siteName <- "Unnamed Site"
            
            
            sp_summary <- .summarize_spatial_info(cm)
            country <- sp_summary$country
            fg <- .firstUpper(.paste_comma_and(cm$setting$focus_groups))
            site_Name <- cm$siteName
            cm$title <- glue("Camera-Trap Report: {fg} at {site_Name}, {country}")
            cm$subtitle <- "Report on Camera Trapping for the European Observatory of Wildlife"
            cm$info <- .d$json
            rm(.d); gc()
            
            cm$setup()
            .addReportSections(cm)
            cm
          }
)
#--------

setMethod('camData', signature(data='datapackage'), 
          function(data,habitat,study_area=NULL,...) {
            
            if (missing(habitat) || !is.data.frame(habitat)) habitat <- NULL
            
            if (missing(study_area)) study_area <- NULL
            #-------
            cm <- camR$new()
            cm$setting$locationLegend <- TRUE
            cm$pkg <- data
            cm$data <- .eval("as_ctdp(pkg, verbose = FALSE,...)",env = environment()) 
            #-------
            if (!is.null(habitat)) cm$habitat <- habitat
            
            if (!is.null(study_area)) {
              if (is.character(study_area)) {
                if (file.exists(study_area)) {
                  cm$study_area <- terra::wrap(vect(study_area))
                } else {
                  warning("study_area filename is not available and ignored...!")
                }
              } else if (inherits(study_area,'SpatVector')) {
                cm$study_area <- terra::wrap(study_area)
              } else if (inherits(study_area,'PackedSpatVector')) {
                cm$study_area <- study_area
              } else if (.eval("inherits(study_area,'sf')",env = environment())) {
                cm$study_area <- terra::wrap(vect(study_area))
              } else {
                warning("study_area is ignored (should be a filename or a spatial dataset)...!")
              }
            }
            #----------
            cm$filterExclude <- list(
              scientificName=c("Homo sapiens", "Canis lupus familiaris", "Felis catus",
                               "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
                               "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
                               "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus"))
            cm$filterKeep <- list(observationType=c("animal"),class=NULL)
            
            cm$add_group('large_mammals',list(order=c("Artiodactyla", "Carnivora")))
            #---------
            cm$filterCount <- 25
            #-------
            cm$add_group('domestic',list(
              scientificName=c("Homo sapiens", "Canis lupus familiaris", "Felis catus",
                               "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
                               "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
                               "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus")))
            
            cm$setting$focus_groups <- 'large_mammals'
            #-------
            cm$siteName <- cm$pkg$project$title   %||% "Unnamed Site"
            
            sp_summary <- .summarize_spatial_info(cm)
            country <- sp_summary$country
            fg <- .firstUpper(.paste_comma_and(cm$setting$focus_groups))
            site_Name <- cm$siteName
            cm$title <- glue("Camera-Trap Report: {fg} at {site_Name}, {country}")
            cm$subtitle <- "Report on Camera Trapping for the European Observatory of Wildlife"
            
            cm$setup()
            
            .addReportSections(cm)
            
            cm
            
          }
)



