# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  March 2026
# Version 1.1
# Licence MIT
#--------

.ct_icons <- function() {
  list(
    green  = "🟢",
    yellow = "🟡",
    orange = "🟠",
    red    = "🔴",
    alarm  = "🚨",
    warn   = "⚠️",
    sea    = "🌊"
  )
}


.camr_getMergedSummary <- function(cm) {
  
  #----------- # dep_loc = deployments + locations --------------------------
  dep_loc <- left_join(cm$data$deployments,cm$data$locations,by="locationID")
  
  if (!"Habitat_Type" %in%  colnames(dep_loc) && 'habitat' %in% colnames(dep_loc)) colnames(dep_loc)[colnames(dep_loc) == "habitat"] <- "Habitat_Type"
  dep_loc$Habitat_Type <- gsub("_", " ", dep_loc$Habitat_Type)
  dep_loc$Habitat_Type <- ifelse(dep_loc$Habitat_Type == "Other", "Unclassified Habitat", dep_loc$Habitat_Type)
  
  #----------------- #Count Deployments Per Location --------------------------
  deployments_per_location <- dep_loc |>
    group_by(locationID) |>
    summarise(
      deploymentID_List = toString(unique(deploymentID)), 
      Num_Deployments = n()
    ) |>
    ungroup()
  #----------------- #Capture Methods Per Location --------------------------
  capture_methods_per_location <- cm$data$sequences |>
    left_join(dplyr::select(dep_loc, deploymentID, locationID), by = "deploymentID") |>
    group_by(locationID) |>
    summarise(CaptureMethod_List = toString(sort(unique(captureMethod)))) |>
    ungroup()
  
  #----------------- #setup By Per Location --------------------------
  # Group by locationID and list unique setupBy names
  setup_per_location <- dep_loc |>
    group_by(locationID) |>
    summarise(Setup_By_List = toString(sort(unique(setupBy)))) |>
    ungroup()
  
  #----------------- #Classify By Per Location --------------------------
  
  #Extract sequenceID to deploymentID mapping from sequences
  sequence_to_deployment <- cm$data$sequences |>
    dplyr::select(sequenceID, deploymentID) |>
    distinct()  # Ensure unique mapping
  
  
  # Merge deploymentID into observations using sequenceID
  observations_with_deployment <- cm$data$observations |>
    dplyr::select(-deploymentID) |>
    left_join(sequence_to_deployment, by = "sequenceID") |>
    left_join(dep_loc[,c("deploymentID","locationID")], by = "deploymentID")  # Merge locationID using deploymentID
  
  
  # Group by locationID and list unique ClassifyBy names
  Classify_per_location <- observations_with_deployment |>
    group_by(locationID) |>
    summarise(Classify_By_List = toString(sort(unique(classifiedBy)))) |>
    ungroup()
  
  # ----------------- List of unique baitUse per location --------------------------
  # Group by locationID and list unique baitUse values, properly separated by ", "
  bait_use_per_location <- dep_loc |>
    group_by(locationID) |>
    summarise(BaitUse_List = paste(sort(unique(baitUse)), collapse = ", ")) |>  # Ensure ", " separation
    ungroup()
  
  
  # ----------------- List of deployment years for each location --------------------------
  
  # Aggregate Year List per locationID
  years_per_location <- dep_loc |>
    group_by(locationID) |>
    summarise(Year_List = paste(sort(unique(Year)), collapse = ", ")) |>  # Keep years in one row
    ungroup()
  
  
  #----------------- How Many Cameras in Each Year? 
  # expanded_years <- years_per_location |>
  #   separate_rows(Year_List, sep = ", ")
  
  location_count_per_year <- table(unique(dep_loc[,c('locationID','Year')])$Year)
  location_count_per_year <- data.frame(Year_List=as.numeric(names(location_count_per_year)),Unique_Locations=as.numeric(location_count_per_year))
  location_count_per_year <- location_count_per_year[order(location_count_per_year$Unique_Locations,decreasing = TRUE),]
  #-------
  
  photos_per_location <- .left_join(dep_loc[,c("deploymentID","locationID")],cm$data$sequences[,c("deploymentID","nrphotos")],by='deploymentID')
  photos_per_location <- photos_per_location |>
    group_by(locationID) |>
    summarise(Total_Photos=sum(nrphotos,na.rm=TRUE)) |>
    ungroup()
  
  # ----------------- List of Species for Each Location (Final Filtering) --------------------------
  # Step 1-1: Merge observations with taxonomy
  observations_with_taxonomy <- cm$data$observations |>
    dplyr::select(-scientificName) |>
    left_join(cm$data$taxonomy, by = "taxonID")
  
  
  # Step 1-2: Group by sequenceID and create a cleaned list of species
  scientific_names_per_observation <- observations_with_taxonomy |>
    group_by(sequenceID) |>
    summarise(ScientificName_List = paste(
      sort(unique(scientificName[
        !is.na(scientificName) & 
          scientificName != "" & 
          grepl("\\s", scientificName)  # Keep only species with at least two words
      ])),
      collapse = ", ")) |>
    ungroup()
  
  
  #---
  # Step 2: Merge with scientific_names_per_observation to get deploymentID
  scientific_names_with_deployment <- scientific_names_per_observation |>
    left_join(sequence_to_deployment, by = "sequenceID")
  
  # Step 3: Select relevant columns from 'dataNew$deployments' to map 'deploymentID' to 'locationID'
  deployment_to_location <- dep_loc |>
    dplyr::select(deploymentID, locationID) |>
    distinct()  # Ensure unique mapping
  
  # Step 4: Merge with scientific_names_with_deployment to get locationID
  species_per_location <- scientific_names_with_deployment |>
    left_join(deployment_to_location, by = "deploymentID") |>
    group_by(locationID) |>
    summarise(Species_List = paste(
      sort(unique(ScientificName_List[!is.na(ScientificName_List) & ScientificName_List != ""])), 
      collapse = ", ")) |>
    ungroup()
  
  
  
  
  
  ################ **Join all data into a single dataframe required for the **Research Area Plot** ################ 
  .d<-left_join(dep_loc, capture_methods_per_location, by= "locationID")
  .d<-left_join(.d, setup_per_location, by= "locationID")
  .d<-left_join(.d, Classify_per_location, by= "locationID")
  .d<-left_join(.d, bait_use_per_location, by= "locationID")
  .d<-left_join(.d, years_per_location, by= "locationID")
  .d<-left_join(.d, photos_per_location, by= "locationID")
  
  .d <- left_join(.d, species_per_location, by= "locationID")
  rm(species_per_location,photos_per_location,years_per_location,bait_use_per_location,Classify_per_location,
     setup_per_location,capture_methods_per_location,dep_loc,observations_with_taxonomy,sequence_to_deployment,deployments_per_location)
  .d
}
#############


.summarize_species <- function(cm,df, class = NULL,order=NULL,domestic=FALSE,scientificName=NULL,.filterCount=TRUE,observationType=NULL) {
  # To summarise, either scientificName, OR other criteria (one or combination of class, order, etc.) is provided!
  # .filterCount = T -> the count threshold (if specified in cm$filterCount) is applied
  
  if (!is.null(observationType) && is.character(observationType)) df <- df[df$observationType %in% observationType,]
  
  if (is.null(scientificName) || length(scientificName) == 0) {
    df <- df[grepl("\\s", df$scientificName),]
    df <- df[!grepl(" sp.$", df$scientificName),]
    
    
    # if domestic is FALSE, they are excluded, if TRUE, summary is for domestic (NULL: all are considered)
    # both only if the scientificName vector is provided by user in cm$filterExclude$scientificName
    # or when domestic group is defined in the group_definition!
    if (!is.null(cm$filterExclude$scientificName) && is.character(cm$filterExclude$scientificName)) {
      if (!is.null(domestic)) {
        if (domestic) {
          if ('domestic' %in% names(cm$group_definition)) df <- df[df$scientificName %in% cm$group_definition$domestic$scientificName,]
          else if (scientificName %in% names(cm$filterExclude)) df <- df[df$scientificName %in% cm$filterExclude$scientificName,]
          else warning('domestic group is not defined!')
        } else {
          if ('domestic' %in% names(cm$group_definition)) df <- df[!df$scientificName %in% cm$group_definition$domestic$scientificName,]
          else if (scientificName %in% names(cm$filterExclude)) df <- df[!df$scientificName %in% cm$filterExclude$scientificName,]
        }
      }
    }
    #---
    if (!is.null(class)) {
      df <- df[df$class %in% class, ]
    }
    #----
    if (!is.null(order)) {
      df <- df[df$order %in% order, ]
    }
  } else {
    df <- df[df$scientificName %in% scientificName,]
  }
  #--------------
  if (.filterCount && length(cm$filterCount) > 0 && nrow(cm$observed_counts) > 0) {
    df <- df[df$scientificName %in% cm$observed_counts$scientificName[cm$observed_counts$count > cm$filterCount[1]], ]
  }
  #---------
  .years <- unique(df$observation_Year)
  .years <- sort(.years[!is.na(.years)])
  
  .n <- .nn <- colnames(df)[grepl('^vernacularName',colnames(df))]
  if (length(.n) > 0) {
    .w <- which(sapply(.n,function(x) length(strsplit(x,'\\.')[[1]])) == 2)
    if (length(.w) > 0) {
      .nn[.w] <- paste0('species_list_',sapply(.n,function(x) strsplit(x,'\\.')[[1]][2]))
    }
  }
  
  if (length(.years) > 0) {
    .df  <- df[1:length(.years),c('observation_Year','count','scientificName',.n)]
    colnames(.df) <- c('observation_Year','total_species','species_list_scientificName',.nn)
    .df$total_observations <- 0
    .df$observation_Year <- as.numeric(.years)
    
    for (i in seq_along(.years)) {
      .w <- which(df$observation_Year == .years[i])
      .df$total_observations[i] <- length(.w)
      .df$total_species[i] <- length(unique(df$scientificName[.w]))
      .df$species_list_scientificName[i] <- paste(sort(unique(df$scientificName[.w])), collapse = ", ")
      if (length(.nn) > 0) {
        for (j in seq_along(.nn)) {
          .df[[.nn[j]]] <- paste(sort(unique(df[[.n[j]]][.w])), collapse = ", ")
        }
      }
    }
    #----
    
    .dfs <- unique(df[,c("scientificName", .n)])
    .dfs <- .dfs[order(.dfs$scientificName),]
    
    list(per_year=.df,site_list=.dfs)
  }
}
#---------
.make_render_env <- function(object = NULL) {
  env <- new.env(parent = getNamespace("camtrapReport"))
  env$object <- object
  env
}
#------


#####################*************############################
#       *********Data_status : Spatial********* 
#####################*************############################

.summarize_spatial <- function(cm, coord_round = 6) {
  
  ic <- .ct_icons()
  g <- ic$green; y <- ic$yellow; o <- ic$orange; r <- ic$red; w <- ic$warn; sea <- ic$sea
  
  
  out <- list(
    total_locationsrow  = NA_integer_,
    total_unique_locations = NA_integer_,
    number_missing_rows   = NA_integer_,
    message_missing = "Not computed",
    num_duplicated_coordinate = NA_integer_,
    status_duplicated_coordinate = "Not computed",
    num_dup_locationID = NA_integer_,
    status_dup_locationID = "Not computed",
    num_dup_locationName  = NA_integer_,
    status_dup_locationName = "Not computed",
    mean_distance_cam = NA_real_,
    min_distance_cam = NA_real_,
    max_distance_cam = NA_real_,
    min_distance_camNames = NA_character_,
    max_distance_camNames = NA_character_,
    num_lowrisk_outliers = 0L,
    num_mediumrisk_outliers = 0L,
    num_highrisk_outliers = 0L,
    num_sea_outliers  = 0L,
    outliers_status = "Not computed",
    spatial_pattern = NA_character_,
    status_spatial = "Not computed",
    MCArea = NA_real_,
    status_MCArea = "Not computed",
    country = NA_character_,
    Region_Top = NA_character_,
    Region_Mid = NA_character_,
    TimeZone = NA_character_,
    summary_country_timezone = "Not computed"
  )
  
  # ---- use cm$data$locations DIRECTLY ----
  dp_json <- cm$info$json
  # spatial_pattern from metadata if available
  sp <- NA_character_
  if (!is.null(dp_json) && !is.null(dp_json$project$samplingDesign)) {
    sp <- as.character(dp_json$project$samplingDesign)
  }
  
  if (length(sp) == 0 || is.na(sp) || !nzchar(trimws(sp))) {
    out$spatial_pattern <- "Not indicated in metadata"
  } else {
    out$spatial_pattern <- paste0(trimws(sp), " (indicated explicitly in metadata)")
  }
  
  # 1) initial cleaning
  out$total_locationsrow <- nrow(cm$data$locations)
  
  location_df2 <- cm$data$locations |>
    dplyr::mutate(
      row          = dplyr::row_number(),
      locationID   = dplyr::na_if(trimws(as.character(locationID)), ""),
      locationName = dplyr::na_if(trimws(as.character(locationName)), ""),
      longitude    = suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(longitude))))),
      latitude     = suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(latitude)))))
    )
  location_cleaned <- location_df2 |>
    dplyr::filter(stats::complete.cases(locationID, locationName, longitude, latitude))
  
  missing_rows <- setdiff(location_df2$row, location_cleaned$row)
  out$number_missing_rows <- length(missing_rows)
  out$message_missing <- if (length(missing_rows) == 0) {
    paste0(g, " No missing data found")
  } else {
    paste0(r, " ", length(missing_rows), " rows with missing data: [",
           paste(missing_rows, collapse = ", "), "]")
  }
  
  # 2) duplicated IDs / Names
  dup_msg <- function(n_groups, n_extra, icon, label) {
    if (n_extra > 0) sprintf("%s %d duplicated %s (%d extra rows).", icon, n_groups, label, n_extra)
    else sprintf("%s No duplicated %s", g, label)
  }
  
  dup_id <- location_cleaned |> dplyr::count(locationID, name = "n")
  out$num_dup_locationID <- sum(pmax(dup_id$n - 1, 0))
  out$status_dup_locationID <- dup_msg(sum(dup_id$n > 1), out$num_dup_locationID, r, "locationIDs")
  
  dup_nm <- location_cleaned |> dplyr::count(locationName, name = "n")
  out$num_dup_locationName <- sum(pmax(dup_nm$n - 1, 0))
  out$status_dup_locationName <- dup_msg(sum(dup_nm$n > 1), out$num_dup_locationName, o, "locationNames")
  
  # 3) duplicated coordinates (rounded)
  location_cleaned <- location_cleaned |>
    dplyr::mutate(
      lon_round = round(longitude, coord_round),
      lat_round = round(latitude,  coord_round)
    )
  
  coord_tab <- location_cleaned |> dplyr::count(lon_round, lat_round, name = "n")
  dup_coord_groups <- sum(coord_tab$n > 1)
  out$num_duplicated_coordinate <- sum(pmax(coord_tab$n - 1, 0))
  out$status_duplicated_coordinate <- if (dup_coord_groups > 0) {
    sprintf("%s Duplicate coordinates found in %d groups; %d duplicate rows.",
            r, dup_coord_groups, out$num_duplicated_coordinate)
  } else paste0(g, " No duplicated coordinates.")
  
  location_cleaned <- location_cleaned |>
    dplyr::group_by(lon_round, lat_round) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-lon_round, -lat_round)
  
  # 4) unique locations
  total_unique_locations_df <- location_cleaned |>
    dplyr::distinct(longitude, latitude, .keep_all = TRUE)
  out$total_unique_locations <- nrow(total_unique_locations_df)
  
  if (out$total_unique_locations <= 1) {
    out$note <- paste0(y, " Only one unique location — spatial analysis skipped.")
    return(out)
  }
  
  # 5) nearest-neighbour outliers
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
  
  out$mean_distance_cam <- round(outlier_res$mean_distance, 2)
  out$min_distance_cam <- round(outlier_res$min_distance, 2)
  out$max_distance_cam <- round(outlier_res$max_distance, 2)
  out$min_distance_camNames <- outlier_res$min_distance_names
  out$max_distance_camNames <- outlier_res$max_distance_names
  
  out$num_lowrisk_outliers <- length(outlier_res$low_prob)
  out$num_mediumrisk_outliers <- length(outlier_res$medium)
  out$num_highrisk_outliers <- length(outlier_res$high_prob)
  
  safe_names <- function(idxs) {
    if (!length(idxs)) return(character(0))
    sort(unique(na.omit(total_unique_locations_df$locationName[idxs])))
  }
  low_names  <- safe_names(outlier_res$low_prob)
  med_names  <- safe_names(outlier_res$medium)
  high_names <- safe_names(outlier_res$high_prob)
  
  distance_outlier_summary <- ""
  if (out$num_highrisk_outliers > 0)
    distance_outlier_summary <- paste0(distance_outlier_summary, r, " High-risk (",
                                       out$num_highrisk_outliers, "): ", paste(high_names, collapse = ", "))
  if (out$num_mediumrisk_outliers > 0)
    distance_outlier_summary <- paste0(distance_outlier_summary,
                                       if (nzchar(distance_outlier_summary)) " | " else "",
                                       o, " Medium-risk (", out$num_mediumrisk_outliers, "): ",
                                       paste(med_names, collapse = ", "))
  if (out$num_lowrisk_outliers > 0)
    distance_outlier_summary <- paste0(distance_outlier_summary,
                                       if (nzchar(distance_outlier_summary)) " | " else "",
                                       y, " Low-risk (", out$num_lowrisk_outliers, "): ",
                                       paste(low_names, collapse = ", "))
  if (!nzchar(distance_outlier_summary))
    distance_outlier_summary <- paste0(g, " No spatial outliers detected")
  
  # 6) sea vs land
  loc   <- vect(total_unique_locations_df, geom = c("longitude", "latitude"), crs = "epsg:4326")
  wrld  <- readRDS(system.file("external/world.map", package="camtrapReport"))
  loc$on_land <- !is.na(terra::extract( wrld[,'name'],loc)$name)
  num_sea_outliers <- sum(!loc$on_land)
  
  sea_outlier_status <- if (num_sea_outliers > 0) {
    paste0("🌊 ", num_sea_outliers, " location(s) fall in the sea.")
  } else {
    "🟢 All locations are on land."
  }
  
  out$outliers_status <- paste(distance_outlier_summary, sea_outlier_status, sep = " | ")
  #-----
  # 7. Minimum Convex Polygon (MCP) & area
  
  center_lon <- mean(total_unique_locations_df$longitude, na.rm = TRUE)
  is_northern <- mean(total_unique_locations_df$latitude, na.rm = TRUE) >= 0
  
  mcp_poly <- hull(.get_projected_vect(loc))
  out$MCArea <- expanse(mcp_poly,unit='km')
  
  
  out$status_MCArea <- paste0(
    "The ", out$total_unique_locations,
    " distinct camera locations are distributed within a minimum convex polygon (MCP) of ",
    round(out$MCArea, 2), " km²."
  )
  
  
  
  # 10. Country / Region / Timezone summary
  
  Country    <- out$country <- .paste_comma_and(unique(terra::extract(wrld, loc)$name))
  
  tzs  <- ""
  
  if (.require('lutz')) {
    tzs <- .eval('lutz::tz_lookup_coords(total_unique_locations_df$latitude,
                                  total_unique_locations_df$longitude,
                                  method = "accurate", warn = FALSE)',env=environment())
    tzs <- tzs[!is.na(tzs)]
  } else {
    
    tzs <- ""
    if (!is.null(cm$data$settings$tz)) tzs  <- cm$data$settings$tz
    
  }
  #-------
  
  if (length(tzs)) {
    tz <- names(sort(table(tzs), decreasing = TRUE))[1]
    mid <- Sys.time()
    z   <- format(as.POSIXct(mid, tz = tz), "%z")
    out$TimeZone <- paste0("UTC", substr(z,1,1), as.integer(substr(z,2,3)),
                           ifelse(substr(z,4,5)=="00","",paste0(":",substr(z,4,5))))
    tz_label <- paste0(tz, " (", format(as.POSIXct(mid, tz = tz), "%Z"), ", ", out$TimeZone, ")")
  } else {
    tz_label <- NA_character_
  }
  
  out$summary_country_timezone <- glue::glue(
    "Dataset spans <b>{out$country}</b> with time zone <b>{tz_label}</b>."
  )
  #----
  # 11. Spatial Pattern Detection (Clark‐Evans/K‐function)
  coords_xy <- total_unique_locations_df |> dplyr::distinct(longitude, latitude) |> na.omit()
  
  if (nrow(coords_xy) >= 9) {
    buffer_ratio <- 0.01
    xr <- range(coords_xy$longitude); yr <- range(coords_xy$latitude)
    xr <- xr + diff(xr) * c(-buffer_ratio, buffer_ratio)
    yr <- yr + diff(yr) * c(-buffer_ratio, buffer_ratio)
    
    if (.require('spatstat')) {
      win <- .eval("owin(xrange = xr, yrange = yr)",env=environment())
      ppp_obj <- .eval("ppp(x = coords_xy$longitude, y = coords_xy$latitude, window = win)",env=environment())
      
      qtest <- .eval("quadrat.test(ppp_obj, nx = 3, ny = 3)",env=environment())
      kres  <- .eval("Kest(ppp_obj, correction = 'iso')",env=environment())
      is_clustered <- is.finite(qtest$p.value) && (qtest$p.value < 0.05)
      is_regular   <- any(kres$iso < kres$theo, na.rm = TRUE)
      is_random    <- !is_clustered && !is_regular
    } else {
      is_clustered <- NULL
      is_regular   <- NULL
      is_random    <- NULL
    }
    
    
    out$status_spatial <- if (is_clustered && is_regular && is_random) {
      "Mixed: Clustered + Regular + Random (using point-pattern analysis)"
    } else if (is_clustered && is_regular) {
      "Mixed: Clustered + Regular (using point-pattern analysis)"
    } else if (is_clustered && is_random) {
      "Mixed: Clustered + Random (using point-pattern analysis)"
    } else if (is_regular && is_random) {
      "Mixed: Regular + Random (using point-pattern analysis)"
    } else if (is_clustered) {
      "Clustered (using point-pattern analysis)"
    } else if (is_regular) {
      "Regular / Possibly Linear (using point-pattern analysis)"
    } else if (is_random) {
      "Random (matches CSR) (using point-pattern analysis)"
    } else {
      "Ambiguous / Inconclusive (using point-pattern analysis)"
    }
  } else {
    out$status_spatial <- "⚠️ Too few locations to detect a spatial pattern"
  }
  
  cm$data_status$Spatial <- out
}


#####################*************############################
#       *********Data_status : Temporal*********
#####################*************############################
.Temporal <- function(cm) {
  
  
  ic <- .ct_icons()
  cm$data_status$Temporal <- list()
  #-------------------
  # Helpers
  
  .year4 <- function(x) {
    y <- substr(.trim_chr(x), 1, 4)
    y[!grepl("^[0-9]{4}$", y)] <- NA_character_
    y
  }
  
  .is_iso_prefix <- function(x) {
    grepl("^\\d{4}-\\d{2}-\\d{2}(\\s|T)", .trim_chr(x))
  }
  
  .as_date <- function(x) {
    x <- .trim_chr(x)
    x[!grepl("^\\d{4}-\\d{2}-\\d{2}", x)] <- NA_character_
    as.Date(substr(x, 1, 10))
  }
  
  .as_posix_utc <- function(x) {
    suppressWarnings(as.POSIXct(.trim_chr(x), tz = "UTC"))
  }
  
  # Parse deployment_interval -> start/end Date + deploymentID
  .parse_deployments <- function(deployments_df) {
    di <- .trim_chr(deployments_df$deployment_interval)
    parts <- strsplit(ifelse(is.na(di), "", di), "--", fixed = TRUE)
    
    start_raw <- vapply(parts, function(z) if (length(z) >= 1) trimws(z[1]) else NA_character_, character(1))
    end_raw   <- vapply(parts, function(z) if (length(z) >= 2) trimws(z[2]) else NA_character_, character(1))
    
    start_d <- .as_date(start_raw)
    end_d   <- .as_date(end_raw)
    
    ok <- !is.na(start_d) & !is.na(end_d)
    start_d <- start_d[ok]
    end_d   <- end_d[ok]
    depid   <- as.character(deployments_df$deploymentID[ok])
    
    swap <- which(end_d < start_d)
    if (length(swap)) {
      tmp <- start_d[swap]; start_d[swap] <- end_d[swap]; end_d[swap] <- tmp
    }
    
    data.frame(deploymentID = depid, start_d = start_d, end_d = end_d, stringsAsFactors = FALSE)
  }
  
  .covered_days <- function(ints) {
    if (!nrow(ints)) return(as.Date(character(0)))
    days <- as.Date(character(0))
    for (i in seq_len(nrow(ints))) {
      days <- c(days, seq(ints$start_d[i], ints$end_d[i], by = "day"))
    }
    unique(days)
  }
  
  .calendar_coverage <- function(days) {
    days <- unique(days[!is.na(days)])
    if (!length(days)) return(paste0("— ", ic$red))
    span_days <- as.integer(max(days) - min(days)) + 1L
    covered <- length(days)
    pct <- round(covered / span_days * 100, 1)
    sprintf("%d of %d days (%.1f%%)", covered, span_days, pct)
  }
  
  .gap_stats <- function(ints_simple) {
    if (nrow(ints_simple) < 2) {
      return(list(
        max_gap = paste("None", ic$green),
        min_gap = paste("None", ic$green),
        n_gaps = 0L
      ))
    }
    
    ints_simple <- ints_simple[order(ints_simple$end_d, ints_simple$start_d), ]
    
    next_start <- ints_simple$start_d[-1]
    this_end   <- ints_simple$end_d[-nrow(ints_simple)]
    gap_days   <- as.integer(next_start - this_end) - 1L
    
    pos <- which(!is.na(gap_days) & gap_days > 0)
    if (!length(pos)) {
      return(list(
        max_gap = paste0("0 days (no gaps) ", ic$green),
        min_gap = paste0("None (no gaps) ", ic$green),
        n_gaps = 0L
      ))
    }
    
    i_max <- pos[which.max(gap_days[pos])]
    max_gap <- sprintf("%d days (from %s to %s)", gap_days[i_max], this_end[i_max] + 1, next_start[i_max] - 1)
    
    if (length(pos) == 1) {
      min_gap <- paste0("Same as max gap (only one gap detected) ", ic$green)
      return(list(max_gap = max_gap, min_gap = min_gap, n_gaps = 1L))
    }
    
    i_min <- pos[which.min(gap_days[pos])]
    min_gap <- sprintf("%d days (from %s to %s)", gap_days[i_min], this_end[i_min] + 1, next_start[i_min] - 1)
    
    list(max_gap = max_gap, min_gap = min_gap, n_gaps = length(pos))
  }
  
  .missing_intervals <- function(deployment_interval) {
    x <- .trim_chr(deployment_interval)
    is_blank <- is.na(x) | !nzchar(x)
    
    parts <- strsplit(ifelse(is_blank, "", x), "--", fixed = TRUE)
    start_raw <- vapply(parts, function(z) if (length(z) >= 1) trimws(z[1]) else NA_character_, character(1))
    end_raw   <- vapply(parts, function(z) if (length(z) >= 2) trimws(z[2]) else NA_character_, character(1))
    
    start_d <- .as_date(start_raw)
    end_d   <- .as_date(end_raw)
    
    valid <- !is_blank & !is.na(start_d) & !is.na(end_d) & (end_d >= start_d)
    n_bad <- sum(!valid)
    
    if (n_bad == 0) paste("None", ic$green) else sprintf("%d invalid/empty interval(s) %s", n_bad, ic$red)
  }
  
  .temporal_outliers <- function(years_raw, max_gap = 10) {
    y <- .trim_chr(years_raw)
    y <- y[grepl("^[0-9]{4}$", y)]
    y <- sort(unique(as.integer(y)))
    y <- y[!is.na(y)]
    
    if (length(y) <= 1) return(paste("None", ic$green))
    
    jumps <- diff(y)
    cluster_id <- c(1L, 1L + cumsum(jumps > max_gap))
    clusters <- split(y, cluster_id)
    
    lens <- lengths(clusters)
    cand <- which(lens == max(lens))
    if (length(cand) > 1) {
      meds <- vapply(clusters[cand], stats::median, numeric(1))
      cand <- cand[which.max(meds)]
    } else cand <- cand[1]
    
    out_years <- setdiff(y, clusters[[cand]])
    if (!length(out_years)) paste("None", ic$green)
    else paste0("Years: ", paste(out_years, collapse = ", "), " ", ic$yellow)
  }
  
  .dep_zero_length <- function(ints) {
    if (!nrow(ints)) return(paste("None", ic$green))
    n_zero <- sum(ints$start_d == ints$end_d, na.rm = TRUE)
    if (n_zero == 0) paste("None", ic$green) else paste0(n_zero, " zero-length interval(s) ", ic$yellow)
  }
  
  # ---- missing years label compressor: "2000–2021, 2023, 2025"
  .missing_years_label <- function(miss_years_int) {
    y <- sort(unique(as.integer(miss_years_int)))
    y <- y[!is.na(y)]
    if (!length(y)) return("")
    runs <- split(y, cumsum(c(1, diff(y) != 1)))
    parts <- vapply(runs, function(r) {
      if (length(r) == 1) as.character(r[1]) else paste0(r[1], "–", r[length(r)])
    }, character(1))
    paste(parts, collapse = ", ")
  }
  
  .years_message <- function(years_chr) {
    years_chr <- sort(unique(years_chr[grepl("^[0-9]{4}$", years_chr)]))
    if (!length(years_chr)) return(paste0("— ", ic$red))
    
    yrs <- sort(unique(as.integer(years_chr)))
    rng <- seq(min(yrs), max(yrs), by = 1L)
    missing <- setdiff(rng, yrs)
    
    range_label <- paste0(min(yrs), " – ", max(yrs))
    
    if (!length(missing)) {
      paste0(range_label, " (", ic$green, " complete)")
    } else {
      paste0(range_label, " (", ic$red, " missing: ", .missing_years_label(missing), ")")
    }
  }
  
  # Month coverage helpers
  .month_abb <- function(m) month.abb[m]
  
  .months_to_label <- function(months_int) {
    months_int <- sort(unique(months_int))
    months_int <- months_int[!is.na(months_int) & months_int >= 1 & months_int <= 12]
    if (!length(months_int)) return("—")
    
    runs <- split(months_int, cumsum(c(1, diff(months_int) != 1)))
    parts <- vapply(runs, function(r) {
      if (length(r) == 1) .month_abb(r[1])
      else paste0(.month_abb(r[1]), "–", .month_abb(r[length(r)]))
    }, character(1))
    
    paste(parts, collapse = ", ")
  }
  
  .dep_month_coverage <- function(dep_ints, years_keep_chr) {
    years_keep_chr <- sort(unique(years_keep_chr[grepl("^[0-9]{4}$", years_keep_chr)]))
    if (!nrow(dep_ints) || !length(years_keep_chr)) {
      return(data.frame(Year = character(0), MonthSpan = character(0), stringsAsFactors = FALSE))
    }
    
    all_year <- character(0)
    all_mon  <- integer(0)
    
    for (i in seq_len(nrow(dep_ints))) {
      s <- as.Date(format(dep_ints$start_d[i], "%Y-%m-01"))
      e <- as.Date(format(dep_ints$end_d[i],   "%Y-%m-01"))
      seq_m <- seq(s, e, by = "month")
      
      yy <- format(seq_m, "%Y")
      mm <- as.integer(format(seq_m, "%m"))
      
      keep <- yy %in% years_keep_chr
      all_year <- c(all_year, yy[keep])
      all_mon  <- c(all_mon,  mm[keep])
    }
    
    if (!length(all_year)) {
      return(data.frame(Year = character(0), MonthSpan = character(0), stringsAsFactors = FALSE))
    }
    
    out <- data.frame(Year = years_keep_chr, MonthSpan = "—", stringsAsFactors = FALSE)
    for (yy in years_keep_chr) {
      m <- all_mon[all_year == yy]
      if (length(m)) out$MonthSpan[out$Year == yy] <- .months_to_label(m)
    }
    
    out[out$MonthSpan != "—", , drop = FALSE]
  }
  
  
  # A) Years covered (use BOTH start & end years)
  
  di <- .trim_chr(cm$data$deployments$deployment_interval)
  parts <- strsplit(ifelse(is.na(di), "", di), "--", fixed = TRUE)
  
  dep_start_year <- vapply(parts, function(z) if (length(z) >= 1) substr(trimws(z[1]), 1, 4) else NA_character_, character(1))
  dep_end_year   <- vapply(parts, function(z) if (length(z) >= 2) substr(trimws(z[2]), 1, 4) else NA_character_, character(1))
  
  cm$data_status$Temporal$dep_years <- sort(unique(c(dep_start_year, dep_end_year)))
  cm$data_status$Temporal$dep_years <- cm$data_status$Temporal$dep_years[grepl("^[0-9]{4}$", cm$data_status$Temporal$dep_years)]
  
  cm$data_status$Temporal$obs_years <- sort(unique(.year4(cm$data$observations$timestamp)))
  cm$data_status$Temporal$obs_years <- cm$data_status$Temporal$obs_years[!is.na(cm$data_status$Temporal$obs_years)]
  
  cm$data_status$Temporal$dep_years_message <- .years_message(cm$data_status$Temporal$dep_years)
  cm$data_status$Temporal$obs_years_message <- .years_message(cm$data_status$Temporal$obs_years)
  
  cm$data_status$Temporal$years_in_dep_not_obs <- sort(setdiff(cm$data_status$Temporal$dep_years, cm$data_status$Temporal$obs_years))
  cm$data_status$Temporal$years_in_obs_not_dep <- sort(setdiff(cm$data_status$Temporal$obs_years, cm$data_status$Temporal$dep_years))
  
  cm$data_status$Temporal$temporal_inconsistency <- {
    dep_not_obs <- cm$data_status$Temporal$years_in_dep_not_obs
    obs_not_dep <- cm$data_status$Temporal$years_in_obs_not_dep
    
    if (length(dep_not_obs) == 0 && length(obs_not_dep) == 0) {
      paste0(ic$green, " Years in observations and deployments are the same")
    } else {
      parts2 <- character(0)
      if (length(dep_not_obs)) parts2 <- c(parts2, paste0("Deployments exist, but observations are missing for: ", paste(dep_not_obs, collapse = ", ")))
      if (length(obs_not_dep)) parts2 <- c(parts2, paste0("Observations exist, but deployments are missing for: ", paste(obs_not_dep, collapse = ", ")))
      paste0(ic$red, " Temporal inconsistency (", paste(parts2, collapse = " | "), ")")
    }
  }
  
  # B) First/Last deployments & observations + last day of last deployment + message
  
  dep_start_posix <- suppressWarnings(as.POSIXct(cm$data$deployments$deploymentStart, tz = "UTC"))
  dep_end_posix   <- suppressWarnings(as.POSIXct(cm$data$deployments$deploymentEnd,   tz = "UTC"))
  obs_time_posix  <- .as_posix_utc(cm$data$observations$timestamp)
  
  dep_min      <- suppressWarnings(min(dep_start_posix, na.rm = TRUE))
  dep_max      <- suppressWarnings(max(dep_start_posix, na.rm = TRUE))   # latest setup/start
  dep_end_last <- suppressWarnings(max(dep_end_posix,   na.rm = TRUE))   # latest end
  
  obs_min <- suppressWarnings(min(obs_time_posix, na.rm = TRUE))
  obs_max <- suppressWarnings(max(obs_time_posix, na.rm = TRUE))
  
  # guard against all-NA cases (min/max -> Inf/-Inf)
  fix_inf <- function(x) if (!is.finite(x)) NA else x
  dep_min <- fix_inf(dep_min)
  dep_max <- fix_inf(dep_max)
  dep_end_last <- fix_inf(dep_end_last)
  obs_min <- fix_inf(obs_min)
  obs_max <- fix_inf(obs_max)
  
  cm$data_status$Temporal$dep_end_last <- dep_end_last
  cm$data_status$Temporal$dep_first_last_setup <- paste(dep_min, dep_max, sep = " – ")
  cm$data_status$Temporal$obs_first_last <- paste(obs_min, obs_max, sep = " – ")
  
  
  if (is.finite(dep_min) && is.finite(obs_min) && obs_min < dep_min) {
    cm$data_status$Temporal$message_first_last <- paste0(
      ic$red, " Earliest observation is earlier than the first deployment start date (check timestamps or timezone). ", ic$alarm
    )
  } else if (is.finite(dep_min) && is.finite(obs_min)) {
    cm$data_status$Temporal$message_first_last <- paste0(ic$green, " All observations are on/after the first deployment start.")
  } else {
    cm$data_status$Temporal$message_first_last <- paste0(ic$yellow, " Cannot compare first/last dates (missing/invalid timestamps).")
  }
  
  # C) Deployments: calendar coverage, gaps, missing intervals, outliers, zero-length
  
  dep_ints <- .parse_deployments(cm$data$deployments)
  dep_days <- .covered_days(dep_ints)
  
  cm$data_status$Temporal$dep_calendar_coverage <- .calendar_coverage(dep_days)
  
  gap <- .gap_stats(dep_ints[, c("start_d", "end_d")])
  cm$data_status$Temporal$dep_max_gap <- gap$max_gap
  cm$data_status$Temporal$dep_min_gap <- gap$min_gap
  
  cm$data_status$Temporal$dep_missing_intervals <- .missing_intervals(cm$data$deployments$deployment_interval)
  cm$data_status$Temporal$temporal_outliers <- .temporal_outliers(cm$data_status$Temporal$dep_years, max_gap = 10)
  cm$data_status$Temporal$dep_zero_length <- .dep_zero_length(dep_ints)
  
  cm$data_status$Temporal$dep_month_coverage <- .dep_month_coverage(
    dep_ints = dep_ints,
    years_keep_chr = cm$data_status$Temporal$dep_years
  )
  
  cm$data_status$Temporal$dep_month_coverage_lines <- if (nrow(cm$data_status$Temporal$dep_month_coverage) == 0) {
    character(0)
  } else {
    paste0(cm$data_status$Temporal$dep_month_coverage$Year, ": ", cm$data_status$Temporal$dep_month_coverage$MonthSpan)
  }
  
  # D) Observations QA: invalid format + future timestamps
  
  ts_raw <- cm$data$observations$timestamp
  bad_fmt_idx <- which(!is.na(ts_raw) & nzchar(.trim_chr(ts_raw)) & !.is_iso_prefix(ts_raw))
  
  cm$data_status$Temporal$invalid_timestamp_format <- if (!length(bad_fmt_idx)) {
    paste("None", ic$green)
  } else {
    paste0(length(bad_fmt_idx), " timestamp(s) have invalid format ", ic$red,
           " (rows: ", paste(head(bad_fmt_idx, 10), collapse = ", "),
           if (length(bad_fmt_idx) > 10) ", ..." else "", ")")
  }
  
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  fut_idx <- which(!is.na(obs_time_posix) & obs_time_posix > now_utc)
  
  cm$data_status$Temporal$obs_future_timestamps <- if (!length(fut_idx)) {
    paste("None", ic$green)
  } else {
    paste0(length(fut_idx), " observation(s) have future timestamps ", ic$red,
           " (rows: ", paste(head(fut_idx, 10), collapse = ", "),
           if (length(fut_idx) > 10) ", ..." else "", ")")
  }
  
}

# Run:
#####################*************############################
#       *********Data_status : Essential*********
#####################*************############################
.Essentials <- function(cm) {
  
  
  ic <- .ct_icons()
  g <- ic$green; y <- ic$yellow; r <- ic$red
  
  # reset
  cm$data_status$Essentials <- list(
    loc   = list(),
    obs   = list(),
    dep   = list(),
    media = list(),
    seq   = list(),
    tax   = list()
  )
  
  
  # helpers
  
  .row_list <- function(idx, max_show = 20) {
    idx <- sort(unique(idx))
    if (!length(idx)) return("")
    shown <- head(idx, max_show)
    s <- paste(shown, collapse = ", ")
    if (length(idx) > max_show) s <- paste0(s, ", ...")
    s
  }
  
  .status_counts <- function(missing_n, total) {
    if (is.na(total) || total <= 0) return(paste0(y, " No data"))
    pct <- 100 * missing_n / total
    if (missing_n <= 0) {
      paste0(g, " Complete")
    } else if (missing_n >= total) {
      sprintf("%s Incomplete (%d of %d missing; 100%%)", r, missing_n, total)
    } else {
      sprintf("%s Partial (%d of %d missing; %.2f%%)", y, missing_n, total, pct)
    }
  }
  
  .summ_status_counts <- function(x, treat_blank = TRUE) {
    x <- .trim_chr(x)
    total <- length(x)
    missing_n <- sum(is.na(x) | (treat_blank & x == ""))
    .status_counts(missing_n, total)
  }
  
  .status_rows <- function(bad_idx, total, all_empty = FALSE) {
    if (is.na(total) || total <= 0) return(paste0(y, " No data"))
    if (!length(bad_idx)) return(paste0(g, " Complete"))
    if (length(bad_idx) == total && all_empty) return(paste0(r, " Incomplete (all rows empty)"))
    if (length(bad_idx) == total) return(paste0(r, " Incomplete (all rows missing/invalid)"))
    paste0(y, " Partial (row: ", .row_list(bad_idx), ")")
  }
  
  .loc_chr_status <- function(x) {
    x <- .trim_chr(x)
    miss <- which(is.na(x) | x == "")
    .status_rows(miss, length(x), all_empty = (length(miss) == length(x)))
  }
  
  .loc_num_status <- function(x, minv, maxv) {
    x_chr <- .trim_chr(x)
    total <- length(x_chr)
    
    empty_idx  <- which(is.na(x_chr) | x_chr == "")
    x_num      <- suppressWarnings(as.numeric(x_chr))
    nonnum_idx <- which(!is.na(x_chr) & x_chr != "" & is.na(x_num))
    range_idx  <- which(!is.na(x_num) & (x_num < minv | x_num > maxv))
    bad_idx    <- sort(unique(c(empty_idx, nonnum_idx, range_idx)))
    
    list(
      status = .status_rows(bad_idx, total, all_empty = (length(empty_idx) == total)),
      num    = x_num
    )
  }
  
  .crs_note <- function(lon_num, lat_num) {
    pair_idx <- which(!is.na(lon_num) & !is.na(lat_num))
    if (!length(pair_idx)) return("")
    deg_like <- abs(lon_num[pair_idx]) <= 180 & abs(lat_num[pair_idx]) <= 90
    if (all(deg_like) || all(!deg_like)) return("")
    idx_deg    <- pair_idx[deg_like]
    idx_nondeg <- pair_idx[!deg_like]
    diff_rows  <- if (length(idx_deg) < length(idx_nondeg)) idx_deg else idx_nondeg
    plural <- if (length(diff_rows) == 1) "row" else "rows"
    paste0(" | ", plural, " ", .row_list(diff_rows), " use a different coordinate system")
  }
  
  .is_iso_prefix <- function(x) grepl("^\\d{4}-\\d{2}-\\d{2}(\\s|T)", .trim_chr(x))
  
  .timestamp_status <- function(x) {
    x_chr <- .trim_chr(x)
    total <- length(x_chr)
    
    missing_n <- sum(is.na(x_chr) | x_chr == "")
    non_missing <- !is.na(x_chr) & x_chr != ""
    invalid_fmt_n <- sum(non_missing & !.is_iso_prefix(x_chr))
    
    x_posix <- suppressWarnings(as.POSIXct(x_chr, tz = "UTC"))
    future_n <- sum(!is.na(x_posix) & x_posix > as.POSIXct(Sys.time(), tz = "UTC"))
    
    status <- .status_counts(missing_n + invalid_fmt_n, total)
    
    note <- character(0)
    if (invalid_fmt_n > 0) note <- c(note, paste0("invalid format: ", invalid_fmt_n))
    if (future_n > 0)      note <- c(note, paste0("future: ", future_n))
    if (length(note)) status <- paste0(status, " | ", paste(note, collapse = "; "))
    
    status
  }
  
  .dep_interval_status <- function(x) {
    x <- .trim_chr(x)
    total <- length(x)
    missing_n <- sum(is.na(x) | x == "")
    
    parts <- strsplit(ifelse(is.na(x), "", x), "--", fixed = TRUE)
    start_raw <- vapply(parts, function(z) if (length(z) >= 1) trimws(z[1]) else NA_character_, character(1))
    end_raw   <- vapply(parts, function(z) if (length(z) >= 2) trimws(z[2]) else NA_character_, character(1))
    
    start_ok <- grepl("^\\d{4}-\\d{2}-\\d{2}", start_raw)
    end_ok   <- grepl("^\\d{4}-\\d{2}-\\d{2}", end_raw)
    
    start_d <- suppressWarnings(as.Date(substr(start_raw, 1, 10)))
    end_d   <- suppressWarnings(as.Date(substr(end_raw,   1, 10)))
    
    valid <- !is.na(x) & x != "" & start_ok & end_ok &
      !is.na(start_d) & !is.na(end_d) & (end_d >= start_d)
    
    invalid_n <- sum(!valid & !(is.na(x) | x == ""))
    
    .status_counts(missing_n + invalid_n, total)
  }
  
  .present_chr <- function(x) {
    x <- .trim_chr(x)
    sum(!is.na(x) & x != "")
  }
  .present_num <- function(x) {
    x_num <- suppressWarnings(as.numeric(.trim_chr(x)))
    sum(!is.na(x_num))
  }
  
  .animal_field_status <- function(present_n, total_animals, label) {
    if (total_animals <= 0) return(paste0(y, " No animal observations"))
    pct <- 100 * present_n / total_animals
    if (present_n == 0) {
      sprintf("%s Incomplete (%s recorded for 0 of %d animals; 0%%)", r, label, total_animals)
    } else if (present_n == total_animals) {
      sprintf("%s Complete (%s recorded for %d of %d animals; 100%%)", g, label, total_animals, total_animals)
    } else {
      sprintf("%s Partial (%s recorded for %d of %d animals; %.2f%%)", y, label, present_n, total_animals, pct)
    }
  }
  
  
  # LOCATIONS 
  
  if (!("locations" %in% names(cm$data)) || !is.data.frame(cm$data$locations)) {
    cm$data_status$Essentials$loc$long     <- paste0(r, " Incomplete (missing cm$data$locations)")
    cm$data_status$Essentials$loc$lat      <- paste0(r, " Incomplete (missing cm$data$locations)")
    cm$data_status$Essentials$loc$locID    <- paste0(r, " Incomplete (missing cm$data$locations)")
    cm$data_status$Essentials$loc$locnName <- paste0(r, " Incomplete (missing cm$data$locations)")
  } else {
    col_lon <- .pick_col(cm$data$locations, c("longitude","long","lon"))
    col_lat <- .pick_col(cm$data$locations, c("latitude","lat"))
    col_id  <- .pick_col(cm$data$locations, c("locationID","locID"))
    col_nm  <- .pick_col(cm$data$locations, c("locationName","locnName","locationnName"))
    
    cm$data_status$Essentials$loc$locID <-
      if (is.na(col_id)) paste0(r, " Incomplete (missing locationID/locID column)")
    else .loc_chr_status(cm$data$locations[[col_id]])
    
    cm$data_status$Essentials$loc$locnName <-
      if (is.na(col_nm)) paste0(r, " Incomplete (missing locationName column)")
    else .loc_chr_status(cm$data$locations[[col_nm]])
    
    if (!is.na(col_lon) && !is.na(col_lat)) {
      lon_stat <- .loc_num_status(cm$data$locations[[col_lon]], -180, 180)
      lat_stat <- .loc_num_status(cm$data$locations[[col_lat]],  -90,  90)
      note <- .crs_note(lon_stat$num, lat_stat$num)
      cm$data_status$Essentials$loc$long <- paste0(lon_stat$status, note)
      cm$data_status$Essentials$loc$lat  <- paste0(lat_stat$status,  note)
    } else {
      cm$data_status$Essentials$loc$long <-
        if (is.na(col_lon)) paste0(r, " Incomplete (missing longitude column)")
      else .loc_num_status(cm$data$locations[[col_lon]], -180, 180)$status
      
      cm$data_status$Essentials$loc$lat <-
        if (is.na(col_lat)) paste0(r, " Incomplete (missing latitude column)")
      else .loc_num_status(cm$data$locations[[col_lat]],  -90,  90)$status
    }
  }
  
  
  # OBSERVATIONS
  
  if (!("observations" %in% names(cm$data)) || !is.data.frame(cm$data$observations)) {
    cm$data_status$Essentials$obs$status <- paste0(r, " Missing table: cm$data$observations")
  } else {
    
    col_ts <- .pick_col(cm$data$observations, c("timestamp","observation_timestamp","eventStart"))
    cm$data_status$Essentials$obs$timestamp <-
      if (is.na(col_ts)) paste0(r, " Missing column: timestamp")
    else .timestamp_status(cm$data$observations[[col_ts]])
    
    col_ot <- .pick_col(cm$data$observations, c("observationType","obsType"))
    if (is.na(col_ot)) {
      cm$data_status$Essentials$obs$obsType_status <- paste0(r, " Missing column: observationType/obsType")
    } else {
      x <- cm$data$observations[[col_ot]]
      cm$data_status$Essentials$obs$obsType_table  <- table(x, useNA = "ifany")
      total <- length(x)
      n_unclassified <- sum(tolower(.trim_chr(x)) == "unclassified", na.rm = TRUE)
      n_unknown      <- sum(tolower(.trim_chr(x)) == "unknown",      na.rm = TRUE)
      cm$data_status$Essentials$obs$obsType_status <- .status_counts(n_unclassified + n_unknown, total)
    }
    
    col_count <- .pick_col(cm$data$observations, c("count","observationCount"))
    cm$data_status$Essentials$obs$count <-
      if (is.na(col_count)) paste0(r, " Missing column: count")
    else .summ_status_counts(cm$data$observations[[col_count]], treat_blank = FALSE)
    
    col_cb <- .pick_col(cm$data$observations, c("classifiedBy"))
    if (is.na(col_cb)) {
      cm$data_status$Essentials$obs$classifiedBy_status <- paste0(r, " Missing column: classifiedBy")
    } else {
      x_all <- cm$data$observations[[col_cb]]
      cm$data_status$Essentials$obs$classifiedBy_table <- table(x_all, useNA = "ifany")
      
      x_use <- x_all
      if ("classificationMethod" %in% names(cm$data$observations)) {
        filt <- cm$data$observations$classificationMethod %in% c("human","machine")
        filt[is.na(filt)] <- FALSE
        x_use <- x_all[filt]
      }
      cm$data_status$Essentials$obs$classifiedBy_status <- .summ_status_counts(x_use, treat_blank = TRUE)
    }
    
    # Animal-only benchmark fields
    if (is.na(col_ot)) {
      msg <- paste0(r, " Missing column: observationType (cannot filter animals)")
      for (k in c("taxonID","behavior","sex","lifeStage","angle","radius","speed","individualID")) {
        cm$data_status$Essentials$obs[[k]] <- msg
      }
    } else {
      idx_animal <- tolower(.trim_chr(cm$data$observations[[col_ot]])) == "animal"
      idx_animal[is.na(idx_animal)] <- FALSE
      n_animal <- sum(idx_animal)
      
      # taxonID: animals + unique
      col_tax <- .pick_col(cm$data$observations, c("taxonID"))
      if (is.na(col_tax)) {
        cm$data_status$Essentials$obs$taxonID <- paste0(r, " Missing column: taxonID")
      } else {
        x_tax <- .trim_chr(cm$data$observations[[col_tax]][idx_animal])
        present_n <- sum(!is.na(x_tax) & x_tax != "")
        uniq <- length(unique(x_tax[!is.na(x_tax) & x_tax != ""]))
        cm$data_status$Essentials$obs$taxonID <- paste0(
          .animal_field_status(present_n, n_animal, "taxonID"),
          " | ", uniq, " unique"
        )
      }
      
      .set_animal_field <- function(key, candidates, label, type = c("chr","num")) {
        type <- match.arg(type)
        col <- .pick_col(cm$data$observations, candidates)
        if (is.na(col)) {
          cm$data_status$Essentials$obs[[key]] <<- paste0(r, " Missing column: ", paste(candidates, collapse = " / "))
          return()
        }
        x <- cm$data$observations[[col]][idx_animal]
        present_n <- if (type == "num") .present_num(x) else .present_chr(x)
        cm$data_status$Essentials$obs[[key]] <<- .animal_field_status(present_n, n_animal, label)
      }
      
      .set_animal_field("behavior", c("behavior"), "behavior","chr")
      .set_animal_field("sex", c("sex"),"sex", "chr")
      .set_animal_field("lifeStage", c("lifeStage"), "lifeStage", "chr")
      .set_animal_field("angle", c("individualPositionAngle","angle"), "angle", "num")
      .set_animal_field("radius", c("individualPositionRadius","radius"), "radius", "num")
      .set_animal_field("speed", c("individualSpeed","speed"), "speed", "num")
      .set_animal_field("individualID", c("individualID"), "individualID", "chr")
    }
  }
  #------
  # DEPLOYMENTS 
  
  if (!("deployments" %in% names(cm$data)) || !is.data.frame(cm$data$deployments)) {
    cm$data_status$Essentials$dep$status <- paste0(r, " Missing table: cm$data$deployments")
  } else {
    add_dep <- function(key, candidates, kind = c("counts","timestamp","dep_interval"), treat_blank = TRUE) {
      kind <- match.arg(kind)
      col <- .pick_col(cm$data$deployments, candidates)
      if (is.na(col)) {
        cm$data_status$Essentials$dep[[key]] <<- paste0(r, " Missing column: ", paste(candidates, collapse=" / "))
        return()
      }
      cm$data_status$Essentials$dep[[key]] <<- if (kind == "timestamp") {
        .timestamp_status(cm$data$deployments[[col]])
      } else if (kind == "dep_interval") {
        .dep_interval_status(cm$data$deployments[[col]])
      } else {
        .summ_status_counts(cm$data$deployments[[col]], treat_blank = treat_blank)
      }
    }
    
    add_dep("depID", c("deploymentID","depID"), "counts", TRUE)
    add_dep("locID", c("locationID","locID"), "counts", TRUE)
    add_dep("baitUse", c("baitUse"), "counts", TRUE)
    add_dep("cameraHeight", c("cameraHeight"),"counts", FALSE)
    add_dep("habitat", c("habitat"), "counts", TRUE)
    add_dep("dep_interval", c("deployment_interval","dep_interval"), "dep_interval")
    add_dep("depStart", c("deploymentStart","depStart"), "timestamp")
    add_dep("depEnd", c("deploymentEnd","depEnd"),"timestamp")
    
    col_sb <- .pick_col(cm$data$deployments, c("setupBy"))
    if (is.na(col_sb)) {
      cm$data_status$Essentials$dep$setupBy_status <- paste0(r, " Missing column: setupBy")
    } else {
      cm$data_status$Essentials$dep$setupBy_table  <- table(cm$data$deployments[[col_sb]], useNA = "ifany")
      cm$data_status$Essentials$dep$setupBy_status <- .summ_status_counts(cm$data$deployments[[col_sb]], treat_blank = TRUE)
    }
  }
  #----
  
  # MEDIA 
  if ("media" %in% names(cm$data) && is.data.frame(cm$data$media)) {
    add_media <- function(key, candidates, kind = c("counts","timestamp"), treat_blank = TRUE) {
      kind <- match.arg(kind)
      col <- .pick_col(cm$data$media, candidates)
      if (is.na(col)) {
        cm$data_status$Essentials$media[[key]] <- paste0(r, " Missing column: ", paste(candidates, collapse=" / "))
        return()
      }
      cm$data_status$Essentials$media[[key]] <- if (kind == "timestamp") {
        .timestamp_status(cm$data$media[[col]])
      } else {
        .summ_status_counts(cm$data$media[[col]], treat_blank = treat_blank)
      }
    }
    
    add_media("comments", c("comments"), "counts", TRUE)
    add_media("favourite", c("favourite","favorite"), "counts", FALSE)
    add_media("file.path", c("filePath","file.path","file_path"), "counts", TRUE)
    add_media("timestamp", c("timestamp"), "timestamp", TRUE)
  }
  #--------
  # SEQUENCES 
  
  if ("sequences" %in% names(cm$data) && is.data.frame(cm$data$sequences)) {
    add_seq <- function(key, candidates, treat_blank = TRUE) {
      col <- .pick_col(cm$data$sequences, candidates)
      if (is.na(col)) {
        cm$data_status$Essentials$seq[[key]] <- paste0(r, " Missing column: ", paste(candidates, collapse=" / "))
      } else {
        cm$data_status$Essentials$seq[[key]] <- .summ_status_counts(cm$data$sequences[[col]], treat_blank = treat_blank)
      }
    }
    add_seq("captureMethod", c("captureMethod"), TRUE)
    add_seq("nrphotos", c("nrphotos"), FALSE)
  }
  #-----
  # TAXONOMY 
  
  if ("taxonomy" %in% names(cm$data) && is.data.frame(cm$data$taxonomy)) {
    
    col_tid <- .pick_col(cm$data$taxonomy, c("taxonID"))
    if (is.na(col_tid)) {
      cm$data_status$Essentials$tax$taxonID <- paste0(r, " Missing column: taxonID")
    } else {
      x <- .trim_chr(cm$data$taxonomy[[col_tid]])
      keep <- !is.na(x) & x != ""
      uniq <- length(unique(x[keep]))
      cm$data_status$Essentials$tax$taxonID <- paste0(uniq, " unique taxonID identified")
    }
    
    add_tax <- function(key, candidates) {
      col <- .pick_col(cm$data$taxonomy, candidates)
      if (is.na(col)) {
        cm$data_status$Essentials$tax[[key]] <- paste0(r, " Missing column: ", paste(candidates, collapse=" / "))
      } else {
        cm$data_status$Essentials$tax[[key]] <- .summ_status_counts(cm$data$taxonomy[[col]], treat_blank = TRUE)
      }
    }
    
    add_tax("scientificName", c("scientificName"))
    add_tax("eng", c("eng", "vernacularNames.eng"))
    add_tax("nld", c("nld", "vernacularNames.nld"))
  }
}

#####################*************############################
#       *********Data_status : Annotation *********
#####################*************############################
.Annotation <- function(cm, machine_q = 0.10, human_q = 0.10) {
  
  ic <- .ct_icons()
  icon_green <- ic$green
  icon_red   <- ic$red
  
  # reset
  cm$data_status$Annotation <- list(
    Machine_q         = machine_q,
    Machine_Threshold = NA_real_,
    Machine_Summary   = NULL,
    Human_q           = human_q,
    Human_Threshold   = NA_real_,
    Human_Summary     = NULL,
    Status            = paste0(icon_green, " OK")
  )
  
  # required columns
  req <- c("classificationMethod", "classificationConfidence")
  if (!all(req %in% names(cm$data$observations))) {
    cm$data_status$Annotation$Status <- paste0(
      icon_red, " Missing required columns: ",
      paste(req[!(req %in% names(cm$data$observations))], collapse = ", ")
    )
    return(NULL)
  }
  
  .safe_quantile <- function(x, q) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    as.numeric(stats::quantile(x, probs = q, na.rm = TRUE, names = FALSE, type = 7))
  }
  
  .make_summary <- function(confs, thr) {
    confs <- confs[!is.na(confs)]
    if (!length(confs)) {
      return(data.frame(
        Statistic = c("Minimum", "Maximum", "Mean", "BelowThreshold", "Total"),
        Value = c(NA, NA, NA, NA, 0),
        stringsAsFactors = FALSE
      ))
    }
    
    data.frame(
      Statistic = c(
        "Minimum", "Maximum", "Mean",
        paste0("BelowThreshold(<", round(thr, 2), ")"),
        "Total"
      ),
      Value = c(
        round(min(confs), 2),
        round(max(confs), 2),
        round(mean(confs), 2),
        if (is.na(thr)) NA_integer_ else sum(confs < thr),
        length(confs)
      ),
      stringsAsFactors = FALSE
    )
  }
  
  conf_m <- cm$data$observations$classificationConfidence[
    cm$data$observations$classificationMethod == "machine" &
      !is.na(cm$data$observations$classificationConfidence)
  ]
  
  conf_h <- cm$data$observations$classificationConfidence[
    cm$data$observations$classificationMethod == "human" &
      !is.na(cm$data$observations$classificationConfidence)
  ]
  
  thr_m <- .safe_quantile(conf_m, machine_q)
  thr_h <- .safe_quantile(conf_h, human_q)
  
  cm$data_status$Annotation$Machine_Threshold <- thr_m
  cm$data_status$Annotation$Machine_Summary   <- .make_summary(conf_m, thr_m)
  
  cm$data_status$Annotation$Human_Threshold <- thr_h
  cm$data_status$Annotation$Human_Summary   <- .make_summary(conf_h, thr_h)
}
#####################*************############################
#       *********Data_status : Validation *********
#####################*************############################
.Validation <- function(cm, prob_tol = 1e-8) {
  
  cm$data_status$Validation <- list()
  
  # required columns (observations)
  col_method <- .pick_col(cm$data$observations, c("classificationMethod"))
  col_type   <- .pick_col(cm$data$observations, c("observationType", "obsType"))
  col_prob   <- .pick_col(cm$data$observations, c("classificationProbability", "classificationConfidence"))
  col_seq    <- .pick_col(cm$data$observations, c("sequenceID", "sequID"))
  
  miss <- character(0)
  if (is.na(col_method)) miss <- c(miss, "classificationMethod")
  if (is.na(col_type)) miss <- c(miss, "observationType/obsType")
  if (is.na(col_prob)) miss <- c(miss, "classificationProbability/classificationConfidence")
  
  if (length(miss)) {
    cm$data_status$Validation$status <- paste0(
      "Missing required columns in observations: ", paste(miss, collapse = ", ")
    )
    cm$data_status$Validation$ClassificationSummary <- NULL
    cm$data_status$Validation$ValidationSummary <- NULL
    cm$data_status$Validation$FinalTableFormatted <- NULL
    return(NULL)
  }
  
  
  # captureMethod
  
  captureMethod <- rep(NA_character_, nrow(cm$data$observations))
  
  col_cap_obs <- .pick_col(cm$data$observations, c("captureMethod"))
  if (!is.na(col_cap_obs)) {
    captureMethod <- .trim_chr(cm$data$observations[[col_cap_obs]])
  } else {
    # sequences join
    if (!is.na(col_seq) && "sequences" %in% names(cm$data) && is.data.frame(cm$data$sequences)) {
      col_seq_seq <- .pick_col(cm$data$sequences, c("sequenceID", "sequID"))
      col_cap_seq <- .pick_col(cm$data$sequences, c("captureMethod"))
      if (!is.na(col_seq_seq) && !is.na(col_cap_seq)) {
        m <- match(.trim_chr(cm$data$observations[[col_seq]]), .trim_chr(cm$data$sequences[[col_seq_seq]]))
        captureMethod <- .trim_chr(cm$data$sequences[[col_cap_seq]][m])
      }
    }
    
    # media fallback
    if (all(is.na(captureMethod)) && !is.na(col_seq) && "media" %in% names(cm$data) && is.data.frame(cm$data$media)) {
      col_seq_med <- .pick_col(cm$data$media, c("sequenceID", "sequID"))
      col_cap_med <- .pick_col(cm$data$media, c("captureMethod"))
      if (!is.na(col_seq_med) && !is.na(col_cap_med)) {
        key <- .trim_chr(cm$data$media[[col_seq_med]])
        first_idx <- !duplicated(key)
        m <- match(.trim_chr(cm$data$observations[[col_seq]]), key[first_idx])
        captureMethod <- .trim_chr(cm$data$media[[col_cap_med]][first_idx][m])
      }
    }
  }
  
  captureMethod[is.na(captureMethod) | captureMethod == ""] <- "UNKNOWN"
  
  # derived fields
  method <- .trim_chr(cm$data$observations[[col_method]])
  method[method == ""] <- NA_character_
  
  type_raw <- tolower(.trim_chr(cm$data$observations[[col_type]]))
  is_animal <- !is.na(type_raw) & (type_raw == "animal" | grepl("\\banimal\\b", type_raw))
  
  prob <- suppressWarnings(as.numeric(.trim_chr(cm$data$observations[[col_prob]])))
  
  # VALIDATION RULE:
  # currently: validated == (prob == 1)
  is_validated <- function(p) !is.na(p) & abs(p - 1) <= prob_tol
  
  
  # Classification summary
  
  cm_levels <- sort(unique(captureMethod))
  
  classification_summary <- data.frame(
    captureMethod = cm_levels,
    Human = 0L,
    Machine = 0L,
    NA_Classification = 0L,
    Total = 0L,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(cm_levels)) {
    cm_i <- cm_levels[i]
    idx <- captureMethod == cm_i
    classification_summary$Human[i] <- sum(method[idx] == "human",   na.rm = TRUE)
    classification_summary$Machine[i] <- sum(method[idx] == "machine", na.rm = TRUE)
    classification_summary$NA_Classification[i] <- sum(is.na(method[idx]))
    classification_summary$Total[i] <- sum(idx)
  }
  
  classification_summary <- rbind(
    classification_summary,
    data.frame(
      captureMethod = "TOTAL",
      Human = sum(classification_summary$Human),
      Machine = sum(classification_summary$Machine),
      NA_Classification = sum(classification_summary$NA_Classification),
      Total = sum(classification_summary$Total),
      stringsAsFactors = FALSE
    )
  )
  
  
  # Validation summary (machine only)
  
  validation_summary <- data.frame(
    captureMethod = cm_levels,
    Machine_Animal = 0L,
    Validated_Animal = 0L,
    Machine_Animal_pr = NA_real_,
    Validated_Animal_pr = NA_real_,
    stringsAsFactors  = FALSE
  )
  
  for (i in seq_along(cm_levels)) {
    cm_i <- cm_levels[i]
    idx_cm <- captureMethod == cm_i
    idx_m <- idx_cm & (method == "machine")
    
    m_animal <- sum(idx_m & is_animal, na.rm = TRUE)
    v_animal <- sum(idx_m & is_animal & is_validated(prob), na.rm = TRUE)
    
    validation_summary$Machine_Animal[i] <- m_animal
    validation_summary$Validated_Animal[i] <- v_animal
    
    machine_n <- classification_summary$Machine[classification_summary$captureMethod == cm_i]
    machine_n <- if (length(machine_n)) machine_n else 0L
    
    validation_summary$Machine_Animal_pr[i] <- round(100 * m_animal / pmax(machine_n, 1), 1)
    validation_summary$Validated_Animal_pr[i] <- if (m_animal > 0) round(100 * v_animal / m_animal, 1) else NA_real_
  }
  
  total_m_animal <- sum(validation_summary$Machine_Animal)
  total_v_animal <- sum(validation_summary$Validated_Animal)
  total_machine  <- classification_summary$Machine[classification_summary$captureMethod == "TOTAL"]
  
  validation_summary <- rbind(
    validation_summary,
    data.frame(
      captureMethod  = "TOTAL",
      Machine_Animal = total_m_animal,
      Validated_Animal = total_v_animal,
      Machine_Animal_pr = round(100 * total_m_animal / pmax(total_machine, 1), 1),
      Validated_Animal_pr = if (total_m_animal > 0) round(100 * total_v_animal / total_m_animal, 1) else NA_real_,
      stringsAsFactors = FALSE
    )
  )
  
  # Final formatted table 
  
  final_table <- merge(classification_summary, validation_summary,
                       by = "captureMethod", all.x = TRUE, sort = FALSE)
  
  # enforce same order as classification_summary
  final_table <- final_table[match(classification_summary$captureMethod, final_table$captureMethod), , drop = FALSE]
  
  fmt_pct <- function(n, total) {
    p <- ifelse(total > 0, round(100 * n / total, 1), NA_real_)
    ifelse(is.na(p), paste0(n, " (NA%)"), paste0(n, " (", p, "%)"))
  }
  
  final_table_formatted <- within(final_table, {
    Human  <- fmt_pct(Human, Total)
    Machine  <- fmt_pct(Machine, Total)
    NA_Classification <- fmt_pct(NA_Classification, Total)
    Machine_Animal <- paste0(Machine_Animal, " (", Machine_Animal_pr, "%)")
    Validated_Animal <- paste0(Validated_Animal, " (", Validated_Animal_pr, "%)")
    Total <- as.character(Total)
  })
  
  final_table_formatted <- final_table_formatted[
    , c("captureMethod", "Human", "Machine", "NA_Classification", "Total", "Machine_Animal", "Validated_Animal"),
    drop = FALSE
  ]
  
  #
  cm$data_status$Validation$ClassificationSummary <- classification_summary
  cm$data_status$Validation$ValidationSummary  <- validation_summary
  cm$data_status$Validation$FinalTableFormatted <- final_table_formatted
  
}


#####################*************############################
#       *********Data_status : species *********
#####################*************############################
.Species <- function(cm) {
  
  # reset
  cm$data_status$Species <- list()
  
  # A) Build Keep_sp (must be in species-level + exclude Homo sapiens)
  
  idx_species <- tolower(.trim_chr(cm$data$taxonomy$taxonRank)) == "species"
  idx_species[is.na(idx_species)] <- FALSE
  
  idx_human <- tolower(.trim_chr(cm$data$taxonomy$scientificName)) == "homo sapiens"
  idx_human[is.na(idx_human)] <- FALSE
  
  Keep_sp <- cm$data$taxonomy[idx_species & !idx_human, , drop = FALSE]
  
  cm$data_status$Species$Keep_sp_n <- nrow(Keep_sp)
  if (nrow(Keep_sp) == 0) {
    cm$data_status$Species$status <- "No species found at species level."
    cm$data_status$Species$Table  <- data.frame()
    return(NULL)
  }
  
  # B) Required columns in observations/sequences
  col_tax_obs <- .pick_col(cm$data$observations, c("taxonID"))
  col_cnt <- .pick_col(cm$data$observations, c("count"))
  col_seq_obs <- .pick_col(cm$data$observations, c("sequenceID", "sequID"))
  col_ot <- .pick_col(cm$data$observations, c("observationType", "obsType"))
  
  col_seq_seq <- .pick_col(cm$data$sequences, c("sequenceID", "sequID"))
  col_nrp <- .pick_col(cm$data$sequences, c("nrphotos"))
  
  miss <- character(0)
  if (is.na(col_tax_obs)) miss <- c(miss, "cm$data$observations$taxonID")
  if (is.na(col_cnt)) miss <- c(miss, "cm$data$observations$count")
  if (is.na(col_seq_obs)) miss <- c(miss, "cm$data$observations$sequenceID")
  if (is.na(col_seq_seq)) miss <- c(miss, "cm$data$sequences$sequenceID")
  if (is.na(col_nrp)) miss <- c(miss, "cm$data$sequences$nrphotos")
  
  if (length(miss)) {
    cm$data_status$Species$status <- paste0("Missing required columns: ", paste(miss, collapse = ", "))
    cm$data_status$Species$Table <- data.frame()
    return(NULL)
  }
  
  
  # C) Filter observations to animals
  
  idx_animal <- rep(TRUE, nrow(cm$data$observations))
  if (!is.na(col_ot)) {
    idx_animal <- tolower(.trim_chr(cm$data$observations[[col_ot]])) == "animal"
    idx_animal[is.na(idx_animal)] <- FALSE
  }
  
  obs_tax <- .trim_chr(cm$data$observations[[col_tax_obs]])[idx_animal]
  obs_seq <- .trim_chr(cm$data$observations[[col_seq_obs]])[idx_animal]
  obs_cnt <- suppressWarnings(as.numeric(.trim_chr(cm$data$observations[[col_cnt]])))[idx_animal]
  obs_cnt[is.na(obs_cnt)] <- 0
  
  seq_id_all <- .trim_chr(cm$data$sequences[[col_seq_seq]])
  nrphotos <- suppressWarnings(as.numeric(.trim_chr(cm$data$sequences[[col_nrp]])))
  nrphotos[is.na(nrphotos)] <- 0
  
  # D) Output table
  out <- data.frame(
    scientificName = .trim_chr(Keep_sp$scientificName),
    family  = if ("family" %in% names(Keep_sp)) .trim_chr(Keep_sp$family) else NA_character_,
    order = if ("order"  %in% names(Keep_sp)) .trim_chr(Keep_sp$order)  else NA_character_,
    class = if ("class"  %in% names(Keep_sp)) .trim_chr(Keep_sp$class)  else NA_character_,
    obs_records_count = 0,
    n_sequences  = 0L,
    stringsAsFactors = FALSE
  )
  
  taxon_ids <- .trim_chr(Keep_sp$taxonID)
  
  for (i in seq_len(nrow(out))) {
    id <- taxon_ids[i]
    rows <- which(obs_tax == id)
    
    out$obs_records_count[i] <- sum(obs_cnt[rows], na.rm = TRUE)
    
    seq_ids <- unique(obs_seq[rows])
    seq_ids <- seq_ids[!is.na(seq_ids) & seq_ids != ""]
    out$n_sequences[i] <- length(seq_ids)
    
  }
  
  out <- out[order(out$obs_records_count, decreasing = TRUE), , drop = FALSE]
  cm$data_status$Species$Table  <- out
  
}

#####################*************############################
#       *********Data_status :  Observation Types by Capture Method *********
#####################*************############################
.Visuals_capture_method <- function(cm) {
  
  if (is.null(cm$data_status$Visuals) || !is.list(cm$data_status$Visuals)) {
    cm$data_status$Visuals <- list()
  }
  
  cm$data_status$Visuals$capt_method <- list()
  
  col_seq_obs <- .pick_col(cm$data$observations, c("sequenceID", "sequID"))
  col_ot <- .pick_col(cm$data$observations, c("observationType", "obsType"))
  col_cap_obs <- .pick_col(cm$data$observations, c("captureMethod"))
  
  if (is.na(col_ot)) {
    cm$data_status$Visuals$capt_method$status <- "Missing observationType/obsType in cm$data$observations."
    return(NULL)
  }
  
  joined_data <- cm$data$observations |>
    dplyr::mutate(
      observationType = .trim_chr(.data[[col_ot]]),
      captureMethod = if (!is.na(col_cap_obs)) .trim_chr(.data[[col_cap_obs]]) else NA_character_
    )
  
  if (all(is.na(joined_data$captureMethod) | joined_data$captureMethod == "")) {
    
    if (!is.na(col_seq_obs) &&
        "sequences" %in% names(cm$data) && is.data.frame(cm$data$sequences)) {
      
      col_seq_seq <- .pick_col(cm$data$sequences, c("sequenceID", "sequID"))
      col_cap_seq <- .pick_col(cm$data$sequences, c("captureMethod"))
      
      if (!is.na(col_seq_seq) && !is.na(col_cap_seq)) {
        
        seq_map <- cm$data$sequences |>
          dplyr::distinct(.data[[col_seq_seq]], .keep_all = TRUE) |>
          dplyr::transmute(
            sequenceID_join = .trim_chr(.data[[col_seq_seq]]),
            captureMethod_seq = .trim_chr(.data[[col_cap_seq]])
          )
        
        joined_data <- joined_data |>
          dplyr::mutate(seq_join_key = .trim_chr(.data[[col_seq_obs]])) |>
          dplyr::left_join(seq_map, by = c("seq_join_key" = "sequenceID_join")) |>
          dplyr::mutate(captureMethod = dplyr::coalesce(captureMethod, captureMethod_seq)) |>
          dplyr::select(-captureMethod_seq, -seq_join_key)
      }
    }
    
    if (all(is.na(joined_data$captureMethod) | joined_data$captureMethod == "") &&
        !is.na(col_seq_obs) &&
        "media" %in% names(cm$data) && is.data.frame(cm$data$media)) {
      
      col_seq_med <- .pick_col(cm$data$media, c("sequenceID", "sequID"))
      col_cap_med <- .pick_col(cm$data$media, c("captureMethod"))
      
      if (!is.na(col_seq_med) && !is.na(col_cap_med)) {
        
        med_map <- cm$data$media |>
          dplyr::mutate(sequenceID_join = .trim_chr(.data[[col_seq_med]])) |>
          dplyr::filter(!is.na(sequenceID_join) & sequenceID_join != "") |>
          dplyr::distinct(sequenceID_join, .keep_all = TRUE) |>
          dplyr::transmute(
            sequenceID_join,
            captureMethod_med = .trim_chr(.data[[col_cap_med]])
          )
        
        joined_data <- joined_data |>
          dplyr::mutate(seq_join_key = .trim_chr(.data[[col_seq_obs]])) |>
          dplyr::left_join(med_map, by = c("seq_join_key" = "sequenceID_join")) |>
          dplyr::mutate(captureMethod = dplyr::coalesce(captureMethod, captureMethod_med)) |>
          dplyr::select(-captureMethod_med, -seq_join_key)
      }
    }
  }
  
  joined_data$captureMethod[is.na(joined_data$captureMethod) | joined_data$captureMethod == ""] <- "UNKNOWN"
  
  capture_methods <- sort(unique(as.character(joined_data$captureMethod)))
  capture_methods_all <- c(capture_methods, "TOTAL")
  
  observation_type_summary <- lapply(capture_methods_all, function(method) {
    subset_df <- if (method == "TOTAL") joined_data else dplyr::filter(joined_data, captureMethod == method)
    tab <- table(subset_df$observationType, useNA = "ifany")
    data.frame(
      ObservationType = names(tab),
      n = as.integer(tab),
      stringsAsFactors = FALSE
    )
  })
  
  names(observation_type_summary) <- capture_methods_all
  
  observation_type_df <- do.call(rbind, lapply(names(observation_type_summary), function(method) {
    df <- observation_type_summary[[method]]
    df$CaptureMethod <- method
    df
  }))
  
  rownames(observation_type_df) <- NULL
  
  cm$data_status$Visuals$capt_method <- list(
    Tables = observation_type_summary,
    Combined = observation_type_df
  )
}