# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  Jan 2026
# Version 1.0
# Licence GPL v3
#--------


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



