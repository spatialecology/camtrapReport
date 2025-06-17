# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  May 2025
# Version 1.2
# Licence GPL v3
#--------



# in group_definition, the groups like large_mammals, wild_mammals can be defined
# each group can be specified based on data columns (e.g., order, class, scientificNames)
# domestic group can be defined based on which the wild_animals also defined (species NOT in domestic)

camR <- setRefClass(
  "camReport",
  fields = list(
    pkg              = 'datapackage',
    data             = "ctdp",
    habitat          = "data.frame",
    data_year        = "list",
    data_merged      = "data.frame",
    study_area       = "PackedSpatVector",
    siteName         = "character",
    title            = "character",
    subtitle         = "character",
    authors          = "character",
    institute        = "character",
    logoPath         = "character",
    annonator        = "character",
    acknowledgement  = "character",
    description      = "character",
    sampling         = "character",
    years            = "numeric",
    group_definition = "list",
    filterExclude    = "list",
    filterKeep       = "list",
    filterCount      = "numeric",
    filterDuration   = "numeric",
    observed_counts  = "data.frame",
    frequent_species = "data.frame",
    locations        = "data.frame",
    camera_stats     = "data.frame",
    camera_setup     = "data.frame",
    observation_stats= "data.frame",
    species_summary  = "list",
    species_stats    = "data.frame",
    species_summary_by_habitat = "data.frame",
    capture          = "data.frame",
    density_estimates= "data.frame",
    setting          = "list",
    .rem_params      = "list",
    rem              = "list",
    sun_times        = "data.frameORnull",
    .species_NO_rem  = 'character', # a vector of species names for which REM could not be fitted!
    packages         = "character",
    .tempObjects     = "list",
    reportObjects    = "list" # a list of .txtSection and .Rchunk objects
  ),
  
  methods = list(
    initialize = function() {
      #.loadlib()
      
      .self$setting = list(locationLegend = FALSE,color=c("#CA6A28","#6C9100","#00A383","#008ADF","#D44CBF"))
      
      
      .self$filterDuration <- 5
      
      .self$siteName <-  "**an unspecified location**"
      
      .self$description <- "No discription has been provided for this site. The report object can be updated by overriding the 'description' field!"
      
      .self$sampling <- "Description of sampling method has NOT been provided. The 'sampling' field can be updated by the user. More details on camera trap deployments can be found in the summarized information table below (Table 1)!"
      
    },
    add_group = function(name,x) {
      # add the group to group_definition
      # x should be a named list
      if (!is.list(x)) stop('x should be a named list...!')
      
      if (!any(c('order','class','scientificName','observationType') %in% names(x))) stop('Currently, only items from c("order","class","scientificName","observationType") are allowed to define a group!')
      
      .self$group_definition[[name]] <- x
      
    },
    get_group = function(name) {
      .self$group_definition[[name]]
    },
    set_focus_group=function(x) {
      if (length(.self$species_summary) > 0) {
        x <- x[x %in% .self$species_summary$count$Group]
        if (length(x) > 0) {
          .self$setting$focus_groups <- x
        } else stop('the specified groups are not defined (or there is no match)...!')
      } else {
        if (length(names(.self$group_definition)) > 0) {
          x <- x[x %in% names(.self$group_definition)]
          if (length(x) > 0) {
            .self$setting$focus_groups <- x
          } else stop('the specified groups are not defined (or there is no match)...!')
        }
      }
      
    },
    get_focus_group = function(sp) {
      # identify the focus group of a species:
      .n <- names(.self$species_summary)
      if (length(.n) > 0) {
        .n <- .n[.n != 'count']
        if (length(sp) > 1) {
          .o <- c()
          for (.s in sp) {
            for (.g in .n) {
              if (sp %in% get_speciesNames(.g)) {
                .o <- c(.o,.g)
                break
              }
            }
          }
          names(.o) <- sp
          return(.o)
        } else {
          for (.g in .n) {
            if (sp %in% get_speciesNames(.g)) {
              return(.g)
              break
            }
          }
        }
      } else stop('No species summay information...run "setup" first...')
      
    },
    get_speciesNames = function(group=NULL) {
      if (length(.self$species_summary) == 0) stop('No species summay information...run "setup" first...')
      #----
      if (is.null(group)) {
        unique(unlist(lapply(.self$species_summary,function(x) x$site_list$scientificName)))
      } else if (is.character(group)) {
        if (length(group) == 1) {
          if (group %in% names(.self$species_summary)) {
            unique(.self$species_summary[[group]]$site_list$scientificName)
          } else {
            if (group %in% .self$species_summary$count$Name) {
              .g <- .self$species_summary$count$Group[.self$species_summary$count$Name == group]
              unique(.self$species_summary[[.g]]$site_list$scientificName)
            } else stop('group is unknown!')
          }
        } else {
          .n <- c()
          for (.g in group) {
            if (.g %in% names(.self$species_summary)) {
              .n <- c(.n,unique(.self$species_summary[[.g]]$site_list$scientificName))
            } else {
              if (.g %in% .self$species_summary$count$Name) {
                .gg <- .self$species_summary$count$Group[.self$species_summary$count$Name == .g]
                .n <- c(.n,unique(.self$species_summary[[.gg]]$site_list$scientificName))
              }
            }
          }
          if (length(.n) > 0) .n
          else stop('group is unknown!')
        } 
      } else stop('group is unknown!')
    },
    richness=function(year=NULL,spList = NULL) {
      # if year is NULL -> total
      # spList can be a vector of scientificNames (e.g., related to a focus_group); if NULL --> all species
      
      .d <- .self$pkg$data$observations %>% 
        dplyr::filter(!is.na(scientificName)) %>% 
        left_join(.self$pkg$data$deployments,by='deploymentID') %>% 
        mutate(year=.getYear(timestamp)) %>%
        dplyr::select(scientificName,locationID,year)
      
      if (!is.null(spList)) .d <- .d[.d$scientificName %in% spList,]
      #------
      if (is.null(year)) {
        .rich <- .d %>% group_by(locationID) %>% 
          summarise(
            Richness = length(unique(scientificName)),
            Species_List = paste(sort(unique(scientificName)),collapse=', '),
            Community_Composition = paste(paste0(paste0(sort(unique(scientificName)),' ('),paste0(table(scientificName)[sort(unique(scientificName))],')')),collapse = ', '),
            .groups = "drop"
          )  %>% left_join(.self$data$locations,by='locationID')
      } else {
        year <- year[year %in% .self$years]
        if (length(year) > 0) {
          .d <- .d[.d$year %in% year,]
          .rich <- .d %>% group_by(locationID,year) %>% 
            summarise(
              Richness = length(unique(scientificName)),
              Species_List = paste(sort(unique(scientificName)),collapse=', '),
              Community_Composition = paste(paste0(paste0(sort(unique(scientificName)),' ('),paste0(table(scientificName)[sort(unique(scientificName))],')')),collapse = ', '),
              .groups = "drop"
            ) %>% left_join(.self$data$locations,by='locationID')
          
        } else stop('No records are available for the specified years...!')
      }
      .rich
    },
    spatial_density=function(x=NULL,species=NULL,year=NULL,.crs=NULL,.ext=NULL) {
      # spatial density raster map
      # x: the output of species_summary_by_location wit cor_matrix=F
      # if x is null -> species_summary_by_location will be called with other arguments
      #---------
      if (is.null(x)) {
        x <- .self$species_summary_by_location(year=year,spList=species,cor_matrix=FALSE)
        x$lon <- x$longitude
        x$lat <- x$latitude
        x <- vect(x,geom=c('lon','lat'),crs=crs(rast()))
        x <- .get_projected_vect(x)
        .crs <- crs(x)
        x <- as.data.frame(x,geom="XY")
      }
      
      xr <- range(x$x, na.rm = TRUE)
      yr <- range(x$y,  na.rm = TRUE)
      
      if (!is.null(.ext) && length(as.vector(.ext)) == 4) {
        .ext <- as.vector(.ext)
        xr[1] <- max(c(.ext[1],xr[1]))
        xr[2] <- max(c(.ext[2],xr[2]))
        yr[1] <- max(c(.ext[3],yr[1]))
        yr[2] <- max(c(.ext[4],yr[2]))
      } else if (!is.null(.self$study_area)) {
        .ext <- as.vector(ext(.self$study_area))
        xr[1] <- max(c(.ext[1],xr[1]))
        xr[2] <- max(c(.ext[2],xr[2]))
        yr[1] <- max(c(.ext[3],yr[1]))
        yr[2] <- max(c(.ext[4],yr[2]))
      }
      
      .win <- .eval("owin(xrange = xr, yrange = yr)",env = environment())
      .ppp_obj <- .eval("ppp(x = x$x, y=x$y,window= .win,marks=x$total_observations)",env = environment())
      den <- density(.ppp_obj, weights = x$total_observations)
      den$v <- den$v / max(den$v, na.rm = TRUE)
      r <- rast(den)
      crs(r) <- .crs
      names(r) <- 'spatial_density'
      r
    },
    species_summary_by_location=function(year=NULL,spList=NULL,cor_matrix=TRUE,PA=TRUE) {
      # cor_matrix = T -> cor of co-occurrence matrix across locations will be returned
      # otherwise, species summary across locations (count, total observations) will be generated
      # PA = T -> co-occurrence matrix will be based on P/A, otherwise, based on count
      #-----------------------
      if (is.null(spList)) {
        .d <- .self$pkg$data$observations %>% 
          dplyr::filter(!is.na(scientificName), 
                        scientificName != "",
                        grepl("\\s", scientificName)) %>%
          left_join(.self$pkg$data$deployments,by='deploymentID') %>% 
          mutate(Year=.getYear(timestamp))
      } else {
        .d <- .self$pkg$data$observations %>% 
          dplyr::filter(scientificName %in% spList) %>%
          left_join(.self$pkg$data$deployments,by='deploymentID') %>% 
          mutate(Year=.getYear(timestamp))
      }
      #----
      if (!is.null(.self$observed_counts$scientificName)) {
        .d <- .d %>%
          dplyr::filter(scientificName %in% .self$observed_counts$scientificName)
      }
      #----
      if (!is.null(year)) {
        year <- year[year %in% .self$years]
        if (length(year) > 0) {
          .d <- .d %>%
            dplyr::filter(Year %in% year)
        } else stop('No records are available for the specified years...!')
      }
      #-------
      .d <- .d %>%
        group_by(locationID, scientificName) %>%
        summarise(
          total_observations = n_distinct(observationID),
          total_count        = sum(count, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(.self$data$locations, by = "locationID")
      #----
      if (cor_matrix) {
        if (PA) {
          .d <- .d %>%
            group_by(locationID, scientificName) %>%
            summarise(count = n(), .groups = "drop") %>%
            pivot_wider(names_from = scientificName, values_from = count, values_fill = 0)
          
          sp_mat <- as.matrix(.d[, -1])  
          rownames(sp_mat) <- .d$locationID
          
          sp_mat <- sp_mat[ , colSums(sp_mat) > 1, drop = FALSE]
          
          
          cor(sp_mat, use = "pairwise.complete.obs")
        } else {
          .d <- .d %>%
            group_by(locationID, scientificName) %>%
            summarise(count = total_count, .groups = "drop") %>%
            pivot_wider(names_from = scientificName, values_from = count, values_fill = 0)
          
          sp_mat <- as.matrix(.d[, -1])  
          rownames(sp_mat) <- .d$locationID
          
          sp_mat <- sp_mat[ , colSums(sp_mat) > 1, drop = FALSE]
          
          
          cor(sp_mat, use = "pairwise.complete.obs",method = 'spearman')
        }
        
      } else .d
      
    },
    .get_REM_Param=function(sp) {
      if (is.null(.self$.rem_params[[sp]])) {
        x <- try({
          radius_model <- fit_detmodel(radius ~ 1, .self$pkg, species = sp, truncation = "5%",quiet=TRUE)
          angle_model <- fit_detmodel(angle ~ 1, .self$pkg, species = sp, unit = "radian",quiet=TRUE)
          speed_model <- fit_speedmodel(.self$pkg, species = sp)
          activity_model <- fit_actmodel(.self$pkg, species = sp, reps = 10)
          
          # Store only if all succeed
          list(
            radius_model = radius_model,
            angle_model = angle_model,
            speed_model = speed_model,
            activity_model = activity_model
          )
        }, silent = TRUE)
        if (!inherits(x,'try-error')) {
          .self$.rem_params[[sp]] <- x
          return(x)
        } else .self$.rem_params[[sp]] <- NA
      } else if (is.list(.self$.rem_params[[sp]])) {
        return(.self$.rem_params[[sp]])
      }
      
    },
    fit_REM=function(sp) {
      
      .g <- .self$get_focus_group(sp)
      
      if (!.g %in% names(.self$rem)) {
        .self$rem[[.g]] <- list()
      }
      
      
      .density_estimate_list <- list()
      
      for (year in .self$years) {
        # Filter observations for species and year
        pkg_filtered <- .self$pkg
        pkg_filtered$data$observations <- pkg_filtered$data$observations %>%
          dplyr::filter(scientificName == sp & .getYear(timestamp) == year)
        
        # Skip if no observations
        if (nrow(pkg_filtered$data$observations) > 0) {
          species_params <- .self$.get_REM_Param(sp)
          if (!is.null(species_params)) {
            x <- try({
              trdat <- get_traprate_data(pkg_filtered, species = sp)
              
              .parameters <- get_parameter_table(
                trdat,
                radius_model = species_params$radius_model,
                angle_model = species_params$angle_model,
                speed_model = species_params$speed_model,
                activity_model = species_params$activity_model,
                strata = NULL,
                reps = 10
              )
              
              .density_estimates <- rem(.parameters) %>%
                convert_units(
                  radius_unit = "m",
                  angle_unit = "degree",
                  active_speed_unit = "km/hour",
                  overall_speed_unit = "km/day"
                )
              
              # English name fallback
              english_name <- .self$data$taxonomy$vernacularNames.eng[.self$data$taxonomy$scientificName == sp]
              if (length(english_name) == 0) english_name <- "Unknown"
              
              
              # Store result
              data.frame(
                scientificName = sp,
                EnglishName = english_name,
                Year = year,
                Metric = rownames(.density_estimates),
                .density_estimates,
                row.names = NULL
              )
            }, silent = TRUE)
            
            if (!inherits(x,'try-error')) {
              .density_estimate_list[[paste0(sp,'_',year)]] <- x
            }
          }
        }
      }
      #----
      if (length(.density_estimate_list) > 0) {
        for (n in names(.density_estimate_list)) {
          .self$rem[[.g]][[n]] <- .density_estimate_list[[n]]
        }
      } else .self$.species_NO_rem <- c(.self$.species_NO_rem,sp)
      
      
    },
    get_REM = function(.sp) {
      # extract REM results for a species from .self$rem
      # if not available, fit_REM is called!
      if (length(.sp) > 1) stop('length(.sp) > 1; a single species name should be provided to get_REM!')
      
      if (!.sp %in% .self$.species_NO_rem) {
        .g <- .self$get_focus_group(.sp)
        if (.g %in% names(.self$rem)) {
          .n <- names(.self$rem[[.g]])
          .spn <- paste0(.sp,'_',.self$years)
          if (any(.spn %in% .n)) {
            .spn <- .spn[.spn %in% .n]
            .self$rem[[.g]][.spn]
          } else {
            .self$fit_REM(.sp)
            .self$get_REM(.sp)
          }
        } else {
          .self$fit_REM(.sp)
          .self$get_REM(.sp)
        }
      }
    },
    setup = function(tz=NULL) {
      
      # add tz (time zone) to setting:
      if (is.null(tz)) {
        if (is.null(.self$setting$tz)) {
          if (!is.null(.self$data$settings$tz)) .self$setting$tz <- .self$data$settings$tz
          else .self$setting$tz <- "CET"
        }
      } else {
        .self$setting$tz <- tz
      }
      #------------------------
      
      
      # joining Habitat types to locations:
      #.self$data$locations <- .self$data$locations %>%
      #  left_join(.self$habitat, by = "locationName")
      # 
      
      #.self$data$locations <- merge(.self$data$locations,.self$habitat, by = "locationName",all.x=TRUE)
      
      #----
      # adding new column (Year) to deployments data.frame:
      .self$data$deployments$Year <- .getYear(.self$data$deployments$deployment_interval) 
      #----
      .y <- .self$extractYears()
      .self$data_year <- lapply(.y,function(yr) {
        start_date <- paste0(yr, "-01-01")
        end_date <- paste0(yr, "-12-31")
        filter_timerange(.self$data, start = start_date, end = end_date)
      })
      #---
      names(.self$data_year) <- as.character(.y)
      ################
      # identify missing taxonomic information and retrieve "class" and "order"
      # from GBIF (it needs the taxize package)
      
      
      .w <- .self$data$taxonomy$scientificName[is.na(.self$data$taxonomy$order) | is.na(.self$data$taxonomy$order)]
      
      if (length(.w) > 0) {
        if (.require('taxize')) {
          .w <- .getMissingTaxon_GBIF(.w)
          #-----
          # assign the retrieved info to the main CameraTrap database:
          for (i in 1:nrow(.w)) {
            w <- which(.self$data$taxonomy$scientificName == .w$scientificName[i])
            .self$data$taxonomy[w,'class'] <- .w$class[i]
            .self$data$taxonomy[w,'order'] <- .w$order[i]
          }
          
          rm(.w,w)
        } else warning(paste0('The package "taxize" is not installed, therefore, the missing taxonomic information for ',length(.w),' cannot be retrived...!'))
      }
      #-------
      .w <- table(.self$data$observations$taxonID)
      .self$observed_counts <- data.frame(taxonID=names(.w),count=as.numeric(.w)) %>% left_join(.self$data$taxonomy,by='taxonID')
      # if (length(.self$filterCount) > 0) {
      #   .self$observed_counts <- .self$observed_counts[.self$observed_counts$count >= .self$filterCount[1],]
      # }
      #-----
      .self$filter()
      ####################
      if (!'large_mammals' %in% names(.self$group_definition)) {
        if (any(c("Artiodactyla","Carnivora") %in% .self$data$taxonomy$order)) {
          .self$group_definition[['large_mammals']] <- list(order=c("Artiodactyla","Carnivora"),domestic=FALSE,observationType='animal')
        }
      }
      #-----
      # if (!'domestic' %in% names(.self$group_definition)) {
      #   
      # }
      if (!'wild_animals' %in% names(.self$group_definition) && 'domestic' %in% names(.self$group_definition)) {
        .self$group_definition[['wild_animals']] <- list(scientificName = .self$data$taxonomy$scientificName[!.self$data$taxonomy$scientificName %in% .self$group_definition$domestic$scientificName])
      }
      
      if (!'birds' %in% names(.self$group_definition) && 'Aves' %in% .self$data$taxonomy$class) {
        .self$group_definition[['birds']] <- list(class='Aves',observationType='animal')
      }
      
      if (!'reptiles' %in% names(.self$group_definition) && 'Reptilia' %in% .self$data$taxonomy$class) {
        .self$group_definition[['reptiles']] <- list(class='Reptilia',observationType='animal')
      }
      
      if (!'amphibians' %in% names(.self$group_definition) && 'Amphibia' %in% .self$data$taxonomy$class) {
        .self$group_definition[['amphibians']] <- list(class='Amphibia',observationType='animal')
      }
      
      if (!'wild_mammals' %in% names(.self$group_definition) && 'Mammalia' %in% .self$data$taxonomy$class 
          && 'domestic' %in% names(.self$group_definition)) {
        
        .w <- .self$data$taxonomy$class == "Mammalia" & (!.self$data$taxonomy$scientificName %in% .self$group_definition$domestic$scientificName) & grepl(" ", .self$data$taxonomy$scientificName)
        if (length(.w) > 0) .self$group_definition[['wild_mammals']] <- list(scientificName = .self$data$taxonomy$scientificName[.w])
        
      }
      #---
      if (!'human_observation' %in% names(.self$group_definition) && "Homo sapiens" %in% .self$data$taxonomy$scientificName) {
        .self$group_definition[['human_observation']] <- list(scientificName="Homo sapiens",observationType='human')
      }
      
      #####################
      # Extract the Year from 'deployment_interval'
      .self$data$deployments$Year  <- .getYear(.self$data$deployments$deployment_interval)
      
      #----------------- #Locations with Habitat --------------------------
      .self$data$locations <- .self$data$locations %>%
        left_join(.self$habitat, by = "locationName")
      
      #----------- # dep_loc = deployments + locations --------------------------
      dep_loc <- .self$data$deployments %>%
        left_join(.self$data$locations,by="locationID")
      colnames(dep_loc)[colnames(dep_loc) == "Habitat"] <- "Habitat_Type"
      
      dep_loc <- dep_loc %>%
        mutate(
          Habitat_Type = gsub("_", " ", Habitat_Type),  # Replace underscores with spaces
          Habitat_Type = ifelse(Habitat_Type == "Other", "Unclassified Habitat", Habitat_Type)  # Rename "Other"
        )
      
      #----------------- #Count Deployments Per Location --------------------------
      deployments_per_location <- dep_loc %>%
        group_by(locationID) %>%
        summarise(
          deploymentID_List = toString(unique(deploymentID)), 
          Num_Deployments = n()
        ) %>%
        ungroup()
      #----------------- #Capture Methods Per Location --------------------------
      capture_methods_per_location <- .self$data$sequences %>%
        left_join(dplyr::select(dep_loc, deploymentID, locationID), by = "deploymentID") %>%
        group_by(locationID) %>%
        summarise(CaptureMethod_List = toString(sort(unique(captureMethod)))) %>%
        ungroup()
      
      #----------------- #setup By Per Location --------------------------
      # Group by locationID and list unique setupBy names
      setup_per_location <- dep_loc %>%
        group_by(locationID) %>%
        summarise(Setup_By_List = toString(sort(unique(setupBy)))) %>%
        ungroup()
      
      #----------------- #Classify By Per Location --------------------------
      
      #Extract sequenceID to deploymentID mapping from sequences
      sequence_to_deployment <- .self$data$sequences %>%
        dplyr::select(sequenceID, deploymentID) %>%
        distinct()  # Ensure unique mapping
      
      
      # Merge deploymentID into observations using sequenceID
      observations_with_deployment <- .self$data$observations %>%
        left_join(sequence_to_deployment, by = "sequenceID") %>%
        left_join(dep_loc[,c("deploymentID","locationID")], by = "deploymentID")  # Merge locationID using deploymentID
      
      
      # Group by locationID and list unique ClassifyBy names
      Classify_per_location <- observations_with_deployment %>%
        group_by(locationID) %>%
        summarise(Classify_By_List = toString(sort(unique(classifiedBy)))) %>%
        ungroup()
      
      # ----------------- List of unique baitUse per location --------------------------
      # Group by locationID and list unique baitUse values, properly separated by ", "
      bait_use_per_location <- dep_loc %>%
        group_by(locationID) %>%
        summarise(BaitUse_List = paste(sort(unique(baitUse)), collapse = ", ")) %>%  # Ensure ", " separation
        ungroup()
      
      
      # ----------------- List of deployment years for each location --------------------------
      
      # Aggregate Year List per locationID
      years_per_location <- dep_loc %>%
        group_by(locationID) %>%
        summarise(Year_List = paste(sort(unique(Year)), collapse = ", ")) %>%  # Keep years in one row
        ungroup()
      
      
      #----------------- How Many Cameras in Each Year? 
      expanded_years <- years_per_location %>%
        separate_rows(Year_List, sep = ", ")
      
      location_count_per_year <- expanded_years %>%
        distinct(locationID, Year_List) %>%  # Ensure unique locationID-Year combinations
        group_by(Year_List) %>%
        summarise(Unique_Locations = n()) %>%  # Count distinct locations
        ungroup() %>%
        arrange(desc(Unique_Locations))  # Sort from highest to lowest
      
      
      # ----------------- Total number of photo for each location --------------------------
      # Sum nrphotos per deploymentID
      photos_per_deployment <- .self$data$sequences %>%
        group_by(deploymentID) %>%
        summarise(Total_Photos = sum(nrphotos, na.rm = TRUE)) %>%  # Sum photos, ignore NA values
        ungroup()
      
      # Merge with deployments to get locationID
      photos_per_location <- photos_per_deployment %>%
        left_join(dplyr::select(dep_loc, deploymentID, locationID), by = "deploymentID") %>%
        group_by(locationID) %>%
        summarise(Total_Photos = sum(Total_Photos, na.rm = TRUE)) %>%  # Sum photos per location
        ungroup()
      
      # ----------------- List of Species for Each Location (Final Filtering) --------------------------
      
      
      # Step 1: Select relevant columns from 'dataNew$sequences' to map 'sequenceID' to 'deploymentID'
      sequence_to_deployment <- .self$data$sequences %>%
        dplyr::select(sequenceID, deploymentID) %>%
        distinct()  # Ensure unique mapping
      
      # Step 1-1: Merge observations with taxonomy
      observations_with_taxonomy <- .self$data$observations %>%
        left_join(.self$data$taxonomy, by = "taxonID")
      
      # Step 1-2: Group by sequenceID and create a cleaned list of species
      scientific_names_per_observation <- observations_with_taxonomy %>%
        group_by(sequenceID) %>%
        summarise(ScientificName_List = paste(
          sort(unique(scientificName[
            !is.na(scientificName) & 
              scientificName != "" & 
              grepl("\\s", scientificName)  # Keep only species with at least two words
          ])),
          collapse = ", ")) %>%
        ungroup()
      
      
      #---
      # Step 2: Merge with scientific_names_per_observation to get deploymentID
      scientific_names_with_deployment <- scientific_names_per_observation %>%
        left_join(sequence_to_deployment, by = "sequenceID")
      
      # Step 3: Select relevant columns from 'dataNew$deployments' to map 'deploymentID' to 'locationID'
      deployment_to_location <- dep_loc %>%
        dplyr::select(deploymentID, locationID) %>%
        distinct()  # Ensure unique mapping
      
      # Step 4: Merge with scientific_names_with_deployment to get locationID
      species_per_location <- scientific_names_with_deployment %>%
        left_join(deployment_to_location, by = "deploymentID") %>%
        group_by(locationID) %>%
        summarise(Species_List = paste(
          sort(unique(ScientificName_List[!is.na(ScientificName_List) & ScientificName_List != ""])), 
          collapse = ", ")) %>%
        ungroup()
      
      # Step 5: Remove species that appear in only one location
      species_counts <- species_per_location %>%
        separate_rows(Species_List, sep = ", ") %>%  # Expand species into separate rows
        group_by(Species_List) %>%
        summarise(Location_Count = n_distinct(locationID))
      
      # Step 6: Remove species appearing in only one location from `species_per_location`
      species_per_location_filtered <- species_per_location %>%
        separate_rows(Species_List, sep = ", ") %>%  # Expand species into separate rows
        dplyr::filter(Species_List %in% species_counts$Species_List) %>%  # Keep only frequent species
        group_by(locationID) %>%
        summarise(Species_List = paste(sort(unique(Species_List)), collapse = ", ")) %>%
        ungroup()
      
      # Step 7: Remove any leading or trailing commas (in case of empty values)
      species_per_location_filtered <- species_per_location_filtered %>%
        mutate(Species_List = trimws(gsub("^,|,$", "", Species_List)))  # Trim extra commas
      
      # Print first few rows to verify
      # head(species_per_location_filtered)
      
      ################ **Join all data into a single dataframe required for the **Research Area Plot** ################ 
      .d<-left_join(dep_loc, capture_methods_per_location, by= "locationID")
      .d<-left_join(.d, setup_per_location, by= "locationID")
      .d<-left_join(.d, Classify_per_location, by= "locationID")
      .d<-left_join(.d, bait_use_per_location, by= "locationID")
      .d<-left_join(.d, years_per_location, by= "locationID")
      .d<-left_join(.d, photos_per_location, by= "locationID")
      .self$data_merged<-left_join(.d, species_per_location, by= "locationID")
      
      #----
      rm(dep_loc,deployments_per_location,capture_methods_per_location,setup_per_location,
         observations_with_deployment,sequence_to_deployment,Classify_per_location,bait_use_per_location,
         years_per_location,expanded_years,location_count_per_year,photos_per_deployment,photos_per_location,
         observations_with_taxonomy,scientific_names_per_observation,scientific_names_with_deployment,
         deployment_to_location,species_per_location,species_counts,species_per_location_filtered,.d)
      gc()
      #----
      
      .tmp <- lapply(names(.self$data_year), function(x) {
        .data_year <- .self$data_year[[x]]
        # Number of unique camera trap locations
        number_camtraps <- length(unique(.data_year$location$locationName))

        # Extract deployment start and end dates
        deployment_intervals <- .data_year$deployments$deployment_interval
        start_dates <- int_start(deployment_intervals)
        end_dates <- int_end(deployment_intervals)

        # Group deployments by location and find min/max dates
        deployments <- .data_year$deployments %>%
          mutate(start_date = start_dates, end_date = end_dates) %>%
          group_by(locationID) %>%
          summarise(start_date = min(start_date), end_date = max(end_date), .groups = "drop")

        # Find the earliest and latest deployment dates
        earliest_start <- min(deployments$start_date)
        latest_end <- max(deployments$end_date)

        # Filter valid pickup dates (excluding cameras that stopped too early)
        valid_end_dates <- deployments %>% dplyr::filter(end_date >= (latest_end - days(.self$filterDuration)))
        earliest_end <- min(valid_end_dates$end_date)

        # Return the processed data as a list
        list(
          year = as.numeric(x),
          number_camtraps = number_camtraps,
          deployment_period = paste(format(earliest_start, "%Y-%m-%d"), "-", format(latest_end, "%Y-%m-%d")),
          setup_period = paste(format(earliest_start, "%d %B"), "-", format(max(deployments$start_date), "%d %B")),
          pickup_period = paste(format(earliest_end, "%B %d"), "-", format(latest_end, "%B %d"))
        )
      })
      
      # Convert Camera Setup Data into a DataFrame
      .self$camera_setup <- bind_rows(.tmp)
      #--------------
      #################
      .tax_obs <-.self$data$observations %>%
        left_join(.self$data$taxonomy, by = "taxonID") %>%
        mutate(
          observation_date = as.Date(observation_timestamp),
          observation_Year = .getYear(observation_timestamp)
        )
      
      #-------
      
      summarize_species <- function(df, class = NULL,order=NULL,domestic=FALSE,scientificName=NULL,.filterCount=TRUE,observationType=NULL) {
        # To summarise, either scientificName, OR other criteria (one or combination of class, order, etc.) is provided!
        # .filterCount = T -> the count threshold (if specified in .self$filterCount) is applied
        
        if (!is.null(observationType) && is.character(observationType)) df <- df[df$observationType %in% observationType,]
        
        if (is.null(scientificName) || length(scientificName) == 0) {
          df <- df[grepl(" ", df$scientificName),]
          df <- df[!grepl(" sp\\.$", df$scientificName),]
          
          # if domestic is FALSE, they are excluded, if TRUE, summary is for domestic (NULL: all are considered)
          # both only if the scientificName vector is provided by user in .self$filterExclude$scientificName
          # or when domestic group is defined in the group_definition!
          if (!is.null(.self$filterExclude$scientificName) && is.character(.self$filterExclude$scientificName)) {
            if (!is.null(domestic)) {
              if (domestic) {
                if ('domestic' %in% names(.self$group_definition)) df <- df[df$scientificName %in% .self$group_definition$domestic$scientificName,]
                else if (scientificName %in% names(.self$filterExclude)) df <- df[df$scientificName %in% .self$filterExclude$scientificName,]
                else warning('domestic group is not defined!')
              } else {
                if ('domestic' %in% names(.self$group_definition)) df <- df[!df$scientificName %in% .self$group_definition$domestic$scientificName,]
                else if (scientificName %in% names(.self$filterExclude)) df <- df[!df$scientificName %in% .self$filterExclude$scientificName,]
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
        if (.filterCount && length(.self$filterCount) > 0 && nrow(.self$observed_counts) > 0) {
          df <- df[df$scientificName %in% .self$observed_counts$scientificName[.self$observed_counts$count > .self$filterCount[1]], ]
        }
        #---------
        .years <- unique(df$observation_Year)
        .years <- sort(.years[!is.na(.years)])
        
        if (length(.years) > 0) {
          .df  <- df[1:length(.years),c('observation_Year','count','scientificName','vernacularNames.eng','vernacularNames.nld')]
          colnames(.df) <- c('observation_Year','total_species','species_list_scientificName','species_list_eng','species_list_nld')
          .df$total_observations <- 0
          .df$observation_Year <- as.numeric(.years)
          
          for (i in seq_along(.years)) {
            .w <- which(df$observation_Year == .years[i])
            .df$total_observations[i] <- length(.w)
            .df$total_species[i] <- length(unique(df$scientificName[.w]))
            .df$species_list_scientificName[i] <- paste(sort(unique(df$scientificName[.w])), collapse = ", ")
            .df$species_list_eng[i] <- paste(sort(unique(df$vernacularNames.eng[.w])), collapse = ", ")
            .df$species_list_nld[i] <- paste(sort(unique(df$vernacularNames.nld[.w])), collapse = ", ")
            
          }
          #----
          
          .dfs <- unique(df[,c("scientificName", "vernacularNames.eng", "vernacularNames.nld")])
          .dfs <- .dfs[order(.dfs$scientificName),]
          
          list(per_year=.df,site_list=.dfs)
        }
      }
      #----
      
      #.classes <- unique(.self$data$taxonomy$class)
      .self$species_summary <- list()
      if (!is.null(names(.self$group_definition))) {
        for (n in names(.self$group_definition)) {
          if ('scientificName' %in%  names(.self$group_definition[[n]])) {
            .self$species_summary[[n]] <- summarize_species(.tax_obs,scientificName = .self$group_definition[[n]]$scientificName)
          } else if (any(c('class','order') %in% names(.self$group_definition[[n]]))) {
            if ('class' %in% names(.self$group_definition[[n]])) {
              if ('order' %in% names(.self$group_definition[[n]])) {
                if ('domestic' %in% names(.self$group_definition[[n]]) && is.logical(.self$group_definition[[n]]$domestic)) {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic)
                  }
                  
                } else {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order)
                  }
                }
              } else {
                if ('domestic' %in% names(.self$group_definition[[n]]) && is.logical(.self$group_definition[[n]]$domestic)) {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,domestic = .self$group_definition[[n]]$domestic,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,domestic = .self$group_definition[[n]]$domestic)
                  }
                  
                } else {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- summarize_species(.tax_obs,class = .self$group_definition[[n]]$class)
                  }
                }
              }
            } else {
              if ('domestic' %in% names(.self$group_definition[[n]]) && is.logical(.self$group_definition[[n]]$domestic)) {
                if ("observationType" %in% names(.self$group_definition[[n]])) {
                  .self$species_summary[[n]] <- summarize_species(.tax_obs,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic,observationType=.self$group_definition[[n]]$observationType)
                } else {
                  .self$species_summary[[n]] <- summarize_species(.tax_obs,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic)
                }
              } else {
                if ("observationType" %in% names(.self$group_definition[[n]])) {
                  .self$species_summary[[n]] <- summarize_species(.tax_obs,order = .self$group_definition[[n]]$order,observationType=.self$group_definition[[n]]$observationType)
                } else {
                  .self$species_summary[[n]] <- summarize_species(.tax_obs,order = .self$group_definition[[n]]$order)
                }
                
              }
            }
          }
        }
      }
      
      #=============================
      # Attach summary counts to species_summary object
      .self$species_summary$count <- data.frame(Group=names(.self$species_summary),Count=sapply(.self$species_summary,function(x) nrow(x$site_list)))
      
      # add a Name column to be used in the report text:
      .self$species_summary$count$Name <- .self$species_summary$count$Group
      .w <- which(.self$species_summary$count$Group == 'wild_animals')
      if (length(.w) > 0) .self$species_summary$count$Name[.w] <- 'wild species'
      
      .w <- which(.self$species_summary$count$Group == 'wild_mammals')
      if (length(.w) > 0) .self$species_summary$count$Name[.w] <- 'mammals'
      
      .w <- which(.self$species_summary$count$Group == 'large_mammals')
      if (length(.w) > 0) .self$species_summary$count$Name[.w] <- 'large mammals'
      #------
      ####################
      
      #---------------------------------------------------
      
      # Capture
      
      
      process_capture_data <- lapply(.self$data_year, function(data_year) {
        
        # Capture summary for the year
        capture_data <- captures(data_year)
        
        # Capture by location (for counting number of unique camera sites)
        capture_data_locations <- captures(data_year, by = "locationName") %>%
          group_by(vernacularNames.eng) %>%
          summarise(location_count = n_distinct(locationName), .groups = "drop") %>%
          arrange(vernacularNames.eng)
        
        # Process and clean
        capture_data <- capture_data %>%
          mutate(
            class = ifelse(order == "Rodentia", "Mammalia", class),
            capture_rate = case_when(
              capture_rate >= 1    ~ round(capture_rate, 2),
              capture_rate >= 0.1  ~ round(capture_rate, 3),
              TRUE                 ~ round(capture_rate, 4)
            )
          ) %>%
          arrange(vernacularNames.eng)
        
        # Return a list of vectors for later combination
        list(
          Species_Name = capture_data$vernacularNames.eng,
          Captures = capture_data$captures,
          Capture_Rate = capture_data$capture_rate,
          Locations = capture_data_locations$location_count
        )
      })
      
      
      #Combine yearly data into a dataframe
      capture_df <- do.call(rbind, lapply(names(process_capture_data), function(year) {
        species_data <- process_capture_data[[year]]
        
        if (length(species_data$Species_Name) > 0) {
          data.frame(
            Year = year,
            Species_Name = species_data$Species_Name,
            Captures = species_data$Captures,
            Capture_Rate = species_data$Capture_Rate,
            Locations = species_data$Locations
          )
        } else {
          NULL
        }
      }))
      
      # Calculate total capture data
      capture_data_total <- captures(.self$data) %>%
        mutate(
          class = ifelse(order == "Rodentia", "Mammalia", class),
          capture_rate = case_when(
            capture_rate >= 1    ~ round(capture_rate, 2),
            capture_rate >= 0.1  ~ round(capture_rate, 3),
            TRUE                 ~ round(capture_rate, 4)
          ))
      
      # Add 'total' year label
      capture_data_total$Year <- "total"
      
      # Count distinct locations per species (total)
      capture_locations <- captures(.self$data, by = "locationName") %>%
        group_by(vernacularNames.eng) %>%
        summarise(Locations = n_distinct(locationName), .groups = "drop")
      
      # Merge location counts
      capture_data_total <- left_join(capture_data_total, capture_locations, by = "vernacularNames.eng")
      
      # Clean and rename
      capture_data_total <- capture_data_total %>%
        dplyr::select(Year, vernacularNames.eng, captures, capture_rate, Locations) %>%
        rename(
          Species_Name = vernacularNames.eng,
          Captures = captures,
          Capture_Rate = capture_rate
        )
      
      # Combine yearly and total data
      capture_df <- bind_rows(capture_df, capture_data_total)
      
      # Merge scientific names from taxonomy table
      taxonomy_info <- .self$data$taxonomy %>%
        dplyr::select(vernacularNames.eng, scientificName) %>%
        distinct()
      
      capture_df <- left_join(
        capture_df,
        taxonomy_info,
        by = c("Species_Name" = "vernacularNames.eng")
      )
      
      # Final column order
      .self$capture <- capture_df %>%
        dplyr::select(Species_Name, scientificName, Year, Captures, Capture_Rate, Locations)
      #----------
      rm(process_capture_data,capture_data_total,capture_locations,taxonomy_info,capture_df)
      gc()
      #-----------
      ############################
      # setting the focus_groups:
      if (!is.null(.self$setting$focus_groups) 
          && any(.self$setting$focus_groups %in% .self$species_summary$count$Group)) {
        .self$setting$focus_groups <- .self$species_summary$count$Group[.self$species_summary$count$Group %in% .self$setting$focus_groups]
      } else if (any(c('wild_mammals','large_mammals','birds','amphibians','reptiles') %in% .self$species_summary$count$Group)) {
        .self$setting$focus_groups <- .self$species_summary$count$Group[.self$species_summary$count$Group %in% c('wild_mammals','large_mammals','birds','amphibians','reptiles')]
      }
      ############################
      if (!is.null(.self$setting$focus_groups)) {
        .sp <- .self$get_speciesNames(.self$setting$focus_groups)
        if (length(.self$.species_NO_rem) > 0) {
          .sp <- .sp[!.sp %in% .self$.species_NO_rem]
        }
        #----
        if (length(.sp) > 0) {
          for (n in .sp) {
            .w <- .self$get_REM(n)
          }
          rm(.w,.sp,n); gc()
        } else rm(.sp)
      }
      #-------
      
      
      
      #dat <- .tax_obs %>% left_join(.self$data$sequences,by='sequenceID',relationship = 'many-to-many') %>% left_join(cm$data_merged[,c("deploymentID", "latitude", "longitude")],by="deploymentID")
      #-----
      # if (!.self$is_REM_done || length(.self$rem) == 0) {
      #   for (.g in .self$setting$focus_groups) {
      #     .spn <- .self$get_speciesNames(group=.g)
      #     #--
      #     
      #     
      #     
      #   }
      #   
      # }
      
      
      
      #------------
      .tmp <- lapply(names(.self$data_year), function(x) {
        .data_year <- .self$data_year[[x]]
        # Extract deployment start and end dates
        deployment_intervals <- .data_year$deployments$deployment_interval
        start_dates <- int_start(deployment_intervals)
        end_dates <- int_end(deployment_intervals)
        
        # Group deployments by location
        deployments_grouped <- .data_year$deployments %>%
          mutate(start_date = start_dates, end_date = end_dates) %>%
          group_by(locationID) %>%
          summarise(start_date = min(start_date), end_date = max(end_date), .groups = "drop")
        
        # Identify latest deployment end date
        latest_end <- max(deployments_grouped$end_date)
        
        # Count failed cameras (stopped >5 days before the latest end date)
        failed_cameras <- deployments_grouped %>%
          dplyr::filter(end_date < (latest_end - days(5))) %>%
          nrow()
        
        # Compute runtime in days
        deployments_grouped <- deployments_grouped %>%
          mutate(runtime_days = as.numeric(end_date - start_date, units = "days"))
        
        # Calculate key statistics
        average_runtime <- mean(deployments_grouped$runtime_days, na.rm = TRUE) %>% round(0)
        runtime_range <- range(deployments_grouped$runtime_days, na.rm = TRUE) %>% round(0)
        runtime_range <- paste(runtime_range[1], "-", runtime_range[2])
        total_runtime_days <- round(sum(deployments_grouped$runtime_days, na.rm = TRUE))
        total_runtime_years <- round(total_runtime_days / 365, 2)
        
        # Return statistics as a list
        list(
          year = as.numeric(x),
          failed_cameras = failed_cameras,
          average_runtime = average_runtime,
          runtime_range = runtime_range,
          total_runtime_days = total_runtime_days,
          total_runtime_years = total_runtime_years
        )
      })
      #---
      .self$camera_stats <- bind_rows(.tmp)
      #------------
      # Calculate observation statistics for each year
      .tmp <- lapply(names(.self$data_year), function(x) {
        .data_year <- .self$data_year[[x]]
        list(
          year = as.numeric(x),
          number_of_photos = nrow(.data_year$media),  # Total number of photos (media files)
          number_of_observations = n_distinct(.data_year$observations$observationID),  # Unique observations
          number_of_sequences = n_distinct(.data_year$observations$sequenceID),  # Unique sequences
          number_of_animals = sum(.data_year$observations$observationType == "animal", na.rm = TRUE)  # Animal observations
        )
      })
      
      #---
      .self$observation_stats <- bind_rows(.tmp)
      #-----------
      # Calculate observation statistics for each year
      .tmp <- lapply(names(.self$data_year), function(x) {
        .data_year <- .self$data_year[[x]]
        # Total species count
        total_species <- length(unique(.data_year$taxonomy$scientificName))
        
        # Filter Wild Mammals (excluding domestic species)
        wild_mammals <- .data_year$taxonomy %>%
          dplyr::filter(class == "Mammalia" & scientificName %in% .self$frequent_species$scientificName)
        
        # Count unique wild mammals
        number_of_wild_mammals <- length(unique(wild_mammals$scientificName))
        list(
          year = as.numeric(x),
          total_species = total_species,
          number_of_wild_mammals = number_of_wild_mammals
        )
      })
      
      #---
      .self$species_stats <- bind_rows(.tmp)
      #----------------------
      #----------------------
      ###### Average sun time:
      if (.require('suncalc')) {
        .s <- sapply(as.character(.self$data$deployments$deployment_interval),function(x) strsplit(x,'--')[[1]][1])
        .e <- sapply(as.character(.self$data$deployments$deployment_interval),function(x) strsplit(x,'--')[[1]][2])
        names(.s) <- names(.e) <-NULL
        
        deploy_dates <-data.frame(start=as.Date(.s),end=as.Date(.e))
        
        
        # Build sun_input and compute sunlight times
        sun_input <- data.frame(
          date = seq(min(deploy_dates$start), max(deploy_dates$end), by = "day"),
          lat = mean(.self$data$locations$latitude, na.rm = TRUE),
          lon = mean(.self$data$locations$longitude, na.rm = TRUE)
        )
        #====
        .sun_times <- .eval('suncalc::getSunlightTimes(data = sun_input, keep = c("sunrise", "sunset"), tz = .self$setting$tz)',env=environment()) 
        
        # Convert to numeric hours
        .sun_times$sunrise_hour <- .get_hour(.sun_times$sunrise)
        .sun_times$sunset_hour <- .get_hour(.sun_times$sunset)
        
        # Compute yearly averages
        avg_sun_times <- .sun_times %>%
          mutate(year = .getYear(date)) %>%
          group_by(year) %>%
          summarise(
            avg_sunrise = mean(sunrise_hour, na.rm = TRUE),
            avg_sunset = mean(sunset_hour, na.rm = TRUE)
          )
        #----
        .self$sun_times <- as.data.frame(avg_sun_times)
        
        rm(avg_sun_times,.sun_times,sun_input,deploy_dates,.s,.e)
        
      } else {
        warning('The "suncalc" package is not installed; it is required for the analysis of activity patterns...')
      }
      #------------
      # effort_by_site <- .self$data$deployments %>%
      #   left_join(
      #     .self$data$locations %>% select(locationID, locationName),
      #     by = "locationID"
      #   ) %>%
      #   mutate(
      #     effort = .get_Time_length(deployment_interval,unit='days')
      #   ) %>%
      #   group_by(locationName) %>%
      #   summarise(effort = sum(effort), .groups = "drop")
      
      # Captures per species × site, with scientificName
      caps_by_site <- captures(.self$data, by = "locationName") %>%
        left_join(
          .self$data$taxonomy %>% 
            dplyr::select(taxonID, scientificName),
          by = "taxonID"
        ) 
      
      
      
      obs_with_habitat <- caps_by_site %>%
        left_join(.self$data$locations, by = "locationName")
      
      summary_by_species_habitat <- obs_with_habitat %>%
        group_by(
          observationType,
          class,
          order,
          Habitat,
          taxonID,
          scientificName
        ) %>%
        summarise(
          captures   = sum(captures, na.rm = TRUE),
          effort     = sum(effort,   na.rm = TRUE),
          .groups    = "drop"
        ) %>%
        mutate(
          capture_rate = captures / effort
        ) %>%
        dplyr::select(
          observationType,
          class,
          order,
          Habitat,
          taxonID,
          scientificName,
          captures,
          effort,
          capture_rate
        ) %>%
        arrange(scientificName, Habitat)
      
      .self$species_summary_by_habitat <- as.data.frame(summary_by_species_habitat)
      
      rm(caps_by_site,obs_with_habitat,summary_by_species_habitat)
      gc()
      
      
      ###########
      # get all specie
      # all_species <- unique(.tax_obs$scientificName)
      # all_species <- all_species[!is.na(all_species)]
      # 
      # # but keep only those with at least one distance measurement
      # usable_species <- pkg$data$observations %>%
      #   filter(!is.na(dist)) %>% 
      #   pull(scientificName) %>% 
      #   unique()
      # 
      # usable_species <- .tax_obs %>%
      #   filter(!is.na(radius)) %>% 
      #   pull(scientificName) %>% 
      #   unique()
      # 
      # 
      # # # optionally warn which you’re dropping
      # # dropped <- setdiff(all_species, usable_species)
      # # if (length(dropped) > 0) {
      # #   warning("Dropping species with no distance data: ", paste(dropped, collapse = ", "))
      # # }
      # # 
      # # for (sp in usable_species) {
      # #   sp_data <- subset(pkg, species == sp)
      # #   if (sum(!is.na(sp_data$radius)) >= 10) {
      # #     fit_detmodel(radius ~ 1, pkg, species = sp, truncation = "5%")
      # #   } else {
      # #     cat("Skipping", sp, "- not enough valid position data.\n")
      # #   }
      # # }
      # 
      # # now use usable_species in your pre-fit loop
      # params_by_species <- list()
      # for (sp in usable_species) {
      #   message("Pre-fitting models for ", sp)
      #   params_by_species[[sp]] <- list(
      #     radius_model   = fit_detmodel(radius ~ 1, .self$pkg, species = sp, truncation = "5%"),
      #     angle_model    = fit_detmodel(angle  ~ 1, .self$pkg, species = sp, unit = "radian"),
      #     speed_model    = fit_speedmodel(.self$pkg, species = sp),
      #     activity_model = fit_actmodel(.self$pkg, species = sp, reps = 10)
      #   )
      # }
      # 
      # 
      # # Combine & export 
      # 
      # estimates_rem_all <- do.call(rbind, density_list)
      
      
      message('Setup is done!')
    
    },
    show = function() {
      cat('Camera trap Object for the site :' , .self$siteName, '\n')
      cat('=====================================================','\n')
      cat('Total number of sequences       : ' ,sum(.self$observation_stats$number_of_sequences,na.rm=TRUE), '\n')
      cat('Total number of observations    : ' ,sum(.self$observation_stats$number_of_observations,na.rm=TRUE), '\n')
      cat('Total number of animals         : ' ,sum(.self$observation_stats$number_of_animals,na.rm=TRUE), '\n')
      cat('Total number of detected species: ' ,max(.self$species_stats$total_species,na.rm=TRUE), '\n')
      cat('Date/time (years) with data     : ' ,.paste_comma_and(.self$observation_stats$year),'\n')
      cat('-----------------------------------------------------\n')
    },
    filter = function() {
      # filter uses the conditions to keep or exclude (also filterCount) to extract
      # frequent_species data.frame (assigned to the field with the same name)
      # later, the names of these species will be used to subset data in analyses
      
      if (length(.self$filterExclude) > 0 || length(.self$filterKeep) > 0 || length(.self$filterCount) > 0) {
        w1 <- w2 <- w3 <- w4 <- TRUE
        if (length(.self$filterKeep) > 0 && 'order' %in% names(.self$filterKeep) && any(!is.na(.self$data$taxonomy$order))) {
          w1 <- .self$data$taxonomy$order %in% .self$filterKeep$order
        } 
        
        if (length(.self$filterExclude) > 0) {
            .n <- names(.self$filterExclude)
            .n <- .n[.n %in% colnames(.self$data$taxonomy)]
            if (length(.n) > 0) {
              for (nn in .n) {
                w2 <- w2 & !(.self$data$taxonomy[[nn]] %in% .self$filterExclude[[nn]])
              }
            }
          }
        #----
        # if (length(.self$filterKeep) > 0 && 'observationType' %in% names(.self$filterKeep)) {
        #   w3 <- .self$data$observations[['observationType']] %in% .self$filterKeep$observationType
        # }
        
        if (length(.self$filterKeep) > 0 && any(names(.self$filterKeep) %in% colnames(.self$data$observations))) {
          for (.n in names(.self$filterKeep)) {
            if (.n %in% colnames(.self$data$observations) && !is.null(.self$filterKeep[[.n]])) {
              w3 <- w3 & (.self$data$observations[[.n]] %in% .self$filterKeep[[.n]])
            }
          }
        }
        #---
        
        w3 <- .self$data$taxonomy$taxonID %in% .self$data$observations$taxonID[w3]
        if (length(.self$filterCount) > 0) {
          w4 <- .self$data$taxonomy$taxonID %in% (.self$observed_counts$taxonID[.self$observed_counts$count > .self$filterCount[1]])
        }
        w <- w1 & w2 & w3 & w4
        .self$frequent_species <- data.frame(scientificName=.self$data$taxonomy$scientificName[w],vernacularNames.eng=.self$data$taxonomy$vernacularNames.eng[w])
        
      } else warning('\nNo filtering condition is specified in the "filterExclude" or "filterKeep" fields!')
    },
    addReportObject = function(x) {
      # add either text or Rchunk object to reportObjects list
      .done <- FALSE
      if (inherits(x,'.textSection')) {
        if (is.null(x@parent) || x@parent == "" || x@parent == ".root") {
          x@headLevel <- 1
          if (is.list(.self$reportObjects[[x@name]])) .self$reportObjects[[x@name]][[x@name]] <- x
          else .self$reportObjects[[x@name]] <- x
          .done <- TRUE
        } else {
          if (x@parent %in% names(.self$reportObjects)) {
            x@headLevel <- 2
            if (is.list(.self$reportObjects[[x@parent]])) {
              .self$reportObjects[[x@parent]][[x@name]] <- x
              .done <- TRUE
            } else {
              .tmp <- list()
              .tmp[[.self$reportObjects[[x@parent]]@name]] <- .self$reportObjects[[x@parent]]
              .self$reportObjects[[x@parent]] <- .tmp
              .self$reportObjects[[x@parent]][[x@name]] <- x
              .done <- TRUE
            }
          } else {
            # for (n in names(.self$reportObjects)) {
            #   if (is.list(.self$reportObjects[[n]]) && x@parent %in% names(.self$reportObjects[[n]])) {
            #     .self$reportObjects[[n]]
            #   }
            # }
            #.tmp <- .findParent(.self$reportObjects,x@parent)
            
            for (.n in names(.self$reportObjects)) {
              .tmp <- .self$reportObjects[[.n]]
              if (is.list(.tmp)) {
                if (x@parent %in% names(.tmp)) {
                  x@headLevel <- 3
                  if (is.list(.tmp[[x@parent]])) {
                    .self$reportObjects[[.n]][[x@parent]][[x@name]] <- x
                    .done <- TRUE
                  } else {
                    .tmp <- list()
                    .tmp[[.self$reportObjects[[.n]][[x@parent]]@name]] <- .self$reportObjects[[.n]][[x@parent]]
                    .self$reportObjects[[.n]][[x@parent]] <- .tmp
                    .self$reportObjects[[.n]][[x@parent]][[x@name]] <- x
                    .done <- TRUE
                  }
                }
              } else if (inherits(.tmp,'.textSection')) {
                if (x@parent == .tmp@name) {
                  x@headLevel <- 2
                  .tmp <- list()
                  .tmp[[.self$reportObjects[[.n]]@name]] <- .self$reportObjects[[.n]]
                  .self$reportObjects[[.n]] <- .tmp
                  .self$reportObjects[[.n]][[x@parent]][[x@name]] <- x
                  .done <- TRUE
                }
              }
            }
          } 
        }
        
        if (!.done) stop('The text section is not added (the "parent" is unknown...)!')
        
      } else if (inherits(x,'.Rchunk')) {
        .added <- FALSE
        if (x@parent %in% names(.self$reportObjects)) {
          if (is.list(.self$reportObjects[[x@parent]])) {
            .tmp <- sapply(.self$reportObjects[[x@parent]],function(.x) .x@name)
            .w <- which(.tmp == x@parent)
            if (length(.w) > 1) {
              if (is.null(.self$reportObjects[[x@parent]][[.w[1]]]@Rchunk)) {
                .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk <- x
                .added <- TRUE
              } else {
                if (is.list(.self$reportObjects[[x@parent]][[.w[1]]]@Rchunk)) {
                  if (length(.self$reportObjects[[x@parent]][[.w[1]]]@Rchunk) > 0) {
                    .n <- sapply(.self$reportObjects[[x@parent]][[.w[1]]]@Rchunk,function(x) x@name)
                    if (x@name %in% .n) {
                      .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk[[.n == x@name]] <- x
                      .added <- TRUE
                    } else {
                      .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                      .added <- TRUE
                    }
                  } else {
                    .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                    .added <- TRUE
                  }
                } else {
                  if (.self$reportObjects[[x@parent]][[.w[1]]]@Rchunk@name == x@name) {
                    .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk <- x
                    .added <- TRUE
                  } else {
                    .tmp <- list()
                    .tmp[[.self$reportObjects[[x@parent]][[.w[1]]]@Rchunk@name]] <- .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk
                    .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk <- .tmp
                    .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                    .added <- TRUE
                  }
                }
              }
            } #else message('Error: the parent does not available; code is NOT added!')
          } else {
            if (is.null(.self$reportObjects[[x@parent]]@Rchunk)) {
              .self$reportObjects[[x@parent]]@Rchunk <- x
              .added <- TRUE
            } else {
              if (is.list(.self$reportObjects[[x@parent]]@Rchunk)) {
                if (length(.self$reportObjects[[x@parent]]@Rchunk) > 0) {
                  .n <- sapply(.self$reportObjects[[x@parent]]@Rchunk,function(x) x@name)
                  if (x@name %in% .n) {
                    .self$reportObjects[[x@parent]]@Rchunk[[.n == x@name]] <- x
                    .added <- TRUE
                  } else {
                    .self$reportObjects[[x@parent]]@Rchunk[[x@name]] <- x
                    .added <- TRUE
                  }
                } else {
                  .self$reportObjects[[x@parent]]@Rchunk[[x@name]] <- x
                  .added <- TRUE
                }
              } else {
                if (.self$reportObjects[[x@parent]]@Rchunk@name == x@name) {
                  .self$reportObjects[[x@parent]]@Rchunk <- x
                  .added <- TRUE
                } else {
                  .tmp <- list()
                  .tmp[[.self$reportObjects[[x@parent]]@Rchunk@name]] <- .self$reportObjects[[x@parent]]@Rchunk
                  .self$reportObjects[[x@parent]]@Rchunk <- .tmp
                  .self$reportObjects[[x@parent]]@Rchunk[[x@name]] <- x
                  .added <- TRUE
                }
              }
            }
          }
        } else {
          for (.n in names(.self$reportObjects)) {
            if (is.list(.self$reportObjects[[.n]])) {
              if (x@parent %in% names(.self$reportObjects[[.n]])) {
                if (is.list(.self$reportObjects[[.n]][[x@parent]])) {
                  .tmp <- sapply(.self$reportObjects[[.n]][[x@parent]],function(.x) .x@name)
                  .w <- which(.tmp == x@parent)
                  if (length(.w) > 1) {
                    if (is.null(.self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk)) {
                      .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk <- x
                      .added <- TRUE
                      break
                    } else {
                      if (is.list(.self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk)) {
                        if (length(.self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk) > 0) {
                          .nn <- sapply(.self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk,function(x) x@name)
                          if (x@name %in% .nn) {
                            .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk[[.nn == x@name]] <- x
                            .added <- TRUE
                            break
                          } else {
                            .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                            .added <- TRUE
                            break
                          }
                        } else {
                          .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                          .added <- TRUE
                          break
                        }
                      } else {
                        if (.self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk@name == x@name) {
                          .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk <- x
                          .added <- TRUE
                          break
                        } else {
                          .tmp <- list()
                          .tmp[[.self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk@name]] <- .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk
                          .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk <- .tmp
                          .self$reportObjects[[.n]][[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                          .added <- TRUE
                          break
                        }
                      }
                    }
                  }
                } else {
                  if (is.null(.self$reportObjects[[.n]][[x@parent]]@Rchunk)) {
                    .self$reportObjects[[.n]][[x@parent]]@Rchunk <- x
                    .added <- TRUE
                    break
                  } else {
                    if (is.list(.self$reportObjects[[.n]][[x@parent]]@Rchunk)) {
                      if (length(.self$reportObjects[[.n]][[x@parent]]@Rchunk) > 0) {
                        .nn <- sapply(.self$reportObjects[[.n]][[x@parent]]@Rchunk,function(x) x@name)
                        if (x@name %in% .nn) {
                          .self$reportObjects[[.n]][[x@parent]]@Rchunk[[.nn[.nn == x@name]]] <- x
                          .added <- TRUE
                          break
                        } else {
                          .self$reportObjects[[.n]][[x@parent]]@Rchunk[[x@name]] <- x
                          .added <- TRUE
                          break
                        }
                      } else {
                        .self$reportObjects[[.n]][[x@parent]]@Rchunk[[x@name]] <- x
                        .added <- TRUE
                        break
                      }
                    } else {
                      if (.self$reportObjects[[.n]][[x@parent]]@Rchunk@name == x@name) {
                        .self$reportObjects[[.n]][[x@parent]]@Rchunk <- x
                        .added <- TRUE
                        break
                      } else {
                        .tmp <- list()
                        .tmp[[.self$reportObjects[[.n]][[x@parent]]@Rchunk@name]] <- .self$reportObjects[[.n]][[x@parent]]@Rchunk
                        .self$reportObjects[[.n]][[x@parent]]@Rchunk <- .tmp
                        .self$reportObjects[[.n]][[x@parent]]@Rchunk[[x@name]] <- x
                        .added <- TRUE
                        break
                      }
                    }
                  }
                }
              } else {
                for (.nn in names(.self$reportObjects[[.n]])) {
                  if (is.list(.self$reportObjects[[.n]][[.nn]])) {
                    if (x@parent %in% names(.self$reportObjects[[.n]][[.nn]])) {
                      if (is.list(.self$reportObjects[[.n]][[.nn]][[x@parent]])) {
                        .tmp <- sapply(.self$reportObjects[[.n]][[.nn]][[x@parent]],function(.x) .x@name)
                        .w <- which(.tmp == x@parent)
                        if (length(.w) > 1) {
                          if (is.null(.self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk)) {
                            .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk <- x
                            .added <- TRUE
                            break
                          } else {
                            if (is.list(.self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk)) {
                              if (length(.self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk) > 0) {
                                .nnn <- sapply(.self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk,function(x) x@name)
                                if (x@name %in% .nnn) {
                                  .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk[[.nnn == x@name]] <- x
                                  .added <- TRUE
                                  break
                                } else {
                                  .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                                  .added <- TRUE
                                  break
                                }
                              } else {
                                .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                                .added <- TRUE
                                break
                              }
                            } else {
                              if (.self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk@name == x@name) {
                                .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk <- x
                                .added <- TRUE
                                break
                              } else {
                                .tmp <- list()
                                .tmp[[.self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk@name]] <- .self$reportObjects[[x@parent]][[.w[1]]]@Rchunk
                                .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk <- .tmp
                                .self$reportObjects[[.n]][[.nn]][[x@parent]][[.w[1]]]@Rchunk[[x@name]] <- x
                                .added <- TRUE
                                break
                              }
                            }
                          }
                        }
                      } else {
                        if (is.null(.self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk)) {
                          .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk <- x
                          .added <- TRUE
                          break
                        } else {
                          if (is.list(.self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk)) {
                            if (length(.self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk) > 0) {
                              .nnn <- sapply(.self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk,function(x) x@name)
                              if (x@name %in% .nnn) {
                                .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk[[.nnn[.nnn == x@name]]] <- x
                                .added <- TRUE
                                break
                              } else {
                                .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk[[x@name]] <- x
                                .added <- TRUE
                                break
                              }
                            } else {
                              .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk[[x@name]] <- x
                              .added <- TRUE
                              break
                            }
                          } else {
                            if (.self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk@name == x@name) {
                              .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk <- x
                              .added <- TRUE
                              break
                            } else {
                              .tmp <- list()
                              .tmp[[.self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk@name]] <- .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk
                              .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk <- .tmp
                              .self$reportObjects[[.n]][[.nn]][[x@parent]]@Rchunk[[x@name]] <- x
                              .added <- TRUE
                              break
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            } else {
              if (.self$reportObjects[[.n]]@name == x@parent) {
                if (is.null(.self$reportObjects[[.n]]@Rchunk)) {
                  .self$reportObjects[[.n]]@Rchunk <- x
                  .added <- TRUE
                  break
                } else {
                  if (is.list(.self$reportObjects[[.n]]@Rchunk)) {
                    if (length(.self$reportObjects[[.n]]@Rchunk) > 0) {
                      .nn <- sapply(.self$reportObjects[[.n]]@Rchunk,function(x) x@name)
                      if (x@name %in% .nn) {
                        .self$reportObjects[[.n]]@Rchunk[[.nn[.nn == x@name]]] <- x
                        .added <- TRUE
                        break
                      } else {
                        .self$reportObjects[[.n]]@Rchunk[[x@name]] <- x
                        .added <- TRUE
                        break
                      }
                    } else {
                      .self$reportObjects[[.n]]@Rchunk[[x@name]] <- x
                      .added <- TRUE
                      break
                    }
                  } else {
                    if (.self$reportObjects[[.n]]@Rchunk@name == x@name) {
                      .self$reportObjects[[.n]]@Rchunk <- x
                      .added <- TRUE
                      break
                    } else {
                      .tmp <- list()
                      .tmp[[.self$reportObjects[[.n]]@Rchunk@name]] <- .self$reportObjects[[.n]]@Rchunk
                      .self$reportObjects[[.n]]@Rchunk <- .tmp
                      .self$reportObjects[[.n]]@Rchunk[[x@name]] <- x
                      .added <- TRUE
                      break
                    }
                  }
                }
              }
            }
          }
        }
        #----
        # if (!.added) message('\nthe parent .textSection is not found... The R chunk is NOT added!!')
        # else message('\n...Done!')
      }
      
    },
    extractYears = function(update=FALSE) {
      if (update || length(.self$years) == 0) {
        .self$years <- as.numeric(unique(substr(as.character(as.Date(.self$data$media$media_timestamp)),1,4)))
      }
      #-----
      .self$years
    },
    generateReport = function(output_file = "cam_report.html") {
      
      rmd_file <- file.path(getwd(), "test_1.Rmd")
      rmd_template <- glue::glue("
---
title: \"{title}\"
author: \"{authors}\"
date: \"`r format(Sys.Date(), '%B %d, %Y')`\"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: flatly       
    highlight: tango    
    df_print: paged
    number_sections: true
    self_contained: true
---

```{{r load library, echo=FALSE, results='hide', message=FALSE, warning=FALSE}}
library(knitr)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(RColorBrewer)
library(rmarkdown)
library(ctdp)
library(camtraptor)
library(htmltools)
library(stringr)
library(xts) 
library(camtrapDensity)
library(lubridate)
library(gridExtra)
library(leaflet)
library(magick)
library(spatstat)
library(terra)
library(tidyverse)
library(DT)
library(data.table)
library(gt)
library(htmlwidgets)
library(ggthemes)
library(camtrapdp)
library(devtools)

```
      ")
      
      for (.n in names(.self$reportObjects)) {
        .x <- .self$reportObjects[[.n]]
        if (is.list(.x)) {
          for (.nn in names(.self$reportObjects[[.n]])) {
            .xx <- .self$reportObjects[[.n]][[.nn]]
            if (is.list(.xx)) {
              for (.nnn in names(.self$reportObjects[[.n]][[.nn]])) {
                .xxx <- .self$reportObjects[[.n]][[.nn]][[.nnn]]
                rmd_template <- paste0(rmd_template,'\n\n',.glueTextSection(.xxx,.envir = .self))
              }
            } else {
              rmd_template <- paste0(rmd_template,'\n\n',.glueTextSection(.xx,.envir = .self))
            }
          }
        } else {
          rmd_template <- paste0(rmd_template,'\n\n',.glueTextSection(.x,.envir = .self))
        }
      }
      
      # Write out the R Markdown file
      cat(rmd_template, file = rmd_file, sep = "\n")
      
      # Render the R Markdown file
      # We can pass an environment so that the Rmd sees the object fields directly
      # One approach: pass the entire object as 'object' in the environment
      render_env <- new.env(parent = globalenv())
      render_env$object <- .self
      
      message("Rendering R Markdown report ...")
      out <- rmarkdown::render(
        input       = rmd_file, 
        output_file = output_file,
        envir       = render_env
      )
      
      message("Report generated at: ", normalizePath(out))
      return(invisible(out))
    },
    
    #---------------------------------------------------------------------------
    # A placeholder for a GUI method, possibly a Shiny app
    #---------------------------------------------------------------------------
    gui = function() {
      # Placeholder for a Shiny or other GUI
      message("Launching GUI ...")
      # ...
    }
  )
)
#-------------------