# Author: Elham Ebrahimi, eebrahimi.bio@gmail.com
# Last Update :  Jan 2026
# Version 2.1
# Licence GPL v3
#--------



# in group_definition, the groups like large_mammals, wild_mammals can be defined
# each group can be specified based on data columns (e.g., order, class, scientificNames)
# domestic group can be defined based on which the wild_animals also defined (species NOT in domestic)

camR <- setRefClass(
  "camReport",
  fields = list(
    data = "list",
    habitat = "data.frame",
    data_merged = "data.frame",
    study_area = "listORnull",
    siteName = "character",
    title = "character",
    subtitle = "character",
    authors = "character",
    institute  = "character",
    logoPath  = "character",
    annonator = "character",
    acknowledgement  = "character",
    description = "character",
    sampling = "character",
    years = "numeric",
    group_definition = "list",
    filterExclude    = "list",
    filterKeep       = "list",
    filterCount      = "numeric",
    filterDuration   = "numeric",
    observed_counts  = "data.frame",
    frequent_species = "data.frame",
    camera_stats = "data.frame",
    camera_setup = "data.frame",
    observation_stats= "data.frame",
    species_summary  = "list",
    species_stats    = "data.frame",
    species_summary_by_habitat = "data.frame",
    capture = "data.frame",
    density_estimates= "data.frame",
    setting = "list",
    sun_times = "data.frameORnull",
    packages = "character",
    info = "list",
    rem = "list",
    .rem_params  = "list",
    .any_data_for_rem  = 'logical', # a vector of species names for which REM could not be fitted!
    .tempObjects  = "list",
    reportTextElements = "list",
    reportObjectElements = "list",
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
      .self$title <- 'Report generated using the camtrapReport package'
      .self$authors <- 'Elham Ebrahimi, Patrick Jansen'
      #-----
      .self$reportObjectElements$color_palette <- c(
        "#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377",
        "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C",
        "#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C",
        "#DAA51B", "#2F8AC4", "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99",
        "#6D904F", "#F6C85F", "#B276B2", "#DECF3F", "#FAA43A", "#60BD68",
        "#F15854", "#4D4D4D", "#B2912F", "#7B615C", "#1F77B4", "#FF7F0E",
        "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#17BECF"
      )
      .self$reportObjectElements$richness_palette <- c(
        "#2D8B3F", "#91FF00", "#FFDA00", "#FF9100", "#FF4800",
        "#FF0000", "#D33682", "#7F00FF", "#00CED1", "#FFA07A",
        "#1E90FF", "#8B4513", "#808000", "#FF69B4", "#00FA9A",
        "#A52A2A", "#9370DB", "#4682B4", "#FF1493", "#20B2AA"
      )
      
      .self$reportObjectElements$acadia_colors <- c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", 
                                                    "#526A83", "#625377", "#68855C", "#9C9C5E", 
                                                    "#A06177", "#8C785D", "#467378", "#7C7C7C")
      .self$.tempObjects$fig.n <- 1
      .self$.tempObjects$tab.n <- 1
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
    get_speciesNames = function(group=NULL,all=FALSE) {
      # if all = T -> all species (without accounting for filtering is returned)
      if (all) return(unique(.self$data$observations$scientificName))
      #-------------------------------
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
    getFigureNumber = function(text=TRUE) {
      # if text if FALSE, only number is returned!
      .n <- .self$.tempObjects$fig.n
      .self$.tempObjects$fig.n <- .n + 1
      if (text) paste0('**Fig.',.n,'**:')
      else .n
    },
    getTableNumber = function(text=TRUE) {
      .n <- .self$.tempObjects$tab.n
      .self$.tempObjects$tab.n <- .n + 1
      if (text) paste0('**Table ',.n,'**:')
      else .n
      
    },
    recetFigTabNumber = function() {
      .self$.tempObjects$fig.n <- .self$.tempObjects$tab.n <- 1
    },
    richness=function(year=NULL,spList = NULL) {
      # if year is NULL -> total
      # spList can be a vector of scientificNames (e.g., related to a focus_group); if NULL --> all species
      
      .d <- .self$data$observations |> 
        dplyr::filter(!is.na(scientificName)) |> 
        left_join(.self$data$deployments,by='deploymentID') |> 
        mutate(year=.getYear(timestamp)) |>
        dplyr::select(scientificName,locationID,year)
      
      if (!is.null(spList)) .d <- .d[.d$scientificName %in% spList,]
      #------
      if (is.null(year)) {
        .rich <- .d |> group_by(locationID) |> 
          summarise(
            Richness = length(unique(scientificName)),
            Species_List = paste(sort(unique(scientificName)),collapse=', '),
            Community_Composition = paste(paste0(paste0(sort(unique(scientificName)),' ('),paste0(table(scientificName)[sort(unique(scientificName))],')')),collapse = ', '),
            .groups = "drop"
          )  |> left_join(.self$data$locations,by='locationID')
      } else {
        year <- year[year %in% .self$years]
        if (length(year) > 0) {
          .d <- .d[.d$year %in% year,]
          .rich <- .d |> group_by(locationID,year) |> 
            summarise(
              Richness = length(unique(scientificName)),
              Species_List = paste(sort(unique(scientificName)),collapse=', '),
              Community_Composition = paste(paste0(paste0(sort(unique(scientificName)),' ('),paste0(table(scientificName)[sort(unique(scientificName))],')')),collapse = ', '),
              .groups = "drop"
            ) |> left_join(.self$data$locations,by='locationID')
          
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
        xr[1] <- min(c(.ext[1],xr[1]))
        xr[2] <- max(c(.ext[2],xr[2]))
        yr[1] <- min(c(.ext[3],yr[1]))
        yr[2] <- max(c(.ext[4],yr[2]))
      } else if (!is.null(object$study_area) && !is.null(object$study_area$path)) {
        .self$study_area$object <- readRDS(.self$study_area$path)
        .ext <- as.vector(ext(.self$study_area$object))
        xr[1] <- min(c(.ext[1],xr[1]))
        xr[2] <- max(c(.ext[2],xr[2]))
        yr[1] <- min(c(.ext[3],yr[1]))
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
        .d <- .self$data$observations |> 
          dplyr::filter(!is.na(scientificName), 
                        scientificName != "",
                        grepl("\\s", scientificName)) |>
          left_join(.self$data$deployments,by='deploymentID') |> 
          mutate(Year=.getYear(timestamp))
      } else {
        .d <- .self$data$observations |> 
          dplyr::filter(scientificName %in% spList) |>
          left_join(.self$data$deployments,by='deploymentID') |> 
          mutate(Year=.getYear(timestamp))
      }
      #----
      if (!is.null(.self$observed_counts$scientificName)) {
        .d <- .d |>
          dplyr::filter(scientificName %in% .self$observed_counts$scientificName)
      }
      #----
      if (!is.null(year)) {
        year <- year[year %in% .self$years]
        if (length(year) > 0) {
          .d <- .d |>
            dplyr::filter(Year %in% year)
        } else stop('No records are available for the specified years...!')
      }
      #-------
      if (nrow(.d) > 0) {
        .d <- .d |>
          group_by(locationID, scientificName) |>
          summarise(
            total_observations = n_distinct(observationID),
            total_count        = sum(count, na.rm = TRUE),
            .groups = "drop"
          ) |>
          left_join(.self$data$locations, by = "locationID",multiple = "any")
        
        
        #----
        if (cor_matrix) {
          if (PA) {
            .d <- .d |>
              group_by(locationID, scientificName) |>
              summarise(count = n(), .groups = "drop") |>
              .pivot_wider(names_from = scientificName, values_from = count, values_fill = 0)
            
            sp_mat <- as.matrix(.d[, -1])  
            rownames(sp_mat) <- .d$locationID
            
            sp_mat <- sp_mat[ , colSums(sp_mat) > 1, drop = FALSE]
            
            
            if (ncol(sp_mat) > 1) cor(sp_mat, use = "pairwise.complete.obs")
          } else {
            .d <- .d |>
              group_by(locationID, scientificName) |>
              summarise(count = total_count, .groups = "drop") |>
              .pivot_wider(names_from = scientificName, values_from = count, values_fill = 0)
            
            sp_mat <- as.matrix(.d[, -1])  
            rownames(sp_mat) <- .d$locationID
            
            sp_mat <- sp_mat[ , colSums(sp_mat) > 1, drop = FALSE]
            
            
            if (ncol(sp_mat) > 1) cor(sp_mat, use = "pairwise.complete.obs",method = 'spearman')
          }
          
        } else .d
      }
      
      
    },
    .get_REM_Param=function(sp) {
      if ((is.null(.self$.rem_params[[sp]]) || !is.list(.self$.rem_params[[sp]])) && .require('camtrapDensity')) {
        x <- try({
          
          radius_model <- .fit_detmodel(radius ~ 1, .self$data$observations, species = sp, truncation = "5%",quiet=TRUE)
          angle_model <- .fit_detmodel(angle ~ 1, .self$data$observations, species = sp, unit = "radian",quiet=TRUE)
          speed_model <- .fit_speedmodel(.self$data$observations, species = sp)
          
          dat <- .self$data$observations %>% left_join(.self$data$deployments %>% left_join(.self$data$locations,by='locationID'),'deploymentID')
          activity_model <- .fit_actmodel(dat, species = sp, reps = 10)
          
          rm(dat)
          
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
        dat <- .self$get_data_subset(year = year)
        if (nrow(dat$observations) > 0) {
          species_params <- .self$.get_REM_Param(sp)
          if (!is.null(species_params)) {
            x <- try({
              trdat <- .get_traprate_data(dat, species = sp)
              .parameters <- .get_parameter_table(trdat, 
                                                  radius_model = species_params$radius_model, 
                                                  angle_model = species_params$angle_model, 
                                                  speed_model = species_params$speed_model, 
                                                  activity_model = species_params$activity_model, 
                                                  reps = 10)
              .density_estimates <- .rem(.parameters)
              .density_estimates <- .eval("camtrapDensity::convert_units(.density_estimates,radius_unit = \"m\",angle_unit = \"degree\",active_speed_unit = \"km/hour\",overall_speed_unit = \"km/day\")", 
                                          environment())
              if ("vernacularNames.eng" %in% colnames(dat$taxonomy)) {
                english_name <- dat$taxonomy$vernacularNames.eng[dat$taxonomy$scientificName == 
                                                                   sp]
              }
              else if ("vernacularNames" %in% colnames(dat$taxonomy)) {
                english_name <- dat$taxonomy$vernacularNames[dat$taxonomy$scientificName == 
                                                               sp]
              }
              else english_name <- "Unknown"
              if (length(english_name) == 0) 
                english_name <- "Unknown"
              data.frame(scientificName = sp, EnglishName = english_name, 
                         Year = year, Metric = rownames(.density_estimates), 
                         .density_estimates, row.names = NULL)
            }, silent = TRUE)
            if (!inherits(x, "try-error")) {
              .density_estimate_list[[paste0(sp, "_", year)]] <- x
            }
          }
        }
      }
      if (length(.density_estimate_list) > 0) {
        for (n in names(.density_estimate_list)) {
          .self$rem[[.g]][[n]] <- .density_estimate_list[[n]]
        }
        #------------
        # also for total (all-years):
        dat <- .self$data
        if (nrow(dat$observations) > 0) {
          species_params <- .self$.get_REM_Param(sp)
          if (!is.null(species_params)) {
            x <- try({
              trdat <- .get_traprate_data(dat, species = sp)
              .parameters <- .get_parameter_table(trdat, 
                                                  radius_model = species_params$radius_model, 
                                                  angle_model = species_params$angle_model, 
                                                  speed_model = species_params$speed_model, 
                                                  activity_model = species_params$activity_model, 
                                                  reps = 10)
              .density_estimates <- .rem(.parameters)
              .density_estimates <- .eval("camtrapDensity::convert_units(.density_estimates,radius_unit = \"m\",angle_unit = \"degree\",active_speed_unit = \"km/hour\",overall_speed_unit = \"km/day\")", 
                                          environment())
              if ("vernacularNames.eng" %in% colnames(dat$taxonomy)) {
                english_name <- dat$taxonomy$vernacularNames.eng[dat$taxonomy$scientificName == sp]
              } else if ("vernacularNames" %in% colnames(dat$taxonomy)) {
                english_name <- dat$taxonomy$vernacularNames[dat$taxonomy$scientificName == 
                                                               sp]
              } else english_name <- "Unknown"
              if (length(english_name) == 0) english_name <- "Unknown"
              data.frame(scientificName = sp, EnglishName = english_name, 
                         Year = year, Metric = rownames(.density_estimates), 
                         .density_estimates, row.names = NULL)
            }, silent = TRUE)
            if (!inherits(x, "try-error")) {
              .self$rem[[.g]][[sp]] <- x
            }
          }
        }
      } else {
        if (length(.self$.any_data_for_rem) == 0) {
          .self$.any_data_for_rem <- .any_data_for_rem(.self$data)
        } else {
          .self$.any_data_for_rem[sp] <- FALSE
        }
      }
      
      
    },
    get_REM = function(.sp) {
      # extract REM results for a species from .self$rem
      # if not available, fit_REM is called!
      
      if (length(.sp) > 1) stop('length(.sp) > 1; a single species name should be provided to get_REM!')
      
      if (length(.self$.any_data_for_rem) > 0 && .sp %in% names(.self$.any_data_for_rem) && .self$.any_data_for_rem[.sp]) {
        .g <- .self$get_focus_group(.sp)
        if (.g %in% names(.self$rem)) {
          .n <- names(.self$rem[[.g]])
          .spn <- c(paste0(.sp,'_',.self$years),.sp)
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
      } else {
        if (length(.self$.any_data_for_rem) == 0) {
          .self$.any_data_for_rem <- .any_data_for_rem(.self$data)
          .self$get_REM(.sp)
        } else if (!.sp %in% names(.self$.any_data_for_rem)) {
          if (.sp %in% .self$data$observations$scientificName) {
            .self$.any_data_for_rem <- .any_data_for_rem(.self$data)
            if (.sp %in% names(.self$.any_data_for_rem) && .self$.any_data_for_rem[.sp]) .self$get_REM(.sp)
          } #else {
            #message('the specified species is not available in the dataset...!')
          #}
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
      # adding new column (Year) to deployments data.frame:
      .self$data$deployments$Year <- .getYear(.self$data$deployments$deployment_interval) 
      #----
      .y <- sort(.self$extractYears())
      ################
      # identify missing taxonomic information and retrieve "class" and "order"
      # from GBIF (it needs the taxize package)
      
      .w <- table(.self$data$observations$taxonID)
      .self$observed_counts <- left_join(data.frame(taxonID=names(.w),count=as.numeric(.w)),.self$data$taxonomy,by='taxonID')
      #-----
      .self$filter()
      ####################
      if (!'large_mammals' %in% names(.self$group_definition)) {
        if (any(c("Artiodactyla","Carnivora") %in% .self$data$taxonomy$order)) {
          .self$group_definition[['large_mammals']] <- list(order=c("Artiodactyla","Carnivora"),domestic=FALSE,observationType='animal')
        }
      }
      #-----
      if (!'domestic' %in% names(.self$group_definition)) {
        .self$group_definition[['domestic']] <- list(scientificName=c("Homo sapiens", "Canis lupus familiaris", "Felis catus",
                                                                      "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
                                                                      "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
                                                                      "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus"))

      }
      #------
      if (!'wild_animals' %in% names(.self$group_definition) && 'domestic' %in% names(.self$group_definition)) {
        .self$group_definition[['wild_animals']] <- list(scientificName = .self$data$taxonomy$scientificName[!.self$data$taxonomy$scientificName %in% .self$group_definition$domestic$scientificName])
      }
      #-----
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
      
      #----------------- #Locations with Habitat --------------------------
      
      if (nrow(.self$habitat) > 0 &&  !any(c('habitat','Habitat','Habitat_Type') %in% colnames(.self$data$locations))) {
        .self$data$locations <- .left_join(.self$data$locations,.self$habitat, by = "locationName")
        if ('habitat' %in% tolower(colnames(.self$data$locations))) {
          colnames(.self$data$locations)[tolower(colnames(.self$data$locations)) == "habitat"] <- "Habitat_Type"
        }
        
      }
      #-------
      
      .self$data_merged<- .camr_getMergedSummary(.self)
      #----
      
      
      .tmp <- lapply(.self$years, function(x) {
        .data_year <- .self$get_data_subset(x)
        if (!is.null(.data_year)) {
          # Number of unique camera trap locations
          number_camtraps <- length(unique(.data_year$location$locationName))
          
          # Group deployments by location and find min/max dates
          deployments <- .data_year$deployments |>
            group_by(locationID) |>
            summarise(start_date = min(deploymentStart,na.rm=TRUE), end_date = max(deploymentEnd,na.rm=TRUE), .groups = "drop")
          
          # Find the earliest and latest deployment dates
          earliest_start <- min(deployments$start_date,na.rm=TRUE)
          latest_end <- max(deployments$end_date,na.rm=TRUE)
          if (!is.null(.self$filterDuration) && .self$filterDuration > 0) {
            .fdur <- .self$filterDuration * 86400 # (24* 60 * 60) seconds!
            
            valid_end_dates <- deployments |> dplyr::filter(end_date >= (latest_end - .fdur))
            earliest_end <- min(valid_end_dates$end_date,na.rm=TRUE)
          } else earliest_end <- min(deployments$end_date,na.rm=TRUE)
          
          # Return the processed data as a list
          list(
            year = as.numeric(x),
            number_camtraps = number_camtraps,
            deployment_period = paste(format(earliest_start, "%Y-%m-%d"), "-", format(latest_end, "%Y-%m-%d")),
            setup_period = paste(format(earliest_start, "%d %B"), "-", format(max(deployments$start_date), "%d %B")),
            pickup_period = paste(format(earliest_end, "%B %d"), "-", format(latest_end, "%B %d"))
          )
        }
        
      })
      
      # Convert Camera Setup Data into a DataFrame
      .self$camera_setup <- bind_rows(.tmp)
      #--------------
      #################
      .tax_obs <- .self$data$observations |>
        dplyr::select(-scientificName) %>%
        left_join(.self$data$taxonomy, by = "taxonID") |>
        mutate(
          observation_date = as.Date(timestamp),
          observation_Year = .getYear(timestamp)
        )
      #-------
      
      .self$species_summary <- list()
      if (!is.null(names(.self$group_definition))) {
        for (n in names(.self$group_definition)) {
          if ('scientificName' %in%  names(.self$group_definition[[n]])) {
            .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,scientificName = .self$group_definition[[n]]$scientificName)
          } else if (any(c('class','order') %in% names(.self$group_definition[[n]]))) {
            if ('class' %in% names(.self$group_definition[[n]])) {
              if ('order' %in% names(.self$group_definition[[n]])) {
                if ('domestic' %in% names(.self$group_definition[[n]]) && is.logical(.self$group_definition[[n]]$domestic)) {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic)
                  }
                  
                } else {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,order = .self$group_definition[[n]]$order)
                  }
                }
              } else {
                if ('domestic' %in% names(.self$group_definition[[n]]) && is.logical(.self$group_definition[[n]]$domestic)) {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,domestic = .self$group_definition[[n]]$domestic,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,domestic = .self$group_definition[[n]]$domestic)
                  }
                  
                } else {
                  if ("observationType" %in% names(.self$group_definition[[n]])) {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class,observationType=.self$group_definition[[n]]$observationType)
                  } else {
                    .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,class = .self$group_definition[[n]]$class)
                  }
                }
              }
            } else {
              if ('domestic' %in% names(.self$group_definition[[n]]) && is.logical(.self$group_definition[[n]]$domestic)) {
                if ("observationType" %in% names(.self$group_definition[[n]])) {
                  .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic,observationType=.self$group_definition[[n]]$observationType)
                } else {
                  .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,order = .self$group_definition[[n]]$order,domestic = .self$group_definition[[n]]$domestic)
                }
              } else {
                if ("observationType" %in% names(.self$group_definition[[n]])) {
                  .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,order = .self$group_definition[[n]]$order,observationType=.self$group_definition[[n]]$observationType)
                } else {
                  .self$species_summary[[n]] <- .summarize_species(.self,.tax_obs,order = .self$group_definition[[n]]$order)
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
      .w <- .self$years
      names(.w) <- .self$years
      
      process_capture_data <- lapply(.w, function(y) {
        
        .x <- .self$get_data_subset(y)
        # Capture summary for the year
        capture_data <- .captures(.x)
        
        # Process and clean
        capture_data <- capture_data %>%
          mutate(
            class = ifelse(order == "Rodentia", "Mammalia", class),
            capture_rate = case_when(
              capture_rate >= 1    ~ round(capture_rate, 2),
              capture_rate >= 0.1  ~ round(capture_rate, 3),
              TRUE                 ~ round(capture_rate, 4)
            )
          )
        
        
        # Capture by location (for counting number of unique camera sites)
        capture_data_locations <- .captures(.x, by = "locationName")
        if ('vernacularNames.eng' %in% colnames(capture_data_locations)) {
          capture_data_locations <- capture_data_locations %>%
            group_by(vernacularNames.eng) %>%
            summarise(location_count = n_distinct(locationName), .groups = "drop") %>%
            arrange(vernacularNames.eng)
          #-------
          capture_data <- capture_data %>%
            left_join(capture_data_locations,by='vernacularNames.eng') %>%
            arrange(vernacularNames.eng)
          capture_data <- data.frame(capture_data[,c('vernacularNames.eng','captures','capture_rate','location_count')])
          colnames(capture_data) <- c('Species_Name','Captures','Capture_Rate','Locations')
          
        } else if ('vernacularNames' %in% colnames(capture_data_locations)) {
          capture_data_locations <- capture_data_locations %>%
            group_by(vernacularNames) %>%
            summarise(location_count = n_distinct(locationName), .groups = "drop") %>%
            arrange(vernacularNames)
          #---
          capture_data <- capture_data %>%
            left_join(capture_data_locations,by='vernacularNames') %>%
            arrange(vernacularNames)
          capture_data <- data.frame(capture_data[,c('vernacularNames','captures','capture_rate','location_count')])
          colnames(capture_data) <- c('Species_Name','Captures','Capture_Rate','Locations')
          
        }
        return(capture_data[!is.na(capture_data$Species_Name),])
      })
      
      #----------------
      
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
      capture_data_total <- .captures(.self$data) |>
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
      capture_locations <- .captures(.self$data, by = "locationName")
      
      # Merge location counts
      if ("vernacularNames.eng" %in% colnames(capture_data_total)) {
        
        capture_locations <- capture_locations |>
          group_by(vernacularNames.eng) |>
          summarise(Locations = n_distinct(locationName), .groups = "drop")
        #-----
        
        capture_data_total <- left_join(capture_data_total, capture_locations, by = "vernacularNames.eng")
        
        # Clean and rename
        capture_data_total <- capture_data_total |>
          dplyr::select(Year, vernacularNames.eng, captures, capture_rate, Locations) |>
          rename(
            Species_Name = vernacularNames.eng,
            Captures = captures,
            Capture_Rate = capture_rate
          )
        
        # Combine yearly and total data
        capture_df <- bind_rows(capture_df, capture_data_total)
        
        # Merge scientific names from taxonomy table
        taxonomy_info <- .self$data$taxonomy |>
          dplyr::select(vernacularNames.eng, scientificName) |>
          distinct()
        
        capture_df <- left_join(
          capture_df,
          taxonomy_info,
          by = c("Species_Name" = "vernacularNames.eng"),
          relationship = 'many-to-many'
        )
      } else if ("vernacularNames" %in% colnames(capture_data_total)) {
        
        capture_locations <- capture_locations |>
          group_by(vernacularNames) |>
          summarise(Locations = n_distinct(locationName), .groups = "drop")
        #---------
        
        capture_data_total <- left_join(capture_data_total, capture_locations, by = "vernacularNames")
        
        # Clean and rename
        capture_data_total <- capture_data_total |>
          dplyr::select(Year, vernacularNames, captures, capture_rate, Locations) |>
          rename(
            Species_Name = vernacularNames,
            Captures = captures,
            Capture_Rate = capture_rate
          )
        
        # Combine yearly and total data
        capture_df <- bind_rows(capture_df, capture_data_total)
        
        # Merge scientific names from taxonomy table
        taxonomy_info <- .self$data$taxonomy |>
          dplyr::select(vernacularNames, scientificName) |>
          distinct()
        
        capture_df <- left_join(
          capture_df,
          taxonomy_info,
          by = c("Species_Name" = "vernacularNames"),
          relationship = 'many-to-many'
        )
      }
      
      
      # Final column order
      .self$capture <- capture_df |>
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
        
        if (length(.self$.any_data_for_rem) == 0) {
          .self$.any_data_for_rem <- .any_data_for_rem(.self$data)
        }
        .sp <- names(.self$.any_data_for_rem[.sp][.self$.any_data_for_rem[.sp]])
        #----
        if (length(.sp) > 0) {
          for (n in .sp) {
            .w <- .self$get_REM(n)
          }
          rm(.w,.sp,n); gc()
        } else rm(.sp)
      }
      #-------
      
      .tmp <- lapply(.self$years, function(x) {
        .data_year <- .self$get_data_subset(x)
        # Extract deployment start and end dates
        start_dates <- .data_year$deployments$deploymentStart
        end_dates <- .data_year$deployments$deploymentEnd
        
        # Group deployments by location
        deployments_grouped <- .data_year$deployments |>
          group_by(locationID) |>
          summarise(start_date = min(deploymentStart,na.rm=TRUE), end_date = max(deploymentEnd,na.rm=TRUE), .groups = "drop")
        
        # Identify latest deployment end date
        latest_end <- max(deployments_grouped$end_date,na.rm=TRUE)
        
        
        
        # 5-days*24h*60m*60s -> 432000 sec.
        
        # Count failed cameras (stopped >5 days before the latest end date)
        failed_cameras <- deployments_grouped |>
          dplyr::filter(end_date < (latest_end - (432000))) |>
          nrow()
        
        # Compute runtime in days
        deployments_grouped <- deployments_grouped |>
          mutate(runtime_days = as.numeric(end_date - start_date, units = "days"))
        
        # Calculate key statistics
        average_runtime <- mean(deployments_grouped$runtime_days, na.rm = TRUE) |> round(0)
        runtime_range <- range(deployments_grouped$runtime_days, na.rm = TRUE) |> round(0)
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
      .tmp <- lapply(.self$years, function(x) {
        .data_year <- .self$get_data_subset(x)
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
      .tmp <- lapply(.self$years, function(x) {
        .data_year <- .self$get_data_subset(x)
        # Total species count
        total_species <- length(unique(.data_year$taxonomy$scientificName))
        
        # Filter Wild Mammals (excluding domestic species)
        wild_mammals <- .data_year$taxonomy |>
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
        #.s <- sapply(as.character(.self$data$deployments$deployment_interval),function(x) strsplit(x,'--')[[1]][1])
        #.e <- sapply(as.character(.self$data$deployments$deployment_interval),function(x) strsplit(x,'--')[[1]][2])
        .s <- as.character(.self$data$deployments$deploymentStart)
        .e <- as.character(.self$data$deployments$deploymentEnd)
        #names(.s) <- names(.e) <-NULL
        
        if (any(.s == "NA",na.rm=TRUE)) .s[.s == "NA"] <- NA
        if (any(.e == "NA",na.rm=TRUE)) .e[.e == "NA"] <- NA
        
        deploy_dates <-data.frame(start=as.Date(.s),end=as.Date(.e))
        
        
        # Build sun_input and compute sunlight times
        sun_input <- data.frame(
          date = seq(min(deploy_dates$start, na.rm = TRUE), max(deploy_dates$end, na.rm = TRUE), by = "day"),
          lat = mean(.self$data$locations$latitude, na.rm = TRUE),
          lon = mean(.self$data$locations$longitude, na.rm = TRUE)
        )
        #====
        .sun_times <- .eval('suncalc::getSunlightTimes(data = sun_input, keep = c("sunrise", "sunset"), tz = .self$setting$tz)',env=environment()) 
        
        # Convert to numeric hours
        .sun_times$sunrise_hour <- .get_hour(.sun_times$sunrise)
        .sun_times$sunset_hour <- .get_hour(.sun_times$sunset)
        
        # Compute yearly averages
        avg_sun_times <- .sun_times |>
          mutate(year = .getYear(date)) |>
          group_by(year) |>
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
      
      
      # Captures per species × site, with scientificName
      
      if (nrow(.self$habitat) > 0) {
        caps_by_site <- .captures(.self$data, by = "locationName") |>
          left_join(
            .self$data$taxonomy |> 
              dplyr::select(taxonID, scientificName),
            by = "taxonID"
          ) 
        
        
        
        obs_with_habitat <- caps_by_site |>
          left_join(.self$data$locations, by = "locationName")
        
        summary_by_species_habitat <- obs_with_habitat |>
          group_by(
            observationType,
            class,
            order,
            Habitat_Type,
            taxonID,
            scientificName
          ) |>
          summarise(
            captures   = sum(captures, na.rm = TRUE),
            effort     = sum(effort,   na.rm = TRUE),
            .groups    = "drop"
          ) |>
          mutate(
            capture_rate = captures / as.numeric(effort)
          ) |>
          dplyr::select(
            observationType,
            class,
            order,
            Habitat_Type,
            taxonID,
            scientificName,
            captures,
            effort,
            capture_rate
          ) |>
          arrange(scientificName, Habitat_Type)
        
        .self$species_summary_by_habitat <- as.data.frame(summary_by_species_habitat)
        
        rm(caps_by_site,obs_with_habitat,summary_by_species_habitat)
        gc()
      }
      
      
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
    get_data_subset=function(year=2023) {
      
      .w <- which(.self$data$deployments$Year == year)
      if (length(.w) > 0) {
        .d <- list()
        .d$deployments <- .self$data$deployments[.w,]
        .d$media <- .self$data$media[.self$data$media$deploymentID %in% .d$deployments$deploymentID, ]
        .d$observations <- .self$data$observations[.self$data$observations$deploymentID %in% .d$deployments$deploymentID,]
        .d$locations <- .self$data$locations[.self$data$locations$locationID %in% .d$deployments$locationID,]
        .d$taxonomy <- .self$data$taxonomy[.self$data$taxonomy$scientificName %in% .d$observations$scientificName,]
        .d$sequences <- .self$data$sequences[.self$data$sequences$deploymentID %in% .d$deployments$deploymentID,]
        .d
      }
      
    },
    extractYears = function(update=FALSE) {
      if (update || length(.self$years) == 0) {
        .self$years <- sort(as.numeric(unique(.getYear(.self$data$deployments$deployment_interval))))
      }
      #-----
      .self$years
    },
    generateReport = function(output_file = "cam_report.html") {
      .self$recetFigTabNumber()
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
library(htmltools)

.paste_comma_and <- camtrapReport:::.paste_comma_and
.require <- camtrapReport:::.require
.get_projected_vect <- camtrapReport:::.get_projected_vect
.plot_effort <- camtrapReport:::.plot_effort
.getYear <- camtrapReport:::.getYear
.eval <- camtrapReport:::.eval

```
      ",.envir = .self)
      
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