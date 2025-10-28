
.data <- read_ctdp('_Dataset/Dataset/Veluwe-2025.zip')


#------------
cm <- camR$new()
cm$data <- .data
cm$habitat <- read.csv('_Dataset/input/habitat.csv')
cm$study_area <- st_read('_Dataset/input/shapefile/studyarea_nl.shp')
#----
cm$authors <- readLines("_Dataset/input/Text/author.txt",warn = F)
cm$title <- readLines("_Dataset/input/Text/title.txt",warn = F)[1]
cm$subtitle <- "Report on Camera Trapping for the European Observatory of Wildlife"
cm$institute <- "Wageningen University, Wildlife Ecology & Conservation Group"
cm$logoPath <- file.path("_Dataset/input/Logo/WUR.jpg")
#----
# site name & site descriptions & sampling method has corresponding fields in the report object:
cm$siteName <- trimws(readLines("_Dataset/input/Text/study_area.txt", warn = FALSE)[1])
cm$description <- paste(readLines('_Dataset/input/Text/site_description.txt'), collapse = "\n")
cm$sampling <- paste0(paste(readLines('_Dataset/input/Text/sampling_section_1.txt'), collapse = "\n"),'\n\n',
                 paste(readLines('_Dataset/input/Text/sampling_section_2.txt'), collapse = "\n"))

###############

cm$filterExclude <- list(
  scientificName=c("Homo sapiens", "Canis lupus familiaris", "Felis catus",
  "Ovis aries", "Bos taurus", "Equus caballus", "Capra hircus",
  "Sus scrofa domesticus", "Equus africanus asinus", "Oryctolagus cuniculus",
  "Camelus dromedarius", "Camelus bactrianus", "Rangifer tarandus domesticus"),
  vernacularNames.eng=c(
    "dog", "domestic goat", "domestic cat", "domestic sheep", "human", 
    "cat", "sheep", "cow", "horse", "goat", "pig", "donkey", "rabbit", 
    "dromedary camel", "bactrian camel", "domestic reindeer"
  ))
#---
cm$filterKeep <- list(order=c("Artiodactyla", "Carnivora"),observationType=c("animal"))
#---------
cm$filterCount <- 25
cm$filter()

#-------

cm$setup()


# Paragraphs in a text section can be provided within a list (e.g., p1 = "paragraph 1 text....", p2 = "paragraph2 ....")

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

.tmpText <- readLines(introduction_file, warn = FALSE, encoding = "UTF-8")
.tmpText <- as.list(.tmpText[.tmpText != ""])
.txx <- .getTextObj(name='introduction',title='Introduction',txt=.tmpText)
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

.txx <- .getTextObj(name='location',title="Camera Locations {.tabset}",parent='methods',
                    txt='The maps below display the locations of camera traps deployed during different years. Use the tabs above to explore data for each sampling year. The last tab shows the study area with all camera locations in this site. Locations are color-coded by habitat type. Click on the points for additional deployment information.')

cm$addReportObject(.txx)

#---------------
.rxx <- .getRchunk(parent='location',name = 'camera_locations_leaflet',setting={c(echo=FALSE,results="asis",fig.show="hold")},code = {
  
  
  # Define a single color for all points
  default_color <- "black"
  
  # Function to plot camera locations with popups
  plot_locations <- function(data, color = default_color) {  
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)  # Return NULL if data is missing or empty
    }
    
    # Create a popup text with Location Name & Habitat Type
    data$popup_text <- paste0(
      "<b>Location Name:</b> ", data$locationName, "<br>",
      "<b>Habitat Type:</b> ", data$Habitat_Type, "<br>",
      "<b>Camera Height:</b> ", data$cameraHeight
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 5, 
        color = color, fillColor = color, fillOpacity = 0.8,
        stroke = TRUE, weight = 1,  
        label = ~locationName,  
        popup = ~popup_text     
      )
  }
  #----
  
  
  #----
  # 📌 **Automatically Create Tabs for Each Year**
  for (doyear in object$years) {
    cat(paste0("### ", doyear, " {.unnumbered}\n\n"))
    
    # ✅ Extract the correct dataset for the year
    
    .dat_year <- object$data_merged[object$data_merged$Year ==  doyear ,c('locationName','cameraHeight','longitude','latitude','Year','Habitat_Type')]
    .dat_year <- .dat_year[!is.na(.dat_year$longitude) & !is.na(.dat_year$longitude),]
    
    # ✅ Ensure the data is valid before proceeding
    if (nrow(.dat_year) > 0) {
      p <- plot_locations(.dat_year, color = default_color)
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
  
  cat("\n### Research area {.unnumbered} \n")
  
  # 📌 **Define File Paths**
  Data_ResearchArea <- object$data_merged
  
  .dat_year <- object$data_merged[,c('locationName','cameraHeight','longitude','latitude',
                                     'Year','Year_List','Habitat_Type','BaitUse_List','Species_List',
                                     'CaptureMethod_List','Total_Photos','Classify_By_List','Setup_By_List')]
  
  # step 3: Define Habitat-Based Color Palette
  .dat_year$Habitat_Type <- factor(.dat_year$Habitat_Type)
  unique_habitats <- unique(.dat_year$Habitat_Type)
  
  habitat_colors <- colorFactor(
    palette = c("#855C75FF", "#D9AF6BFF", "#AF6458FF", "#736F4CFF", "#526A83FF", "#625377FF"),
    domain = unique_habitats, na.color = "#bdbdbd"
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
  .tmpStudyArea <- object$study_area %>%
    st_transform(crs = 4326)
  
  # ✅ **Step 5: Generate and Display Interactive Leaflet Map**
  leaflet(.dat_year, width = "100%", height = "400px") %>%
    
    # 🔹 **Add Base Map**
    addTiles() %>%
    
    # 🔹 **Overlay Study Area Polygon (Ensuring it's Below the Points)**
    addPolygons(
      data = .tmpStudyArea,
      fillColor = "transparent",
      fillOpacity = 0.3,
      color = "black",
      weight = 2
    ) %>%
    
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
    ) %>%
    
    # ✅ **Ensure Proper Rendering on Tab Switch**
    htmlwidgets::onRender("
    function(el, x) {
      el.style.width = '100%';
      el.style.height = '400px';  // ✅ Standardized Height for all Maps
      el._leaflet_map.invalidateSize();
    }
  ")
  cat('\n\n')
  
  cat('##  {.unnumbered}\n')
  
  cat("**Fig. 1**: *Interactive map of camera trap locations. Click on a marker to view the corresponding location details and additional metadata.*\n")
    
})

cm$addReportObject(.rxx) # adding R chunk!

#--------

.txx <- .getTextObj(name='effort',title="Sampling Efforts",parent='methods')
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
  plot_test <- plot_effort(cm$data) %>%
    dyAxis("x", label = "Time") %>%
    dyAxis("y", label = "Active Camera Count") %>%
    dyOptions(fillGraph = TRUE, colors = "#033800FF") %>%
    dyRangeSelector()
  
  # Check if the plot is valid
  if (!is.null(plot_test)) {
    
    # Save the plot as an HTML widget
    temp_file <- file.path(tempdir(), "effort_plot.html")
    saveWidget(plot_test, file = temp_file, selfcontained = TRUE)
    
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
.txx <- .getTextObj(name='captures',title="Captures",parent='results',txt='A yearly summary of the observation data—including the total number of photos, observations, image sequences, and animal detections—is presented in **Table 2**.')
cm$addReportObject(.txx) 




###########
## Temporary solution:

.rxx <- .getRchunk(parent='captures',name = 'focus_text',setting={c(echo=FALSE,results="asis")},code={
  
  focus_groups <- c("large_mammals")  
  obs_threshold <- 20
  
  species_summary <- readRDS("_Dataset/output/species_summary.rds")
  # Summary Metrics
  total_species <- nrow(species_summary$wild_species$site_list)
  total_domestic <- nrow(species_summary$domestic$site_list)
  total_human <- sum(species_summary$human_observation$per_year$total_observations)
  years_vector <- sort(species_summary$wild_species$per_year$observation_Year)
  
  # Format years as range if consecutive
  if (length(object$years) > 1 && all(diff(object$years) == 1)) {
    year_text <- paste0(min(object$years), " to ", max(object$years))
  } else {
    year_text <- paste(object$years, collapse = ", ")
  }
  
  # Main summary sentence
  cat(paste0(
    "Across the period from ", year_text, ", the observations recorded a total of ",
    total_species, " different wild species and ", total_domestic, " domestic species."
  ))
  
  # Dynamic breakdown of species groups
  breakdown <- c()
  if (nrow(species_summary$birds$site_list) > 0) {
    breakdown <- c(breakdown, paste0(nrow(species_summary$birds$site_list), " bird species"))
  }
  if (nrow(species_summary$wild_mammals$site_list) > 0) {
    breakdown <- c(breakdown, paste0(nrow(species_summary$wild_mammals$site_list), " mammal species"))
  }
  if (nrow(species_summary$reptiles$site_list) > 0) {
    breakdown <- c(breakdown, paste0(nrow(species_summary$reptiles$site_list), " reptile species"))
  }
  if (nrow(species_summary$amphibians$site_list) > 0) {
    breakdown <- c(breakdown, paste0(nrow(species_summary$amphibians$site_list), " amphibian species"))
  }
  if (length(breakdown) > 0) {
    cat(paste0("Among the wild species were ", paste(breakdown, collapse = ", "), ".\n"))
  }
  
  
  # Human observation
  cat(paste0(
    "Over the same period, camera traps detected human presence ", total_human,
    " times, indicating potential human activity within the monitored area.\n\n"
  ))
  
  # Focus group summary
  label_map <- c(
    wild_mammals = "wild mammals",
    large_mammals = "large mammals",
    birds = "birds",
    reptiles = "reptiles",
    amphibians = "amphibians"
  )
  group_label <- paste(label_map[focus_groups], collapse = " and ")
  
  cat(paste0(
    "This report focuses on ", group_label,
    "—specifically those observed at least ", obs_threshold, " times during the study period.\n\n"
  ))
  
  cat("*A detailed list of these species—including their number of observations, locations, and total photos—is presented in Table 2. A comprehensive list of all observed species and their respective capture details is included in Appendix, Table 1.*\n")
  #---
  
  # Map species groups
  group_map <- list(
    wild_mammals = species_summary$wild_mammals$site_list,
    birds        = species_summary$birds$site_list,
    reptiles     = species_summary$reptiles$site_list,
    amphibians   = species_summary$amphibians$site_list,
    large_mammals = species_summary$large_mammals$site_list
  )
  
  # Select species of interest
  selected_species <- bind_rows(group_map[focus_groups]) %>%
    select(any_of(c("scientificName", "vernacularNames.eng", "family"))) %>%
    distinct()
  
  # Join with sequence & location data
  enriched_obs <- tax_obs %>%
    filter(scientificName %in% selected_species$scientificName) %>%
    left_join(dataNew$sequences %>% select(sequenceID, deploymentID, nrphotos), by = "sequenceID") %>%
    left_join(dataNew$deployments %>% select(deploymentID, locationID), by = "deploymentID") %>%
    left_join(dataNew$locations %>% select(locationID, locationName), by = "locationID")
  
  # Summarize
  group_summary <- enriched_obs %>%
    group_by(scientificName, vernacularNames.eng, family) %>%
    summarise(
      TotalCaptures = n(),
      UniqueLocations = n_distinct(locationName),
      TotalPhotos = sum(nrphotos, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(TotalCaptures >= obs_threshold) %>%
    arrange(desc(TotalCaptures))
  
  # Emoji for group
  emoji <- if ("large_mammals" %in% focus_groups) "🦌" else if ("birds" %in% focus_groups) "🕊️" else "🐾"
  group_labels <- paste(label_map[focus_groups], collapse = " and ")
  
  # Output Table
  if (nrow(group_summary) == 0) {
    cat("\n! No species met the threshold of ", obs_threshold, " observations.\n")
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
        subtitle = md(paste0("Species with ≥ ", obs_threshold, " observations"))
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




cm$generateReport()


