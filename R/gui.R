#' Launch the camtrapReport graphical user interface
#'
#' @param object Optional camReport object created by [camData()]. If supplied,
#'   the GUI opens with this object already loaded.
#' @param launch.browser Logical. Should the app open in the browser?
#' @param max_upload_mb Maximum upload size in MB for files uploaded through
#'   the GUI. The default is 2000 MB.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly launches the Shiny application.
#'
#' @examples
#' \dontrun{
#' cm <- camData("Leuven-data.zip")
#' gui(cm)
#' }
#'
#' @export
gui <- function(object = NULL,
                launch.browser = TRUE,
                max_upload_mb = 2000,
                ...) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "The 'shiny' package is required to use gui().\n",
      "Please install it with install.packages('shiny').",
      call. = FALSE
    )
  }
  
  old_limit <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  
  on.exit({
    options(shiny.maxRequestSize = old_limit)
  }, add = TRUE)
  
  app <- .camtrapReport_gui_app(object = object)
  
  shiny::runApp(
    app,
    launch.browser = launch.browser,
    ...
  )
}


.camtrapReport_gui_app <- function(object = NULL) {
  
  `%||%` <- function(x, y) {
    if (is.null(x) ||
        length(x) == 0 ||
        (length(x) == 1 && is.na(x))) {
      y
    } else {
      x
    }
  }
  
  .safe <- function(expr, fallback = NULL) {
    tryCatch(expr, error = function(e) fallback)
  }
  
  .as_text <- function(x) {
    if (is.null(x) || length(x) == 0) return("")
    x <- as.character(x)
    x <- x[!is.na(x)]
    paste(x, collapse = ", ")
  }
  
  .copy_upload <- function(upload) {
    if (is.null(upload)) return(NULL)
    
    out <- file.path(tempdir(), upload$name[1])
    
    file.copy(
      from = upload$datapath[1],
      to = out,
      overwrite = TRUE
    )
    
    normalizePath(out, winslash = "/", mustWork = FALSE)
  }
  
  .copy_study_area_upload <- function(upload) {
    if (is.null(upload)) return(NULL)
    
    d <- tempfile("study_area_")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    
    for (i in seq_len(nrow(upload))) {
      file.copy(
        from = upload$datapath[i],
        to = file.path(d, upload$name[i]),
        overwrite = TRUE
      )
    }
    
    shp <- list.files(
      d,
      pattern = "\\.shp$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(shp) == 0) return(NULL)
    
    shp <- normalizePath(shp[1], winslash = "/", mustWork = FALSE)
    
    if (requireNamespace("terra", quietly = TRUE)) {
      return(.safe(terra::vect(shp), shp))
    }
    
    shp
  }
  
  .read_habitat <- function(upload = NULL, path = NULL) {
    path <- trimws(path %||% "")
    
    if (nzchar(path)) {
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      return(utils::read.csv(path, stringsAsFactors = FALSE))
    }
    
    if (!is.null(upload)) {
      return(utils::read.csv(upload$datapath, stringsAsFactors = FALSE))
    }
    
    NULL
  }
  
  .get_zip_path <- function(upload = NULL, path = NULL) {
    path <- trimws(path %||% "")
    
    if (nzchar(path)) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
    
    if (!is.null(upload)) {
      return(.copy_upload(upload))
    }
    
    NULL
  }
  
  .get_info_value <- function(cm, name, fallback = "") {
    value <- .safe(info(cm, name = name), NULL)
    
    if (is.null(value) || length(value) == 0) {
      value <- .safe(cm[[name]], fallback)
    }
    
    .as_text(value)
  }
  
  .get_info <- function(cm) {
    list(
      title = .get_info_value(cm, "title"),
      subtitle = .get_info_value(cm, "subtitle"),
      authors = .get_info_value(cm, "authors"),
      institute = .get_info_value(cm, "institute"),
      siteName = .get_info_value(cm, "siteName"),
      logoPath = .get_info_value(cm, "logoPath"),
      description = .as_text(.safe(cm$description, "")),
      acknowledgement = .as_text(.safe(cm$acknowledgement, ""))
    )
  }
  
  .set_info <- function(cm, name, value) {
    ok <- .safe({
      info(cm, name = name) <- value
      TRUE
    }, FALSE)
    
    if (!isTRUE(ok)) {
      .safe({
        cm[[name]] <- value
        TRUE
      }, FALSE)
    }
    
    invisible(cm)
  }
  
  .get_all_sections <- function(cm = NULL) {
    out <- character()
    
    if (!is.null(cm)) {
      out <- .safe(section_names(cm), character())
    }
    
    if (length(out) == 0) {
      out <- .safe(section_names(), character())
    }
    
    if (length(out) == 0 && !is.null(cm)) {
      out <- .safe(sections(cm), character())
    }
    
    unique(as.character(out))
  }
  
  .get_included_sections <- function(cm) {
    out <- .safe(sections(cm), character())
    
    if (length(out) == 0) {
      out <- .get_all_sections(cm)
    }
    
    unique(as.character(out))
  }
  
  .unique_values <- function(cm, field) {
    dfs <- list(
      .safe(cm$data$observations, NULL),
      .safe(cm$data$taxonomy, NULL),
      .safe(cm$data_merged, NULL)
    )
    
    vals <- unlist(lapply(dfs, function(df) {
      if (!is.data.frame(df)) return(character())
      if (!field %in% names(df)) return(character())
      
      x <- as.character(df[[field]])
      x <- x[!is.na(x)]
      x <- trimws(x)
      x <- x[nzchar(x)]
      x
    }))
    
    sort(unique(vals))
  }
  
  .species_table <- function(cm) {
    obs <- .safe(cm$data$observations, NULL)
    
    if (!is.data.frame(obs) || !"scientificName" %in% names(obs)) {
      return(data.frame(
        scientificName = character(),
        records = integer()
      ))
    }
    
    sp <- as.character(obs$scientificName)
    sp <- sp[!is.na(sp) & nzchar(trimws(sp))]
    
    tab <- sort(table(sp), decreasing = TRUE)
    
    data.frame(
      scientificName = names(tab),
      records = as.integer(tab),
      row.names = NULL
    )
  }
  
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$title("camtrapReport GUI"),
      shiny::tags$style(shiny::HTML("
        html, body {
          margin: 0;
          padding: 0;
          background: #eef1f4;
          color: #223247;
          font-family: Arial, Helvetica, sans-serif;
        }

        .container-fluid {
          padding-left: 0;
          padding-right: 0;
        }

        .app-shell {
          display: grid;
          grid-template-columns: 360px 1fr;
          min-height: 100vh;
        }

        .sidebar {
          background: #162733;
          color: white;
          padding: 24px;
        }

        .sidebar h2 {
          margin-top: 0;
          font-weight: 700;
          font-size: 32px;
        }

        .sidebar p {
          color: rgba(255,255,255,0.85);
        }

        .sidebar .form-control,
        .sidebar .btn {
          border-radius: 10px;
        }

        .sidebar label {
          color: #f3f7fa;
        }

        .sidebar hr {
          border-top: 1px solid rgba(255,255,255,0.28);
          margin-top: 24px;
          margin-bottom: 24px;
        }

        .sidebar .help-block {
          color: rgba(255,255,255,0.70);
        }

        .main {
          padding: 28px;
          overflow-x: hidden;
        }

        .main h1 {
          margin-top: 0;
          margin-bottom: 18px;
          font-size: 44px;
          font-weight: 400;
        }

        .card {
          background: white;
          border-radius: 18px;
          padding: 24px;
          margin-bottom: 22px;
          box-shadow: 0 8px 22px rgba(31,42,68,0.08);
          border: 1px solid #e5ebf0;
        }

        .card h3 {
          margin-top: 0;
          margin-bottom: 18px;
        }

        .status-good {
          background: #eaf8ef;
          border: 1px solid #bde8cd;
          color: #116b35;
          border-radius: 12px;
          padding: 14px;
          line-height: 1.55;
        }

        .status-missing {
          background: #fff3e5;
          border: 1px solid #ffd8a8;
          color: #8a4b00;
          border-radius: 12px;
          padding: 14px;
          line-height: 1.55;
        }

        .btn-primary {
          background: #0b66c3;
          border-color: #0b66c3;
        }

        .btn-success {
          background: #08a34a;
          border-color: #08a34a;
        }

        textarea {
          font-family: Arial, Helvetica, sans-serif;
        }

        .small-note {
          font-size: 13px;
          color: #66727d;
          margin-top: 6px;
        }

        @media (max-width: 950px) {
          .app-shell {
            display: block;
          }

          .sidebar {
            width: 100%;
          }

          .main h1 {
            font-size: 34px;
          }
        }
      "))
    ),
    
    shiny::div(
      class = "app-shell",
      
      shiny::div(
        class = "sidebar",
        
        shiny::h2("camtrapReport"),
        shiny::p("Interactive report builder"),
        
        shiny::hr(),
        
        shiny::textInput(
          "data_zip_path",
          "Camtrap-DP zip file path",
          value = "",
          placeholder = "C:/Users/.../Leuven-data.zip"
        ),
        
        shiny::fileInput(
          "data_zip",
          "Or upload Camtrap-DP zip file",
          accept = ".zip"
        ),
        
        shiny::textInput(
          "habitat_csv_path",
          "Optional habitat CSV path",
          value = "",
          placeholder = "C:/Users/.../habitat.csv"
        ),
        
        shiny::fileInput(
          "habitat_csv",
          "Or upload habitat CSV",
          accept = ".csv"
        ),
        
        shiny::fileInput(
          "study_area_files",
          "Optional study-area shapefile files",
          multiple = TRUE,
          accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg")
        ),
        
        shiny::actionButton(
          "load_cm",
          "Load data",
          class = "btn btn-success btn-block"
        ),
        
        shiny::hr(),
        
        shiny::uiOutput("object_status"),
        
        shiny::hr(),
        
        shiny::actionButton(
          "apply_all",
          "Apply settings to object",
          class = "btn btn-primary btn-block"
        ),
        
        shiny::br(),
        shiny::br(),
        
        shiny::actionButton(
          "generate_status",
          "Generate data status report",
          class = "btn btn-default btn-block"
        ),
        
        shiny::br(),
        shiny::br(),
        
        shiny::actionButton(
          "generate_report",
          "Generate ecological report",
          class = "btn btn-success btn-block"
        )
      ),
      
      shiny::div(
        class = "main",
        
        shiny::h1("camtrapReport GUI"),
        
        shiny::tabsetPanel(
          id = "tabs",
          
          shiny::tabPanel(
            "Metadata",
            
            shiny::div(
              class = "card",
              shiny::h3("Report information"),
              
              shiny::textInput("meta_title", "Title"),
              shiny::textInput("meta_subtitle", "Subtitle"),
              shiny::textInput("meta_authors", "Authors"),
              shiny::textInput("meta_institute", "Institute"),
              shiny::textInput("meta_siteName", "Site name"),
              shiny::textInput("meta_logoPath", "Logo path"),
              
              shiny::textAreaInput(
                "meta_description",
                "Study area / description text",
                rows = 6
              ),
              
              shiny::textAreaInput(
                "meta_acknowledgement",
                "Acknowledgement",
                rows = 4
              )
            ),
            
            shiny::div(
              class = "card",
              shiny::h3("Time and filtering"),
              
              shiny::uiOutput("years_ui"),
              
              shiny::numericInput(
                "filter_count",
                "Minimum records per species",
                value = 25,
                min = 1,
                step = 1
              )
            )
          ),
          
          shiny::tabPanel(
            "Focus group",
            
            shiny::div(
              class = "card",
              shiny::h3("Existing focus group"),
              
              shiny::uiOutput("existing_group_ui"),
              
              shiny::actionButton(
                "set_existing_group",
                "Set selected focus group",
                class = "btn btn-primary"
              )
            ),
            
            shiny::div(
              class = "card",
              shiny::h3("Create or update focus group"),
              
              shiny::p(
                "Define a group using one field. Typical fields are scientificName, class, order, or observationType."
              ),
              
              shiny::textInput(
                "new_group_name",
                "New focus group name",
                value = "my_focus_group"
              ),
              
              shiny::selectInput(
                "group_field",
                "Field used to define the group",
                choices = c("scientificName", "class", "order", "observationType"),
                selected = "scientificName"
              ),
              
              shiny::uiOutput("group_values_ui"),
              
              shiny::actionButton(
                "add_focus_group",
                "Add group and use it",
                class = "btn btn-success"
              )
            )
          ),
          
          shiny::tabPanel(
            "Species",
            
            shiny::div(
              class = "card",
              shiny::h3("Unique species in the dataset"),
              shiny::p("This table is read directly from the loaded camReport object."),
              shiny::tableOutput("species_table")
            )
          ),
          
          shiny::tabPanel(
            "Sections",
            
            shiny::div(
              class = "card",
              shiny::h3("Include / exclude report sections"),
              
              shiny::uiOutput("sections_ui"),
              
              shiny::actionButton(
                "apply_sections",
                "Apply section selection",
                class = "btn btn-primary"
              )
            ),
            
            shiny::div(
              class = "card",
              shiny::h3("Edit text of a report section"),
              
              shiny::uiOutput("section_edit_select_ui"),
              
              shiny::textAreaInput(
                "section_new_text",
                "New text",
                rows = 8,
                placeholder = "Write replacement or additional text here..."
              ),
              
              shiny::checkboxInput(
                "section_append",
                "Append text instead of replacing existing text",
                value = FALSE
              ),
              
              shiny::actionButton(
                "apply_section_text",
                "Update section text",
                class = "btn btn-success"
              )
            ),
            
            shiny::div(
              class = "card",
              shiny::h3("Current section list"),
              shiny::verbatimTextOutput("section_list")
            )
          ),
          
          shiny::tabPanel(
            "Generate",
            
            shiny::div(
              class = "card",
              shiny::h3("Output"),
              
              shiny::textInput(
                "output_dir",
                "Output directory",
                value = getwd()
              ),
              
              shiny::p(
                "The report functions save outputs in the working directory. This GUI temporarily switches to the directory above when generating reports."
              ),
              
              shiny::actionButton(
                "save_cm",
                "Save current camReport object as RDS",
                class = "btn btn-default"
              )
            ),
            
            shiny::div(
              class = "card",
              shiny::h3("Current object summary"),
              shiny::verbatimTextOutput("cm_summary")
            )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    rv <- shiny::reactiveValues(
      cm = object
    )
    
    cm_current <- shiny::reactive({
      shiny::validate(
        shiny::need(!is.null(rv$cm), "Please load a Camtrap-DP zip file first.")
      )
      
      rv$cm
    })
    
    refresh_inputs <- function(cm) {
      inf <- .get_info(cm)
      
      shiny::updateTextInput(session, "meta_title", value = inf$title %||% "")
      shiny::updateTextInput(session, "meta_subtitle", value = inf$subtitle %||% "")
      shiny::updateTextInput(session, "meta_authors", value = inf$authors %||% "")
      shiny::updateTextInput(session, "meta_institute", value = inf$institute %||% "")
      shiny::updateTextInput(session, "meta_siteName", value = inf$siteName %||% "")
      shiny::updateTextInput(session, "meta_logoPath", value = inf$logoPath %||% "")
      
      shiny::updateTextAreaInput(
        session,
        "meta_description",
        value = inf$description %||% ""
      )
      
      shiny::updateTextAreaInput(
        session,
        "meta_acknowledgement",
        value = inf$acknowledgement %||% ""
      )
      
      shiny::updateNumericInput(
        session,
        "filter_count",
        value = .safe(cm$filterCount, 25)
      )
    }
    
    session$onFlushed(function() {
      if (!is.null(rv$cm)) {
        refresh_inputs(rv$cm)
      }
    }, once = TRUE)
    
    apply_gui_settings <- function(run_setup = TRUE) {
      cm <- cm_current()
      
      .set_info(cm, "title", input$meta_title %||% "")
      .set_info(cm, "subtitle", input$meta_subtitle %||% "")
      .set_info(cm, "authors", input$meta_authors %||% "")
      .set_info(cm, "institute", input$meta_institute %||% "")
      .set_info(cm, "siteName", input$meta_siteName %||% "")
      .set_info(cm, "logoPath", input$meta_logoPath %||% "")
      
      cm$description <- input$meta_description %||% ""
      cm$acknowledgement <- input$meta_acknowledgement %||% ""
      
      yrs <- suppressWarnings(as.numeric(input$years_selected %||% numeric()))
      yrs <- yrs[!is.na(yrs)]
      
      if (length(yrs) > 0) {
        cm$years <- yrs
      }
      
      cm$filterCount <- as.numeric(input$filter_count %||% 25)
      
      fg <- input$existing_group %||% NULL
      
      if (!is.null(fg) && length(fg) > 0 && nzchar(fg[1])) {
        cm$set_focus_group(x = fg[1])
      }
      
      if (!is.null(input$sections_keep)) {
        sections(cm, n = input$sections_keep)
      }
      
      if (isTRUE(run_setup)) {
        cm$setup()
      }
      
      rv$cm <- cm
      
      invisible(cm)
    }
    
    shiny::observeEvent(input$load_cm, {
      zip_path <- NULL
      
      tryCatch(
        {
          zip_path <- .get_zip_path(
            upload = input$data_zip,
            path = input$data_zip_path
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not find zip file:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
      
      shiny::validate(
        shiny::need(!is.null(zip_path), "Please provide a Camtrap-DP zip file or local path.")
      )
      
      shiny::showModal(shiny::modalDialog(
        title = "Loading data",
        "Please wait while camtrapReport builds the camReport object...",
        footer = NULL
      ))
      
      on.exit(shiny::removeModal(), add = TRUE)
      
      habitat <- tryCatch(
        {
          .read_habitat(
            upload = input$habitat_csv,
            path = input$habitat_csv_path
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not read habitat CSV:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
      
      study_area <- .copy_study_area_upload(input$study_area_files)
      
      cm <- tryCatch(
        {
          if (!is.null(study_area)) {
            camData(
              data = zip_path,
              habitat = habitat,
              study_area = study_area
            )
          } else {
            camData(
              data = zip_path,
              habitat = habitat
            )
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not load data:", conditionMessage(e)),
            type = "error",
            duration = 12
          )
          NULL
        }
      )
      
      if (!is.null(cm)) {
        rv$cm <- cm
        refresh_inputs(cm)
        
        shiny::showNotification(
          "camReport object loaded successfully.",
          type = "message"
        )
      }
    })
    
    shiny::observeEvent(input$apply_all, {
      apply_gui_settings(run_setup = TRUE)
      
      shiny::showNotification(
        "Settings applied to camReport object.",
        type = "message"
      )
    })
    
    output$object_status <- shiny::renderUI({
      if (is.null(rv$cm)) {
        shiny::div(
          class = "status-missing",
          shiny::strong("No object loaded"),
          shiny::br(),
          "Load a Camtrap-DP zip file or call gui(cm)."
        )
      } else {
        cm <- rv$cm
        
        shiny::div(
          class = "status-good",
          shiny::strong("Object loaded"),
          shiny::br(),
          "Site: ", .safe(cm$siteName, "unknown"),
          shiny::br(),
          "Focus group: ",
          paste(.safe(cm$setting$focus_groups, "unknown"), collapse = ", "),
          shiny::br(),
          "Years: ",
          paste(.safe(cm$years, "not set"), collapse = ", "),
          shiny::br(),
          "Filter count: ",
          .safe(cm$filterCount, "not set")
        )
      }
    })
    
    output$years_ui <- shiny::renderUI({
      cm <- cm_current()
      
      yrs <- .safe(cm$extractYears(), NULL)
      
      if (is.null(yrs) || length(yrs) == 0) {
        yrs <- .safe(cm$years, numeric())
      }
      
      yrs <- sort(unique(as.numeric(yrs)))
      yrs <- yrs[!is.na(yrs)]
      
      if (length(yrs) == 0) {
        return(shiny::p("No years detected yet."))
      }
      
      shiny::checkboxGroupInput(
        "years_selected",
        "Years included in report",
        choices = yrs,
        selected = .safe(cm$years, yrs)
      )
    })
    
    output$existing_group_ui <- shiny::renderUI({
      cm <- cm_current()
      
      groups <- names(.safe(cm$group_definition, list()))
      current <- .safe(cm$setting$focus_groups, NULL)
      
      groups <- unique(c(current, groups))
      groups <- groups[!is.na(groups) & nzchar(groups)]
      
      if (length(groups) == 0) {
        return(shiny::p("No focus groups found."))
      }
      
      shiny::selectInput(
        "existing_group",
        "Available focus groups",
        choices = groups,
        selected = current %||% groups[1]
      )
    })
    
    shiny::observeEvent(input$set_existing_group, {
      cm <- cm_current()
      
      shiny::req(input$existing_group)
      
      tryCatch(
        {
          cm$set_focus_group(x = input$existing_group)
          cm$setup()
          rv$cm <- cm
          
          shiny::showNotification(
            paste("Focus group updated to:", input$existing_group),
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not set focus group:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })
    
    output$group_values_ui <- shiny::renderUI({
      cm <- cm_current()
      
      vals <- .unique_values(
        cm,
        input$group_field %||% "scientificName"
      )
      
      if (length(vals) == 0) {
        return(shiny::p("No values found for this field."))
      }
      
      shiny::selectizeInput(
        "group_values",
        "Values included in this group",
        choices = vals,
        selected = character(),
        multiple = TRUE,
        options = list(
          placeholder = "Select one or more values"
        )
      )
    })
    
    shiny::observeEvent(input$add_focus_group, {
      cm <- cm_current()
      
      nm <- trimws(input$new_group_name %||% "")
      field <- input$group_field %||% "scientificName"
      vals <- input$group_values %||% character()
      
      shiny::validate(
        shiny::need(nzchar(nm), "Please provide a focus group name."),
        shiny::need(length(vals) > 0, "Please select at least one value.")
      )
      
      rule <- list()
      rule[[field]] <- vals
      
      tryCatch(
        {
          cm$add_group(name = nm, x = rule)
          cm$set_focus_group(x = nm)
          cm$setup()
          
          rv$cm <- cm
          
          shiny::showNotification(
            paste0("Focus group '", nm, "' added and selected."),
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not add focus group:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })
    
    output$species_table <- shiny::renderTable({
      utils::head(.species_table(cm_current()), 100)
    })
    
    output$sections_ui <- shiny::renderUI({
      cm <- cm_current()
      
      all_sec <- .get_all_sections(cm)
      included <- .get_included_sections(cm)
      
      if (length(all_sec) == 0) {
        return(shiny::p("No report sections found."))
      }
      
      shiny::checkboxGroupInput(
        "sections_keep",
        "Sections included in the ecological report",
        choices = all_sec,
        selected = included
      )
    })
    
    output$section_edit_select_ui <- shiny::renderUI({
      cm <- cm_current()
      
      all_sec <- .get_all_sections(cm)
      
      if (length(all_sec) == 0) {
        return(shiny::p("No editable sections found."))
      }
      
      shiny::selectInput(
        "section_to_edit",
        "Section to edit",
        choices = all_sec
      )
    })
    
    shiny::observeEvent(input$apply_sections, {
      cm <- cm_current()
      
      tryCatch(
        {
          sections(cm, n = input$sections_keep %||% character())
          rv$cm <- cm
          
          shiny::showNotification(
            "Section selection updated.",
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not update sections:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })
    
    shiny::observeEvent(input$apply_section_text, {
      cm <- cm_current()
      
      shiny::validate(
        shiny::need(
          nzchar(input$section_to_edit %||% ""),
          "Please select a section."
        ),
        shiny::need(
          nzchar(input$section_new_text %||% ""),
          "Please write some text."
        )
      )
      
      tryCatch(
        {
          updateReportSection(
            cm,
            section = input$section_to_edit,
            text = input$section_new_text,
            append_text = isTRUE(input$section_append)
          )
          
          rv$cm <- cm
          
          shiny::showNotification(
            "Section text updated.",
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not update section text:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })
    
    output$section_list <- shiny::renderPrint({
      cm <- cm_current()
      
      out <- .safe(listReportSections(cm), NULL)
      
      if (is.null(out)) {
        sections(cm)
      } else {
        out
      }
    })
    
    output$cm_summary <- shiny::renderPrint({
      cm <- cm_current()
      
      list(
        siteName = .safe(cm$siteName, NA),
        title = .safe(cm$title, NA),
        subtitle = .safe(cm$subtitle, NA),
        authors = .safe(cm$authors, NA),
        institute = .safe(cm$institute, NA),
        years = .safe(cm$years, NA),
        filterCount = .safe(cm$filterCount, NA),
        focus_group = .safe(cm$setting$focus_groups, NA),
        included_sections = .safe(sections(cm), NA),
        n_species = nrow(.species_table(cm))
      )
    })
    
    shiny::observeEvent(input$save_cm, {
      cm <- apply_gui_settings(run_setup = FALSE)
      
      out_dir <- input$output_dir %||% getwd()
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      out_file <- file.path(out_dir, "camtrapReport_gui_object.rds")
      
      saveRDS(cm, out_file)
      
      shiny::showNotification(
        paste(
          "Saved:",
          normalizePath(out_file, winslash = "/", mustWork = FALSE)
        ),
        type = "message",
        duration = 8
      )
    })
    
    shiny::observeEvent(input$generate_status, {
      cm <- apply_gui_settings(run_setup = TRUE)
      
      out_dir <- input$output_dir %||% getwd()
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      
      setwd(out_dir)
      
      tryCatch(
        {
          status(cm, view = TRUE)
          
          shiny::showNotification(
            "Data status report generated.",
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not generate status report:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })
    
    shiny::observeEvent(input$generate_report, {
      cm <- apply_gui_settings(run_setup = TRUE)
      
      out_dir <- input$output_dir %||% getwd()
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      
      setwd(out_dir)
      
      tryCatch(
        {
          report(cm, view = TRUE)
          
          shiny::showNotification(
            "Ecological report generated.",
            type = "message"
          )
        },
        error = function(e) {
          shiny::showNotification(
            paste("Could not generate ecological report:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
        }
      )
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
