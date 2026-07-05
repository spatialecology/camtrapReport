# app.R
# Standalone camtrapReport-style GUI
# Install once if needed: install.packages("shiny")

library(shiny)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# ============================================================
# SETTINGS
# ============================================================

logo_path <- "C:/Users/ebrah010/OneDrive - Wageningen University & Research/Package_camReport/_GitHUB/camtrapReport/man/figures/logo.png"

# Use a YouTube embed URL if you have one, for example:
# "https://www.youtube.com/embed/dQw4w9WgXcQ"
video_url <- ""

default_welcome_text <- paste(
  "camtrapReport helps you turn camera-trap data into clear ecological reports through an automated workflow.\n\n",
  "It first checks your data and tells you if anything needs to be corrected before moving on.  It then creates summaries, visualisations, descriptive information, and ecological analyses, all in one report.\n\n",
  "From the navigation panel on the left, you can customize the report, including text, sections, focal species, and more.\n\n",
  "For a quick walkthrough, have a look at the video below."
)

sections_df <- data.frame(
  id = c("overview", "sampling", "species", "activity", "occupancy", "appendix"),
  label = c(
    "Overview",
    "Sampling effort",
    "Focal species",
    "Temporal activity",
    "Occupancy summary",
    "Appendix"
  ),
  text = c(
    "General overview of the study area, data source, and reporting goals.",
    "Description of camera deployment, number of stations, and sampling period.",
    "Section focused on selected focal species and species-specific summaries.",
    "Daily and seasonal activity patterns derived from camera trap records.",
    "Summary of occupancy-style outputs and interpretation notes.",
    "Additional tables, diagnostics, and supporting material."
  ),
  code = c(
    "# overview\ncat('Overview section')",
    "# sampling\ncat('Sampling effort section')",
    "# species\ncat('Focal species section')",
    "# activity\ncat('Temporal activity section')",
    "# occupancy\ncat('Occupancy summary section')",
    "# appendix\ncat('Appendix section')"
  ),
  stringsAsFactors = FALSE
)

# ============================================================
# LOGO
# ============================================================

logo_src <- NULL
if (!is.null(logo_path) && nzchar(logo_path) && file.exists(logo_path)) {
  logo_prefix <- paste0("camreport_logo_", as.integer(Sys.time()))
  addResourcePath(
    prefix = logo_prefix,
    directoryPath = normalizePath(dirname(logo_path), winslash = "/", mustWork = TRUE)
  )
  logo_src <- paste0(logo_prefix, "/", basename(logo_path))
}

# ============================================================
# HELPERS
# ============================================================

nav_button <- function(id, label, icon_name) {
  actionButton(
    inputId = id,
    label = div(icon(icon_name), span(label)),
    class = "nav-btn"
  )
}

stat_card <- function(title, output_id, icon_name) {
  div(
    class = "stat-card",
    div(class = "stat-icon", icon(icon_name)),
    div(
      class = "stat-text",
      div(class = "stat-title", title),
      div(class = "stat-value", textOutput(output_id, inline = TRUE))
    )
  )
}

split_paragraphs <- function(x) {
  x <- x %||% ""
  x <- gsub("\r\n", "\n", x, fixed = TRUE)
  x <- trimws(x)
  if (!nzchar(x)) return(character(0))
  unlist(strsplit(x, "\\n\\s*\\n"))
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  tags$head(
    tags$title("camtrapReport GUI"),
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        background: #d9dde2;
        font-family: Arial, Helvetica, sans-serif;
      }

      body {
        overflow: hidden;
      }

      .container-fluid {
        padding-left: 0;
        padding-right: 0;
      }

      .app-shell {
        display: flex;
        min-height: 100vh;
        background: #d9dde2;
      }

      .sidebar {
        width: 380px;
        min-width: 380px;
        max-width: 380px;
        background: linear-gradient(180deg, #162733 0%, #213642 100%);
        color: #eef4f7;
        display: flex;
        flex-direction: column;
        box-shadow: 4px 0 20px rgba(0,0,0,0.12);
      }

      .brand {
        padding: 18px 22px 14px 22px;
        border-bottom: 1px solid rgba(255,255,255,0.08);
        background: rgba(0,0,0,0.08);
      }

      .brand-title {
        font-size: 38px;
        font-weight: 700;
        color: #ffffff;
        line-height: 1.05;
      }

      .brand-subtitle {
        margin-top: 8px;
        font-size: 17px;
        color: rgba(255,255,255,0.82);
      }

      .logo-wrap {
        text-align: center;
        padding: 22px 16px 12px 16px;
      }

      .sidebar-logo {
        max-width: 255px;
        width: 78%;
        height: auto;
        display: block;
        margin: 0 auto 10px auto;
        background: #ffffff;
        padding: 8px;
      }

      .logo-note {
        color: rgba(255,255,255,0.80);
        font-size: 14px;
      }

      .logo-missing {
        background: rgba(255,255,255,0.06);
        border: 1px dashed rgba(255,255,255,0.20);
        border-radius: 14px;
        padding: 30px 12px;
        color: #ffffff;
      }

      .sidebar-scroll {
        flex: 1;
        overflow-y: auto;
        padding: 16px 18px 20px 18px;
      }

      .side-block {
        margin-bottom: 18px;
      }

      .side-title {
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        font-weight: 700;
        color: rgba(255,255,255,0.68);
        margin: 0 0 10px 2px;
      }

      .side-card {
        background: rgba(255,255,255,0.04);
        border: 1px solid rgba(255,255,255,0.08);
        border-radius: 16px;
        padding: 14px;
      }

      .nav-btn {
        width: 100%;
        margin-bottom: 10px;
        text-align: left;
        border-radius: 14px;
        background: rgba(255,255,255,0.06);
        color: #eef4f7;
        border: 1px solid rgba(255,255,255,0.08);
        font-weight: 700;
        font-size: 16px;
        padding-top: 10px;
        padding-bottom: 10px;
      }

      .nav-btn:hover,
      .nav-btn:focus,
      .nav-btn:active {
        color: #ffffff !important;
        background: rgba(255,255,255,0.10) !important;
        border-color: rgba(255,255,255,0.14) !important;
      }

      .nav-btn .fa {
        width: 20px;
        margin-right: 10px;
      }

      .sidebar .form-control,
      .sidebar textarea,
      .sidebar .btn {
        border-radius: 10px;
      }

      .sidebar .control-label,
      .sidebar .checkbox,
      .sidebar .radio,
      .sidebar .shiny-input-container > label {
        color: #f3f7fa;
      }

      .sidebar .form-control,
      .sidebar textarea {
        background: rgba(255,255,255,0.96);
        color: #223247;
        border: 1px solid rgba(255,255,255,0.15);
      }

      .sidebar hr {
        border-top: 1px solid rgba(255,255,255,0.10);
      }

      .main {
        flex: 1;
        min-width: 0;
        display: flex;
        flex-direction: column;
      }

      .topbar {
        height: 70px;
        background: #08a34a;
        color: #ffffff;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 26px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.10);
      }

      .topbar-left {
        font-size: 28px;
        font-weight: 700;
      }

      .topbar-right {
        font-size: 16px;
        color: rgba(255,255,255,0.92);
        font-weight: 600;
      }

      .main-scroll {
        flex: 1;
        overflow-y: auto;
        padding: 26px;
      }

      .hero {
        background: rgba(255,255,255,0.74);
        border: 1px solid rgba(255,255,255,0.60);
        border-radius: 24px;
        padding: 34px;
        box-shadow: 0 10px 24px rgba(31,42,68,0.08);
        margin-bottom: 22px;
      }

      .hero h1 {
        margin-top: 0;
        margin-bottom: 22px;
        font-size: 44px;
        color: #273447;
      }

      .hero p {
        font-size: 18px;
        line-height: 1.8;
        color: #2f3f50;
        margin-bottom: 14px;
        max-width: 1080px;
      }

      .video-box {
        margin-top: 22px;
        background: #eef2f5;
        border: 2px dashed #b8c2cc;
        border-radius: 18px;
        min-height: 260px;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        color: #66727d;
        text-align: center;
        padding: 20px;
      }

      .video-box .fa {
        font-size: 30px;
        margin-bottom: 12px;
      }

      .video-frame {
        width: 100%;
        max-width: 980px;
        height: 460px;
        border: 0;
        border-radius: 14px;
        background: #ffffff;
      }

      .stats-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(180px, 1fr));
        gap: 16px;
        margin-bottom: 20px;
      }

      .stat-card {
        background: #ffffff;
        border-radius: 20px;
        padding: 18px;
        display: flex;
        align-items: center;
        gap: 14px;
        box-shadow: 0 8px 20px rgba(31,42,68,0.08);
        border: 1px solid rgba(20,40,60,0.06);
      }

      .stat-icon {
        width: 52px;
        height: 52px;
        border-radius: 14px;
        background: #e9f7ef;
        color: #0a8e46;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 20px;
        flex: 0 0 auto;
      }

      .stat-title {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        color: #6b7785;
        font-weight: 700;
        margin-bottom: 3px;
      }

      .stat-value {
        font-size: 26px;
        font-weight: 700;
        color: #223247;
      }

      .content-grid {
        display: grid;
        grid-template-columns: minmax(0, 2fr) minmax(320px, 1fr);
        gap: 18px;
      }

      .panel-card {
        background: #ffffff;
        border-radius: 20px;
        box-shadow: 0 10px 24px rgba(31,42,68,0.08);
        border: 1px solid rgba(20,40,60,0.06);
        overflow: hidden;
      }

      .panel-header {
        padding: 18px 22px;
        border-bottom: 1px solid #edf1f4;
        font-weight: 700;
        color: #223247;
        background: #fbfcfd;
        font-size: 18px;
      }

      .panel-body {
        padding: 18px 22px 22px 22px;
      }

      .mono-box {
        white-space: pre-wrap;
        font-family: Consolas, 'Courier New', monospace;
        font-size: 13px;
        line-height: 1.6;
        max-height: 70vh;
        overflow-y: auto;
        background: #fbfcfd;
        border: 1px solid #edf1f4;
        border-radius: 12px;
        padding: 14px;
      }

      .section-summary {
        background: #f7fafc;
        border: 1px solid #e4ebf0;
        border-radius: 14px;
        padding: 16px;
      }

      .tab-content {
        padding-top: 14px;
      }

      @media (max-width: 1200px) {
        .stats-grid {
          grid-template-columns: repeat(2, minmax(180px, 1fr));
        }
        .content-grid {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 900px) {
        body {
          overflow: auto;
        }
        .app-shell {
          display: block;
        }
        .sidebar {
          width: 100%;
          min-width: 100%;
          max-width: 100%;
        }
        .stats-grid {
          grid-template-columns: 1fr;
        }
        .video-frame {
          height: 260px;
        }
      }
    "))
  ),
  
  div(
    class = "app-shell",
    
    div(
      class = "sidebar",
      
      div(
        class = "brand",
        div(class = "brand-title", "camtrapReport"),
        div(class = "brand-subtitle", "Convert raw data into ecological insights")
      ),
      
      div(
        class = "logo-wrap",
        if (!is.null(logo_src)) {
          tags$img(src = logo_src, class = "sidebar-logo")
        } else {
          div(
            class = "logo-missing",
            icon("camera"),
            div("Logo not found")
          )
        },
        div(class = "logo-note", "NPS logo disclaimer")
      ),
      
      div(
        class = "sidebar-scroll",
        
        div(
          class = "side-block",
          div(class = "side-title", "Navigation"),
          div(
            class = "side-card",
            nav_button("go_home", "Home", "home"),
            nav_button("go_customize", "Customize report", "sliders"),
            nav_button("go_sections", "Sections", "list"),
            nav_button("go_species", "Focal species", "paw"),
            nav_button("go_preview", "Preview", "file-text")
          )
        ),
        
        div(
          class = "side-block",
          div(class = "side-title", "Quick actions"),
          div(
            class = "side-card",
            actionButton("save_settings", "Save current settings", class = "btn btn-success btn-block"),
            br(), br(),
            actionButton("reset_settings", "Reset defaults", class = "btn btn-default btn-block")
          )
        )
      )
    ),
    
    div(
      class = "main",
      
      div(
        class = "topbar",
        div(class = "topbar-left", "camtrapReport GUI"),
        div(class = "topbar-right", textOutput("topbar_site", inline = TRUE))
      ),
      
      div(
        class = "main-scroll",
        
        div(
          class = "hero",
          h1("Welcome !"),
          uiOutput("welcome_ui"),
          uiOutput("video_ui")
        ),
        
        div(
          class = "stats-grid",
          stat_card("Sections", "stat_sections", "list"),
          stat_card("Selected", "stat_selected", "check"),
          stat_card("Years", "stat_years", "calendar"),
          stat_card("Focal species", "stat_species", "paw")
        ),
        
        tabsetPanel(
          id = "main_tabs",
          type = "hidden",
          
          tabPanel(
            title = "Home", value = "home",
            div(
              class = "content-grid",
              div(
                class = "panel-card",
                div(class = "panel-header", "Project overview"),
                div(
                  class = "panel-body",
                  p("Use this GUI to prepare a simple report structure for camtrapReport."),
                  p("The layout is designed with a dark left navigation panel, a green top bar, editable welcome text, and a clean report editor.")
                )
              ),
              div(
                class = "panel-card",
                div(class = "panel-header", "Current settings"),
                div(
                  class = "panel-body",
                  uiOutput("home_summary")
                )
              )
            )
          ),
          
          tabPanel(
            title = "Customize", value = "customize",
            div(
              class = "content-grid",
              div(
                class = "panel-card",
                div(class = "panel-header", "Report metadata"),
                div(
                  class = "panel-body",
                  textInput("site_name", "Site name", value = "Biodivers+ Project"),
                  textInput("report_title", "Report title", value = "camtrapReport"),
                  textInput("report_subtitle", "Subtitle", value = "Camera trap Report Generator"),
                  textInput("authors", "Authors", value = "Elham Ebrahimi"),
                  textInput("institute", "Institute", value = "Wageningen University & Research"),
                  textInput("years", "Years", value = "2024, 2025"),
                  textAreaInput(
                    "description",
                    "Description",
                    value = "This report summarizes camera trap records and translates them into ecological insights.",
                    rows = 5
                  ),
                  textAreaInput(
                    "acknowledgement",
                    "Acknowledgement",
                    value = "We thank all collaborators and field teams involved in camera deployment and data collection.",
                    rows = 4
                  )
                )
              ),
              div(
                class = "panel-card",
                div(class = "panel-header", "Intro text"),
                div(
                  class = "panel-body",
                  textAreaInput(
                    "welcome_text",
                    "Home page text",
                    value = default_welcome_text,
                    rows = 12
                  )
                )
              )
            )
          ),
          
          tabPanel(
            title = "Sections", value = "sections",
            div(
              class = "content-grid",
              div(
                class = "panel-card",
                div(class = "panel-header", "Section selection"),
                div(
                  class = "panel-body",
                  textInput("section_search", "Search section", value = "", placeholder = "Type to search..."),
                  checkboxGroupInput(
                    "active_sections",
                    "Include sections",
                    choices = setNames(sections_df$id, sections_df$label),
                    selected = sections_df$id
                  ),
                  selectInput(
                    "section_id",
                    "Inspect section",
                    choices = setNames(sections_df$id, sections_df$label),
                    selected = sections_df$id[1]
                  )
                )
              ),
              div(
                class = "panel-card",
                div(class = "panel-header", "Section details"),
                div(
                  class = "panel-body",
                  tabsetPanel(
                    tabPanel("Text", div(class = "mono-box", textOutput("section_text"))),
                    tabPanel("Code", div(class = "mono-box", textOutput("section_code"))),
                    tabPanel("Summary", uiOutput("section_summary"))
                  )
                )
              )
            )
          ),
          
          tabPanel(
            title = "Species", value = "species",
            div(
              class = "content-grid",
              div(
                class = "panel-card",
                div(class = "panel-header", "Focal species"),
                div(
                  class = "panel-body",
                  checkboxGroupInput(
                    "focal_species",
                    "Choose focal species",
                    choices = c("Red fox", "Wild boar", "Roe deer", "Badger", "Wolf", "Brown hare"),
                    selected = c("Red fox", "Roe deer")
                  )
                )
              ),
              div(
                class = "panel-card",
                div(class = "panel-header", "Species note"),
                div(
                  class = "panel-body",
                  textAreaInput(
                    "species_note",
                    "Text for species section",
                    value = "Selected focal species will receive dedicated space in the report.",
                    rows = 8
                  )
                )
              )
            )
          ),
          
          tabPanel(
            title = "Preview", value = "preview",
            div(
              class = "content-grid",
              div(
                class = "panel-card",
                div(class = "panel-header", "Preview"),
                div(
                  class = "panel-body",
                  uiOutput("preview_ui")
                )
              ),
              div(
                class = "panel-card",
                div(class = "panel-header", "Export"),
                div(
                  class = "panel-body",
                  p("Download a simple HTML preview of the current settings."),
                  downloadButton("download_preview", "Download HTML preview", class = "btn btn-success")
                )
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  defaults <- list(
    site_name = "trap project",
    report_title = "camtrapReport",
    report_subtitle = "Camera trap Report Generator",
    authors = "Elham Ebrahimi",
    institute = "Wageningen University & Research",
    years = "2024, 2025",
    description = "This report summarizes camera trap records and translates them into ecological insights.",
    acknowledgement = "We thank all collaborators and field teams involved in camera deployment and data collection.",
    welcome_text = default_welcome_text,
    active_sections = sections_df$id,
    section_id = sections_df$id[1],
    focal_species = c("Red fox", "Roe deer"),
    species_note = "Selected focal species will receive dedicated space in the report."
  )
  
  observeEvent(input$go_home,      updateTabsetPanel(session, "main_tabs", selected = "home"))
  observeEvent(input$go_customize, updateTabsetPanel(session, "main_tabs", selected = "customize"))
  observeEvent(input$go_sections,  updateTabsetPanel(session, "main_tabs", selected = "sections"))
  observeEvent(input$go_species,   updateTabsetPanel(session, "main_tabs", selected = "species"))
  observeEvent(input$go_preview,   updateTabsetPanel(session, "main_tabs", selected = "preview"))
  
  filtered_sections <- reactive({
    q <- trimws(input$section_search %||% "")
    df <- sections_df
    if (nzchar(q)) {
      keep <- grepl(q, paste(df$label, df$text, df$code), ignore.case = TRUE)
      df <- df[keep, , drop = FALSE]
    }
    df
  })
  
  observe({
    df <- filtered_sections()
    choices <- stats::setNames(df$id, df$label)
    
    current <- isolate(input$section_id)
    selected <- if (!is.null(current) && current %in% df$id) {
      current
    } else if (nrow(df) > 0) {
      df$id[1]
    } else {
      character(0)
    }
    
    updateSelectInput(session, "section_id", choices = choices, selected = selected)
  })
  
  current_section <- reactive({
    req(input$section_id)
    df <- sections_df[sections_df$id == input$section_id, , drop = FALSE]
    req(nrow(df) == 1)
    df
  })
  
  observeEvent(input$reset_settings, {
    updateTextInput(session, "site_name", value = defaults$site_name)
    updateTextInput(session, "report_title", value = defaults$report_title)
    updateTextInput(session, "report_subtitle", value = defaults$report_subtitle)
    updateTextInput(session, "authors", value = defaults$authors)
    updateTextInput(session, "institute", value = defaults$institute)
    updateTextInput(session, "years", value = defaults$years)
    updateTextAreaInput(session, "description", value = defaults$description)
    updateTextAreaInput(session, "acknowledgement", value = defaults$acknowledgement)
    updateTextAreaInput(session, "welcome_text", value = defaults$welcome_text)
    updateCheckboxGroupInput(session, "active_sections", selected = defaults$active_sections)
    updateSelectInput(session, "section_id", selected = defaults$section_id)
    updateCheckboxGroupInput(session, "focal_species", selected = defaults$focal_species)
    updateTextAreaInput(session, "species_note", value = defaults$species_note)
    
    showNotification("Defaults restored.", type = "message")
  })
  
  observeEvent(input$save_settings, {
    showNotification("Current settings saved in this session.", type = "message")
  })
  
  output$topbar_site <- renderText({
    input$site_name %||% "Report builder"
  })
  
  output$welcome_ui <- renderUI({
    paragraphs <- split_paragraphs(input$welcome_text %||% "")
    if (length(paragraphs) == 0) return(NULL)
    
    tagList(
      lapply(paragraphs, function(par) {
        p(par)
      })
    )
  })
  
  output$video_ui <- renderUI({
    if (!is.null(video_url) && nzchar(video_url)) {
      tags$iframe(
        src = video_url,
        class = "video-frame",
        allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
        allowfullscreen = NA
      )
    } else {
      div(
        class = "video-box",
        icon("play-circle"),
        div("Walkthrough video area"),
        tags$small("Add a YouTube embed URL in video_url to show the video here.")
      )
    }
  })
  
  output$stat_sections <- renderText({
    nrow(sections_df)
  })
  
  output$stat_selected <- renderText({
    length(input$active_sections %||% character(0))
  })
  
  output$stat_years <- renderText({
    yrs <- trimws(input$years %||% "")
    if (!nzchar(yrs)) "—" else yrs
  })
  
  output$stat_species <- renderText({
    length(input$focal_species %||% character(0))
  })
  
  output$home_summary <- renderUI({
    tags$div(
      class = "section-summary",
      tags$p(tags$strong("Site name: "), input$site_name %||% ""),
      tags$p(tags$strong("Report title: "), input$report_title %||% ""),
      tags$p(tags$strong("Authors: "), input$authors %||% ""),
      tags$p(tags$strong("Institute: "), input$institute %||% ""),
      tags$p(tags$strong("Years: "), input$years %||% ""),
      tags$p(tags$strong("Selected sections: "), length(input$active_sections %||% character(0))),
      tags$p(tags$strong("Focal species: "), paste(input$focal_species %||% character(0), collapse = ", "))
    )
  })
  
  output$section_text <- renderText({
    current_section()$text[1]
  })
  
  output$section_code <- renderText({
    current_section()$code[1]
  })
  
  output$section_summary <- renderUI({
    sec <- current_section()
    tags$div(
      class = "section-summary",
      tags$p(tags$strong("Section ID: "), sec$id[1]),
      tags$p(tags$strong("Label: "), sec$label[1]),
      tags$p(tags$strong("Included in report: "), if (sec$id[1] %in% (input$active_sections %||% character(0))) "Yes" else "No")
    )
  })
  
  output$preview_ui <- renderUI({
    selected_ids <- input$active_sections %||% character(0)
    selected_df <- sections_df[sections_df$id %in% selected_ids, , drop = FALSE]
    selected_species <- input$focal_species %||% character(0)
    
    tags$div(
      class = "section-summary",
      tags$h3(style = "margin-top: 0;", input$report_title %||% "camtrapReport"),
      tags$p(tags$strong("Subtitle: "), input$report_subtitle %||% ""),
      tags$p(tags$strong("Site name: "), input$site_name %||% ""),
      tags$p(tags$strong("Authors: "), input$authors %||% ""),
      tags$p(tags$strong("Institute: "), input$institute %||% ""),
      tags$p(tags$strong("Years: "), input$years %||% ""),
      tags$hr(),
      tags$p(tags$strong("Description")),
      tags$p(input$description %||% ""),
      tags$hr(),
      tags$p(tags$strong("Included sections")),
      if (nrow(selected_df) == 0) {
        tags$p("No sections selected.")
      } else {
        tags$ul(lapply(selected_df$label, tags$li))
      },
      tags$p(tags$strong("Focal species")),
      if (length(selected_species) == 0) {
        tags$p("No focal species selected.")
      } else {
        tags$ul(lapply(selected_species, tags$li))
      },
      tags$p(tags$strong("Species note")),
      tags$p(input$species_note %||% ""),
      tags$hr(),
      tags$p(tags$strong("Acknowledgement")),
      tags$p(input$acknowledgement %||% "")
    )
  })
  
  output$download_preview <- downloadHandler(
    filename = function() {
      "camtrapReport_preview.html"
    },
    content = function(file) {
      selected_ids <- input$active_sections %||% character(0)
      selected_df <- sections_df[sections_df$id %in% selected_ids, , drop = FALSE]
      selected_species <- input$focal_species %||% character(0)
      
      html <- paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>", htmltools::htmlEscape(input$report_title %||% "camtrapReport"), "</title>",
        "<style>",
        "body{font-family:Arial,Helvetica,sans-serif;margin:40px;line-height:1.6;color:#223247;}",
        "h1{color:#08a34a;} h2{margin-top:28px;} .box{background:#f7fafc;border:1px solid #e4ebf0;border-radius:12px;padding:18px;}",
        "</style></head><body>",
        "<h1>", htmltools::htmlEscape(input$report_title %||% "camtrapReport"), "</h1>",
        "<p><strong>Subtitle:</strong> ", htmltools::htmlEscape(input$report_subtitle %||% ""), "</p>",
        "<p><strong>Site name:</strong> ", htmltools::htmlEscape(input$site_name %||% ""), "</p>",
        "<p><strong>Authors:</strong> ", htmltools::htmlEscape(input$authors %||% ""), "</p>",
        "<p><strong>Institute:</strong> ", htmltools::htmlEscape(input$institute %||% ""), "</p>",
        "<p><strong>Years:</strong> ", htmltools::htmlEscape(input$years %||% ""), "</p>",
        "<div class='box'><h2>Welcome text</h2><p>",
        paste(htmltools::htmlEscape(split_paragraphs(input$welcome_text %||% "")), collapse = "</p><p>"),
        "</p></div>",
        "<div class='box'><h2>Description</h2><p>", htmltools::htmlEscape(input$description %||% ""), "</p></div>",
        "<div class='box'><h2>Included sections</h2><ul>",
        paste(sprintf("<li>%s</li>", htmltools::htmlEscape(selected_df$label)), collapse = ""),
        "</ul></div>",
        "<div class='box'><h2>Focal species</h2><ul>",
        paste(sprintf("<li>%s</li>", htmltools::htmlEscape(selected_species)), collapse = ""),
        "</ul></div>",
        "<div class='box'><h2>Species note</h2><p>", htmltools::htmlEscape(input$species_note %||% ""), "</p></div>",
        "<div class='box'><h2>Acknowledgement</h2><p>", htmltools::htmlEscape(input$acknowledgement %||% ""), "</p></div>",
        "</body></html>"
      )
      
      writeLines(html, file, useBytes = TRUE)
    }
  )
}

shinyApp(ui, server)
