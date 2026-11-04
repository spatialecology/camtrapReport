# Functions for the camtrapReport graphical user interface
# Licence: MIT
#--------

setGeneric("gui", function(object, launch.browser, max_upload_mb, ...) {
  methods::standardGeneric("gui")
})

#' Launch the camtrapReport GUI
#'
#' Launch an interactive Shiny interface for configuring and generating
#' camtrapReport outputs from an existing [`camReport`][camReport-classes]
#' object.
#'
#' The function starts a Shiny-based graphical user interface for editing report
#' metadata, selecting report sections, defining focus groups, and generating
#' data-status or ecological reports.
#'
#' The GUI can be used with an existing `camReport` object and provides an
#' interactive alternative to configuring reports directly in R.
#' Choose an existing output directory explicitly before loading ZIP data or
#' saving output. ZIP data are extracted there so saved objects retain their
#' source paths across R sessions.
#' Existing report, R Markdown, and saved-object files are protected unless
#' the overwrite checkbox is selected. Generating output does not change the
#' R working directory. On local Windows sessions, Browse opens R's native
#' folder chooser; manual path entry remains available on every platform.
#' The Sections tab edits the loaded source narrative without evaluating its
#' placeholders or replacing code-generated content. Reset restores the text
#' present when the dataset was loaded into the GUI. Save the current object
#' as RDS to retain edits to included sections. Excluded sections retain their
#' edits only for the current GUI session. Focus-group choices come from the data;
#' missing taxonomy is explained without automatically contacting GBIF.
#' ZIP imports have a separate, initially unchecked permission to overwrite an
#' existing extraction directory; see [camData()] for its location and limits.
#' GBIF taxonomy enrichment is also initially unchecked. Selecting it permits
#' sending scientific names to GBIF when importing fresh data with `taxize`
#' installed. Loading an existing processed-object cache does not enrich it.
#'
#' @param object A [`camReport`][camReport-classes] object, usually created with
#'   [camData()].
#' @param launch.browser A logical value (default `TRUE`) indicating whether the
#'   application is opened in the default web browser.
#' @param max_upload_mb A numeric value (default `2000`) giving the maximum
#'   file-upload size in megabytes.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Invisibly returns the supplied `camReport` object after launching the
#'   application.
#'
#' @seealso [camData()], [report()], [status()], [info()]
#' @family report generation
#'
#' @usage gui(object, launch.browser, max_upload_mb, ...)
#' @rdname gui
#' @aliases gui
#'
#' @examplesIf interactive() && identical(Sys.getenv("CAMTRAPREPORT_RUN_GUI_EXAMPLE"), "true")
#' # Set CAMTRAPREPORT_RUN_GUI_EXAMPLE=true deliberately to launch this example.
#' source_dataset <- system.file(
#'   "external",
#'   "dataset",
#'   package = "camtrapReport"
#' )
#' example_dataset <- tempfile("camtrapReport-example-")
#' dir.create(example_dataset)
#' invisible(file.copy(
#'   list.files(source_dataset, full.names = TRUE),
#'   example_dataset,
#'   recursive = TRUE
#' ))
#'
#' cm <- camData(example_dataset)
#'
#' gui(cm)
#'
#' unlink(example_dataset, recursive = TRUE, force = TRUE)
setMethod(
  "gui",
  signature(object = "camReport"),
  function(object, launch.browser = TRUE, max_upload_mb = 2000, ...) {
    if (missing(object) || is.null(object)) {
      stop(
        "Please provide a camReport object, for example: gui(cm).",
        call. = FALSE
      )
    }

    if (!requireNamespace("shiny", quietly = TRUE)) {
      stop(
        "The 'shiny' package is required to use gui().\n",
        "Please install it with install.packages('shiny').",
        call. = FALSE
      )
    }

    if (missing(launch.browser)) {
      launch.browser <- TRUE
    }

    if (missing(max_upload_mb)) {
      max_upload_mb <- 2000
    }

    old_limit <- getOption("shiny.maxRequestSize")
    options(shiny.maxRequestSize = max_upload_mb * 1024^2)

    on.exit(
      {
        options(shiny.maxRequestSize = old_limit)
      },
      add = TRUE
    )

    app <- .camtrapReport_gui_app(object = object)

    shiny::runApp(
      app,
      launch.browser = launch.browser,
      ...
    )

    invisible(object)
  }
)


.camtrapReport_gui_app <- function(object) {
  `%||%` <- function(x, y) {
    if (
      is.null(x) ||
      length(x) == 0 ||
      (length(x) == 1 && is.na(x))
    ) {
      y
    } else {
      x
    }
  }

  .safe <- function(expr, fallback = NULL) {
    tryCatch(expr, error = function(e) fallback)
  }

  .default_output_dir <- function(cm) {
    ""
  }

  .output_directory <- function(path) {
    path <- trimws(path %||% "")
    if (length(path) != 1L || is.na(path) || !nzchar(path) || !dir.exists(path)) {
      stop("Choose an existing output directory before saving or rendering.",
           call. = FALSE)
    }
    .check_output_directory(path)
    normalizePath(path, winslash = "/", mustWork = TRUE)
  }

  .choose_output_directory <- function(default = "") {
    if (.Platform$OS.type != "windows") {
      stop("Folder browsing is available on local Windows sessions. ",
           "Enter an existing path manually on this system.", call. = FALSE)
    }
    chooser <- getExportedValue("utils", "choose.dir")
    chooser(default = default, caption = "Choose report output directory")
  }

  .check_gui_report_output <- function(cm, out_dir, stem, overwrite) {
    destination <- .resolve_report_output(
      cm, file.path(out_dir, paste0(stem, ".html")), stem,
      format = "html", format_missing = TRUE
    )
    .check_report_destination(destination, overwrite, announce = FALSE)
  }

  .as_text <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("")
    }
    x <- as.character(x)
    x <- x[!is.na(x)]
    toString(x)
  }

  .copy_upload <- function(upload, temp_dir) {
    if (is.null(upload)) {
      return(NULL)
    }

    .validate_gui_upload(upload, "zip")
    d <- tempfile("upload_", tmpdir = temp_dir)
    if (!dir.create(d)) stop("Could not create the upload directory.")
    copied <- FALSE
    on.exit(if (!copied) unlink(d, recursive = TRUE), add = TRUE)
    out <- file.path(d, upload$name[1])
    if (!file.copy(from = upload$datapath[1], to = out, overwrite = FALSE)) {
      stop("Could not copy the uploaded ZIP file.", call. = FALSE)
    }
    copied <- TRUE
    normalizePath(out, winslash = "/", mustWork = TRUE)
  }

  .copy_study_area_upload <- function(upload, temp_dir) {
    if (is.null(upload)) {
      return(NULL)
    }

    .validate_gui_upload(upload, c("shp", "shx", "dbf", "prj", "cpg"),
                         multiple = TRUE)
    shp <- grepl("\\.shp$", upload$name, ignore.case = TRUE)
    stems <- tolower(sub("\\.[^.]+$", "", upload$name))
    if (sum(shp) != 1L || length(unique(stems)) != 1L) {
      stop("Upload one .shp file and matching sidecars with the same basename.",
           call. = FALSE)
    }
    d <- tempfile("study_area_", tmpdir = temp_dir)
    if (!dir.create(d)) stop("Could not create the study-area directory.")
    copied <- FALSE
    on.exit(if (!copied) unlink(d, recursive = TRUE), add = TRUE)
    if (!all(file.copy(upload$datapath, file.path(d, upload$name),
                       overwrite = FALSE))) {
      stop("Could not copy the uploaded study-area files.", call. = FALSE)
    }
    copied <- TRUE
    shp <- normalizePath(file.path(d, upload$name[shp]), winslash = "/",
                         mustWork = TRUE)

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
      .validate_gui_upload(upload, "csv")
      return(utils::read.csv(upload$datapath, stringsAsFactors = FALSE))
    }

    NULL
  }

  .get_zip_path <- function(upload = NULL, path = NULL, temp_dir = NULL) {
    path <- trimws(path %||% "")

    if (nzchar(path)) {
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      if (!dir.exists(path) && !.isZip(path)) {
        stop("Choose a ZIP file or an extracted dataset directory.",
             call. = FALSE)
      }
      return(path)
    }

    if (!is.null(upload)) {
      if (is.null(temp_dir)) {
        stop(
          "A temporary directory is required for uploaded files.",
          call. = FALSE
        )
      }
      return(.copy_upload(upload, temp_dir = temp_dir))
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
    ok <- .safe(
      {
        info(cm, name = name) <- value
        TRUE
      },
      FALSE
    )

    if (!isTRUE(ok)) {
      .safe(
        {
          cm[[name]] <- value
          TRUE
        },
        FALSE
      )
    }

    invisible(cm)
  }

  # Keep the loaded tree (including runtime/custom sections) and offer other
  # available modules without replacing the loaded sections with registry copies.
  .filter_section_tree <- function(tree, keep) {
    if (inherits(tree, ".textSection")) {
      return(if (tree@name %in% keep) tree else NULL)
    }
    if (!is.list(tree)) return(tree)
    out <- lapply(tree, .filter_section_tree, keep = keep)
    out[!vapply(out, is.null, logical(1))]
  }

  .gui_section_tree <- function(cm) {
    if (is.null(cm)) return(list())
    editor <- cm$copy(shallow = TRUE)
    available <- .available_pool_names(cm, "report")
    extra <- setdiff(available, .reportSection_catalog(cm$reportObjects)$name)
    for (name in extra) {
      before <- editor$reportObjects
      # Use the existing attachment method for hierarchy and heading levels.
      # A module whose parent is unavailable must not replace a custom tree.
      tryCatch(editor$addReportObject(.module_pool(cm, "report")[[name]]),
               error = function(e) { editor$reportObjects <- before })
    }
    editor$reportObjects
  }

  .section_choices <- function(tree) {
    catalog <- .reportSection_catalog(tree)
    labels <- ifelse(nzchar(catalog$title), catalog$title, catalog$name)
    labels <- paste0(ifelse(catalog$parent == ".root", "",
                            paste0(catalog$parent, " / ")),
                     labels, " [", catalog$name, "]")
    stats::setNames(catalog$name, labels)
  }

  .gui_section <- function(tree, name) {
    hit <- .matchReportSection(.reportSection_catalog(tree), name, by = "name")
    nodes <- .flatten_attached_sections(tree)
    nodes[[which(vapply(nodes, function(x) identical(x@name, hit$name), logical(1)))]]
  }

  .select_gui_sections <- function(cm, tree, selected) {
    catalog <- .reportSection_catalog(tree)
    if (!all(selected %in% catalog$name)) stop("The section selection is no longer available.")
    if (!length(selected)) stop("Select at least one section.")
    # Selecting a child also requires its ancestors. Resolve dependencies in
    # the GUI rather than silently dropping a user's selected sections.
    # Follow the catalogue hierarchy without rebuilding the edited tree.
    requested <- unique(selected)
    repeat {
      parents <- catalog$parent[match(selected, catalog$name)]
      parents <- parents[!is.na(parents) & parents %in% catalog$name]
      next_selected <- unique(c(selected, parents))
      if (length(next_selected) == length(selected)) break
      selected <- next_selected
    }
    selected <- catalog$name[catalog$name %in% selected]
    cm$reportObjects <- .filter_section_tree(tree, selected)
    invisible(setdiff(selected, requested))
  }

  .edit_gui_section <- function(cm, tree, name, text) {
    # The existing editor handles only attached sections. Give it a shallow
    # object copy with the GUI tree so excluded sections can also be edited.
    editor <- cm$copy(shallow = TRUE)
    editor$reportObjects <- tree
    updateReportSection(editor, section = name, text = text)
    editor$reportObjects
  }

  .unique_values <- function(cm, field) {
    dfs <- list(
      .safe(cm$data$observations, NULL),
      .safe(cm$data$taxonomy, NULL),
      .safe(cm$data_merged, NULL)
    )

    vals <- unlist(lapply(dfs, function(df) {
      if (!is.data.frame(df)) {
        return(character())
      }
      if (!field %in% names(df)) {
        return(character())
      }

      x <- as.character(df[[field]])
      x <- x[!is.na(x)]
      x <- x[nzchar(trimws(x))]
      x
    }))

    sort(unique(vals))
  }

  .species_table <- function(cm) {
    obs <- .safe(cm$data$observations, NULL)

    if (!is.data.frame(obs) || !"scientificName" %in% names(obs)) {
      return(data.frame(
        scientificName = character(),
        records = integer(),
        stringsAsFactors = FALSE
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
      shiny::tags$style(shiny::HTML(
        "
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
      "
      ))
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
          "Camtrap DP ZIP file path",
          value = "",
          placeholder = "C:/Users/.../Leuven-data.zip"
        ),

        shiny::fileInput(
          "data_zip",
          "Or upload Camtrap DP ZIP file",
          accept = ".zip"
        ),

        shiny::checkboxInput(
          "overwrite_dataset",
          "Allow overwriting an existing ZIP extraction directory",
          value = FALSE
        ),

        shiny::helpText(
          "ZIP files extract to a folder named after the archive in the ",
          "selected output directory. Unrelated old files are retained. This ",
          "permission is separate from overwriting report output."
        ),

        shiny::checkboxInput(
          "enrich_taxonomy",
          "Enrich missing taxonomy using GBIF (optional taxize package)",
          value = FALSE
        ),

        shiny::helpText(
          "Opting in sends scientific names to GBIF for fresh imports. ",
          "Source classifications are retained. Existing processed-object ",
          "caches are reused unchanged; use camData(update = TRUE, ",
          "enrich_taxonomy = TRUE) in R to rebuild an existing cache."
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
                paste0(
                  "Define a group using one field. Typical fields are ",
                  "scientificName, class, order, or observationType."
                )
              ),

              shiny::textInput(
                "new_group_name",
                "New focus group name",
                value = "my_focus_group"
              ),

              shiny::selectInput(
                "group_field",
                "Field used to define the group",
                choices = c(
                  "scientificName",
                  "class",
                  "order",
                  "observationType"
                ),
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
              shiny::p(
                "This table is read directly from the loaded camReport object."
              ),
              shiny::tableOutput("species_table")
            )
          ),

          shiny::tabPanel(
            "Sections",

            shiny::div(
              class = "card",
              shiny::h3("Include / exclude report sections"),

              shiny::uiOutput("sections_ui"),
              shiny::helpText("Selecting a child automatically includes its required parent sections. To exclude a parent and its children, remove the children too."),

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

              shiny::uiOutput("section_text_ui"),
              shiny::p(
                "Edit the source narrative, keeping placeholders such as {siteName} ",
                "where dynamic values are wanted. Figures, tables and generated ",
                "content remain intact. Save changes before switching sections."
              ),
              shiny::actionButton(
                "apply_section_text", "Save section text", class = "btn btn-success"
              ),
              shiny::actionButton("reset_section_text", "Reset to original"),
              shiny::helpText(
                "Reset restores the text present when this dataset was loaded into ",
                "the GUI. Save the current camReport object as RDS to retain ",
                "included sections. Edits to excluded sections last only for ",
                "this GUI session."
              ),
              shiny::textOutput("section_summary")
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
                value = .default_output_dir(object)
              ),

              shiny::p(
                paste0(
                  "Choose an existing directory. Reports and their R Markdown ",
                  "sources are saved at the paths below without changing ",
                  "the R working directory."
                )
              ),

              shiny::actionButton("browse_output_dir", "Browse for folder..."),
              shiny::helpText(
                "Browse opens a folder chooser on the Windows computer running R. ",
                "For remote or other-platform sessions, enter a path manually."
              ),
              shiny::verbatimTextOutput("output_paths"),

              shiny::checkboxInput(
                "overwrite_outputs",
                "Allow replacing existing reports, R Markdown or saved objects",
                value = FALSE
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
    session_temp_dir <- tempfile("camtrapReport-gui-")

    if (!dir.create(session_temp_dir, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create the temporary GUI directory.")
    }

    session$onSessionEnded(function() {
      unlink(session_temp_dir, recursive = TRUE, force = TRUE)
    })
    #---
    initial_tree <- .gui_section_tree(object)
    rv <- shiny::reactiveValues(
      cm = object, revision = 0L,
      section_tree = initial_tree, original_sections = initial_tree
    )

    touch_object <- function(cm) {
      rv$cm <- cm
      # camReport is a reference object: assignment alone need not invalidate UI.
      rv$revision <- shiny::isolate(rv$revision) + 1L
    }

    shiny::observeEvent(input$browse_output_dir, {
      tryCatch({
        current <- input$output_dir %||% ""
        chosen <- .choose_output_directory(if (dir.exists(current)) current else "")
        if (is.null(chosen) || length(chosen) == 0L ||
            (length(chosen) == 1L && (is.na(chosen) || !nzchar(chosen)))) return()
        chosen <- .output_directory(chosen)
        shiny::updateTextInput(session, "output_dir", value = chosen)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    cm_current <- shiny::reactive({
      rv$revision
      shiny::validate(
        shiny::need(!is.null(rv$cm), "Please load a Camtrap-DP zip file first.")
      )

      rv$cm
    })

    refresh_inputs <- function(cm) {
      inf <- .get_info(cm)

      if (!nzchar(trimws(shiny::isolate(input$output_dir) %||% ""))) {
        shiny::updateTextInput(
          session, "output_dir", value = .default_output_dir(cm)
        )
      }
      shiny::updateTextInput(session, "meta_title", value = inf$title %||% "")
      shiny::updateTextInput(
        session,
        "meta_subtitle",
        value = inf$subtitle %||% ""
      )
      shiny::updateTextInput(
        session,
        "meta_authors",
        value = inf$authors %||% ""
      )
      shiny::updateTextInput(
        session,
        "meta_institute",
        value = inf$institute %||% ""
      )
      shiny::updateTextInput(
        session,
        "meta_siteName",
        value = inf$siteName %||% ""
      )
      shiny::updateTextInput(
        session,
        "meta_logoPath",
        value = inf$logoPath %||% ""
      )

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

    session$onFlushed(
      function() {
        cm0 <- shiny::isolate(rv$cm)

        if (!is.null(cm0)) {
          refresh_inputs(cm0)
        }
      },
      once = TRUE
    )

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
        .select_gui_sections(cm, rv$section_tree, input$sections_keep)
      }

      if (isTRUE(run_setup)) {
        cm$setup()
      }

      touch_object(cm)

      invisible(cm)
    }

    shiny::observeEvent(input$load_cm, {
      zip_path <- NULL

      tryCatch(
        {
          zip_path <- .get_zip_path(
            upload = input$data_zip,
            path = input$data_zip_path,
            temp_dir = session_temp_dir
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
        shiny::need(
          !is.null(zip_path),
          "Please provide a Camtrap-DP zip file or local path."
        )
      )

      shiny::showModal(shiny::modalDialog(
        title = "Loading data",
        "Please wait while camtrapReport builds the camReport object...",
        footer = NULL
      ))

      on.exit(shiny::removeModal(), add = TRUE)

      cm <- tryCatch(
        {
          habitat <- .read_habitat(
            upload = input$habitat_csv,
            path = input$habitat_csv_path
          )
          study_area <- .copy_study_area_upload(
            input$study_area_files,
            session_temp_dir
          )
          overwrite_dataset <- !dir.exists(zip_path) && .isZip(zip_path) &&
            isTRUE(input$overwrite_dataset)
          if (!is.null(study_area)) {
            camData(
              data = zip_path,
              habitat = habitat,
              study_area = study_area,
              enrich_taxonomy = isTRUE(input$enrich_taxonomy),
              path = if (.isZip(zip_path)) .output_directory(input$output_dir) else NULL,
              overwrite = overwrite_dataset
            )
          } else {
            camData(
              data = zip_path,
              habitat = habitat,
              enrich_taxonomy = isTRUE(input$enrich_taxonomy),
              path = if (.isZip(zip_path)) .output_directory(input$output_dir) else NULL,
              overwrite = overwrite_dataset
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
        rv$section_tree <- .gui_section_tree(cm)
        rv$original_sections <- rv$section_tree
        touch_object(cm)
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
      rv$revision
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
          "Site: ",
          .safe(cm$siteName, "unknown"),
          shiny::br(),
          "Focus group: ",
          toString(.safe(cm$setting$focus_groups, "unknown")),
          shiny::br(),
          "Years: ",
          toString(.safe(cm$years, "not set")),
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
          touch_object(cm)

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
        return(shiny::p(
          "No recorded values are available for this field. Taxonomic class or ",
          "order may be absent from the source data. Supply source taxonomy or ",
          "explicitly opt in to enrichment when reimporting; this selector ",
          "does not query GBIF."
        ))
      }

      shiny::tagList(shiny::selectizeInput(
        "group_values",
        "Values included in this group",
        choices = vals,
        selected = character(),
        multiple = TRUE,
        options = list(
          placeholder = "Search and select one or more values", create = FALSE
        )
      ), shiny::helpText(
        "Values come from the loaded dataset. Missing or blank values are ",
        "omitted; absent taxonomy does not mean a species is absent. ",
        "No GBIF lookup is performed."
      ))
    })

    shiny::observeEvent(input$add_focus_group, {
      cm <- cm_current()

      nm <- trimws(input$new_group_name %||% "")
      field <- input$group_field %||% "scientificName"
      vals <- input$group_values %||% character()

      rule <- list()
      rule[[field]] <- vals

      tryCatch(
        {
          if (!nzchar(nm)) stop("Please provide a focus group name.")
          if (!field %in% c("class", "order", "scientificName", "observationType")) {
            stop("Choose a supported grouping field.")
          }
          if (!length(vals)) stop("Please select at least one value.")
          if (!all(vals %in% .unique_values(cm, field))) {
            stop("Select values from the current field and dataset.")
          }
          cm$add_group(name = nm, x = rule)
          # The existing setter validates against the computed group summary.
          cm$setup()
          cm$set_focus_group(x = nm)
          cm$setup()

          touch_object(cm)

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
      shiny::selectizeInput(
        "sections_keep", "Sections included in the ecological report",
        choices = .section_choices(rv$section_tree),
        selected = .reportSection_catalog(cm$reportObjects)$name,
        multiple = TRUE, options = list(plugins = list("remove_button"))
      )
    })

    output$section_edit_select_ui <- shiny::renderUI({
      cm_current()
      choices <- .section_choices(rv$section_tree)
      if (!length(choices)) return(shiny::p("No report sections are available."))
      selected <- shiny::isolate(input$section_to_edit)
      if (length(selected) != 1L || is.na(selected) || !selected %in% choices) {
        selected <- unname(choices[1])
      }
      shiny::selectInput("section_to_edit", "Section to edit",
                         choices = choices, selected = selected)
    })

    output$section_text_ui <- shiny::renderUI({
      shiny::req(input$section_to_edit)
      section <- tryCatch(.gui_section(rv$section_tree, input$section_to_edit),
                          error = function(e) NULL)
      if (is.null(section)) return(shiny::p(
        "This section cannot be identified uniquely. Use unique section names ",
        "with the existing report-section functions before editing."
      ))
      shiny::tagList(
        shiny::div(style = "display:none;",
                   shiny::textInput("section_editor_name", NULL, value = section@name)),
        shiny::textAreaInput("section_new_text", "Source text (Markdown)",
                             value = .collapse_section_text(section@txt), rows = 10, width = "100%")
      )
    })

    shiny::observeEvent(input$apply_sections, {
      cm <- cm_current()
      tryCatch({
        added <- .select_gui_sections(cm, rv$section_tree,
                                      input$sections_keep %||% character())
        touch_object(cm)
        shiny::showNotification(if (length(added)) {
          paste0("Section selection updated. Required parent section(s) added: ",
                 paste(added, collapse = ", "), ".")
        } else {
          "Section selection updated."
        }, type = "message")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    save_section_text <- function(reset = FALSE) {
      cm <- cm_current()
      name <- input$section_to_edit %||% ""
      if (!nzchar(name) || !identical(input$section_editor_name, name)) {
        stop("Wait for the selected section's text to load before saving.")
      }
      .gui_section(rv$section_tree, name) # Reject ambiguous targets before editing.
      if (reset) {
        original <- .gui_section(rv$original_sections, name)
        tree <- .updateReportSection_tree(rv$section_tree, name, function(sec) {
          sec@txt <- original@txt
          sec
        })
      } else {
        text <- input$section_new_text
        if (!is.character(text) || length(text) != 1L || is.na(text)) {
          stop("The section text has not loaded yet.")
        }
        tree <- .edit_gui_section(cm, rv$section_tree, name, text)
      }
      included <- .reportSection_catalog(cm$reportObjects)$name
      .select_gui_sections(cm, tree, included)
      rv$section_tree <- tree
      touch_object(cm)
      shiny::showNotification(if (reset) "Original section text restored." else
        "Section text saved in this session; include it when saving the object.",
        type = "message")
    }

    shiny::observeEvent(input$apply_section_text, {
      tryCatch(save_section_text(), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })
    shiny::observeEvent(input$reset_section_text, {
      tryCatch(save_section_text(reset = TRUE), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    })

    output$section_summary <- shiny::renderText({
      cm <- cm_current()
      paste(length(.reportSection_catalog(cm$reportObjects)$name),
            "sections included. Text edits do not change installed module files.")
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
        included_sections = .safe(.reportSection_catalog(cm$reportObjects)$name, NA),
        n_species = nrow(.species_table(cm))
      )
    })

    output$output_paths <- shiny::renderText({
      path <- trimws(input$output_dir %||% "")

      if (!nzchar(path)) {
        return("Select an output directory to preview report paths.")
      }

      out_dir <- tryCatch(
        .output_directory(path),
        error = function(e) NULL
      )

      if (is.null(out_dir)) {
        return("Please select a valid, writable output directory.")
      }

      paste(file.path(out_dir, c(
        "report.html",
        "report.Rmd",
        "data_status.html",
        "data_status.Rmd",
        "camtrapReport_gui_object.rds"
      )), collapse = "\n")
    })


    shiny::observeEvent(input$save_cm, {
      tryCatch({
        out_dir <- .output_directory(input$output_dir)
        out_file <- file.path(out_dir, "camtrapReport_gui_object.rds")
        if (file.exists(out_file) && !isTRUE(input$overwrite_outputs)) {
          stop("The saved object already exists. Select the overwrite ",
               "checkbox or choose another output directory.", call. = FALSE)
        }
        cm <- apply_gui_settings(run_setup = FALSE)
        .save_camreport_cache(cm, out_file,
                              overwrite = isTRUE(input$overwrite_outputs))
        shiny::showNotification(
          paste("Saved:", out_file), type = "message", duration = 8
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Could not save object:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    shiny::observeEvent(input$generate_status, {
      tryCatch(
        {
          out_dir <- .output_directory(input$output_dir)
          .check_gui_report_output(
            cm_current(), out_dir, "data_status",
            overwrite = isTRUE(input$overwrite_outputs)
          )
          cm <- apply_gui_settings(run_setup = TRUE)
          status(
            cm, filename = file.path(out_dir, "data_status.html"),
            view = TRUE, overwrite = isTRUE(input$overwrite_outputs)
          )

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
      tryCatch(
        {
          out_dir <- .output_directory(input$output_dir)
          .check_gui_report_output(
            cm_current(), out_dir, "report",
            overwrite = isTRUE(input$overwrite_outputs)
          )
          cm <- apply_gui_settings(run_setup = TRUE)
          result <- report(
            cm, filename = file.path(out_dir, "report.html"),
            view = TRUE, overwrite = isTRUE(input$overwrite_outputs)
          )
          if (inherits(result, "try-error")) {
            stop(as.character(result), call. = FALSE)
          }

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
