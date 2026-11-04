test_that("GUI section editing preserves nested and generated content", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  intro <- reportSection("custom_intro", title = "Custom introduction",
                         txt = list("Original {siteName}", "Second paragraph"))
  child <- reportSection("custom_child", parent = "custom_intro", txt = "Child")
  child@Rchunk <- .make_rchunk_from_text(code = "stop('Do not evaluate in GUI')")
  other <- reportSection("custom_other", txt = NULL)
  original <- list(nested = list(intro = intro, child = child), other = other)
  cm$reportObjects <- original
  registry <- cm$reportObjectElements
  module_files <- list.files(system.file("reportSections", package = "camtrapReport"),
                             recursive = TRUE, full.names = TRUE)
  hashes <- tools::md5sum(module_files)
  app <- .camtrapReport_gui_app(cm)
  out <- tempfile("gui-sections-")
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(section_to_edit = "custom_intro",
                      section_editor_name = "custom_intro")
    expect_match(output$section_text_ui$html, "Original {siteName}", fixed = TRUE)
    expect_match(output$section_text_ui$html, "Second paragraph", fixed = TRUE)
    expect_match(output$sections_ui$html, "Custom introduction", fixed = TRUE)
    expect_match(output$section_summary, "3 sections included", fixed = TRUE)

    session$setInputs(section_new_text = "Edited {siteName}", apply_section_text = 1)
    expect_identical(cm$reportObjects$nested$intro@txt, "Edited {siteName}")
    expect_identical(cm$reportObjects$nested$child, child)
    expect_match(output$section_text_ui$html, "Edited {siteName}", fixed = TRUE)
    expect_identical(cm$reportObjectElements, registry)

    # Choosing a child without its parent includes the required parent.
    session$setInputs(sections_keep = c("custom_child", "custom_other"),
                      apply_sections = 1)
    expect_identical(.reportSection_catalog(cm$reportObjects)$name,
                     c("custom_intro", "custom_child", "custom_other"))
    expect_match(output$section_summary, "3 sections included", fixed = TRUE)
    # Removing both parent and child excludes them without losing edits.
    session$setInputs(sections_keep = "custom_other", apply_sections = 2)
    expect_identical(.reportSection_catalog(cm$reportObjects)$name, "custom_other")
    expect_match(output$section_summary, "1 sections included", fixed = TRUE)
    session$setInputs(section_new_text = "Edited while excluded", apply_section_text = 2)
    session$setInputs(sections_keep = c("custom_intro", "custom_child", "custom_other"),
                      apply_sections = 3)
    expect_identical(cm$reportObjects$nested$intro@txt, "Edited while excluded")
    expect_identical(cm$reportObjects$nested$child, child)
    expect_identical(cm$reportObjects$other, other)

    # Saving settings must not reconstruct sections from the installed registry.
    session$setInputs(output_dir = out, save_cm = 1)
    saved <- readRDS(file.path(out, "camtrapReport_gui_object.rds"))
    expect_identical(saved$reportObjects, cm$reportObjects)
    expect_identical(saved$reportObjects$nested$intro@txt, "Edited while excluded")
    session$setInputs(reset_section_text = 1)
    expect_identical(cm$reportObjects$nested$intro, intro)

    # A delayed textarea from the previous section cannot overwrite this one.
    session$setInputs(section_to_edit = "custom_child", section_new_text = "stale",
                      apply_section_text = 3)
    expect_identical(cm$reportObjects$nested$child, child)
    session$setInputs(section_editor_name = "custom_child", section_new_text = "",
                      apply_section_text = 4)
    expect_identical(cm$reportObjects$nested$child@txt, "")
    expect_identical(cm$reportObjects$nested$child@Rchunk, child@Rchunk)
    session$setInputs(reset_section_text = 2)
    expect_identical(cm$reportObjects$nested$child, child)
    before <- cm$reportObjects
    session$setInputs(sections_keep = character(), apply_sections = 4)
    expect_identical(cm$reportObjects, before)
  })
  expect_identical(tools::md5sum(module_files), hashes)
})

test_that("GUI reset uses the loaded object and ambiguous sections are protected", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  section <- reportSection("custom", txt = "Loaded original")
  cm$reportObjects <- list(section)
  app <- .camtrapReport_gui_app(cm)
  root <- tempfile("gui-load-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  other <- cm$copy(shallow = FALSE)
  other$reportObjects <- list(reportSection("custom", txt = "New dataset original"))
  local_mocked_bindings(camData = function(...) other, .package = "camtrapReport")
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(section_to_edit = "custom", section_editor_name = "custom",
                      section_new_text = "Edited", apply_section_text = 1)
    expect_identical(cm$reportObjects[[1]]@txt, "Edited")
    session$setInputs(data_zip_path = root, habitat_csv_path = "", load_cm = 1)
    expect_identical(rv$cm, other)
    expect_match(output$section_text_ui$html, "New dataset original", fixed = TRUE)
    session$setInputs(section_new_text = "Edited again", apply_section_text = 2)
    session$setInputs(reset_section_text = 1)
    expect_identical(other$reportObjects[[1]]@txt, "New dataset original")
  })
  cm$reportObjects <- list(a = section, b = list(section))
  app <- .camtrapReport_gui_app(cm)
  shiny::testServer(app$serverFuncSource(), {
    before <- cm$reportObjects
    session$setInputs(section_to_edit = "custom", section_editor_name = "custom",
                      section_new_text = "Ambiguous", apply_section_text = 1)
    expect_identical(cm$reportObjects, before)
    expect_match(output$section_text_ui$html, "cannot be identified uniquely", fixed = TRUE)
  })
})

test_that("GUI adds available modules under their parents using existing attachment", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  parent <- reportSection("custom_parent", txt = "Previously edited parent")
  child <- reportSection("custom_child", parent = "custom_parent",
                         title = "Available child", txt = "Available child")
  tail <- reportSection("custom_tail", txt = "Last section")
  cm$reportObjects <- list(custom_parent = parent, custom_tail = tail)
  cm$reportObjectElements$Modules <- list(custom_parent = parent, custom_child = child)
  cm$reportObjectElements$Modules_info <- data.frame(
    name = c("custom_parent", "custom_child"), tested = NA)
  original <- cm$reportObjects
  app <- .camtrapReport_gui_app(cm)
  shiny::testServer(app$serverFuncSource(), {
    expect_identical(cm$reportObjects, original)
    expect_match(output$sections_ui$html, "Available child", fixed = TRUE)
    session$setInputs(sections_keep = c("custom_parent", "custom_child", "custom_tail"),
                      apply_sections = 1)
    expect_identical(.reportSection_catalog(cm$reportObjects)$name,
                     c("custom_parent", "custom_child", "custom_tail"))
    expect_identical(cm$reportObjects$custom_parent$custom_parent, parent)
    expect_identical(cm$reportObjects$custom_parent$custom_child@headLevel, 2)
    expect_identical(cm$reportObjects$custom_tail, tail)
    session$setInputs(section_to_edit = "custom_child", section_editor_name = "custom_child",
                      section_new_text = "Changed child", apply_section_text = 1)
    expect_identical(cm$reportObjects$custom_parent$custom_child@txt, "Changed child")
  })
})

test_that("GUI folder browsing validates paths without writing or changing wd", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  app <- .camtrapReport_gui_app(cm)
  env <- environment(app$serverFuncSource())
  root <- tempfile("gui-browse-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  root <- normalizePath(root, winslash = "/")
  file <- file.path(root, "sentinel.txt")
  writeLines("unchanged", file)
  hashes <- tools::md5sum(file)
  old_wd <- getwd()
  state <- new.env(parent = emptyenv())
  state$chosen <- root
  state$updates <- character()
  state$errors <- character()
  assign(".choose_output_directory", function(default) {
    state$default <- default
    state$chosen
  }, envir = env)
  local_mocked_bindings(showNotification = function(ui, ...) {
    state$errors <- c(state$errors, ui)
  }, .package = "shiny")
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    session$sendInputMessage <- function(inputId, message) {
      if (identical(inputId, "output_dir")) state$updates <- c(state$updates, message$value)
    }
    session$setInputs(output_dir = root, browse_output_dir = 1)
    expect_identical(state$updates, root)
    expect_identical(state$default, root)
    state$chosen <- NA_character_
    session$setInputs(browse_output_dir = 2)
    expect_identical(state$updates, root)
    state$chosen <- file.path(root, "missing")
    session$setInputs(browse_output_dir = 3)
    state$chosen <- file
    session$setInputs(browse_output_dir = 4)
    state$chosen <- .libPaths()[1]
    session$setInputs(browse_output_dir = 5)
    expect_identical(state$updates, root)
    expect_true(any(grepl("existing output directory", state$errors, fixed = TRUE)))
    expect_true(any(grepl("R library", state$errors, fixed = TRUE)))
    expect_identical(getwd(), old_wd)
    expect_identical(list.files(root), "sentinel.txt")
    expect_identical(tools::md5sum(file), hashes)
  })
  # Test the same guard used by manual entry with deterministic permission failure.
  local_mocked_bindings(file.access = function(...) -1L, .package = "base")
  expect_error(get(".output_directory", env)(root), "not writable")
})

test_that("GUI browsing uses the native Windows chooser only on Windows", {
  app <- .camtrapReport_gui_app(NULL)
  choose <- get(".choose_output_directory", environment(app$serverFuncSource()))
  if (.Platform$OS.type == "windows") {
    local_mocked_bindings(choose.dir = function(default, caption) {
      expect_identical(default, "chosen-default")
      expect_identical(caption, "Choose report output directory")
      NA_character_
    }, .package = "utils")
    expect_identical(choose("chosen-default"), NA_character_)
  } else {
    expect_error(choose(), "Enter an existing path manually")
  }
})

test_that("GUI group values are searchable data values and use existing group methods", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  cm$data$observations <- data.frame(
    scientificName = c("Species one", "Species two", " Species three ", NA, ""),
    observationType = c("animal", "human", "animal", "", NA))
  cm$data$taxonomy <- data.frame(class = c("Mammalia", "Aves", NA, " "),
                                 order = c("Carnivora", "Passeriformes", NA, ""))
  cm$data_merged <- data.frame()
  state <- new.env(parent = emptyenv())
  state$setups <- list()
  assign("setup", function(...) {
    state$setups[[length(state$setups) + 1L]] <- cm$setting$focus_groups
    cm$species_summary <- list(count = data.frame(Group = names(cm$group_definition)))
  }, envir = cm)
  app <- .camtrapReport_gui_app(cm)
  values <- get(".unique_values", environment(app$serverFuncSource()))
  expect_setequal(values(cm, "scientificName"),
                  c("Species one", "Species two", " Species three "))
  expect_setequal(values(cm, "class"), c("Mammalia", "Aves"))
  expect_setequal(values(cm, "order"), c("Carnivora", "Passeriformes"))
  expect_setequal(values(cm, "observationType"), c("animal", "human"))
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(group_field = "class")
    expect_match(output$group_values_ui$html, "Mammalia", fixed = TRUE)
    expect_match(output$group_values_ui$html, "multiple", fixed = TRUE)
    expect_match(output$group_values_ui$html, "Search and select", fixed = TRUE)
    session$setInputs(new_group_name = "GUI selected", group_values = c("Mammalia", "Aves"),
                      add_focus_group = 1)
    expect_identical(cm$get_group("GUI selected"), list(class = c("Mammalia", "Aves")))
    expect_identical(cm$setting$focus_groups, "GUI selected")
    expect_length(state$setups, 2L)
    expect_identical(state$setups[[2]], "GUI selected")
    expect_match(output$existing_group_ui$html, 'selected>GUI selected', fixed = TRUE)
    expect_match(output$object_status$html, "GUI selected", fixed = TRUE)
    session$setInputs(group_field = "scientificName")
    expect_match(output$group_values_ui$html, "Species one", fixed = TRUE)
    expect_false(grepl("Mammalia", output$group_values_ui$html, fixed = TRUE))
    # Values left over from the old field must not create an invalid rule.
    session$setInputs(new_group_name = "Stale", add_focus_group = 2)
    expect_null(cm$get_group("Stale"))
    session$setInputs(group_values = c("Species one", " Species three "),
                      new_group_name = "Exact values", add_focus_group = 3)
    expect_identical(cm$get_group("Exact values"),
                     list(scientificName = c("Species one", " Species three ")))
    cm$data$taxonomy <- data.frame()
    touch_object(cm)
    session$setInputs(group_field = "order")
    expect_match(output$group_values_ui$html, "No recorded values", fixed = TRUE)
    expect_match(output$group_values_ui$html, "does not query GBIF", fixed = TRUE)
  })
})

test_that("GUI includes methods automatically for selected location and study_area", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  intro <- reportSection("introduction", txt = "Introduction")
  methods <- reportSection("methods", txt = "Methods")
  location <- reportSection("location", parent = "methods", txt = "Locations")
  study_area <- reportSection("study_area", parent = "methods", txt = "Study area")
  cm$reportObjects <- list(intro = intro, methods = list(
    methods = methods, location = location, study_area = study_area))
  app <- .camtrapReport_gui_app(cm)
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(sections_keep = c("introduction", "location", "study_area"),
                      apply_sections = 1)
    expect_identical(.reportSection_catalog(cm$reportObjects)$name,
                     c("introduction", "methods", "location", "study_area"))
    expect_match(output$section_summary, "4 sections included", fixed = TRUE)
    # The UI should redisplay every effective selection, including its parent.
    expect_match(output$sections_ui$html, '<option value="study_area" selected>', fixed = TRUE)
    # Explicit removal remains possible: exclude the children and parent.
    session$setInputs(sections_keep = "introduction", apply_sections = 2)
    expect_identical(.reportSection_catalog(cm$reportObjects)$name, "introduction")
    # All original source objects must remain available in the GUI tree.
    expect_true(all(c("introduction", "methods", "location", "study_area") %in%
                      .reportSection_catalog(rv$section_tree)$name))
  })
})
