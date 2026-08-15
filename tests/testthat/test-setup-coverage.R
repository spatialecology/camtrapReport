run_setup_coverage <- function(cm, tz = NULL) {
  original_require <- ct_internal(".require")

  fake_summary <- function(...) {
    list(
      site_list = data.frame(
        scientificName = character()
      )
    )
  }

  suppressWarnings(
    testthat::with_mocked_bindings(
      cm$setup(tz = tz),
      .summarize_species = fake_summary,
      .require = function(x) {
        package <- as.character(x)[1]

        if (
          !is.na(package) &&
            identical(package, "suncalc")
        ) {
          return(FALSE)
        }

        original_require(x)
      },
      .package = "camtrapReport"
    )
  )

  invisible(cm)
}


test_that(
  "setup handles supported class and order group definitions",
  {
    cm <- camtrap_test_report()$copy(
      shallow = FALSE
    )

    data <- cm$data

    data$taxonomy$class <- rep(
      "TestClass",
      nrow(data$taxonomy)
    )

    data$taxonomy$order <- rep(
      "TestOrder",
      nrow(data$taxonomy)
    )

    cm$data <- data

    custom_groups <- list(
      class_order_dom_obs = list(
        class = "TestClass",
        order = "TestOrder",
        domestic = FALSE,
        observationType = "animal"
      ),
      class_order_dom = list(
        class = "TestClass",
        order = "TestOrder",
        domestic = FALSE
      ),
      class_order_obs = list(
        class = "TestClass",
        order = "TestOrder",
        observationType = "animal"
      ),
      class_order_plain = list(
        class = "TestClass",
        order = "TestOrder"
      ),
      class_dom_obs = list(
        class = "TestClass",
        domestic = FALSE,
        observationType = "animal"
      ),
      class_dom = list(
        class = "TestClass",
        domestic = FALSE
      ),
      class_obs = list(
        class = "TestClass",
        observationType = "animal"
      ),
      class_plain = list(
        class = "TestClass"
      ),
      order_dom_obs = list(
        order = "TestOrder",
        domestic = FALSE,
        observationType = "animal"
      ),
      order_dom = list(
        order = "TestOrder",
        domestic = FALSE
      ),
      order_obs = list(
        order = "TestOrder",
        observationType = "animal"
      ),
      order_plain = list(
        order = "TestOrder"
      )
    )

    cm$group_definition <- custom_groups

    setting <- cm$setting
    setting$focus_groups <- NULL
    cm$setting <- setting

    cm$filterDuration <- 0

    run_setup_coverage(
      cm,
      tz = "UTC"
    )

    expect_identical(
      cm$setting$tz,
      "UTC"
    )

    expected <- names(custom_groups)

    expect_true(
      all(
        expected %in%
          cm$species_summary$count$Group
      )
    )

    counts <- cm$species_summary$count$Count[
      match(
        expected,
        cm$species_summary$count$Group
      )
    ]

    expect_true(
      all(counts == 0)
    )
  }
)


test_that(
  "setup creates standard taxonomic groups when taxa support them",
  {
    cm <- camtrap_test_report()$copy(
      shallow = FALSE
    )

    data <- cm$data

    if (nrow(data$taxonomy) < 5) {
      skip(
        "Bundled taxonomy has fewer than five taxa"
      )
    }

    data$taxonomy$class[] <- "OtherClass"
    data$taxonomy$order[] <- "OtherOrder"

    data$taxonomy$class[1] <- "Aves"
    data$taxonomy$class[2] <- "Reptilia"
    data$taxonomy$class[3] <- "Amphibia"

    data$taxonomy$class[4] <- "Mammalia"
    data$taxonomy$order[4] <- "Carnivora"
    data$taxonomy$scientificName[4] <-
      "Testus mammalus"

    data$taxonomy$class[5] <- "Mammalia"
    data$taxonomy$scientificName[5] <-
      "Homo sapiens"

    cm$data <- data

    cm$group_definition <- list()

    setting <- cm$setting
    setting$focus_groups <- "domestic"
    cm$setting <- setting

    cm$.any_data_for_rem <- c(
      dummy = FALSE
    )

    run_setup_coverage(cm)

    expected <- c(
      "large_mammals",
      "domestic",
      "wild_animals",
      "birds",
      "reptiles",
      "amphibians",
      "wild_mammals",
      "human_observation"
    )

    expect_true(
      all(
        expected %in%
          names(cm$group_definition)
      )
    )

    expect_identical(
      cm$group_definition$birds$class,
      "Aves"
    )

    expect_identical(
      cm$group_definition$reptiles$class,
      "Reptilia"
    )

    expect_identical(
      cm$group_definition$amphibians$class,
      "Amphibia"
    )

    expect_true(
      "Testus mammalus" %in%
        cm$group_definition$wild_mammals$
          scientificName
    )

    expect_identical(
      cm$group_definition$human_observation$
        scientificName,
      "Homo sapiens"
    )
  }
)


test_that(
  "setup joins habitat data and summarizes captures by habitat",
  {
    cm <- camtrap_test_report()$copy(
      shallow = FALSE
    )

    data <- cm$data

    data$taxonomy$class[] <- "TestClass"
    data$taxonomy$order[] <- "TestOrder"

    habitat_columns <- tolower(
      names(data$locations)
    ) %in% c(
      "habitat",
      "habitat_type"
    )

    data$locations <- data$locations[
      ,
      !habitat_columns,
      drop = FALSE
    ]

    if (is.null(data$settings)) {
      data$settings <- list()
    }

    data$settings$tz <- "Europe/Amsterdam"

    cm$data <- data

    location_names <- unique(
      as.character(
        data$locations$locationName
      )
    )

    location_names <- location_names[
      !is.na(location_names) &
        nzchar(location_names)
    ]

    cm$habitat <- data.frame(
      locationName = location_names,
      habitat = rep(
        "forest",
        length(location_names)
      ),
      stringsAsFactors = FALSE
    )

    cm$group_definition <- list(
      test_group = list(
        scientificName =
          data$taxonomy$scientificName[1]
      )
    )

    setting <- cm$setting
    setting$tz <- NULL
    setting$focus_groups <- NULL
    cm$setting <- setting

    cm$species_summary_by_habitat <-
      data.frame()

    run_setup_coverage(cm)

    expect_identical(
      cm$setting$tz,
      "Europe/Amsterdam"
    )

    expect_true(
      "Habitat_Type" %in%
        names(cm$data$locations)
    )

    expect_gt(
      nrow(cm$species_summary_by_habitat),
      0
    )

    habitats <- unique(
      stats::na.omit(
        cm$species_summary_by_habitat$
          Habitat_Type
      )
    )

    expect_identical(
      habitats,
      "forest"
    )
  }
)
