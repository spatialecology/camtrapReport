test_that("legacy stock sections refresh in pools and attached trees without changing profiles", {
  object <- camR$new()
  object$info$report_sections_revision <- NULL
  section <- function(directory, name) {
    .read_yml(system.file(directory, paste0(name, ".yml"), package = "camtrapReport"))
  }
  old <- section("compat/sections-v1", "richness")
  old_co <- section("compat/sections-v1", "co_occurrence")
  object$reportObjectElements$Modules <- structure(list(richness = old), provenance = "saved")
  object$reportObjects <- list(parent = list(item = old, empty = NULL))
  object$statusReportObjects <- list(item = old_co)
  profiles <- object$reportObjectElements$Profiles
  expect_message(.refresh_saved_sections(object), "Refreshed saved bundled")
  expect_identical(object$reportObjects$parent$item, section("reportSections", "richness"))
  expect_identical(object$statusReportObjects$item, section("reportSections", "co_occurrence"))
  expect_named(object$reportObjects$parent, c("item", "empty"))
  expect_identical(attr(object$reportObjectElements$Modules, "provenance"), "saved")
  expect_identical(object$reportObjectElements$Profiles, profiles)
  expect_silent(.refresh_saved_sections(object))
})

test_that("authored legacy sections retain their code and receive a review diagnostic", {
  object <- camR$new()
  object$info$report_sections_revision <- NULL
  custom <- .read_yml(system.file("compat/sections-v1", "richness.yml", package = "camtrapReport"))
  custom@Rchunk@code <- c(custom@Rchunk@code, "# an authored observation")
  object$reportObjects <- list(custom)
  expect_warning(.refresh_saved_sections(object), "Retained modified.*richness")
  expect_identical(object$reportObjects[[1L]], custom)
  expect_warning(.refresh_saved_sections(object), "before using its scientific output")
})

test_that("full-angle parameter tables require explicit units and agree with REM geometry", {
  skip_if_not_installed("camtrapDensity")
  p <- data.frame(estimate = c(0.6, 2, 0.005, 1), se = 0,
                  unit = c("n/day", "km/day", "km", "degree"),
                  row.names = c("trap_rate", "overall_speed", "radius", "angle"))
  expect_equal(.rem(p)["density", "estimate"], pi * 0.6 / (2 * 0.005 * (2 + pi / 180)))
  p["angle", "unit"] <- "radian"
  expect_equal(.rem(p)["density", "estimate"], pi * 0.6 / (2 * 0.005 * 3))
  p["angle", "unit"] <- NA_character_
  expect_error(.rem(p), class = "rem_angle_unit_error")
})

test_that("direct render preparation retains an authored EOW layout", {
  object <- camR$new()
  object$info$report_sections_revision <- NULL
  object$info$rem_analysis_signature <- "rem-v2-annual-scope-individual-count"
  bundled <- .read_yml(system.file("reportSections", "model_parameters_EOW.yml",
                                    package = "camtrapReport"))
  old <- .read_yml(system.file("compat/sections-v1", "richness.yml", package = "camtrapReport"))
  layout <- list(manual = bundled, reserved = NULL)
  object$reportObjects <- list(annual = layout, richness = old)
  expect_message(.make_render_env(object), "Refreshed saved bundled")
  expect_identical(object$reportObjects$annual, layout)
  expect_identical(object$reportObjects$richness,
                   .read_yml(system.file("reportSections", "richness.yml", package = "camtrapReport")))
})
