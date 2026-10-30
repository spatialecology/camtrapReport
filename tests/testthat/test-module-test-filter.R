test_that("module attachment preserves original test-result indices", {
  for (kind in c("report", "status")) {
    for (tested in list(c(NA, TRUE, FALSE), c(FALSE, NA, TRUE),
                        c(FALSE, FALSE, FALSE), c(NA, NA, NA))) {
      cm <- new.env(parent = emptyenv())
      module_names <- c("a", "b", "c")
      modules <- as.list(stats::setNames(module_names, module_names))
      metadata <- data.frame(name = module_names, tested = tested)
      cm$reportObjectElements <- list(
        Modules = modules, Modules_info = metadata,
        Status_modules = modules, Status_modules_info = metadata
      )
      cm$addReportObject <- function(x) {
        cm$reportObjects[[x]] <- x
      }
      cm$addStatusReportObject <- function(x) {
        cm$statusReportObjects[[x]] <- x
      }
      attach_modules <- if (kind == "report") {
        .attach_modules
      } else {
        .attach_status_modules
      }
      testthat::with_mocked_bindings(
        attach_modules(cm, module_names),
        .get_module_names = function() module_names,
        .check_parent = function(n) NULL,
        .package = "camtrapReport"
      )
      attached <- if (kind == "report") cm$reportObjects else cm$statusReportObjects
      expect_identical(
        unname(unlist(attached)),
        if (all(!is.na(tested) & !tested)) NULL else
          module_names[is.na(tested) | tested],
        info = paste(kind, paste(tested, collapse = ","))
      )
    }
  }
})
