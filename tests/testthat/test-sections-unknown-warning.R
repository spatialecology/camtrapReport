test_that("partial selections warn with every unknown name and retain order", {
  for (kind in c("report", "status")) {
    cm <- camtrap_test_report()$copy(shallow = FALSE)
    available <- sections(cm, report_type = kind)
    keep <- available[seq_len(min(2L, length(available)))]
    requested <- c(keep[1], "missing_alpha", keep[-1], "missing_beta")
    result <- capture_report_conditions(sections(cm, requested, report_type = kind))
    expect_identical(result$value, cm)
    expect_length(result$warnings, 1L)
    expect_match(result$warnings, "Unknown sections ignored:", fixed = TRUE)
    expect_match(result$warnings, "missing_alpha", fixed = TRUE)
    expect_match(result$warnings, "missing_beta", fixed = TRUE)
    field <- if (identical(kind, "report")) "reportObjects" else "statusReportObjects"
    attached <- .flatten_attached_sections(cm[[field]])
    expect_identical(unname(vapply(attached, slot, character(1), "name")), keep)
  }
})

test_that("valid selections stay quiet and wholly unknown selections still fail", {
  cm <- camtrap_test_report()$copy(shallow = FALSE)
  valid <- sections(cm)[1]
  result <- capture_report_conditions(sections(cm, valid))
  expect_length(result$warnings, 0L)
  before <- cm$reportObjects
  expect_error(sections(cm, "missing_alpha"), "None", fixed = TRUE)
  expect_identical(cm$reportObjects, before)
})
