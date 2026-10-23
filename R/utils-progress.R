# Console-only report progress; these messages are not part of report content.
.report_stage_start <- function(label) {
  stage <- new.env(parent = emptyenv())
  stage$label <- label
  stage$started <- proc.time()
  stage$finished <- FALSE
  message("[camtrapReport] ", label, " started.")
  stage
}

.report_stage_end <- function(stage, complete = TRUE) {
  if (is.null(stage) || isTRUE(stage$finished)) return(invisible(NULL))
  elapsed <- proc.time() - stage$started
  stage$finished <- TRUE
  message(
    "[camtrapReport] ", stage$label,
    if (isTRUE(complete)) " completed" else " stopped",
    " after ", sprintf("%.1f", elapsed[["elapsed"]]),
    " s (R CPU ",
    sprintf("%.1f", sum(elapsed[c("user.self", "sys.self")])), " s)."
  )
  invisible(NULL)
}

.with_report_stage <- function(label, expr) {
  stage <- .report_stage_start(label)
  complete <- FALSE
  on.exit(.report_stage_end(stage, complete), add = TRUE)
  result <- withVisible(force(expr))
  complete <- TRUE
  if (result$visible) result$value else invisible(result$value)
}

.render_report_with_progress <- function(input, output_file, envir) {
  saved_chunks <- knitr::opts_chunk$get()
  saved_options <- knitr::opts_hooks$get()
  saved_hooks <- knitr::knit_hooks$get()
  state <- new.env(parent = emptyenv())
  state$installed <- FALSE
  state$chunk <- NULL
  state$stage <- .report_stage_start("Knitr module execution")
  complete <- FALSE

  on.exit({
    knitr::opts_chunk$restore(saved_chunks)
    knitr::opts_hooks$restore(saved_options)
    knitr::knit_hooks$restore(saved_hooks)
    .report_stage_end(state$chunk, complete = FALSE)
    .report_stage_end(state$stage, complete)
  }, add = TRUE)

  # rmarkdown installs its output hooks after render() starts. Capture those
  # hooks at the first chunk, then return their output without modification.
  previous_label_hook <- saved_options$label
  knitr::opts_hooks$set(label = function(options) {
    if (is.function(previous_label_hook)) {
      options <- previous_label_hook(options)
    }
    if (!state$installed) {
      chunk_hook <- knitr::knit_hooks$get("chunk")
      document_hook <- knitr::knit_hooks$get("document")
      knitr::knit_hooks$set(
        chunk = function(x, options) {
          out <- chunk_hook(x, options)
          .report_stage_end(state$chunk)
          out
        },
        document = function(x) {
          out <- document_hook(x)
          .report_stage_end(state$chunk)
          .report_stage_end(state$stage)
          state$stage <- .report_stage_start("HTML rendering/Pandoc")
          out
        }
      )
      state$installed <- TRUE
    }
    .report_stage_end(state$chunk)
    state$chunk <- .report_stage_start(paste0("Module/chunk ", options$label))
    options
  })

  out <- rmarkdown::render(
    input = input,
    output_file = output_file,
    envir = envir,
    quiet = TRUE
  )
  complete <- TRUE
  out
}
