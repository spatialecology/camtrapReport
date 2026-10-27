# Opt in only for explicitly math-free render fixtures. Production report
# templates and normal rmarkdown defaults are not modified by this helper.
math_free_html_fixture_renderer <- function(render) {
  # render() looks up its namespace metadata via environment(render), so the
  # forwarding binding must retain that environment rather than a helper frame.
  wrapper <- substitute(function(...) {
    original_render <- ORIGINAL_RENDER
    call <- match.call(definition = original_render, call = sys.call())
    call[[1L]] <- original_render
    output_options <- if ("output_options" %in% names(call)) {
      eval(call$output_options, envir = parent.frame())
    } else {
      list()
    }
    output_options["mathjax"] <- list(NULL)
    call$output_options <- output_options
    eval(call, envir = parent.frame())
  }, list(ORIGINAL_RENDER = render))
  eval(wrapper, envir = environment(render))
}

local_math_free_html_fixture <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    render = math_free_html_fixture_renderer(rmarkdown::render),
    .package = "rmarkdown",
    .env = .env
  )
}
