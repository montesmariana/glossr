#' glossr: Use interlinear glosses in R Markdown
#'
#' Read examples with interlinear glosses from files or from text
#' and print them in a way compatible with both Latex and HTML outputs.
#'
#' @keywords internal
"_PACKAGE"

#' Use glossr
#'
#' Call in a setup chunk.
# TODO add leipzig.js reference, gb4e reference.
#'
#' @param html_format Whether the html output should use leipzig.js or tooltips.
#'
#' @return Set glossr.output option
#' @export
use_glossr <- function(html_format = NULL) {
  html_formats <- c("leipzig", "tooltip")
  opt <- getOption("glossr.output")
  if (knitr::is_latex_output()) {
    output <- "latex"
  } else if (is.null(opt)) {
    output <- if (is.null(html_format)) "leipzig" else match.arg(html_format, html_formats)
  } else if (!is.null(html_format)) {
    output <- match.arg(html_format, html_formats)
  } else {
    output <- opt
  }
  if (!output %in% c("latex", html_formats)) {
    output <- "leipzig"
  }
  options("glossr.output" = output)
  options("glossr.first_leipzig" = TRUE)
  message(sprintf("Setting up the `%s` engine.", output))
}
