#' Define a default value
#'
#' @param x Variable to define
#' @param default Default value
#'
#' @return New value
set_default <- function(x, default = "") {
  if (is.null(x) || is.na(x) || !is.character(x) || nchar(x) == 0) default else x
}

#' Parse latex
#'
#' @param string Latex string to parse
#'
#' @return Character vector
#' @name parse_latex
NULL

#' Validate output format
#'
#' @param output Character string with output format required.
#'
#' @return Invisible, the output. It also sets it as the 'glossr.output' option.
#' @export
validate_output <- function(output = c("word", "latex", "leipzig", "tooltip", "html", "pdf")) {
  output <- tryCatch(
    error = function(cnd) {
      warning(
      "The output format '", output, "' is not supported.\
      Switching to default 'latex'.",
      call. = FALSE)
      output <- "latex"
      },
    match.arg(output)
    )
  if (output == "html") output <- "leipzig"
  if (output == "pdf") output <- "latex"
  options("glossr.output" = output)
  invisible(output)
}
