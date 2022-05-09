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
#' @param is_cell For word output, whether to style raw text or a cell
#'
#' @return Character vector
#' @name parse_latex
NULL

#' Validate output format
#'
#' @param output Character string with output format required.
#'
#' @return Invisible, the output. It also sets it as the 'glossr.output' option.
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

#' List of styling options
#'
#' @param format \code{i} for italics and \code{b} for bold
#'
#' @return Character vector with the ways that a certain format (italics or bold) can be specified.
style_options <- function(format = c("i", "b")) {
  switch(
    format,
    i = c("i", "it", "italics", "textit"),
    b = c("b", "bf", "bold", "textbf")
  )
}

#' Check if required packages are installed
#'
#' Calls \code{\link{requireNamespace}} with the required packages.
#'
#' @param output_format Word, Leipzig or Tooltip, desired format
check_packages <- function(output_format) {
  if (output_format == "word") {
    requireNamespace("officer", quietly = TRUE)
    requireNamespace("flextable", quietly = TRUE)
  } else if (output_format %in% c("leipzig", "tooltip")) {
    requireNamespace("htmltools", quietly = TRUE)
  }
}
