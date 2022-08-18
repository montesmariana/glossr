#' Define a default value
#'
#' @param x Variable to define
#' @param default Default value
#' @noRd
#' @return New value
set_default <- function(x, default = "") {
  if (is.null(x) || is.na(x) || !is.character(x) || nchar(x) == 0) default else x
}

#' Validate output format
#'
#' @param output Character string with output format required.
#'
#' @noRd
#' @return Invisible, the output. It also sets it as the 'glossr.output' option.
validate_output <- function(output = c("word", "latex", "leipzig", "tooltip", "html", "pdf")) {
  output <- tryCatch(
    error = function(cnd) {
      cli::cli_warn(c("The output format {.var {output}} is not supported. ",
                              "Switching to default {.var latex}."))
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
#' @param format `i` for italics and `b` for bold
#'
#' @noRd
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
#' Calls [requireNamespace()] with the required packages.
#'
#' @noRd
#' @param output_format Word, Leipzig or Tooltip, desired format
check_packages <- function(output_format) {
  if (output_format == "word") {
    requireNamespace("officer", quietly = TRUE)
    requireNamespace("flextable", quietly = TRUE)
  } else if (output_format %in% c("leipzig", "tooltip")) {
    requireNamespace("htmltools", quietly = TRUE)
  }
}
