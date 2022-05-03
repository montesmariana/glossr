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
