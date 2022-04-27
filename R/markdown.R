#' Print method for glosses
#'
#' @param x Object to print
#' @param ... Other options for knit_print
#' @importFrom knitr knit_print
#' @exportS3Method knitr::knit_print gloss
#' @export
knit_print.gloss <- function(x, ...) {
  output <- getOption("glossr.output")
  if (output == "latex") {
    x <- c(
      "\\begin{exe}\n",
      x,
      "\\end{exe}\n"
    )
    knitr::asis_output(x, meta = list(rmarkdown::latex_dependency("gb4e", extra_lines = "\\noautomath")))
  } else if (output == "leipzig") {
    knitr::asis_output(x, meta = list(use_leipzig()))
  } else {
    knitr::asis_output(x, meta = list(rmarkdown::html_dependency_jquery(), use_tooltip()))
  }
}

#' Reference gloss
#'
#' Latex output uses \code{\@ref(label)} to reference examples,
#'   whereas HTML output is based on pandocs examples, i.e. \code{(@label)}.
#'   \code{`r gloss(label)`}, written inline in the text, will return the
#'   appropriate reference based on the selected output.
#'
#' @param label Label for reference
#'
#' @return Character string with label reference
#' @export
gloss <- function(label) {
  output <- getOption("glossr.output", "latex")
  if (output == "latex") {
    sprintf("(\\@ref(%s))", label)
  } else {
    sprintf("(@%s)", label)
  }
}


