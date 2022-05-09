#' Print method for glosses
#'
#' @param x Object to print
#' @param ... Other options for knit_print
#' @importFrom knitr knit_print
#' @exportS3Method knitr::knit_print gloss
#' @export
knit_print.gloss <- function(x, ...) {
  output <- getOption("glossr.output")
  validate_output(output)
  if (output == "latex") {
    latex_params = c(
      "exskip=0pt",
      "belowglpreambleskip=0pt",
      "aboveglftskip=0pt",
      paste0("everyglpreamble=", format_pdf("preamble")),
      paste0("everygla=", format_pdf("a")),
      paste0("everyglb=", format_pdf("b")),
      paste0("everyglc=", format_pdf("c")),
      paste0("everyglft=", format_pdf("translation"))
    )
    knitr::asis_output(
      c(
        sprintf("\\lingset{%s}", paste(latex_params, collapse = ",")),
        x),
      meta = list(rmarkdown::latex_dependency("expex")))
  } else if (output == "leipzig") {
    knitr::asis_output(x, meta = list(use_leipzig()))
  } else if (output == "word") {
    knitr::asis_output(x)
  } else {
    knitr::asis_output(x, meta = list(
      rmarkdown::html_dependency_jquery(),
      use_tooltip())
      )
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


