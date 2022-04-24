#' Render a gloss
#'
#' @param original First line for the interlinear gloss,
#'   i.e. example in original language.
#' @param parsed Second line for the interlinear gloss,
#'   i.e. glosses for individual words or expression.
#' @param translation Free translation (optional).
#' @param label Label for the example (optional).
#'
#' @return Character vector to be printed.
#' @name gloss_render
#'
#' @encoding UTF-8
#' @examples
#' ex_sp <- "Un ejemplo en español"
#' ex_gloss <- "DET.M.SG example in Spanish"
#' ex_trans <- "An example in Spanish"
#' gloss_pdf(ex_sp, ex_gloss, ex_trans, "ex1")
#'
#' gloss_html(ex_sp, ex_gloss, ex_trans, "ex1")
NULL


#' @describeIn gloss_render Render in PDF
#' @export
gloss_pdf <- function(original, parsed, translation = NULL, label = NULL) {
  label_part <- if (is.null(label)) "" else sprintf("\\label{%s}", label)
  trans_part <- if (is.null(translation)) "" else sprintf("        \\trans %s \n", translation)
  c(
    sprintf("    \\ex%s\n", label_part),
    sprintf("        \\gll %s \\\\ \n", original),
    sprintf("        %s \\\\ \n", parsed),
    trans_part
  )
}

#' @describeIn gloss_render Render in HTML
#' @export
gloss_html <- function(original, parsed, translation = NULL, label = NULL) {
  label_part <- if (is.null(label)) "(@nolabel) " else sprintf("(@%s) ", label)
  trans_part <- if (is.null(translation)) "" else sprintf("\n    %s\n", ignore_latex(translation))
  words <- htmltools::span(
    htmltools::tagList(
      htmltools::span(" ", .noWS = "outside"),
      gloss_linetooltip(original, parsed)
      ))
  c(
    label_part,
    as.character(words),
    "\n",
    trans_part
  )
}
