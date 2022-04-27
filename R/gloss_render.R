#' Render a gloss
#'
#' @param original First line for the interlinear gloss,
#'   i.e. example in original language.
#' @param parsed Second line for the interlinear gloss,
#'   i.e. glosses for individual words or expression.
#' @param translation Free translation (optional).
#' @param label Label for the example (optional).
#' @param df Dataframe with at least \code{original} and \code{parsed} columns
#'   to generate multiple glosses.
#'
#' @return Object of class \code{gloss}
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
#'
#' # Within R Markdown
#' as_gloss(ex_sp, ex_gloss, ex_trans, "ex1")
NULL


#' @describeIn gloss_render Render in PDF
#' @export
gloss_pdf <- function(original, parsed, translation = NULL, label = NULL) {
  label_part <- if (is.null(label)) "" else sprintf("\\label{%s}", label)
  trans_part <- if (is.null(translation)) "" else sprintf("        \\trans %s \n", translation)
  c(
    sprintf("    \\ex%s\n", label_part),
    sprintf("        \\gll %s \\\\ \n", original),
    sprintf("         %s \\\\ \n", parsed),
    trans_part
  )
}

#' @describeIn gloss_render Render in HTML
#' @export
gloss_html <- function(original, parsed, translation = NULL, label = NULL) {
  label_part <- if (is.null(label)) "(@nolabel) " else sprintf("(@%s) ", label)
  output <- getOption("glossr.output", "tooltip")
  func <- if (output == "leipzig") gloss_leipzig else gloss_tooltip
  c(label_part, func(original, parsed, translation))
}

#' @describeIn gloss_render Tooltip rendering for HTML
#' @export
gloss_tooltip <- function(original, parsed, translation = NULL) {
  trans_part <- if (is.null(translation)) "" else sprintf("\n    %s\n", ignore_latex(translation))
  words <- htmltools::span(
    htmltools::tagList(
      htmltools::span(" ", .noWS = "outside"),
      gloss_linetooltip(original, parsed)
    ))
  c(as.character(words), "\n", trans_part)
}

#' @describeIn gloss_render Leipzig.js engine
#' @export
gloss_leipzig <- function(original, parsed, translation = NULL) {
  is_first <- getOption("glossr.first_leipzig", TRUE)
  translation <- if (is.null(translation)) "" else translation
  g <- htmltools::div(
    htmltools::tagList(
      htmltools::p(ignore_latex(original)),
      htmltools::p(ignore_latex(parsed)),
      htmltools::p(ignore_latex(translation))
    ),
    `data-gloss` = "",
    .noWS = "outside"
  )
  if (is_first) {
    g <- htmltools::tagList(
      g,
      leipzig_script()
    )
    options("glossr.first_leipzig" = FALSE)
  }

  c(as.character(g), "\n")
}
#' @describeIn gloss_render Render based on R Markdown output
#'
#' @export
as_gloss <- function(original, parsed, translation = NULL, label = NULL) {
  if (knitr::is_latex_output()) {
    g <- gloss_pdf(original, parsed, translation, label)
  } else {
    g <- gloss_html(original, parsed, translation, label)
  }
  structure(g, class = "gloss")
}

#' @describeIn gloss_render Render glosses from a dataframe
#'
#' @export
gloss_df <- function(df) {
  stopifnot("original" %in% colnames(df))
  stopifnot("parsed" %in% colnames(df))
  wanted_columns <- c("original", "parsed", "translation", "label")
  present_columns <- wanted_columns[wanted_columns %in% colnames(df)]
  g <- unlist(purrr::pmap(df[,present_columns], as_gloss))
  structure(g, class = "gloss")
}
