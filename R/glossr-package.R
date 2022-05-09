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
#' @inheritParams set_style_options
#'
#' @return Set options
#' @export
use_glossr <- function(
    html_format = NULL,
    styling = list()) {
  html_formats <- c("leipzig", "tooltip")
  opt <- getOption("glossr.output")
  if (knitr::is_latex_output()) {
    output <- "latex"
  } else if (!knitr::is_html_output()) {
    output <- "word"
  } else if (is.null(opt)) {
    output <- if (is.null(html_format)) "leipzig" else match.arg(html_format, html_formats)
  } else if (!is.null(html_format)) {
    output <- match.arg(html_format, html_formats)
  } else {
    output <- opt
  }
  if (!output %in% c("latex", "word", html_formats)) {
    output <- "leipzig"
  }
  check_packages(output)
  options("glossr.output" = output)
  options("glossr.first_leipzig" = TRUE)
  set_style_options(styling = styling)
  message(sprintf("Setting up the `%s` engine.", output))
}


#' Set general styling options
#'
#' This is a helper function to set \code{\link{options}} that control style characteristics
#' for glosses across the full document. It is called within \code{\link{use_glossr}}
#' but can be overridden later but setting the appropriate options.
#'
#' There are two types of settings that can be provided in the list.
#' First, \code{trans_quotes} sets the characters that must surround the free translation in a gloss.
#' If no value is specified, it will be double quotes. There are no real restrictions
#' for this value.
#'
#' Second, the following elements can set general styling instructions for different
#' sections of a gloss, formatting them completely in italics OR bold. The items with a \code{|}
#' indicate that various names are possible.
#' \describe{
#'   \item{source|preamble}{The line of the glosses where the \code{source} is rendered.}
#'   \item{a|first}{The first line of the glosses, with the original language text.}
#'   \item{b|second}{The second line of the glosses.}
#'   \item{c|third}{The third line of the glosses if it exists.}
#'   \item{ft|trans|translation}{The line of the glosses where the free \code{translation}
#'   is rendered.}
#' }
#' Each of these items can take one of a few values:
#' \itemize{
#'    \item \code{i}, \code{it}, \code{italics}, \code{textit} set italics.
#'    \item \code{b}, \code{bf}, \code{bold}, \code{textbf} set boldface.
#' }
#'
#' #TODO create vignette
#'
#' @param styling Named list of styling options for specific elements of glosses.
#'
#' @return Set the appropriate options.
#' @export
set_style_options <- function(styling = list()) {
  variables <- c(
    source = "preamble",
    preamble = "preamble",
    a = "a", first = "a",
    b = "b", second = "b",
    c = "c", third = "c",
    translation = "translation",
    ft = "translation",
    trans = "translation"
    )
  style_opts <- list()
  for (v in names(variables)) {
    if (v %in% names(styling)) {
      if (!styling[[v]] %in% c(style_options("i"), style_options("b"))) {
        msg_template <- "The '%s' option is not supported, please provide one of {%s} for italics or one of {%s} for bold."
        warning(sprintf(msg_template,
                        styling[[v]],
                        paste(style_options("i"), collapse = ", "),
                        paste(style_options("b"), collapse = ", ")),
                call. = FALSE)
      } else {
        style_opts[[paste0("glossr.format.", variables[[v]])]] <- styling[[v]]
      }
    }
  }
  if ("trans_quotes" %in% names(styling)) {
    style_opts$glossr.trans.quotes = styling$trans_quotes
  }
  options(style_opts)
  extra <- setdiff(names(styling), c(names(variables), "trans_quotes"))
  for (e in extra) {
    warning(sprintf("'%s' is not a valid style option.", e),
            call. = FALSE)
    }
}
