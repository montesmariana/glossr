#' Include glosses in R Markdown
#'
#' Finishes off the output of \code{\link{gloss_render}} for
#'   an R Markdown file, based on its output format.
#'   The input can be one glossed example, multiple ones,
#'   a list of glossed examples or even a glossed list
#'   created by \code{\link{gloss_list}}.
#'
#'   This function should be called in an R chunk with
#'   the `results = "asis"` option (and `echo = FALSE` to avoid
#'   showing the code). The name of the chunk is
#'   not relevant.
#'
#' @param ... One or more glosses.
#'
#' @return Prints text for markdown, in either latex or html formats.
#' @export
#'
#' @examples
#' ex_sp <- "Un ejemplo en español"
#' ex_gloss <- "DET.M.SG example in Spanish"
#' ex_trans <- "An example in Spanish"
#' my_gloss <- as_gloss(ex_sp, ex_gloss, ex_trans, "ex1")
#' gloss_frame(my_gloss)
gloss_frame <- function(...) {
  glosses <- if (class(...) == "list") purrr::flatten_chr(...) else c(...)
  if (knitr::is_latex_output()) {
    glosses <- c(
      "\\begin{exe}\n",
      glosses,
      "\\end{exe}\n"
    )
  }
  cat(glosses)
  invisible(glosses)
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
  if (knitr::is_latex_output()) {
    sprintf("(\\@ref(%s))", label)
  } else {
    sprintf("(@%s)", label)
  }
}


