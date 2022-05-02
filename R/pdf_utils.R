#' Apply latex formatting to many words
#'
#' Facilitates applying the same latex formatting
#'   to different words in a row.
#'
#' @param text Character vector of length 1.
#' @param formatting Latex formatting code, e.g. `textit` or `textsc`.
#'
#' @return Reformatted string
#' @export
#'
#' @examples
#' gloss_format_words("Many words to apply italics on.", "textit")
gloss_format_words <- function(text, formatting) {
  gloss_linesplit(text) %>%
    purrr::map_chr(~ sprintf("\\%s{%s}", formatting, .x)) %>%
    purrr::map_chr(~ ifelse(
      stringr::str_detect(.x, " "),
      sprintf("{%s}", .x),
      .x)) %>%
    paste(collapse = " ")
}

#' Sublist glosses
#'
#' Takes a series of glosses from \code{\link{gloss_render}}
#'   and puts them in a list within one example for PDF output.
#'
#' @param glist Concatenation of \code{gloss} objects, e.g.
#'   as output of \code{\link{gloss_df}}.
#' @param listlabel Label for the full list (optional)
#'
#' @return Character vector including the frame for a list of glosses.
#' @export
gloss_list <- function(glist, listlabel = NULL) {
  output <- getOption("glossr.output", "latex")

  if (output == "latex") {
    llabel <- if (is.null(listlabel)) "" else sprintf("\\label{%s}", listlabel)
    glist <- stringr::str_replace(glist, "\\\\ex", "\\\\a")
    glist <- glist[glist != "\\xe \n"]
    g <- c(
      sprintf("\\pex%s \n", llabel),
      glist,
      sprintf("\\xe \n")
    )
    structure(g, class = "gloss")
  } else {
    glist
  }
}
