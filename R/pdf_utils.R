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
  if (formatting %in% style_options("i")) formatting <- "textit"
  if (formatting %in% style_options("b")) formatting <- "textbf"
  split_line <- sprintf("\\%s{%s}", formatting, gloss_linesplit(text))
  split_line <- ifelse(grepl(" ", split_line), sprintf("{%s}", split_line), split_line) |>
    paste(collapse = " ")
  gsub("\\s+", " ", split_line)
}

#' Sublist glosses
#'
#' Takes a series of glosses from [gloss_render()]
#'   and puts them in a list within one example for PDF output.
#'
#' @param glist Concatenation of [`gloss`] objects, e.g.
#'   as output of [gloss_df()].
#' @param listlabel Label for the full list (optional)
#'
#' @return Character vector including the frame for a list of glosses.
#' @export
gloss_list <- function(glist, listlabel = NULL) {
  if (!inherits(glist, "gloss")) {
    cli::cli_abort(c("{.fun gloss_list} needs an object of class {.cls gloss}",
                     "please use {.fun as_gloss} or {.fun gloss_df} first."))
  }
  output <- config$output
  clean_gloss <- unclass(glist)
  attr(clean_gloss, "data") <- NULL

  if (output == "latex") {
    llabel <- if (is.null(listlabel)) "" else sprintf("\\label{%s}", listlabel)
    clean_gloss <- gsub("\\\\ex", "\\\\a", clean_gloss)
    clean_gloss <- gsub("\\\\xe \\n", "", clean_gloss) |>
      paste(collapse = "\n")
    clean_gloss <- sprintf("\\pex%s %s \\xe \n", llabel, clean_gloss)
    new_gloss(attr(glist, "data"), clean_gloss)
  } else {
    glist
  }
}

#' Read Latex formatting options
#'
#' @param level Gloss line to format
#' @noRd
#' @return Key for expex
format_pdf <- function(level) {
  format <- config$format[[level]]
  if (is.null(format) | format == "") {
    NULL
  } else if (format %in% style_options("i")) {
    "\\itshape"
  } else if (format %in% style_options("b")) {
    "\\bfseries"
  } else {
    NULL
  }
}

#' Convert from latex to Markdown
#'
#' @param string Character string
#' @return formatted string
#' @noRd
latex2md <- function(string) {
  string <- gsub(latex_tag("textsc"), "\\U\\1", string, perl=TRUE)
  string <- gsub("\\\\O", "&#8709;", string)
  string <- gsub("\\$\\\\(emptyset|varnothing)\\$", "&#8709;", string)

  string
}
