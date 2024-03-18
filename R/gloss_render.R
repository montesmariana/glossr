#' Render a gloss
#'
#' These functions are output-specific and can be used to check the specific output
#'   of certain calls, but are not meant to be used in an R Markdown file. Instead,
#'   use [as_gloss()] or [gloss_df()].
#'
#' @param gloss Object of class [`gloss_data`]
#' @param numbering Whether the gloss should be numbered (in HTML and Word).
#'
#' @return Object of class [`gloss`][new_gloss()].
#' @name gloss_render
#'
#' @encoding UTF-8
#' @examples
#' ex_sp <- "Un ejemplo en español"
#' ex_gloss <- "DET.M.SG example in Spanish"
#' ex_trans <- "An example in Spanish"
#' my_gloss <- new_gloss_data(list(ex_sp, ex_gloss), translation = ex_trans, label="ex1")
#' gloss_pdf(my_gloss)
#'
#' gloss_html(my_gloss)
NULL


#' @describeIn gloss_render Render in PDF
#' @export
gloss_pdf <- function(gloss) {
  stopifnot(inherits(gloss, "gloss_data"))
  # define label
  if (nchar(attr(gloss, "label")) > 0){
    attr(gloss, "label") <- sprintf("\\label{%s}", attr(gloss, "label"))
  }

  # define source
  if (attr(gloss, "has_source")){
    attr(gloss, "source") <- sprintf("\\glpreamble %s// ", attr(gloss, "source"))
  }

  # define translation
  if (attr(gloss, "has_translation")){
    attr(gloss, "translation") <- sprintf("\\glft %s// \n", attr(gloss, "translation"))
  }
  gloss_lines <- purrr::imap_chr(gloss[1:min(3, length(gloss))],
                                 ~ sprintf("\\gl%s %s//", letters[.y], .x)) %>%
    paste(collapse = " ")
  gloss_text <- sprintf(
    "\\ex%s \\begingl %s%s %s \\endgl \\xe \n",
    attr(gloss, "label"),
    attr(gloss, "source"),
    gloss_lines,
    attr(gloss, "translation")
    )
  new_gloss(gloss, gloss_text)
}

#' @describeIn gloss_render Render in HTML
#' @export
gloss_html <- function(gloss, numbering = TRUE) {
  stopifnot(inherits(gloss, "gloss_data"))
  output <- getOption("glossr.output", "leipzig")
  func <- if (output == "tooltip") gloss_tooltip else gloss_leipzig
  g <- c(
    if (numbering) sprintf("(@%s) ", attr(gloss, "label")) else NULL,
    func(gloss, numbering))
  new_gloss(gloss, g)
}

#' @describeIn gloss_render Tooltip rendering for HTML
#' @export
gloss_tooltip <- function(gloss, numbering = TRUE) {
  stopifnot(inherits(gloss, "gloss_data"))
  trans_part <- if (!attr(gloss, "has_translation")) {
    ""}
  else {
    sprintf("\n    %s\n", latex2html(attr(gloss, "translation")))
  }
  words <- htmltools::span(
    htmltools::tagList(
      htmltools::span(" ", .noWS = "outside"),
      gloss_linetooltip(gloss[[1]], gloss[[2]])
    ))
  c(as.character(words), "\n", trans_part)
}

#' @describeIn gloss_render Leipzig.js engine
#' @export
gloss_leipzig <- function(gloss, numbering = TRUE) {
  stopifnot(inherits(gloss, "gloss_data"))
  is_first <- getOption("glossr.first_leipzig", TRUE)

  # define source
  source <- if (attr(gloss, "has_source")) {
    htmltools::p(attr(gloss, "source"), class = "gloss__line--original")
  } else if (numbering) {
    htmltools::p(htmltools::HTML("&#160;"), class = "gloss__line--original")
  } else {
    NULL
  }

  # define glosses
  gloss_list <- purrr::map(gloss, ~ htmltools::p(latex2html(.x)))

  # define translation
  translation <- if (attr(gloss, "has_translation")){
    htmltools::p(attr(gloss, "translation"), class = "gloss__line--free")
  } else {
    NULL
  }

  g <- htmltools::div(
    htmltools::tagList(
      source,
      gloss_list,
      translation
    ),
    `data-gloss` = "",
    .noWS = "outside"
  )

  if (is_first) {
    g <- htmltools::tagList(
      format_html(),
      g,
      leipzig_script()
    )
    options("glossr.first_leipzig" = FALSE)
  } else {
    g <- htmltools::tagList(
      format_html(),
      g
    )
  }

  c(as.character(g), "\n")
}

#' @describeIn gloss_render Render in Word
#'
#' @export
gloss_word <- function(gloss, numbering = TRUE) {
  stopifnot(inherits(gloss, "gloss_data"))

  # Split lines and count characters
  gloss_print <- if (length(gloss) > 1) align_word(gloss) else unclass(gloss)

  if (attr(gloss, "has_source")) {
    source <- format_word_section(attr(gloss, "source"), "preamble")
    gloss_print <- c(source, gloss_print)
  }

  if (numbering) {
    gloss_print[[1]] <- sprintf("(@%s) %s", attr(gloss, "label"), gloss_print[[1]])
  }

  if (attr(gloss, "has_translation")) {
    translation <- format_word_section(attr(gloss, "translation"), "translation")
    gloss_print <- c(gloss_print, translation)
  }
  new_gloss(gloss, paste(gloss_print, collapse = "\n\n    "))
}

#' Render a non interlinear gloss
#'
#' This function is called when a gloss has only one line beyond the translation
#'   (or even no translation at all). Like other `gloss_render()` functions, it
#'   is only meant to be called internally, but it can be used for debugging.
#'
#' Render a gloss with only one line or one line and free translation in HTML and
#' Word.
#'
#' @inheritParams gloss_render
#'
#' @return Object of class [`gloss`][new_gloss()].
#' @noRd
gloss_single <- function(gloss, numbering = TRUE) {
  stopifnot(inherits(gloss, "gloss_data"))
  if (length(gloss) != 1) {
    cli::cli_abort("{.fun gloss_single} requires a gloss with only one line.")
  }
  label <- if (numbering) sprintf("(@%s) ", attr(gloss, "label")) else ""
  source <- if (attr(gloss, "has_source")) sprintf(" %s \n\n    ", attr(gloss, "source")) else ""
  gloss_text <- format_word_section(gloss, "a")
  translation <- if (attr(gloss, "has_translation")) sprintf(" \n\n    %s\n\n", attr(gloss, "translation")) else ""
  new_gloss(gloss, paste0(label, source, gloss_text, translation))
}


#' Render gloss from a dataframe
#'
#' @param df Dataframe one row per gloss. Columns `translation`,
#'   `source` and `label` have special meaning
#'   (see [as_gloss()]); all the others will be interpreted as
#'   lines to align in the order given.
#' @inheritParams as_gloss
#'
#' @return Object of class [`gloss`][new_gloss()] with the original input as `data` attribute.
#' @export
#'
#' @examples
#' my_gloss <- data.frame(
#'   first_line = "my first line",
#'   second_line = "my second line",
#'   translation = "Translation of my example",
#'   label = "label"
#' )
#' gloss_df(my_gloss)
gloss_df <- function(df, output_format = getOption("glossr.output", "latex"),
                     numbering = getOption("glossr.numbering", TRUE)) {
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.fun gloss_df} requires a {.cls data.frame} object.")
  }
  if (nrow(df) == 0) {
    cli::cli_abort("{.fun gloss_df} has received an empty dataframe.")
  }
  g <- unlist(purrr::pmap(df, as_gloss, output_format = output_format, numbering = numbering))
  new_gloss(df, g)
}
