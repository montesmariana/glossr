#' Render a gloss
#'
#' This functions are output-specific and can be used to check the specific output
#'   of certain calls, but are not meant to be used in an R Markdown file. Instead,
#'   use \code{\link{as_gloss}} or \code{\link{gloss_df}}.
#'
#' @param gloss Object of class \code{gloss_data}
#'
#' @return Object of class \code{gloss}
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
    attr(gloss, "source") <- sprintf("\\glpreamble %s// \n", attr(gloss, "source"))
  }

  # define translation
  if (attr(gloss, "has_translation")){
    attr(gloss, "translation") <- sprintf("\\glft %s// \n", attr(gloss, "translation"))
  }
  gloss_lines <- purrr::imap_chr(gloss[1:min(3, length(gloss))],
                                 ~ sprintf("\\gl%s %s// \n", letters[.y], .x))
  gloss_text <- c(
    "\\ex",
    sprintf("%s\n", attr(gloss, "label")),
    "\\begingl \n",
    attr(gloss, "source"),
    gloss_lines,
    attr(gloss, "translation"),
    "\\endgl \n",
    "\\xe \n"
    )
  new_gloss(gloss, gloss_text)
}

#' @describeIn gloss_render Render in HTML
#' @export
gloss_html <- function(gloss) {
  stopifnot(inherits(gloss, "gloss_data"))
  output <- getOption("glossr.output", "leipzig")
  func <- if (output == "tooltip") gloss_tooltip else gloss_leipzig
  g <- c(sprintf("(@%s) ", attr(gloss, "label")), func(gloss))
  new_gloss(gloss, g)
}

#' @describeIn gloss_render Tooltip rendering for HTML
#' @export
gloss_tooltip <- function(gloss) {
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
gloss_leipzig <- function(gloss) {
  stopifnot(inherits(gloss, "gloss_data"))
  is_first <- getOption("glossr.first_leipzig", TRUE)

  # define source
  source <- if (attr(gloss, "has_source")) attr(gloss, "source") else htmltools::HTML("&#160;")

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
      htmltools::p(source, class = "gloss__line--original"),
      gloss_list,
      translation
    ),
    `data-gloss` = "",
    .noWS = "outside"
  )
  first_it <- ".gloss__word .gloss__line:first-child {font-style:italic;}"
  orig_bf <- ".gloss__line--original {}"
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
#' @import dplyr
#' @export
gloss_word <- function(gloss) {
  stopifnot(inherits(gloss, "gloss_data"))
  # define source
  source <- if (attr(gloss, "has_source")) attr(gloss, "source") else "_"

  # Split lines and count characters
  gloss_lines <- gloss_word_lines(unclass(gloss))
  if (attr(gloss, "has_translation")) {
    translation <- data.frame(translation = attr(gloss, "translation"))
    gloss_lines[[length(gloss_lines) + 1]] <- gloss_table(translation, TRUE)
  }


  # Create sequence of tables for different lines
  ft_lines <- purrr::map(gloss_lines, function(g) {
    g <- if (inherits(g, "flextable")) g else gloss_table(g)
    flextable::flextable_to_rmd(g, ft.align = "left", print = FALSE)
  }) %>% unlist()

  gloss_print <- c(
    sprintf("(@%s) %s\n", attr(gloss, "label"), format_word_source(source)),
    ft_lines
    )
  new_gloss(gloss, gloss_print)
}


#' Render gloss from a dataframe
#'
#' @param df Dataframe one row per gloss. Columns \code{translation},
#'   \code{source} and \code{label} have special meaning
#'   (see \code{\link{new_gloss_data}}); all the others will be interpreted as
#'   lines to align in the order given.
#' @inheritParams as_gloss
#'
#' @return Object of class \code{gloss} with the original input as \code{data} attribute.
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
gloss_df <- function(df, output_format = getOption("glossr.output", "latex")) {
  if (!inherits(df, "data.frame")) {
    stop("`gloss_df` requires a `data.frame` object.",
         call. = FALSE)
  }
  if (nrow(df) == 0) {
    stop("`gloss_df` has received an empty dataframe.",
         call. = FALSE)
  }
  g <- unlist(purrr::pmap(df, as_gloss, output_format = output_format))
  new_gloss(df, g)
}
