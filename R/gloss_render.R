#' Render a gloss
#'
#' @param gloss Object of class \code{gloss_data}
#' @param df Dataframe one row per gloss. Columns \code{translation},
#'   \code{source} and \code{label} have special meaning
#'   (see \code{\link{create_gloss}}); all the others will be interpreted as
#'   lines to align in the order given.
#'
#' @return Object of class \code{gloss}
#' @name gloss_render
#'
#' @encoding UTF-8
#' @examples
#' ex_sp <- "Un ejemplo en español"
#' ex_gloss <- "DET.M.SG example in Spanish"
#' ex_trans <- "An example in Spanish"
#' my_gloss <- create_gloss(ex_sp, ex_gloss, translation = ex_trans, label="ex1")
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
  structure(gloss_text, class = "gloss")
}

#' @describeIn gloss_render Render in HTML
#' @export
gloss_html <- function(gloss) {
  output <- getOption("glossr.output", "leipzig")
  func <- if (output == "leipzig") gloss_leipzig else gloss_tooltip
  g <- c(sprintf("(@%s) ", attr(gloss, "label")), func(gloss))
  structure(g, class = "gloss")
}

#' @describeIn gloss_render Tooltip rendering for HTML
#' @export
gloss_tooltip <- function(gloss) {
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
  if (is_first) {
    g <- htmltools::tagList(
      g,
      leipzig_script()
    )
    options("glossr.first_leipzig" = FALSE)
  }

  c(as.character(g), "\n")
}

#' @describeIn gloss_render Render in Word
#'
#' @import dplyr
#' @export
gloss_word <- function(gloss) {
  # define source
  source <- if (attr(gloss, "has_source")) attr(gloss, "source") else "_"

  # Split lines and count characters
  gloss_lines <- gloss_word_lines(unclass(gloss))

  # Create sequence of tables for different lines
  ft_lines <- purrr::imap(gloss_lines, function(g, i) {
    if (i == length(gloss_lines)) {
      gt <- gloss_table(g, attr(gloss, "translation"))
    } else {
      gt <- gloss_table(g)
    }
    flextable::flextable_to_rmd(gt, ft.align = "left", print = FALSE)
  }) %>% unlist()

  structure(
    c(
      sprintf("(@%s) %s\n", attr(gloss, "label"), source),
      ft_lines
    ),
    class = "gloss"
  )
}


#' @describeIn gloss_render Render glosses from a dataframe
#'
#' @export
gloss_df <- function(df) {
  g <- unlist(purrr::pmap(df, as_gloss))
  structure(g, class = "gloss")
}
