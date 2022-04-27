#' Add tooltip to one word
#'
#' @param x Word or expression
#' @param title Text of the tooltip
#'
#' @return `shiny.tag` with attributes for a tooltip
#' @export
#'
#' @examples
#' tooltip("One", "DET.SG")
tooltip <- function(x, title) {
  htmltools::tagAppendAttributes(
    htmltools::span(sprintf(" %s ", x), .noWS = "outside"),
    `data-toggle` = "tooltip",
    title = title
  )
}

#' Apply tooltip to a full gloss
#'
#' @inheritParams gloss_render
#'
#' @return List of `shiny.tag`
#' @export
#'
#' @encoding UTF-8
#' @examples
#' ex_sp <- "Un ejemplo en español"
#' ex_gloss <- "DET.M.SG example in Spanish"
#' gloss_linetooltip(ex_sp, ex_gloss)
gloss_linetooltip <- function(original, parsed) {
  original <- gloss_linesplit(ignore_latex(original))
  parsed <- gloss_linesplit(ignore_latex(parsed))
  stopifnot(length(original) == length(parsed))
  purrr::map2(original, parsed, ~ tooltip(.x, .y))
}
