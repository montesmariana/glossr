#' Ignore latex notation
#'
#' Replace most of the latex notation with with HTML equivalent
#'
#' @param string Latex string to parse
#'
#' @return Character vector
ignore_latex <- function(string) {
  # TODO refine for replacement instead of removal
  string <- gsub(latex_tag("textit"), "*\\1*", string)
  string <- gsub(latex_tag("em"), "*\\1*", string)
  string <- gsub(latex_tag("textbf"), "**\\1**", string)
  sc_to_upper(string)
}


#' Small caps to upper case
#'
#' Replaces small caps tags from LaTeX to upper case within a string.
#'
#' @param string String with a LaTeX small caps tag
#'
#' @return Character vector of length one
sc_to_upper <- function(string) {
  replaced <- gsub(
    latex_tag("textsc"),
    "&&&\\1&&",
    string)
  replaced <- stringr::str_split(replaced, "&&")[[1]]
  purrr::map_chr(
    replaced[replaced != ""],
    ~ ifelse(startsWith(.x, "&"), gsub(".(.+)", "\\1", toupper(.x)), .x)
  ) %>% paste(collapse = "")
}

#' Regex for a latex tag
#'
#' @param tag Latex tag
#'
#' @returns Regex expression to extract tagged string.
latex_tag <- function(tag) {
  sprintf("\\\\%s\\{([^\\}]+)\\}", tag)
  }

#' Split lines for HTML
#'
#' Splits a character string by spaces keeping groups of words
#'   surrounded by curly braces together.
#'
#' @param line Character string to split.
#'
#' @return Character vector of elements.
gloss_linesplit <- function(line) {
  first_split <- stringr::str_replace_all(
    line,
    " \\{([^}]+)\\}\\.?([^ ]*)?",
    " :::%\\1\\2:::"
    ) %>%
    stringr::str_split(" ?::: ?")
  first_split[[1]][first_split[[1]] != ""] %>%
    purrr::map(function(x) {
      if (stringr::str_starts(x, "%")) {
        stringr::str_remove(x, "%")
      } else {
        stringr::str_split(x, " ")[[1]]
      }
    }) %>%
    purrr::flatten_chr()
}

#' HTML dependency for leipzig.js
#'
#' @return \code{\link[htmltools]{htmlDependency}}
#' @export
use_leipzig <- function() {
  htmltools::htmlDependency(
    name = "leipzig",
    version = "0.8.0",
    src = "leipzig",
    script = "leipzig.js",
    stylesheet = "leipzig.css",
    package = "glossr"
  )
}

#' HTML dependency for tooltip format
#'
#' @return \code{\link[htmltools]{htmlDependency}}
#' @export
use_tooltip <- function() {
  htmltools::htmlDependency(
    name = "tooltip",
    version = "0.1.0",
    src = "tooltip",
    script = "tooltip.js",
    stylesheet = "tooltip.css",
    package = "glossr"
  )
}

#' Script for leipzig.js
#'
#' To append after the first gloss.
#'
#' @return \code{\link[htmltools]{tag}}
#' @export
leipzig_script <- function() {
  htmltools::tags$script(
    "document.addEventListener('DOMContentLoaded', function() {Leipzig().gloss();});"
    )
}

