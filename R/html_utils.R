#' Ignore latex notation
#'
#' Replace most of the latex notation with asterisks.
#'
#' @param string Latex string to purge
#'
#' @return Character vector
ignore_latex <- function(string) {
  # TODO refine for replacement instead of removal
  stringr::str_replace_all(string, "\\\\[a-z]+\\{([^\\}]+)\\}", "*\\1*")
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
  setdiff(first_split[[1]], "") %>%
    purrr::map(function(x) {
      if (stringr::str_starts(x, "%")) {
        stringr::str_remove(x, "%")
      } else {
        stringr::str_split(x, " ")[[1]]
      }
    }) %>%
    purrr::flatten_chr()
}


