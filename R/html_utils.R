#' @describeIn parse_latex Convert to HTML
latex2html <- function(string) {
  # TODO refine for replacement instead of removal
  string <- gsub(latex_tag("textit"), "<em>\\1</em>", string)
  string <- gsub(latex_tag("em"), "<em>\\1</em>", string)
  string <- gsub(latex_tag("textbf"), "<strong>\\1</strong>", string)
  string <- gsub("\\\\O", "&#8709;", string)
  string <- gsub("\\$\\\\(emptyset|varnothing)\\$", "&#8709;", string)
  string <- sc_to_upper(string)
  htmltools::HTML(string)
}


#' Small caps to upper case
#'
#' Replaces small caps tags from LaTeX to upper case within a string.
#'
#' @param string String with a LaTeX small caps tag
#'
#' @noRd
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
#' @noRd
#' @returns Regex expression to extract tagged string.
latex_tag <- function(tag) {
  sprintf("\\\\%s\\{([^}]+)\\}", tag)
  }

#' Split lines for HTML
#'
#' Splits a character string by spaces keeping groups of words
#'   surrounded by curly braces together.
#'
#' @param line Character string to split.
#'
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
#' @return \code{\link[htmltools]{tag}}
#' @export
leipzig_script <- function() {
  htmltools::tags$script(
    paste0(
      "document.addEventListener('DOMContentLoaded', function() ",
      "{Leipzig({lastLineFree: false}).gloss();});"
    )
  )
}

#' Read HTML formatting options
#'
#' @noRd
#' @return Style tag
format_html <- function() {
  levels <- c(
    preamble = ".gloss__line--original",
    a = ".gloss__word .gloss__line:first-child",
    b = ".gloss__word .gloss__line--2",
    c = ".gloss__word .gloss__line--3",
    translation = ".gloss__line--free")

  style <- purrr::imap_chr(levels, function(css_class, level) {
    format <- getOption(sprintf("glossr.format.%s", level))
    if (is.null(format)) {
      sprintf("%s {font-style:normal;font-weight:normal}", css_class)
    } else if (format %in% style_options("i")) {
      sprintf("%s {font-style:italic;}", css_class)
    } else if (format %in% style_options("b")) {
      sprintf("%s {font-weight: bold;}", css_class)
    } else {
      ""
    }
  })
  htmltools::tags$style(trimws(paste(style, collapse = " ")))

}

