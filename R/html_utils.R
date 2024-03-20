#' Convert from latex to HTML
#' @param string Character string
#' @return HTML tag
#' @noRd
latex2html <- function(string) {
  # TODO refine for replacement instead of removal
  string <- latex2md(string)
  string <- gsub(latex_tag("textit"), "<em>\\1</em>", string)
  string <- gsub(latex_tag("em"), "<em>\\1</em>", string)
  string <- gsub(latex_tag("textbf"), "<strong>\\1</strong>", string)

  htmltools::HTML(string)
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
  tokenized <- gsub(
    " \\{([^}]+)\\}\\.?([^ ]*)?",
    " :::%\\1\\2:::",
    line
    )
  first_line <- strsplit(tokenized, " ?::: ?")[[1]]
  first_line <- first_line[first_line != ""]
  ifelse(
    startsWith(first_line, "%"),
    gsub("^%", "", first_line),
    strsplit(first_line, " ")
    ) |>
    unlist()
}

#' HTML dependency for leipzig.js
#'
#' @noRd
#' @return [htmltools::htmlDependency]
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
#' @return [htmltools::htmlDependency]
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
#' @return [`htmltools::tag`]
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
    format <- config$format[[level]]
    if (is.null(format) | format == "") {
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

