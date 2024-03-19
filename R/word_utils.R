bold_regex <- "^[*_]{2}(.+)[*_]{2}$"
italics_regex <- "^[_*]([^*_].*[^*_]?)[_*]$"

#' Convert from latex to Word
#'
#' @param string Character string
#' @return formatted string
#' @noRd
latex2word <- function(string) {
  string <- latex2md(string)
  string <- gsub(latex_tag("textbf"), "**\\1**", string)
  string <- gsub(latex_tag("textit"), "_\\1_", string)

  string
}

#' Add markdown format to gloss element
#'
#' @param line Gloss element, e.g. source, translation, line, word
#' @param section Key of the gloss element to format, i.e. how to
#'   search in the options:
#'   - 'a', 'b', 'c' for the gloss lines
#'   - 'preamble' for the source
#'   - 'translation' for the translation
#'
#' @return Formatted line
#' @noRd
format_word_section <- function(line, section) {
  option <- getOption(paste0("glossr.format.", section))
  if (is.null(option)) {
    return(line)
  }
  if (option %in% style_options("i")) {
    return(sprintf("_%s_", line))
  }
  if (option %in% style_options("b")) {
    return(sprintf("**%s**", line))
  }
  line
}


#' Compute with of word based on font family and size
#'
#' This is a function factory of sorts: based on the font family and size,
#' it sets up smaller functions and a variable used to apply to each word
#' in a given gloss line. It is set up like this because different lines
#' may have different font families or sizes.
#'
#' @param font_family Name of the font family to provide
#'   to `systemfonts::string_width()`.
#' @param font_size Font size to provide to `systemfonts::string_width()`.
#'
#' @return A list with three elements:
#'   - `word_to_pixels()`, a function that takes a word, reads whether it
#'     is in bold or italics or neither, and computes the width with
#'     `systemfonts::String_width()`.
#'   - `space_width`, the width of a space in this font family and size.
#'   - `pad_word()`, a function that takes a word and the width it has to achieve
#'     and adds spaces until it gets to that width
#' @noRd
fit_width <- function(font_family = "", font_size = 12) {
  word_to_pixels <- function(word) {
    bold <- FALSE
    italics <- FALSE
    if (grepl(bold_regex, word)) {
      bold <- TRUE
      word <- gsub(bold_regex, "\\1", word)
    }
    if (grepl(italics_regex, word)) {
      italics <- TRUE
      word <- gsub(italics_regex, "\\1", word)
    }
    systemfonts::string_width(word, family=font_family, size = font_size, italic = italics, bold = bold)
  }
  space_width <- word_to_pixels(" ")

  pad_word <- function(word, max_width) {
    while (word_to_pixels(word) < max_width) {
      word <- paste0(word, " ")
    }
    gsub(" ", "&nbsp;", word)
  }

  list(
    word_to_pixels = word_to_pixels,
    space_width = space_width,
    pad_word = pad_word
  )

}

#' Make bold
#'
#' @param word Character string
#'
#' @return String with markdown boldface formatting
#' @noRd
make_bold <- function(word) {
  ifelse(!grepl(bold_regex, word), sprintf("**%s**", word), word)
}

#' Make italics
#'
#' @param word Character string
#'
#' @return String with markdown italice formatting
#' @noRd
make_italics <- function(word) {
  ifelse(!grepl(italics_regex, word), sprintf("_%s_", word), word)
}

#' Align gloss lines for Word output
#'
#' Take two or three lines of glosses, parse $\LaTeX$ formatting if
#' relevant, split them into words, apply boldface or italics formatting
#' if necessary, compute the expected width and then align each word of
#' each line with their corresponding word in the other lines,
#' calculating the width they need to achieve and padding them with spaces.
#' This also reads the 'glossr.font.family', 'glossr.font.size' options
#' to check for desired font families and sizes (one or multiple different
#' ones) and 'glossr.page.width' (by default 411) for the width of the
#' writing page "in pixels".
#'
#' @param gdata A character vector with up to three lines; it can be
#'   also of class "gloss_data".
#'
#' @return A character vector of the same length, in which the words
#'  have been padded with fixed spaces ("&nbsp;").
#' @noRd
align_word <- function(gdata) {
  stopifnot(length(gdata) <= 3)
  gdata_split <- purrr::map(gdata, \(x) gloss_linesplit(latex2word(x))) |>
    stats::setNames(c("a", "b", "c")[1:length(gdata)])

  gdata_widths <- purrr::imap(gdata_split, function(line, name) {
    font_family <- getOption("glossr.font.family", "")
    if (!is.null(names(font_family))) {
      font_family <- if (name %in% names(font_family)) font_family[[name]] else ""
    }
    font_size <- getOption("glossr.font.size", 12)
    if (is.null(names(font_size))) {
      font_size <- if (name %in% names(font_size)) font_size[[name]] else 12
    }
    fit_width_line <- fit_width(font_family, font_size)

    line <- format_word_section(line, name)

    list(
      line = line,
      width = purrr::map_int(line, fit_width_line$word_to_pixels) + fit_width_line$space_width,
      pad_word = fit_width_line$pad_word
    )
  })

  max_widths <- purrr::map(gdata_widths, "width") |>
    purrr::pmap_int(max)

  as_tbl <- purrr::map(gdata_widths, function(line) {
    purrr::map2_chr(line$line, max_widths, line$pad_word)
  }) |>
    tibble::as_tibble()
  as_tbl$cum_width <- cumsum(max_widths)
  as_tbl$line_number <- ceiling(as_tbl$cum_width / getOption("glossr.page.width", 411))
  split(as_tbl[names(gdata_split)], as_tbl$line_number) |>
    purrr::map(\(line) purrr::map_chr(line, paste, collapse = "")) |>
    purrr::flatten_chr() |>
    unname()
}
