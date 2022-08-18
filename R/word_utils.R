#' Process elements of gloss lines for word
#'
#' @param gloss_lines Unclassed content of a [`gloss_data`] object
#'
#' @noRd
#' @return List of tibbles to print
#' @import dplyr
#' @importFrom rlang .data
gloss_word_lines <- function(gloss_lines) {
  names(gloss_lines) <- paste0("c", seq(length(gloss_lines)))
  gloss_split <- tibble::as_tibble_row(gloss_lines) %>%
    mutate(across(
      everything(), purrr::map, ~ gloss_linesplit(latex2word(.x, is_cell =FALSE)))
    )
  gloss_split_df <- gloss_split %>%
    tidyr::unnest(everything()) %>%
    mutate(across(everything(), nchar))

  gloss_split_df <- gloss_split_df %>%
    mutate(
      maximum = purrr::pmap_dbl(gloss_split_df, pmax),
      line = 1,
      cum_max = cumsum(.data$maximum))

  # Asign items to different lines
  while (max(gloss_split_df$cum_max) > 0) {
    gloss_split_df <- gloss_split_df %>%
      mutate(purrr::pmap_dfr(
        list(.data$maximum, .data$cum_max, .data$line), reset_max),
        cum_max = cumsum(.data$maximum))
  }

  purrr::map(
    unique(gloss_split_df$line), function(l) {
      indices <- which(gloss_split_df$line == l)
      purrr::map_dfr(
        t(gloss_split)[,1],
        ~ stats::setNames(.x[indices], paste0("c", seq(length(indices))))
      )
    })

}

#' Divide lines for Word
#'
#' Helper for [gloss_word()]
#'
#' @noRd
#' @param m Maximum number of characters for a slot
#' @param c Cumulative sum of maximums
#' @param l Current line
reset_max <- function(m, c, l) {
  threshold <- getOption("glossr.word.threshold", 55)
  if (c < threshold) {
    tibble::tibble(maximum = 0, line = l)
  } else {
    tibble::tibble(maximum = m, line = l + 1)
  }
}

#' Create table from a gloss
#'
#' @param gloss_output Gloss lines as a table for Word
#' @param is_translation Whether the table is for the free translation line.
#'
#' @noRd
#' @return [`flextable::flextable`] object
#' @import flextable
#' @importFrom rlang .data
gloss_table <- function(gloss_output, is_translation = FALSE) {
  ft <- flextable(gloss_output) %>%
    delete_part("header") %>%
    border_remove() %>%
    padding(padding = 0) %>%
    line_spacing(space = 1.5) %>%
    padding(j = 1, padding.left = 30) %>%
    autofit()
  for (i in 1:nrow(gloss_output)) {
    ft <- ft %>%
      compose(i,
              value = as_paragraph(latex2word(.data$.)),
              use_dot = TRUE)
  }
  if (is_translation) format_word_translation(ft) else format_word_glosses(ft)
}

#' Convert from latex to Word
#'
#' @param string Character string
#' @param is_cell Logical. Is this a cell in a table
#' @return formatted string
#' @noRd
latex2word <- function(string, is_cell = TRUE) {
  if (is_cell) {
    if (grepl(latex_tag("textsc"), string)) {
      string <- toupper(gsub(latex_tag("textsc"), "\\1", string))
    }
    it_pattern <- latex_tag("textit")
    bf_pattern <- latex_tag("textbf")
    both_pattern <- "\\\\text(it|bf)\\{\\\\text(bf|it)\\{([^\\}]+)\\}\\}"
    if (grepl(both_pattern, string)) {
      flextable::as_i(
        flextable::as_b(
          gsub(both_pattern, "\\3", string)
        )
      )
    } else if (grepl(it_pattern, string)) {
      flextable::as_i(gsub(it_pattern, "\\1", string))
    } else if (grepl(bf_pattern, string)) {
      flextable::as_b(gsub(bf_pattern, "\\1", string))
    } else {
      string
    }
  } else {
    # string <- gsub(latex_tag("textit"), word_knitr("\\1", italic = TRUE), string)
    # string <- gsub(latex_tag("em"), word_knitr("\\1", italic = TRUE), string)
    # string <- gsub(latex_tag("textbf"), word_knitr("\\1", bold = TRUE), string)
    if (grepl(latex_tag("textsc"), string)) {
      gsub(latex_tag("textsc"), toupper("\\1"), string)
    } else {
      string
    }

  }
}

#' Format glosses for word
#'
#' @param ft [`flextable::flextable`] for gloss lines
#' @return Formatted table or text
#' @noRd
format_word_glosses <- function(ft) {
  lines <- c("a", "b", "c")
  have_italics <- purrr::map_lgl(
    lines,
    ~ getOption(paste0("glossr.format.", .x), "x") %in% style_options("i")
    )
  have_bold <- purrr::map_lgl(
    lines,
    ~ getOption(paste0("glossr.format.", .x), "x") %in% style_options("b")
    )
  if (sum(have_italics) > 0) {
    ft <- ft %>% flextable::italic(i = which(have_italics))
  }
  if (sum(have_bold) > 0) {
    ft <- ft %>% flextable::bold(i = which(have_bold))
  }
  ft
}

#' Format translation for word
#' @param ft [`flextable::flextable`] for gloss lines or translation
#' @return Formatted table or text
#' @noRd
format_word_translation <- function(ft) {
  tr_option <- getOption("glossr.format.translation", "x")
  if (tr_option %in% style_options("i")) {
    flextable::italic(ft)
  } else if (tr_option %in% style_options("b")) {
    flextable::bold(ft)
  } else {
    ft
  }
}

#' Format source text for Word
#'
#' @param source Character vector with the source text.
#' @return Formatted table or text
#' @noRd
format_word_source <- function(source) {
  s_option <- getOption("glossr.format.preamble", "x")
  if (s_option %in% style_options("i")) {
    word_knitr(source, italic = TRUE)
  } else if (s_option %in% style_options("b")) {
    word_knitr(source, bold = TRUE)
  } else {
    source
  }
}

#' Apply officer formatting
#'
#' @param text Text to format
#' @param bold Whether the word should be in bold
#' @param italic Whether the word should be in italics
#'
#' @noRd
#' @return Knitr-ready text
word_knitr <- function(text, bold = FALSE, italic = FALSE) {
  text <- officer::to_wml(
    officer::ftext(
      text,
      officer::fp_text(
        italic = italic,
        bold = bold
      )
    )
  )
  sprintf("`%s`{=openxml}", text)
}
