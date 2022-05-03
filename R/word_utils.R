#' Process elements of gloss lines for word
#'
#' @param gloss_lines Unclassed content of a \code{gloss_data} object
#'
#' @return List of tibbles to print
#' @import dplyr
#' @importFrom rlang .data
gloss_word_lines <- function(gloss_lines) {
  names(gloss_lines) <- paste0("c", seq(length(gloss_lines)))
  gloss_split <- tibble::as_tibble_row(gloss_lines) %>%
    mutate(across(
      everything(), purrr::map, ~ gloss_linesplit(latex2word(.x)))
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
#' Helper for \code{\link{gloss_word}}
#'
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
#' @param translation (Optional) Text for free translation
#'
#' @return \code{\link[flextable]{flextable}} object
#' @import flextable
#' @export
gloss_table <- function(gloss_output, translation = NULL) {
  # TODO add formatting
  ft <- flextable(gloss_output) %>%
    delete_part("header") %>%
    border_remove() %>%
    padding(padding = 0) %>%
    line_spacing(space = 1.5) %>%
    padding(j = 1, padding.left = 30) %>%
    autofit()
  if (is.null(translation)) {
    ft
  } else {
    ft %>% footnote(value = as_paragraph(translation), ref_symbols = "\t\t")
  }
}

#' @describeIn parse_latex Convert to Word
latex2word <- function(string) {
  string <- gsub(latex_tag("textit"), "\\1", string)
  string <- gsub(latex_tag("em"), "\\1", string)
  string <- gsub(latex_tag("textbf"), "\\1", string)
  string <- gsub(latex_tag("textsc"), "\\1", string)
  string
  # to_apply <- c()
  # # Check and remove tags
  # if (grep(latex_tag("textit", string))) {
  #   string <- gsub(latex_tag("textit"), "\\1", string)
  #   to_apply <- c(to_apply, "i")
  # }
  # if (grep(latex_tag("em", string))) {
  #   string <- gsub(latex_tag("em"), "\\1", string)
  #   to_apply <- c(to_apply, "i")
  # }
  # if (grep(latex_tag("textbf", string))) {
  #   string <- gsub(latex_tag("textbf"), "\\1", string)
  #   to_apply <- c(to_apply, "b")
  # }
  # if (grep(latex_tag("textsc", string))) {
  #   string <- gsub(latex_tag("textsc"), "\\1", string)
  #   to_apply <- c(to_apply, "sc")
  # }
  # # Apply formatting
  # if ("sc" %in% to_apply) {
  #   string <- toupper(string)
  # }
  # if ("i" %in% to_apply) {
  #   string <- flextable::as_i(string)
  # }
  # if ("b" %in% to_apply) {
  #   string <- flextable::as_b(string)
  # }
  # string
}
