#' Create gloss_data object
#'
#' Based on a character vectors and up to three label arguments,
#' create an object where those arguments are attributes.
#' These are:
#' \describe{
#'    \item{source}{Where the text comes from. This will be printed
#'    in the first line of the example, without word alignment.}
#'    \item{translation}{Free translation. This will be printed as the
#'    last line of the example, without word alignment and in quotation
#'    marks if so desired.}
#'    \item{label}{Named label of the example, for cross-references.}
#'    \item{lengths}{This is computed within the function, not provider, and
#'    it's the number of items identified in each gloss line.}
#' }
#'
#' @param ... Lines for glossing
#' @param source (Optional) Source of example
#' @param translation (Optional) Free translation
#' @param label (Optional) Example label
#' @param trans_quotes (Optional) Quotes to surround the free translation with.
#'
#' @return Object of class `gloss_data`
#' @export
#'
#' @examples
create_gloss <- function(
    ...,
    source = NULL,
    translation = NULL,
    label = NULL,
    trans_quotes = getOption("glossr.trans.quotes", '"')
) {
  gloss_lines <- unname(c(...))
  lengths <- purrr::map_dbl(gloss_lines, ~ length(gloss_linesplit(.x)))
  source <- set_default(source, NULL) # set to NULL if invalid
  translation <- set_default(translation, NULL) # set to NULL if invalid
  if (!is.null(translation)) {
    # add quotes if not null
    translation <- sprintf("%s%s%s", trans_quotes, translation, trans_quotes)
  }
  structure(
    gloss_lines,
    has_source = !is.null(source),
    source = set_default(source), # set to empty character if absent
    has_translation = !is.null(translation),
    translation = set_default(translation), # set to empty character if absent
    label = set_default(label),
    lengths = lengths,
    class = "gloss_data"
  )
}
