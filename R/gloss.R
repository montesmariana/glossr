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
#'    \item{trans_quotes}{Types of quotes to surround the free translation with,
#'    if available.}
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
  gloss_lines <- c(...)
  lengths <- purrr::map_dbl(gloss_lines, ~ length(gloss_linesplit(.x)))
  structure(
    gloss_lines,
    source = set_default(source),
    translation = set_default(translation),
    label = set_default(label),
    lengths = lengths,
    trans_quotes = trans_quotes,
    class = "gloss_data"
  )
}
