#' gloss_data class
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
#' This function is mostly for internal use, but may be useful for debugging or
#' checking the output of specific calls. Normally, it's best to use
#' \code{\link{as_gloss}} or \code{\link{gloss_df}}.
#' Note that, unlike \code{\link{as_gloss}}, \code{new_gloss_data} requires a list
#' of gloss lines.
#'
#' @param gloss_lines Lines for glossing, as a list
#' @param source (Optional) Source of example
#' @param translation (Optional) Free translation
#' @param label (Optional) Example label
#' @param trans_quotes (Optional) Quotes to surround the free translation with.
#'
#' @return Object of class \code{gloss_data}
#' @export
new_gloss_data <- function(
    gloss_lines,
    source = NULL,
    translation = NULL,
    label = NULL,
    trans_quotes = getOption("glossr.trans.quotes", '"')
) {
  if (!inherits(gloss_lines, "list")) {
    stop("The gloss lines must be provided as a list.",
         call. = FALSE)
  }
  gloss_lines <- unname(purrr::map_chr(gloss_lines, as.character))
  lengths <- purrr::map_dbl(gloss_lines, ~ length(gloss_linesplit(.x)))
  source <- set_default(source, NULL) # set to NULL if invalid
  translation <- set_default(translation, NULL) # set to NULL if invalid
  if (!is.null(translation)) {
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
    class = c("gloss_data", "character")
  )
}

#' Helper to create gloss objects
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
#' @param output_format (Optional) Whether it will use latex, word or html format.
#'
#' @return Object of class \code{gloss}, ready to be printed based on the chosen output format,
#'   and with a \code{gloss_data} object as \code{data} attribute (or, in the case of calls via
#'   \code{\link{gloss_df}}, the original input as \code{data}.
#' @export
#' @examples
#' ex_sp <- "Un ejemplo en español"
#' ex_gloss <- "DET.M.SG example in Spanish"
#' ex_trans <- "An example in Spanish"
#' my_gloss <- as_gloss(ex_sp, ex_gloss, translation = ex_trans, label="ex1")
#'
#' # check the gloss data
#' attr(my_gloss, "data")
as_gloss <- function(...,
                     source = NULL,
                     translation = NULL,
                     label = NULL,
                     trans_quotes = getOption("glossr.trans.quotes", '"'),
                     output_format = getOption("glossr.output", "latex")
                     ) {
  validate_output(output_format)
  gloss <- new_gloss_data(
    list(...),
    source = source, translation = translation, label = label,
    trans_quotes = trans_quotes
    )
  if (output_format == "latex") {
    gloss_pdf(gloss)
  } else if (output_format == "word") {
    gloss_word(gloss)
  } else {
    gloss_html(gloss)
  }
}

#' gloss class
#'
#' The \code{gloss} class contains how a gloss will be printed and its original input
#' (Object of class \code{\link{new_gloss_data}}) as \code{data} attribute.
#' It also has a \code{\link[knitr]{knit_print}} method for rendering in R Markdown.
#'
#' @param input A \code{gloss_data} object.
#' @param output How the gloss must be printed, depending on the output.
#'
#' @return Object of class \code{gloss}
#' @export
new_gloss <- function(input, output){
  structure(
    output,
    class = c("gloss", "character"),
    data = input
  )
}
