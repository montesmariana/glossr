#' Print method for glosses
#'
#' This method print [`gloss`][new_gloss()] objects with `{knitr}`.
#'
#' @param x Object to print
#' @param ... Other options
#' @importFrom knitr knit_print
#' @exportS3Method knitr::knit_print gloss
#' @export
knit_print.gloss <- function(x, ...) {
  output <- config$output
  if (output == "latex") {
    latex_params = c(
      sprintf("exskip=%dpt", config$pdf$par_spacing),
      sprintf("belowglpreambleskip=%dpt", config$pdf$belowglpreambleskip),
      sprintf("aboveglftskip=%dpt", config$pdf$aboveglftskip),
      sprintf("extraglskip=%dpt", config$pdf$extraglskip),
      paste0("everyglpreamble=", format_pdf("preamble")),
      paste0("everygla=", format_pdf("a")),
      paste0("everyglb=", format_pdf("b")),
      paste0("everyglc=", format_pdf("c")),
      paste0("everyglft=", format_pdf("translation"))
    )
    for_xelatex <- c("\\let\\expexgla\\gla", "\\AtBeginDocument{\\let\\gla\\expexgla}")
    knitr::asis_output(
      c(
        sprintf("\\lingset{%s}", paste(latex_params, collapse = ",")),
        x),
      meta = list(rmarkdown::latex_dependency("expex", extra_lines = for_xelatex)))
  } else if (output == "word") {
    knitr::asis_output(paste(x, collapse = "\n\n"))
  } else if (length(attr(x, 'data')) == 1) {
    knitr::asis_output(x)
  } else if (output == "leipzig") {
    knitr::asis_output(paste(x, collapse = ""), meta = list(use_leipzig()))
  } else {
    knitr::asis_output(x, meta = list(
      rmarkdown::html_dependency_jquery(),
      use_tooltip())
      )
  }
}

#' Reference gloss
#'
#' Latex output uses \code{\@ref(label)} to reference examples,
#'   whereas HTML output is based on pandoc examples, i.e. \code{(@label)}.
#'   \code{`r gloss(label)`}, written inline in the text, will return the
#'   appropriate reference based on the selected output.
#'
#' @param label Label for reference
#'
#' @return Character string with label reference
#' @noMd
#' @export
gloss <- function(label) {
  output <- config$output
  if (output == "latex") {
    sprintf("(\\ref{%s})", label)
  } else {
    sprintf("(@%s)", label)
  }
}

#' Validate gloss factory
#'
#' Use [{cli}] functions to inform the user how the factory went.
#'
#' @param glosses Dataframe with glosses
#'
#' @noRd
validate_gloss_factory <- function(glosses) {
  arg_columns <- c("source", "translation", "label",
                   "trans_quotes", "output_format", "numbering")
  text_columns <- setdiff(colnames(glosses), arg_columns)

  # Check that there are columns for text
  if (length(text_columns) > 3) {
    cli_names <- cli::cli_vec(text_columns)
    cli::cli_alert_warning(c(
      "There are {length(text_columns)} columns that can be printed as text: {.var {cli_names}}. ",
      "Only the first three will be used."
      ), wrap = TRUE)
    text_columns <- text_columns[1:3]
  } else if (length(text_columns) == 0) {
    cli::cli_abort("There are no columns to use as gloss text.")
  }

  # Report gloss elements
  gloss_lines <- cli::cli_vec(text_columns)
  gloss_elements <- c(
    "{.var source} (not aligned!)",
    "{.var {gloss_lines}} (aligned columns)",
    "{.var translation} (not aligned!)")
  names(gloss_elements) <- c(
    if ("source" %in% colnames(glosses)) "v" else "x",
    "v",
    if ("translation" %in% colnames(glosses)) "v" else "x"
  )
  cli::cli_alert_info(c(
    "The following columns will be used for the gloss texts, ",
    "in the following order:"))
  cli::cli_bullets(gloss_elements)
  if ("label" %in% colnames(glosses)) {
    cli::cli_alert_success("The {.var label} column will be used for labels.")
  } else {
    cli::cli_alert_warning("No {.var label} column was found, examples will not have names.")
  }
}

#' Function factory to print glosses from dataframe
#'
#' This function takes a dataframe with glosses and returns another function
#' that takes either an id or list of ids (if `use_conditionals` is `FALSE`)
#' or a conditional statement (if `TRUE`) and runs [gloss_df()] on the filtered
#' dataframe.
#'
#' @param glosses Dataframe with gloss data.
#' @param use_conditionals Boolean. If `TRUE`, the returned function will use
#'   conditional statements to filter the dataframe. Otherwise, it will use
#'   ids and match them to the values in the `id_column`.
#' @param id_column Name of the column with ids for filtering, if `use_conditionals`
#'   is `FALSE`.
#' @param ignore_columns Optional character vector with names of columns that could
#'   be used for filtering but should not be provided to [gloss_df()].
#' @param validate Boolean. If `TRUE`, running [gloss_factory()] will print a few
#'   informative messages about how glossr is reading the dataframe.
#'
#' @return A function.
#'
#'   If `use_conditionals` is `FALSE` (the default), the returned
#'   function will take a character vector or a series of character vectors with
#'   id's to filter. If `id_column` is "label", running that function will be
#'   the equivalent to filtering `glosses` based on the values in the `label` column.
#'
#'   If `use_conditionals` is `TRUE`, the returned function will take the same
#'   conditions that a [dplyr::filter()] would.
#' @export
#'
#' @importFrom dplyr filter
#' @examples
#' my_glosses <- dplyr::select(glosses, -language)
#' by_label <- gloss_factory(my_glosses)
#'
#' by_label("heartwarming-jp")
#'
#' by_label("heartwarming-jp", "languid-jp")
#'
#' by_cond <- gloss_factory(my_glosses, use_conditional = TRUE)
#' by_cond(stringr::str_ends(label, "jp"))
gloss_factory <- function(
    glosses,
    use_conditionals = FALSE,
    id_column = "label",
    ignore_columns = NULL,
    validate = TRUE) {

  remove_cols <- function(x) {
    ok_columns <- colnames(x)[!colnames(x) %in% ignore_columns]
    x[,ok_columns]
  }

  if (validate) validate_gloss_factory(remove_cols(glosses))


  if (use_conditionals) {
    function(...) {
      glosses |> filter(...) |>
        remove_cols() |>
        gloss_df()
    }
  } else {
    if (!id_column %in% colnames(glosses)) {
      cli::cli_abort("There is no {.var {id_column}} column in the dataset.")
    }

    if (max(table(glosses[[id_column]])) > 1) {
      cli::cli_alert_warning(c(
        "The values in {.var {id_column}} are not unique. ",
        "Only the first match of repeated ids will be returned."))
    }

    function(...) {
      ids <- unique(c(...))
      extras <- setdiff(ids, glosses[[id_column]])

      if (length(extras) > 0) {
        cli::cli_alert_warning("The following ids are not present in the dataset:")
        cli::cli_ul(extras)
        ids <- setdiff(ids, extras)
      }

      idxs <- stats::setNames(seq_along(glosses[[id_column]]), glosses[[id_column]])

      selection <- idxs[ids]

      if (sum(selection) == 0) {
        cli::cli_alert_danger("No rows in the dataset match this selection.")
        return()
      } else {
        remove_cols(glosses[selection,]) |> gloss_df()
      }

    }
  }
}

