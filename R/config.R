config <- rlang::env(
  emptyenv(),
  format = list(
    a = "",
    b = "",
    c = "",
    preamble = "",
    translation = ""
  ),
  pdf = list(
    par_spacing = 0,
    belowglpreambleskip = 0,
    aboveglftskip = 0,
    extraglskip = 0
  ),
  word = list(
    font_family = "",
    font_size = 12,
    page_width = 411
  ),
  trans_quotes = '"',
  numbering = TRUE,
  first_leipzig = TRUE,
  output = "latex"
  )

#' Print the glossr configuration
#'
#' @param config_sections Character vector with the following possible values:
#' - **format**: to show the formatting options (italics / bold / nothing) for
#' the different lines.
#' - **pdf**: to show $\LaTeX$-specific formatting
#' - **word**: to show Word-specific formatting
#' - **other**: to show the current output format, translation quotes and
#' the `numbering` setting.
#'
#' @return Invisibly, a named list with the printed values
#' @export
#'
#' @examples
#' print_config()
#' print_config("pdf") # print only pdf-specific configuration
print_config <- function(config_sections = c("format", "pdf", "word", "other")) {
  valid_sections <- c("format", "pdf", "word", "other")
  if (!inherits(config_sections, "character")) {
    cli::cli_abort(
      x = "Pl.ease provide a character vector",
      "Valid values are {.emph {valid_sections}}")
  }
  return_value <- list()
  if ("format" %in% config_sections) {
    cli::cli_h1("General line formatting")
    format_ol <- cli::cli_ol()
    for (item in names(config$format)) {
      cli::cli_li("{.strong {item}}: {config$format[[item]]}")
    }
    cli::cli_end(format_ol)
    return_value$format <- config$format
  }

  if ("pdf" %in% config_sections) {
    cli::cli_h1("PDF-specific formatting")
    pdf_ol <- cli::cli_ol()
    for (item in names(config$pdf)) {
      cli::cli_li("{.strong {item}}: {config$pdf[[item]]}")
    }
    cli::cli_end(pdf_ol)
    return_value$pdf <- config$pdf
  }
  if ("word" %in% config_sections) {
    cli::cli_h1("Word-specific formatting")
    word_ol <- cli::cli_ol()
    for (item in names(config$word)) {
      cli::cli_li("{.strong {item}}: {config$word[[item]]}")
    }
    cli::cli_end(word_ol)
    return_value$word <- config$pdf
  }
  if ("other" %in% config_sections) {
    cli::cli_h1("Other formatting")
    other_ol <- cli::cli_ol()
    return_value$other <- list()
    for (item in c("output", "trans_quotes", "numbering")) {
      cli::cli_li("{.strong {item}}: {config[[item]]}")
      return_value$other[[item]] <- config[[item]]
    }
    cli::cli_end(other_ol)
  }
  invisible(return_value)

}

#' Use glossr
#'
#' Override default configuration.
#'
#' @param output_format Desired output format
#' @inheritParams set_style_options
#'
#' @return Set options
#' @export
use_glossr <- function(
    output_format = NULL,
    styling = list()
    ) {
  set_style_options(styling)

  original_output <- config$output
  if ((!is.null(output_format)) && original_output != output_format) {
    output <- set_output(output_format)
    cli::cli_alert_info("Switching to the {.emph {output}} engine.")
  } else if (length(styling) == 0){
    cli::cli_alert_info("No changes have been made; this call was not needed.")
  }
}

#' Change output format
#'
#' @param output_format Name of the output format: "latex", "word", "leipzig",
#' "tooltip".
#'
#' @return Invisibly, the final output format.
#' @noRd
set_output <- function(output_format = NULL) {
  html_formats <- c("leipzig", "tooltip")
  if (knitr::is_latex_output() || (!is.null(output_format) && output_format %in% c("pdf", "latex"))) {
    output <- "latex"
  } else if (!knitr::is_html_output() || (!is.null(output_format) && output_format == "word")) {
    output <- "word"
  } else if (is.null(output_format)) {
    output <- "leipzig"
  } else {
    output <- match.arg(output_format, html_formats)
  }

  if (!output %in% c("latex", "word", html_formats)) {
    output <- "leipzig"
  }

  if (output %in% html_formats) {
    requireNamespace("htmltools", quietly = TRUE)
  }
  config$output <- output
  invisible(output)
}

#' Set general styling options
#'
#' This is a helper function to set the configuration that control style characteristics
#' for glosses across the full document. It is called within [use_glossr()].
#'
#' There are five types of settings that can be provided in the list.
#'
#' First, `trans_quotes` sets the characters that must surround the free translation in a gloss.
#' If no value is specified, it will be double quotes. There are no real restrictions
#' for this value.
#'
#' Second, the following elements can set general styling instructions for different
#' sections of a gloss, formatting them completely in italics OR bold. The items with a `|`
#' indicate that various names are possible.×
#' - **source|preamble**: The line of the glosses where the `source` is rendered.
#' - **a|first**: The first line of the glosses, with the original language text.
#' - **b|second**: The second line of the glosses.
#' - **c|third**: The third line of the glosses if it exists.
#' - **ft|trans|translation**: The line of the glosses where the free `translation`
#'  is rendered.
#'
#' Each of these items can take one of a few values:
#' - `i`, `it`, `italics` and `textit` set italics.
#' - `b`, `bf`, `bold` and `textbf` set boldface.
#'
#' Third, there are a few LaTeX-specific settings documented in the
#' [expex documentation](https://mirror.lyrahosting.com/CTAN/macros/generic/expex/expex-doc.pdf).
#' In all cases the default value is 0 (0pt).
#' (If you would like other settings to be supported, write up an Issue and I will look into it!)
#' - **exskip|par_spacing**: Space above *and* below the example. The `par_spacing` name
#' is allowed for backwards compatibility, but the actual name in `expex` is `exskip`.
#' - **belowglpreambleskip**: Space under the preamble (where the `source` is printed).
#' - **aboveglftskip**: The spacing above the free translation.
#' - **extraglskip**: The spacing between the aligned lines.
#'
#' Fourth, there is one setting that is not available in $\LaTeX$, particularly
#' thinking of slides: **numbering**, that is,
#' whether the glosses should be numbered (in HTML).
#'
#' Finally, you may set the following values for Word output, used in computing
#' the spaces for alignment:
#' - **font_family**: A character vector of length 1 indicating the font family used in the
#' lines to be aligned, or a character vector with names "a" and "b" (and "c" if
#' relevant) indicating the font families of specific lines.
#' - **font_size**: A numeric vector of length one indicating the font size used in the
#' lines to be aligned, or a numeric vector with names "a" and "b" (and "c" if
#' relevant) indicating the font sizes of specific lines.
#' - **page_width**: The width of the space between the margins of the Word file,
#' in pixels, used to wrap long examples. The default is 411; if you see that your
#' output does not match what you want, you can tweak it with this value.
#'
#' @param styling Named list of styling options for specific elements of glosses.
#'
#' @return Set the appropriate options.
#' @export
set_style_options <- function(styling = list()) {
  if (length(styling) == 0) {
    return()
  }
  formatted <- set_format_options(styling)
  pdf_options <- set_pdf_options(styling)
  word_options <- set_word_options(styling)
  other_options <- set_other_options(styling)

  extra <- setdiff(names(styling), c(formatted, pdf_options, word_options, other_options))
  for (e in extra) {
    cli::cli_warn(c("!" = "{.var {e}} is not a valid style option and was ignored."))
  }
}

#' Set formatting options
#'
#' Validate and override options for italics or bold for the different lines.
#'
#' @param styling
#'
#' @return Invisibly, the names of the styling options that were modified.
#' @noRd
set_format_options <- function(styling = list()) {
  variables <- c(
    source = "preamble",
    preamble = "preamble",
    a = "a", first = "a",
    b = "b", second = "b",
    c = "c", third = "c",
    translation = "translation",
    ft = "translation",
    trans = "translation"
    )
  styling <- styling[intersect(names(styling), names(variables))]
  if (length(styling) == 0) {
    return()
  }

  bad_styling <- c()
  for (v in names(variables)) {
    if (v %in% names(styling)) {
      if (!styling[[v]] %in% c(style_options("i"), style_options("b"))) {
        bad_styling <- c(bad_styling, styling[[v]])
      } else {
        # style_opts[[paste0("glossr.format.", variables[[v]])]] <- styling[[v]]
        config$format[[variables[[v]]]] <- styling[[v]]
      }
    }
  }
  styling_vars <- styling[names(styling) %in% names(variables)]
  if (any(!styling_vars %in% c(style_options("i"), style_options("b")))) {
    i_options <- cli::cli_vec(style_options("i"), list(vec_last = " or "))
    b_options <- cli::cli_vec(style_options("b"), list(vec_last = " or "))
    bad_options <- cli::cli_vec(bad_styling)
    cli::cli_warn(c("!" = "Please provide one of {.emph {i_options}} for italics or one of {.emph {b_options}} for boldface.",
                    "The following option{?s} {?is/are} not supported: {.var {bad_options}}."))
  }

  invisible(names(styling))
}

#' Set other options
#'
#' Validate and override options for translation quotes and numbering.
#'
#' @param styling
#'
#' @return Invisibly, the names of the styling options that were modified.
#' @noRd
set_other_options <- function(styling = list()) {
  var_classes <- c("trans_quotes" = "character", "numbering" = "logical")
  styling <- styling[intersect(names(styling), names(var_classes))]
  if (length(styling) == 0) {
    return()
  }
  for (opt in names(styling)) {
    if (!inherits(styling[[opt]], var_classes[[opt]])) {
      abort("{.var {opt}} must be of class {.var {var_classes[[opt]]}}.")
    } else {
      config[[opt]] <- styling[[opt]]
    }
  }
  return(names(styling))
}

#' Set PDF options
#'
#' Validate and override $\LaTeX$-specific options.
#'
#' @param styling
#'
#' @return Invisibly, the names of the styling options that were modified.
#' @noRd
set_pdf_options <- function(styling = list()) {
  var_classes <- c("par_spacing",
                "belowglpreambleskip", "aboveglftskip", "extraglskip")
  for (opt in names(styling)) {
    if (opt == "exskip") {
      if (!"par_spacing" %in% names(styling)) {
        styling$par_spacing <- styling$exskip
      }
      styling$exskip <- NULL
    }
  }
  styling <- styling[intersect(names(styling), var_classes)]
  if (length(styling) == 0) {
    return()
  }
  for (opt in names(styling)) {
    if (!inherits(styling[[opt]], "numeric")) {
      abort("{.var {opt}} must be of class {.var numeric}.")
    }
    config$pdf[[opt]] <- styling[[opt]]
  }
  invisible(names(styling))
}

#' Set Word options
#'
#' Validate and override Word-specific options.
#'
#' @param styling
#'
#' @return Invisibly, the names of the styling options that were modified.
#' @noRd
set_word_options <- function(styling = list()) {
  var_classes <- c("page_width" = "numeric",
                     "font_size" = "numeric",
                     "font_family" = "character")

  styling <- styling[intersect(names(styling), names(var_classes))]
  if (length(styling) == 0) {
    return()
  }
  name_options <- c("a", "b", "c")
  with_default <- c(name_options, "default")
  font_defaults <- c(font_family = "", font_size = 12)
  for (opt in names(styling)) {
    if (!inherits(styling[[opt]], var_classes[[opt]])) {
      abort("{.var {opt}} must be of class {.var {valid_classes[[opt]]}}.")
    }
    if (opt == "page_width") {
      config$word[[opt]] <- styling[[opt]]
    } else if (is.null(names(styling[[opt]]))) {
      config$word[[opt]]<- setNames(rep(styling[[opt]], 3), name_options)
    } else {
      for (line_name in names(styling[[opt]])) {
        if (!line_name %in% with_default) {
          cli::cli_warn(c("!" = "If {.var {opt}} is a named vector, the names must be one of {.emph {with_default}}.",
                          "{.emph {line_name}} is not supported and will be ignored."))
        }
      }
      line_based_config <- purrr::map_chr(name_options, \(line_name) {
        if (line_name %in% names(styling[[opt]])) {
          return(styling[[opt]][[line_name]])
        } else if ("default" %in% names(styling[[opt]])) {
          return(styling[[opt]]$default)
        } else {
          return(font_defaults[[opt]])
        }
      }) |> setNames(name_options)
      config$word[[opt]] <- line_based_config
      }
  }
  invisible(names(styling))
}


