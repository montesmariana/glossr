#' glossr: Use interlinear glosses in R Markdown
#'
#' Read examples with interlinear glosses from files or from text
#' and print them in a way compatible with both Latex and HTML outputs.
#'
#' @keywords internal
"_PACKAGE"

#' Use glossr
#'
#' Call in a setup chunk.
# TODO add leipzig.js reference, gb4e reference.
#'
#' @param html_format Whether the html output should use leipzig.js or tooltips.
#' @inheritParams set_style_options
#'
#' @return Set options
#' @export
use_glossr <- function(
    html_format = NULL,
    styling = list()
    ) {
  html_formats <- c("leipzig", "tooltip")
  opt <- getOption("glossr.output")
  if (knitr::is_latex_output()) {
    output <- "latex"
  } else if (!knitr::is_html_output() && is.null(html_format)) {
    output <- "word"
  } else if (is.null(opt)) {
    output <- if (is.null(html_format)) "leipzig" else match.arg(html_format, html_formats)
  } else if (!is.null(html_format)) {
    output <- match.arg(html_format, html_formats)
  } else {
    output <- opt
  }
  if (!output %in% c("latex", "word", html_formats)) {
    output <- "leipzig"
  }
  check_packages(output)
  options("glossr.output" = output)
  # options("glossr.first_leipzig" = TRUE)
  set_style_options(styling = styling)
  cli::cli_alert_info("Setting up the {.emph {output}} engine.")
}


#' Set general styling options
#'
#' This is a helper function to set [options()] that control style characteristics
#' for glosses across the full document. It is called within [use_glossr()]
#' but can be overridden later by setting the appropriate options.
#'
#' There are four types of settings that can be provided in the list.
#'
#' First, `trans_quotes` sets the characters that must surround the free translation in a gloss.
#' If no value is specified, it will be double quotes. There are no real restrictions
#' for this value.
#'
#' Second, the following elements can set general styling instructions for different
#' sections of a gloss, formatting them completely in italics OR bold. The items with a `|`
#' indicate that various names are possible.
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
#' Finally, there is one setting that is not available in LaTeX, particularly
#' thinking of slides: **numbering**, that is,
#' whether the glosses should be numbered (in HTML).
#'
#' @param styling Named list of styling options for specific elements of glosses.
#'
#' @return Set the appropriate options.
#' @export
set_style_options <- function(styling = list()) {
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

  other_vars <- c('trans_quotes', 'par_spacing', 'numbering', 'exskip',
                'belowglpreambleskip', 'aboveglftskip', 'extraglskip',
                "font_family", "font_size", "page_width")

  style_opts <- list()
  bad_styling <- c()
  for (v in names(variables)) {
    if (v %in% names(styling)) {
      if (!styling[[v]] %in% c(style_options("i"), style_options("b"))) {
        bad_styling <- c(bad_styling, styling[[v]])
      } else {
        style_opts[[paste0("glossr.format.", variables[[v]])]] <- styling[[v]]
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

  for (opt in other_vars) {
    if (opt %in% names(styling)) {
      name <- if (opt == 'exskip') 'glossr.par.spacing' else paste0('glossr.', gsub('_', '.', opt))
      if (startsWith(opt, "font") & !is.null(names(styling[[opt]]))) {
        name_options <- c("a", "b", "c")
        for (line_name in names(styling[[opt]])) {
          if (!line_name %in% name_options) {
            cli::cli_warn(c("!" = "If {.var {opt}} is a named vector, the names must be one of {.emph {name_options}}.",
                    "{.emph {line_name}} is not supported and will be ignored."))
          }
        }
      }
      style_opts[[name]] <- styling[[opt]]
    }
  }

  options(style_opts)
  extra <- setdiff(names(styling), c(names(variables), other_vars))
  for (e in extra) {
    cli::cli_warn(c("!" = "{.var {e}} is not a valid style option and was ignored."))
  }
}
