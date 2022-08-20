
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glossr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/glossr)](https://CRAN.R-project.org/package=glossr)
[![DOI](https://zenodo.org/badge/485119883.svg)](https://zenodo.org/badge/latestdoi/485119883)
![Release](https://img.shields.io/github/v/release/montesmariana/glossr)
[![R-CMD-check](https://github.com/montesmariana/glossr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/montesmariana/glossr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/glossr/branch/main/graph/badge.svg)](https://codecov.io/gh/r-lib/glossr?branch=main)

<!-- badges: end -->

The glossr package gives you tools to include interlinear glosses in
your R Markdown file. If you are writing a linguistics paper and you
want some interlinear glosses, this is for you!

Maybe you already use `gb4e` or `expex` and you’re happy with your PDF
files, good for you! But maybe you want to spice things up, have your
examples in one file and read a dataframe to generate them on demand,
instead of *typing* and *mistyping* and having your examples all over
the place. If that’s the case, glossr is for you!

Or maybe you want HTML output to work? If you use `gb4e` or `expex`,
your examples will disappear from the HTML output! Here glossr can most
definitely help. It even offers a helper function for cross-references
that work in both formats! In fact, I included two different HTML
outputs: one using
[leipzig.js](https://github.com/bdchauvette/leipzig.js/) and a sadder
one that *might* be more accessible, just less pro-looking.

If you also want Word output, glossr can also take care of it,
generating invisible tables for the right alignment.

But please, don’t take my word for it —you can check the PDF, HTML and
MS Word outputs of `vignette("glossr_how")` [stored in the
repository](https://github.com/montesmariana/glossr/tree/main/inst/examples).

## Installation

Install glossr via CRAN:

``` r
install.packages("glossr")
```

You can also install the development version of glossr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("montesmariana/glossr")
```

## Example

This is a basic example; check `vignette("glossr_how")` for more
examples and their output.

``` r
library(glossr)
use_glossr()

my_gloss <- as_gloss(
  "她 哇的一聲 大 哭起來，",
  "tā wā=de-yì-shēng dà kū-qǐlái,",
  "TSG waa.IDEO-LINK-one-sound big cry-inch",
  translation = "Waaaaa, she began to wail.",
  label = "my-label",
  source = "ASBC (nº 100622)"
)
```

## Acknowledgements

This package is possible thanks to the existence of other packages it
has built on, mostly `{rmarkdown}`, `{htmltools}`, `{officedown}` and
`{flextable}`, as well as [the `expex`
package](https://ctan.org/pkg/expex?lang=en) for PDF output and
[leipzig.js](https://github.com/bdchauvette/leipzig.js/) for the HTML
output. The HexSticker was designed in [Krita](https://krita.org/en/)
and rendered with `{hexSticker}`.

I would also like to acknowledge the input and encouragement of Giulia
Mazzola and Thomas Van Hoey, who shared ideas and tested the code as it
evolved.

Last but not least, I’d like to acknowledge the source of the examples
in the small “dataset” provided by this package, taken from [Maria
Koptjevskaja-Tamm’s *The Linguistics of
Temperature*](https://www.jbe-platform.com/content/books/9789027269171)
(see `vignette("glossr")`).

## Questions and suggestions?

Bring them on to the [issues section of the
repository](https://github.com/montesmariana/glossr/issues)!
