# glossr <img src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/485119883.svg)](https://zenodo.org/badge/latestdoi/485119883)
![Release](https://img.shields.io/github/v/release/montesmariana/glossr)

<!-- badges: end -->

The `glossr` package gives you tools to include interlinear glosses in your R Markdown file.
If you are writing a linguistics paper and you want some interlinear glosses, this is for you!

Maybe you already use `gb4e` and you're happy with your PDF files, good for you!
But maybe you want to spice things up, have your examples in one file and read a dataframe
to generate them on demand, instead of *typing* and *mistyping* and having your examples
all over the place. If that's the case, `glossr` is for you!

Or maybe you want HTML output to work? If you use `gb4e`, your examples disappear!
Here `glossr` can most definitely help. It even offers a helper function for cross-references
that work in both formats!^[There are even TWO different HTML outputs: one using [leipzig.js](https://bdchauvette.net/leipzig.js)
and a sadder one that *might* be more maleable and accessible, just less pro-looking.]

But please, don't take my word for it ---you can check the output in the following articles:

- `vignette("glossr_pdf")` for the PDF glosses (which is not rendered in [the pkgdown site](https://montesmariana.github.io/glossr), but can be found [in the repo](https://github.com/montesmariana/glossr/blob/v0.1.0/inst/pdf/glossr_pdf.pdf));

- `vignette("glossr_leipzig")` for its leipzig.js counterpart, _where the only difference is the output format_;

- and `vignette("glossr_tooltip")` for the sad original solution I came up with for HTML before I discovered leipzig.js.

## Installation

You can install the github development version with:

``` r
remotes::install_github("montesmariana/glossr")
```

One day I will get this to CRAN.

## Questions and suggestions?

Bring them on to the [issues section of the repository](https://github.com/montesmariana/glossr/issues)!
