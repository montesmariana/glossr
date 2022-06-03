This is the first submission of glossr.

## R CMD check results

### Locally

There were no ERRORs, WARNINGs or NOTEs.

### R-hub checks

There were no ERRORs or WARNINGs. There were two NOTEs:

- Found the following (possibly) invalid URLs:
  URL: https://github.com/montesmariana/glossr/raw/main/inst/examples/ (moved to https://github.com/montesmariana/glossr/tree/main/inst/examples)
    From: inst/doc/glossr.html
    Status: 200
    Message: OK

- Found the following files/directories:
  'lastMiKTeXException'
  

The first note refers to an URL to a folder in the GitHub repository. It is a valid URL.

The second note only occurs with the Windows Build and I don't know how to deal with it.

### win_devel

There were no ERRORs or WARNINGs. There was one NOTE:

- R Under development (unstable) (2022-06-02 r82447 ucrt)

This is because I'm using R-devel.

## Downstream dependencies

There are currently no downstream dependencies for this package.
