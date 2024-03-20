.onLoad <- function(libname, pkgname) {
  set_output()
  cli::cli_inform("Setting up the {.emph {config$output}} engine.",
                  class="packageStartupMessage")
}
