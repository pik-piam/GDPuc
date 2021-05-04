# Abort and warn helpers
abort <- function(x, .envir = parent.frame()) {
  rlang::abort(glue::glue(x, .envir = .envir))
}
warn <- function(x, .envir = parent.frame()) {
  rlang::warn(glue::glue(x, .envir = .envir))
}

# All console output must eventually go through cli_inform() so that
# it can be turned on and off with 'GDPuc.verbose' when needed.
cli_inform <- function(..., verbose = getOption("GDPuc.verbose", default = FALSE)) {
  if (verbose) do.call(..., args = list()) else invisible()
}

# Function called by conversion functions to write messages
cli_elemental <- function(from, to, with, unit, val) {
  # Create named vector for conversion factor list
  names <- if ("year" %in% colnames(val)) {
    paste0(val$iso3c, ", ", val$year)
  } else val$iso3c
  values <- dplyr::pull(val, length(val)) %>% setNames(names)

  my_cli <- function() {
    cli::cli({
      cli::cli_text("{.strong {from}} {crayon::green(cli::symbol$arrow_right)} {.strong {to}}")
      cli::cli_text("{crayon::blue(with)} {cli::qty(as.character(values))} conversion factor{?s} in {unit} used:")
      cli::cli_dl(c(values))
    })
  }
  cli_inform(my_cli)
}
