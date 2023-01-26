# Abort and warn helpers
abort <- function(x, .envir = parent.frame()) {
  rlang::abort(glue::glue(x, .envir = .envir))
}
warn <- function(x, .envir = parent.frame()) {
  if(getOption("GDPuc.warn", default = TRUE)) rlang::warn(glue::glue(x, .envir = .envir))
}

# All console output must eventually go through cli_inform() so that
# it can be turned on and off with 'GDPuc.verbose' when needed.
cli_inform <- function(..., verbose = getOption("GDPuc.verbose", default = FALSE)) {
  if (verbose) do.call(..., args = list()) else invisible()
}

# Function called by conversion functions to write messages
cli_elemental <- function(from, to, with, unit, val) {
  # Create named vector for conversion factor list
  names <- if ("year" %in% colnames(val)) paste0(val$iso3c, ", ", val$year) else val$iso3c
  values <- dplyr::pull(val, length(val)) %>% stats::setNames(names)

  my_cli <- function() {
    cli::cli({
      cli::cli_text("{.strong {from}} {crayon::green(cli::symbol$arrow_right)} {.strong {to}}")
      cli::cli_text("{crayon::blue(with)}{cli::qty(as.character(values))}{?s} in {unit} used:")
      cli::cli_dl(c(signif(values, 6)))
    })
  }
  cli_inform(my_cli)
}

# Function called by print_source_info
cli_source_info <- function(name, origin, date, html, note = NULL) {
  cli::cli({
    cli::cli_rule(left = "{name}")
    cli::cli_alert("Origin: {origin}")
    cli::cli_alert("Date: {date}")
    cli::cli_alert("Html: {html}")
    if (!is.null(note)) cli::cli_alert("Note: {note}")
    cli::cli_rule()
  })
}
