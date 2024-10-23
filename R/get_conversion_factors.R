# A function to extract the conversion factors returned when verbose == TRUE, and return as a tibble
get_conversion_factors <- function(...) {
  rlang::check_installed(c("testthat", "purrr"), reason = "in order for 'return_cfs = TRUE' to work.")

  # Make sure all messages are printed to one line. Otherwise some unwanted \n may automatically pop up
  withr::local_options(list("cli.width" = 120))

  # Run convertGDP while capturing the messages
  # IMPORTANT: return_cfs has to be FALSE to avoid infinite recursion. It is by default, but written
  # explicitly here to avoid mistakes.
  x <- testthat::evaluate_promise(convertGDP(..., verbose = TRUE, return_cfs = FALSE))

  # If using with_regions, drop first message
  if (!is.null(list(...)$with_regions)) x$messages <- x$messages[-1]

  # Get number of countries = number of lines in the 2 message, - 2
  my_message_lines <- purrr::map(x$messages, strsplit, "\\n")
  n_c <- length(my_message_lines[[2]][[1]]) - 2

  # Get list indices to extract specific lines from the captured messages
  i_cf <- seq_along(x$messages)[-1]
  i_lines <- 0L:n_c + 2L
  my_lines <- purrr::pmap_chr(tidyr::expand_grid("y" = i_lines, "x" = i_cf),
                              ~ purrr::pluck(my_message_lines, .y, 1, .x))

  # Separate lines into conversion factor names and values
  my_names <- gsub(" used:", "", grep("used:", my_lines, value = TRUE))
  my_reg_values <- grep("used:", my_lines, value = TRUE, invert = TRUE)

  tibble::as_tibble(my_reg_values) %>%
    tidyr::separate("value", c("iso3c", "value"), sep = ": ") %>%
    dplyr::mutate(value = as.numeric(.data$value), x = rep(my_names, n_c)) %>%
    # Suppress NA warnings
    suppressWarnings() %>%
    tidyr::pivot_wider(names_from = x)
}
