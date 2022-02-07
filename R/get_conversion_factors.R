# A function to extract the conversion factors returned when verbose == TRUE, and return as a tibble
get_conversion_factors <- function(...) {
  rlang::check_installed(c("testthat", "purrr"), reason = "in order for 'return_cfs = TRUE' to work.")

  # Make sure all messages are printed to one line. Otherwise some unwanted \n may automatically pop up
  old_value2 <- getOption("cli.width")
  options("cli.width" = 120)
  on.exit(options("cli.width" = old_value2))

  # Run convertGDP while capturing the messages
  # IMPORTANT: return_cfs has to be FALSE to avoid infinite recursion. It is by default, but written
  # explicitly here to avoid mistakes.
  x <- testthat::evaluate_promise(convertGDP(..., verbose = TRUE, return_cfs = FALSE))

  # Get number of countries = number of lines in the 2 message, - 3
  my_message_lines <- purrr::map(x$messages, stringr::str_split, "\\n")
  n_c <- length(my_message_lines[[2]][[1]]) - 3

  # Get list indices to extract specific lines from the captured messages
  i_cf <- seq_along(x$messages)[-1]
  i_lines <- 0L:n_c + 2L
  my_lines <- purrr::pmap_chr(purrr::cross_df(list("y" = i_lines, "x" = i_cf)),
                              ~ purrr::pluck(my_message_lines, .y, 1, .x))

  # Separate lines into conversion factor names and values
  my_names <- gsub(" used:", "", grep("used:", my_lines, value = TRUE))

  my_reg_values <- grep("used:", my_lines, value = TRUE, invert = TRUE)
  my_reg <- unique(gsub(":(.*)$", "", my_reg_values))
  my_values <- as.numeric(gsub("^...: ", "", my_reg_values))
  my_values <- purrr::map(seq_along(i_cf), ~ my_values[(1 + n_c * (.x - 1)):(n_c * .x)])

  # Assign names to values
  names(my_values) <- my_names

  # Transform into tibble
  tibble::tibble("iso3c" = my_reg) %>%
    dplyr::bind_cols(tibble::as_tibble(my_values))
}
