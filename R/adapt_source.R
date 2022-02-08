#
adapt_source <- function(gdp, source, with_regions, replace_NAs) {
  rlang::check_installed(c("tidyr", "zoo"), reason = "in order for 'replace_NAs' to work.")

  . <- NULL

  # Create adapted source object
  source_adapted <- source %>%
    # Add any iso3c-year combinations from gdp, not available in source
    dplyr::bind_rows(gdp %>%
                       {if ("gdpuc_region" %in% colnames(.)) dplyr::filter(., is.na(.data$gdpuc_region)) else .} %>%
                       dplyr::select(.data$iso3c, .data$year) %>%
                       dplyr::distinct() %>%
                       dplyr::anti_join(source, by = c("iso3c", "year"))) %>%
    tidyr::complete(.data$iso3c, .data$year)

  if (replace_NAs %in% c("linear", "linear_regional_average")) {
    # Make sure that source contains obersvations for every year between min and max years.
    # This is important for the function lin_int_ext, which works with indices, to compute the
    # correct values
    source_adapted <- source_adapted %>%
      # Add any iso3c-year combinations from gdp, not available in source
      dplyr::bind_rows(tibble::tibble("year" = min(.$year):max(.$year))) %>%
      tidyr::complete(.data$iso3c, .data$year)

    source_adapted <- source_adapted %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::arrange(.data$year) %>%
      dplyr::mutate(dplyr::across(.cols = c(
        .data$`GDP deflator`,
        .data$`MER (LCU per US$)`,
        .data$`PPP conversion factor, GDP (LCU per international $)`),
        lin_int_ext))
  }

  if (replace_NAs %in% c("regional_average", "linear_regional_average")) {
    # Get GDP variable from source object, with its unit
    regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
    weight_var <- grep(regex_var, colnames(source), value = TRUE)[1]

    with_regions <- dplyr::rename(with_regions, "gdpuc_region" = .data$region)

    reg_averages <- source_adapted %>%
      dplyr::full_join(with_regions, by = "iso3c") %>%
      dplyr::group_by(.data$gdpuc_region, .data$year) %>%
      dplyr::mutate(dplyr::across(.cols = c(
        .data$`GDP deflator`,
        .data$`MER (LCU per US$)`,
        .data$`PPP conversion factor, GDP (LCU per international $)`),
        ~ sum(.x * eval(rlang::sym(weight_var)) /sum(eval(rlang::sym(weight_var)), na.rm = TRUE), na.rm = TRUE),
        .names = "ra_{.col}")) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$iso3c, .data$year, dplyr::starts_with("ra_"))

    source_adapted <- source_adapted %>%
      # Join the regional averages
      dplyr::left_join(reg_averages, by = c("iso3c", "year")) %>%
      # Mutate the 3 important columns
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(.cols = c(
        .data$`GDP deflator`,
        .data$`MER (LCU per US$)`,
        .data$`PPP conversion factor, GDP (LCU per international $)`),
        ~ if (is.na(.x)) {
          eval(rlang::sym(paste0("ra_", dplyr::cur_column())))
        } else .x),
        .keep = "unused") %>%
      dplyr::ungroup()
  }

  if (replace_NAs == 1) {
    source_adapted <- source_adapted %>%
      # Mutate the 3 important columns
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(.cols = c(
        .data$`GDP deflator`,
        .data$`MER (LCU per US$)`,
        .data$`PPP conversion factor, GDP (LCU per international $)`),
        ~ if (is.na(.x)) 1 else .x)) %>%
      dplyr::ungroup()
  }

  source_adapted
}


# Linear inter- and extrapolation of numeric vector
lin_int_ext <- function(x, n_ext = 5) {
  # Interpolate
  x <- zoo::na.approx(x, na.rm = FALSE)
  # Extrapolate linearly using the previous n_ext observations
  if (length(x[!is.na(x)]) < n_ext) {
    return(x)
  }
  # extrapolate first in one direction...
  n <- 0
  for (i in seq_along(x)) {
    if (is.na(x[i]) && n < n_ext) next
    n <- n + 1
    if (n > n_ext && is.na(x[i])) {
      x[i] <- x[i - 1] + (x[i - 1] - x[i - 1 - n_ext]) / n_ext
    }
  }
  # ...then in the other.
  for (i in rev(seq_along(x))) {
    if (is.na(x[i])) {
      x[i] <- x[i + 1] + (x[i + 1] - x[i + 1 + n_ext]) / n_ext
    }
  }
  x
}

