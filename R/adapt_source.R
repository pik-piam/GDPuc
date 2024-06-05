adapt_source_USA <- function(gdp, source, replace_NAs) {
  source_USA <- source %>%
    dplyr::filter(.data$iso3c == "USA") %>%
    dplyr::select("year", "USA_deflator" = "GDP deflator")

  source_adapted <- source %>%
    dplyr::left_join(source_USA, by = dplyr::join_by("year")) %>%
    dplyr::mutate("GDP deflator" = .data$USA_deflator)

  if (!is.null(replace_NAs) && replace_NAs[1] == "with_USA") {
    source_USA <- source %>% dplyr::filter(.data$iso3c == "USA")
    source_adapted <- purrr::map(unique(gdp$iso3c), ~dplyr::mutate(source_USA, "iso3c" = .x)) %>%
      purrr::list_rbind()
  }
  source_adapted
}

#
adapt_source <- function(gdp, source, with_regions, replace_NAs, require_year_column) {
  rlang::check_installed(c("zoo"), reason = "in order for 'replace_NAs' to work.")

  . <- NULL

  # Create adapted source object
  ## Columns by which to identify missing source entries
  hcol <- if (require_year_column) c("iso3c", "year") else "iso3c"
  source_adapted <- source %>%
    # Add any hcol combinations from gdp, not available in source
    dplyr::bind_rows(gdp %>%
                       {if ("gdpuc_region" %in% colnames(.)) dplyr::filter(., is.na(.data$gdpuc_region)) else .} %>%
                       dplyr::select(tidyselect::all_of(hcol)) %>%
                       dplyr::distinct() %>%
                       dplyr::anti_join(source, by = hcol)) %>%
    tidyr::complete(.data$iso3c, .data$year) %>%
    dplyr::filter(!is.na(.data$year))

  if (replace_NAs[1] == "linear") {
    # Make sure that source contains observations for every year between min and max years.
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
        "GDP deflator",
        "MER (LCU per US$)",
        "PPP conversion factor, GDP (LCU per international $)"),
        lin_int_ext))
  }

  if ("regional_average" %in% replace_NAs) {
    # Get GDP variable from source object, with its unit
    regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
    weight_var <- grep(regex_var, colnames(source), value = TRUE)[1]

    with_regions <- dplyr::rename(with_regions, "gdpuc_region" = "region")

    reg_averages <- source_adapted %>%
      dplyr::full_join(with_regions, by = "iso3c") %>%
      dplyr::group_by(.data$gdpuc_region, .data$year) %>%
      dplyr::mutate(dplyr::across(.cols = c(
        "GDP deflator",
        "MER (LCU per US$)",
        "PPP conversion factor, GDP (LCU per international $)"),
        ~ sum(.x * eval(rlang::sym(weight_var)) /sum(eval(rlang::sym(weight_var)), na.rm = TRUE), na.rm = TRUE),
        .names = "ra_{.col}")) %>%
      dplyr::ungroup() %>%
      dplyr::select("iso3c", "year", dplyr::starts_with("ra_"))

    source_adapted <- source_adapted %>%
      # Join the regional averages
      dplyr::left_join(reg_averages, by = c("iso3c", "year")) %>%
      # Mutate the 3 important columns
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(.cols = c(
        "GDP deflator",
        "MER (LCU per US$)",
        "PPP conversion factor, GDP (LCU per international $)"),
        ~ if (is.na(.x)) {
          eval(rlang::sym(paste0("ra_", dplyr::cur_column())))
        } else .x),
        .keep = "unused") %>%
      dplyr::ungroup()
  }

  if (1 %in% replace_NAs) {
    source_adapted <- source_adapted %>%
      # Mutate the 3 important columns
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(c("GDP deflator",
                                    "MER (LCU per US$)",
                                    "PPP conversion factor, GDP (LCU per international $)"),
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

