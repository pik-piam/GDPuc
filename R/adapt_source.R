adapt_source_USA <- function(gdp, source) {
  # Filter for USA, and then complete source for all countries with the unique pairs of all columns in source,
  # excluding iso3c.
  source %>%
    dplyr::filter(.data$iso3c == "USA") %>%
    tidyr::complete(iso3c = unique(gdp$iso3c), tidyr::nesting(!!!(rlang::syms(colnames(source)[-1]))))
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
    dplyr::bind_rows(
      gdp %>%
        {
          if ("gdpuc_region" %in% colnames(.)) dplyr::filter(., is.na(.data$gdpuc_region)) else .
        } %>%
        dplyr::select(tidyselect::all_of(hcol)) %>%
        dplyr::distinct() %>%
        dplyr::anti_join(source, by = hcol)
    ) %>%
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
      dplyr::mutate(dplyr::across(
        .cols = c("GDP deflator", "MER (LCU per US$)", "PPP conversion factor, GDP (LCU per international $)"),
        lin_int_ext
      ))
  }

  if ("regional_average" %in% replace_NAs) {
    # Get GDP variable from source object, with its unit
    regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
    weight_var <- grep(regex_var, colnames(source), value = TRUE)[1]

    with_regions <- dplyr::rename(with_regions, "gdpuc_region" = "region")

    reg_averages <- source_adapted %>%
      dplyr::full_join(with_regions, by = "iso3c") %>%
      dplyr::group_by(.data$gdpuc_region, .data$year) %>%
      dplyr::mutate(dplyr::across(
        .cols = c("GDP deflator", "MER (LCU per US$)", "PPP conversion factor, GDP (LCU per international $)"),
        ~ sum(.x * eval(rlang::sym(weight_var)) / sum(eval(rlang::sym(weight_var)), na.rm = TRUE), na.rm = TRUE),
        .names = "ra_{.col}"
      )) %>%
      dplyr::ungroup() %>%
      dplyr::select("iso3c", "year", dplyr::starts_with("ra_"))

    source_adapted <- source_adapted %>%
      # Join the regional averages
      dplyr::left_join(reg_averages, by = c("iso3c", "year")) %>%
      # Mutate the 3 important columns
      dplyr::rowwise() %>%
      dplyr::mutate(
        dplyr::across(
          .cols = c("GDP deflator", "MER (LCU per US$)", "PPP conversion factor, GDP (LCU per international $)"),
          ~ if (is.na(.x)) {
            eval(rlang::sym(paste0("ra_", dplyr::cur_column())))
          } else {
            .x
          }
        ),
        .keep = "unused"
      ) %>%
      dplyr::ungroup()
  }

  if ("with_USA" %in% replace_NAs) {
    USA_def_growth <- source %>%
      dplyr::filter(.data$iso3c == "USA") %>%
      dplyr::select("year", "gd" = "GDP deflator") %>%
      dplyr::mutate("gd" = .data$gd / dplyr::lag(.data$gd))

    source_adapted <- source_adapted %>%
      dplyr::filter(.data$iso3c %in% unique(gdp$iso3c)) %>%
      # Fill in the MER and PPPs with the growth rates from the USA (= 1)
      dplyr::group_by(.data$iso3c) %>%
      tidyr::fill(
        c("MER (LCU per US$)", "PPP conversion factor, GDP (LCU per international $)"),
        .direction = "updown"
      ) %>%
      dplyr::ungroup() %>%
      # For the deflator, we need to multiply the bordering values with the actual USA growth
      dplyr::left_join(USA_def_growth, by = dplyr::join_by("year")) %>%
      # Backward
      dplyr::arrange(-.data$year) %>%
      dplyr::mutate(
        `GDP deflator` = purrr::accumulate(
          dplyr::row_number(),
          ~ dplyr::coalesce(.data$`GDP deflator`[.y], .x / .data$gd[.y]),
          .init = NA
        )[-1],
        .by = c("iso3c")
      ) %>%
      dplyr::arrange(.data$iso3c, .data$year) %>%
      # Forward
      dplyr::mutate(
        `GDP deflator` = purrr::accumulate(
          dplyr::row_number(),
          ~ dplyr::coalesce(.data$`GDP deflator`[.y], .x * .data$gd[.y]),
          .init = NA
        )[-1],
        .by = c("iso3c")
      ) %>%
      dplyr::select(-"gd")

    # If there is no PPP data whatsoever for the country, use MERs
    source_adapted <- source_adapted %>%
      dplyr::mutate(
        "PPP conversion factor, GDP (LCU per international $)" = dplyr::if_else(
          is.na(.data$`PPP conversion factor, GDP (LCU per international $)`),
          .data$`MER (LCU per US$)`,
          .data$`PPP conversion factor, GDP (LCU per international $)`
        )
      )

    # If there is no deflator data whatsoever for the country, use US values
    ec <- dplyr::group_by(source_adapted, .data$iso3c) %>%
      dplyr::filter(all(is.na(.data$`GDP deflator`))) %>%
      dplyr::pull("iso3c") %>%
      unique()

    source_ec <- source %>%
      dplyr::filter(.data$iso3c == "USA") %>%
      tidyr::complete(iso3c = ec, tidyr::nesting(!!!(rlang::syms(colnames(source)[-1])))) %>%
      dplyr::filter(.data$iso3c %in% ec)

    source_adapted <- source_adapted %>% dplyr::filter(!.data$iso3c %in% ec) %>% dplyr::bind_rows(source_ec)
  }

  source_adapted
}


# Linear inter- and extrapolation (max_ext points away) of numeric vector using OLS over the closest n_ext points
lin_int_ext <- function(x, n_ext = 5, max_ext = 5) {
  # Interpolate interior NAs
  x <- zoo::na.approx(x, na.rm = FALSE)

  valid_idx <- which(!is.na(x))
  if (length(valid_idx) < n_ext) {
    return(x)
  }

  # Helper to compute OLS slope (beta) using all points in the window
  get_regression_slope <- function(indices, values) {
    if (length(indices) < 2) {
      return(0)
    }
    stats::cov(indices, values) / stats::var(indices)
  }

  # 1. Forward extrapolation (tail)
  last_valid <- utils::tail(valid_idx, 1)
  if (last_valid < length(x)) {
    # Get the last n_ext valid points and their indices
    tail_indices <- utils::tail(valid_idx, n_ext)
    tail_vals <- x[tail_indices]

    slope_fwd <- get_regression_slope(tail_indices, tail_vals)

    # Extrapolate forward up to max_ext steps away
    end_extrap_idx <- min(length(x), last_valid + max_ext)
    for (i in (last_valid + 1):end_extrap_idx) {
      x[i] <- x[i - 1] + slope_fwd
    }
  }

  # 2. Backward extrapolation (head)
  first_valid <- valid_idx[1]
  if (first_valid > 1) {
    # Get the first n_ext valid points and their indices
    head_indices <- utils::head(valid_idx, n_ext)
    head_vals <- x[head_indices]

    slope_bwd <- get_regression_slope(head_indices, head_vals)

    # Backcast backward up to max_ext steps away
    start_extrap_idx <- max(1, first_valid - max_ext)
    for (i in (first_valid - 1):start_extrap_idx) {
      x[i] <- x[i + 1] - slope_bwd
    }
  }

  x
}
