# 
adapt_source <- function(gdp, base_y, source, with_regions, replace_NAs) {
  . <- NULL
  # Create adapted source object
  source_adapted <- source %>%
    # Add any iso3c-year combinations from gdp, not available in source
    dplyr::bind_rows(gdp %>%
                       {if ("gdpuc_region" %in% colnames(.)) {
                         dplyr::filter(., is.na(.data$gdpuc_region))
                       } else . } %>%
                       dplyr::select(.data$iso3c, .data$year) %>%
                       dplyr::distinct() %>%
                       dplyr::anti_join(source, by = c("iso3c", "year"))) %>%
    # Add any missing iso3c-base_y combinations, if base_y exists
    {if (!is.null(base_y)) {
      h1 <- .
      dplyr::bind_rows(h1,
                       expand.grid("iso3c" = unique(gdp$iso3c),
                                   "year" = base_y) %>%
                         tibble::as_tibble() %>%
                         dplyr::anti_join(h1, by = c("iso3c", "year")))
    } else . }

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

  if (replace_NAs == "regional_average") {
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
        ~ sum(.x * eval(rlang::sym(weight_var)) /
                sum(eval(rlang::sym(weight_var)), na.rm = TRUE),
              na.rm = TRUE),
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

  source_adapted
}
