#' Transform user input for package internal use
#'
#' @inheritParams check_user_input
#'
#' @return List
transform_user_input <- function(gdp, unit_in, unit_out, source, with_regions, replace_NAs) {

  . = NULL

  # Convert to tibble, if necessary
  if (class(gdp)[1] == "magpie"){
    gdp <- mag2tibb(gdp)
  }

  # Extract base years if they exist, and adjust string
  if (grepl("constant", unit_in)) {
    base_x <- stringr::str_match(unit_in, "constant (....)")[,2] %>% as.double()
    unit_in <- sub(base_x, "YYYY", unit_in) %>%
      paste0(" base x")
  }
  if (grepl("constant", unit_out)) {
    base_y <- stringr::str_match(unit_out, "constant (....)")[,2] %>% as.double()
    unit_out <- sub(base_y, "YYYY", unit_out) %>%
      paste0(" base y")
  }

  # Rename columns if necessary
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp)

    if (length(i_iso3c) != 1) {
      abort("Invalid 'gdp' argument. `gdp` has no 'iso3c' column, and no other \\
               column could be identified in its stead.")
    }

    warn("No 'iso3c' column in 'gdp' argument. Using '{i_iso3c}' column instead.")

    gdp <- gdp %>%
      dplyr::rename("iso3c" = !!rlang::sym(i_iso3c)) %>%
      dplyr::arrange("iso3c", 1)
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp)

    if (length(i_year) != 1) {
      abort("Invalid 'gdp' argument. 'gdp' does not have the required \\
               'year' column, and no other column could be identified in its stead.")
    }

    warn("No 'year' column in 'gdp' argument. Using '{i_year}' column instead.")

    gdp <- gdp %>%
      dplyr::rename("year" = !!rlang::sym(i_year)) %>%
      dplyr::arrange("year", 2)
  }

  # Evaluate source
  q <- source
  q_expr <- rlang::quo_get_expr(q)
  q_env <- rlang::quo_get_env(q)
  source_name <- as.character(q_expr)
  if (is.character(q_expr)) {
    q <- rlang::quo_set_expr(q, rlang::sym(q_expr))
  }
  if (!exists(source_name, q_env)) {
    q <- rlang::quo_set_env(q, rlang::current_env())
  }
  source <- rlang::eval_tidy(q)


  # Disaggregate to country level if with_regions is activated
  if (!is.null(with_regions) && any(gdp$iso3c %in% with_regions$region)) {
    # Separate real regions from county-regions
    my_reg <- intersect(gdp$iso3c, unique(with_regions$region))
    gdp_reg <- dplyr::filter(gdp, .data$iso3c %in% my_reg)
    gdp <- dplyr::filter(gdp, ! .data$iso3c %in% my_reg)

    # Disaggregate regions
    weight_unit <- stringr::str_match(unit_in, "\\$(...)")[2]
    weight_year <- base_x
    with_regions <- dplyr::rename(with_regions, "gdpuc_region" = .data$region)
    gdp_reg <- disaggregate_regions(gdp_reg, with_regions, weight_unit, weight_year, source)

    # Bind original countries and countries from disaggregation (duplicates now possible)
    gdp <- dplyr::bind_rows(gdp, gdp_reg)
  }

  # Need this to check for existence of base_y and base_x
  this_e <- environment()

  # Use different source if required
  if (!is.null(replace_NAs)) {

    # Create adapted source object
    source_adapted <- source %>%
      # Add any iso3c-year combinations from gdp, not available in source
      dplyr::bind_rows(gdp %>%
                         {if("gdpuc_region" %in% colnames(with_regions)) {
                           dplyr::filter(., is.na(.data$gdpuc_region))
                           } else {.} } %>%
                         dplyr::select(.data$iso3c, .data$year) %>%
                         dplyr::anti_join(source, by = c("iso3c", "year"))) %>%
      # Add any missing iso3c-base_y combinations, if base_y exists
      {if (exists("base_y", envir = this_e, inherits = FALSE)) {
        h1 <- .
        dplyr::bind_rows(., expand.grid("iso3c" = unique(gdp$iso3c), "year" = base_y) %>%
                           tibble::as_tibble() %>%
                           dplyr::anti_join(h1, by = c("iso3c", "year")))
      } else . }

    if (replace_NAs == 1) {
      source_adapted <- source_adapted %>%
        # Mutate the 3 important columns
        dplyr::rowwise() %>%
        dplyr::mutate(dplyr::across(.cols = c(.data$`GDP deflator`,
                                              .data$`MER (LCU per US$)`,
                                              .data$`PPP conversion factor, GDP (LCU per international $)`),
                                    ~ if (is.na(.x)) {1} else {.x})) %>%
        dplyr::ungroup()
    }

    if (replace_NAs == "regional_average") {
      # Get GDP variable from source object, with its unit
      regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
      weight_var <- grep(regex_var, colnames(source), value = TRUE)[1]

      # Rename, if necessary
      if(!"gdpuc_region" %in% colnames(with_regions)) {
        with_regions <- dplyr::rename(with_regions, "gdpuc_region" = .data$region)
      }

      reg_averages <- source_adapted %>%
        dplyr::full_join(with_regions, by = "iso3c") %>%
        dplyr::group_by(.data$gdpuc_region, .data$year) %>%
        dplyr::mutate(dplyr::across(.cols = c(.data$`GDP deflator`,
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
        dplyr::mutate(dplyr::across(.cols = c(.data$`GDP deflator`,
                                              .data$`MER (LCU per US$)`,
                                              .data$`PPP conversion factor, GDP (LCU per international $)`),
                                    ~ if (is.na(.x))
                                      {eval(rlang::sym(paste0("ra_", dplyr::cur_column())))}
                                    else {.x}),
                      .keep = "unused") %>%
        dplyr::ungroup()
    }

    source_name <- paste0(source_name, "_adapted")
    source <- source_adapted
  }

  # Check availability of required conversion factors in source
  if (length(intersect(unique(gdp$year), unique(source$year))) == 0) {
    abort("No information in source {crayon::bold(source_name)} for years in 'gdp'.")
  }
  if (length(intersect(unique(gdp$iso3c), unique(source$iso3c))) == 0 ) {
    abort("No information in source {crayon::bold(source_name)} for countries in 'gdp'.")
  }

  out <- list("gdp" = gdp,
              "unit_in" = unit_in,
              "unit_out" = unit_out,
              "source" = source,
              "source_name" = source_name) %>%
    {if (exists("base_x", envir = this_e, inherits = FALSE)) c(., "base_x" = base_x) else .} %>%
    {if (exists("base_y", envir = this_e, inherits = FALSE)) c(., "base_y" = base_y) else .}

  return(out)
}












#' Take gdp data at regional level and disaggregate to country level
#'
#' @param weight_unit A string, either "PPP" or "MER
#' @param weight_year An integer equal to the base_x year
#' @inheritParams convertGDP
#'
#' @return A tibble with the values gdp at country level
disaggregate_regions <- function (gdp, with_regions, weight_unit, weight_year, source) {

  # Get GDP variable from source object, with its unit
  regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
  regex_year <- "GDP, PPP \\(constant (....) international \\$\\)"
  share_var <- grep(regex_var, colnames(source), value = TRUE)[1]
  share_year <- stringr::str_match(share_var, regex_year)[,2]
  unit_in <- paste("constant", share_year, "Int$PPP")

  # Convert that variable to desired unit, and compute shares of GDP per region
  if (weight_unit == "PPP") {
    unit_out <- paste("constant", weight_year, "Int$PPP")
  } else {
    unit_out <- paste("constant", weight_year, "US$MER")
  }

  shares <- source %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::all_of(share_var)) %>%
    dplyr::left_join(with_regions, by = "iso3c") %>%
    dplyr::filter(.data$year == weight_year, !is.na(.data$gdpuc_region)) %>%
    convertGDP(unit_in, unit_out, source = source) %>%
    dplyr::group_by(.data$gdpuc_region) %>%
    dplyr::mutate(share = .data$value / sum(.data$value, na.rm = TRUE), .keep = "unused") %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$year) %>%
    suppressWarnings()

  # Dissagregate regions
  gdp %>%
    dplyr::rename("gdpuc_region" = .data$iso3c) %>%
    dplyr::right_join(with_regions, by = "gdpuc_region") %>%
    dplyr::left_join(shares, by = c("gdpuc_region", "iso3c")) %>%
    dplyr::mutate(value = .data$value * .data$share, .keep = "unused")
}




#' Transform user input for package internal use
#'
#' @param x A tibble, the result of the internal conversion
#' @inheritParams convertGDP
#'
#' @return x, with the same type and names of gdp
transform_internal <- function(x, gdp, with_regions) {

  if (!is.null(with_regions) && "gdpuc_region" %in% colnames(x)) {
    x_reg <- dplyr::filter(x, !is.na(.data$gdpuc_region))
    x <- x %>%
      dplyr::filter(is.na(.data$gdpuc_region)) %>%
      dplyr::select(-.data$gdpuc_region)

    x_reg <- x_reg %>%
      dplyr::group_by(.data$gdpuc_region, .data$year) %>%
      dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename("iso3c" = .data$gdpuc_region)

    i_iso3c <- if (! "iso3c" %in% colnames(gdp)) smart_select_iso3c(gdp) else "iso3c"

    x <- x %>%
      dplyr::bind_rows(x_reg) %>%
      dplyr::arrange(factor(.data$iso3c, levels = unique(gdp[[i_iso3c]])))
  }

  # Transform into original gdp type
  if (class(gdp)[1] == "magpie"){
    x <- magclass::as.magpie(x[,-1], spatial="iso3c", temporal="year")
    return(x)
  }

  # Get original iso3c and year column names
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp)
    x <- x %>% dplyr::rename(!!rlang::sym(i_iso3c) := "iso3c")
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp)
    x <- x %>% dplyr::rename(!!rlang::sym(i_year) := "year")
  }

  x
}



smart_select_iso3c <- function(gdp) {
  gdp %>%
    dplyr::select(tidyselect::vars_select_helpers$where(
      ~ (is.character(.x) || is.factor(.x)) &&
        all(nchar(as.character(.x)) == 3) &&
        all(.x == toupper(.x))
    )) %>%
    colnames()
}

smart_select_year <- function(gdp) {
  gdp %>%
    dplyr::select(tidyselect::vars_select_helpers$where(
      ~ is.numeric(.x) &&
        all(!is.na(.x)) &&
        all(nchar(as.character(.x)) == 4)
    )) %>%
    colnames()
}

get_source_name <- function(source) {

}



