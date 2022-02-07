test_that("gdp argument", {
  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value2" = 100)
  expect_error(check_user_input(gdp), "Invalid 'gdp' argument. 'gdp' does not have the required 'value' column.")

  gdp <- tibble::tibble("iso3c" = "EUR", "value" = 100)
  expect_error(check_user_input(gdp), "Invalid 'gdp' argument. 'gdp' must have at least 3 columns.")

  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2007, "value" = "100")
  expect_error(check_user_input(gdp), "Invalid 'gdp' argument. The 'value' column is not numeric.")

  gdp <- "blabla"
  expect_error(check_user_input(gdp), "Invalid 'gdp' argument. 'gdp' is neither a data-frame nor a 'magpie' object.")
  gdp <- array()
  expect_error(check_user_input(gdp), "Invalid 'gdp' argument. 'gdp' is neither a data-frame nor a 'magpie' object.")

  gdp <- magclass::new.magpie()
  expect_error(check_user_input(gdp), "Invalid 'gdp' argument. No year information in magpie object.")
})


test_that("Abort with bad input", {

  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  expect_error(check_user_input(gdp, unit_in = "blabla"), "Invalid 'unit_in' argument")
  expect_error(check_user_input(gdp, unit_in = "current LCU", unit_out = 2), "Invalid 'unit_out' argument")

  unit_in = "current Int$PPP"
  unit_out = "current US$MER"

  s <- rlang::new_quosure(1)
  expect_error(check_user_input(gdp, unit_in, unit_out, source = s),
               "Invalid 'source' argument. 'source' is neither a data frame nor a charater.")
  s <- rlang::new_quosure("blabla")
  expect_error(check_user_input(gdp, unit_in, unit_out, source = s),
               glue::glue("Invalid 'source' argument. Has to be either one of the internal sources, or \\
                          valid custom source. Use print_source_info\\(\\) for information on available sources."))

  # Following doesn't work in covr for some reason
  my_bad_source <- tibble::tibble("iso3c" = 1)
  s <- rlang::new_quosure("my_bad_source")
  expect_error(check_user_input(gdp, unit_in, unit_out, source = s))

  s <- rlang::new_quosure("wb_wdi")

  expect_error(check_user_input(gdp, unit_in, unit_out, source = s, with_regions = "blabla"))
  with_regions = tibble::tibble("blabla" = "FRA", "region" = "USA")
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, with_regions = with_regions))
  with_regions = tibble::tibble("iso3c" = "FRA", "region" = "USA")
  expect_error(check_user_input(gdp, unit_in, "current LCU",  source = s, with_regions = with_regions))
  my_bad_source <- wb_wdi %>% dplyr::select(
    "iso3c",
    "year",
    "GDP deflator",
    "MER (LCU per US$)",
    "PPP conversion factor, GDP (LCU per international $)"
  )
  s <- rlang::new_quosure("my_bad_source")
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, with_regions = with_regions))

  s <- rlang::new_quosure("wb_wdi")
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, replace_NAs = 2, with_regions = NULL))

  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s,
                                with_regions = NULL, replace_NAs = NULL, verbose = "blabla"))

  expect_error(check_user_input(
    gdp,
    unit_in,
    unit_out,
    source = s,
    with_regions = NULL,
    replace_NAs = NULL,
    verbose = TRUE,
    return_cfs = "blabla"))
})
