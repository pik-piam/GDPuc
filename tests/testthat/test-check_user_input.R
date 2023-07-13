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

  if (rlang::is_installed("magclass")) {
    gdp <- magclass::new.magpie()
    expect_error(check_user_input(gdp), "Invalid 'gdp' argument. No year information in magpie object.")
  }
})

test_that("unit arguments", {
  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  expect_error(check_user_input(gdp, unit_in = "blabla"), "Invalid 'unit_in' argument")
  expect_error(check_user_input(gdp, unit_in = "current LCU", unit_out = 2), "Invalid 'unit_out' argument")
})

test_that("source argument", {
  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  unit_in <- "current Int$PPP"
  unit_out <- "current US$MER"

  s <- 1
  expect_error(check_user_input(gdp, unit_in, unit_out, source = s),
               "Invalid 'source' argument. 'source' is neither a data frame nor a string.")
  s <- "blabla"
  expect_error(check_user_input(gdp, unit_in, unit_out, source = s),
               glue::glue("Invalid 'source' argument. If 'source' is a string, it must be one of the internal \\
                          sources. Use print_source_info\\(\\) for information on available sources. \\
                          If you are trying to pass a custom source, pass the data frame directly, not its name."))

  # Following doesn't work in covr for some reason
  my_bad_source <- tibble::tibble("iso3c" = 1)
  expect_error(check_user_input(gdp, unit_in, unit_out, source = my_bad_source),
               "Invalid 'source' argument. Required columns are:(.*)")
})

test_that("with_regions argument", {
  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  unit_in <- "current Int$PPP"
  unit_out <- "current US$MER"
  s <- wb_wdi

  expect_error(check_user_input(gdp, unit_in, unit_out, source = s, with_regions = "blabla"))
  with_regions <- tibble::tibble("blabla" = "FRA", "region" = "USA")
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, with_regions = with_regions))
  with_regions <- tibble::tibble("iso3c" = "FRA", "region" = "USA")
  expect_error(check_user_input(gdp, unit_in, "current LCU",  source = s, with_regions = with_regions))
  my_bad_source <- wb_wdi %>% dplyr::select(
    "iso3c",
    "year",
    "GDP deflator",
    "MER (LCU per US$)",
    "PPP conversion factor, GDP (LCU per international $)"
  )
  s <- my_bad_source
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, with_regions = with_regions))
})

test_that("replace_NAs argument", {

  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  unit_in <- "current Int$PPP"
  unit_out <- "current US$MER"
  s <- wb_wdi

  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, replace_NAs = 2, with_regions = NULL),
               glue::glue("Invalid 'replace_NAs' argument. Has to be either NULL, NA, 0, 1, no_conversion, linear, \\
                          regional_average or a combination of the above."))
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = s, replace_NAs = c(0, 1), with_regions = NULL),
               glue::glue("Invalid 'replace_NAs' argument. The only accepted combinations of arguments start with \\
                          'linear', e.g. c\\('linear', 'no_conversion'\\)."))
  expect_error(
    check_user_input(gdp, unit_in, unit_out,  source = s, replace_NAs = "linear_regional_average", with_regions = NULL)
  )

  expect_error(
    check_user_input(gdp, unit_in, unit_out,  source = s, replace_NAs = "regional_average", with_regions = NULL),
    glue::glue("Using 'regional_average' requires a region mapping. The 'with_regions' argument can't be NULL.")
  )
})

test_that("boolean arguments", {

  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  unit_in <- "current Int$PPP"
  unit_out <- "current US$MER"
  s <- wb_wdi

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
