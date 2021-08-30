test_that("Abort with bad input", {
  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value2" = 100)
  expect_error(check_user_input(gdp))

  gdp <- tibble::tibble("iso3c" = "EUR", "value" = 100)
  expect_error(check_user_input(gdp))

  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2007, "value" = "100")
  expect_error(check_user_input(gdp))

  gdp <- "blabla"
  expect_error(check_user_input(gdp))
  gdp <- magclass::new.magpie()
  expect_error(check_user_input(gdp))

  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value" = 100)
  expect_error(check_user_input(gdp, unit_in = "blabla"))
  expect_error(check_user_input(gdp, unit_in = "current LCU", unit_out = 2))

  unit_in = "current Int$PPP"
  unit_out = "current US$MER"

  s <- rlang::new_quosure("blabla")
  expect_error(check_user_input(gdp, unit_in, unit_out, source = s))

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
})
