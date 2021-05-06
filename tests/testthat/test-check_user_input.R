test_that("Abort with bad input", {
  gdp <- tibble::tibble("iso3c" = "EUR", "year" = 2010, "value2" = 100)
  expect_error(check_user_input(gdp))

  gdp <- tibble::tibble("iso3c" = "EUR", "value" = 100)
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

  expect_error(check_user_input(gdp, unit_in, unit_out, source = "blabla"))

  # Following doesn't work in covr for some reason
  my_bad_source <- tibble::tibble("iso3c" = 1)
  expect_error(check_user_input(gdp, unit_in, unit_out, source = "my_bad_source"))

  expect_error(check_user_input(gdp, unit_in, unit_out, source = "wb_wdi", with_regions = "blabla"))
  with_regions = tibble::tibble("blabla" = "FRA", "region" = "USA")
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = "wb_wdi", with_regions = with_regions))
  with_regions = tibble::tibble("iso3c" = "FRA", "region" = "USA")
  expect_error(check_user_input(gdp, unit_in, "current LCU",  source = "wb_wdi", with_regions = with_regions))
  my_bad_source <- wb_wdi %>% dplyr::select(
    "iso3c",
    "year",
    "GDP deflator",
    "MER (LCU per US$)",
    "PPP conversion factor, GDP (LCU per international $)"
  )
  expect_error(check_user_input(gdp, unit_in, unit_out,  source = "my_bad_source", with_regions = with_regions))

  expect_error(check_user_input(gdp, unit_in, unit_out,  source = "wb_wdi",
                                with_regions = NULL, verbose = "blabla"))
})
