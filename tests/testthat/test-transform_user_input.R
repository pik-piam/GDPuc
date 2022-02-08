test_that("source and unit year compatibility", {
  gdp <- tibble::tibble("iso3c" = "DEU", "year" = 2010, "value" = 100)

  expect_error(transform_user_input(gdp,
                                    unit_in = "constant 2010 LCU",
                                    unit_out = "constant 2100 LCU",
                                    source = "wb_wdi",
                                    with_regions = NULL,
                                    replace_NAs = NULL))
  expect_error(transform_user_input(gdp,
                                    unit_in = "constant 2100 LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    with_regions = NULL,
                                    replace_NAs = NULL))

})
