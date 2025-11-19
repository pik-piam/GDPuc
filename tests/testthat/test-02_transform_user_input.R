test_that("source and unit year compatibility", {
  gdp <- tibble::tibble("iso3c" = "DEU", "year" = 2010, "value" = 100)

  expect_error(transform_user_input(gdp,
                                    unit_in = "constant 2010 LCU",
                                    unit_out = "constant 2100 LCU",
                                    source = "wb_wdi",
                                    use_USA_cf_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL,
                                    iso3c_column = "iso3c",
                                    year_column = "year"))
  expect_error(transform_user_input(gdp,
                                    unit_in = "constant 2100 LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    use_USA_cf_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL,
                                    iso3c_column = "iso3c",
                                    year_column = "year"))

})

test_that("unit and year availability compatibility", {
  gdp <- tibble::tibble("iso3c" = "DEU", "value" = 100)

  expect_error(transform_user_input(gdp,
                                    unit_in = "current LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    use_USA_cf_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL,
                                    iso3c_column = "iso3c",
                                    year_column = "year"),
               glue::glue("Invalid 'gdp' argument. 'gdp' does not have a 'year' column, required when \\
                          converting current values, and no other column could be identified in its stead."))
})

test_that("unit and year availability compatibility - magclass", {
  skip_if_not_installed("magclass")

  gdp <- magclass::new.magpie("USA", years = NULL, names = c("ssp1", "ssp2"), fill = 100)

  expect_error(transform_user_input(gdp,
                                    unit_in = "current LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    use_USA_cf_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL,
                                    iso3c_column = "iso3c",
                                    year_column = "year"),
               glue::glue("Invalid 'gdp' argument. 'gdp' does not have a 'year' column, required when \\
                          converting current values, and no other column could be identified in its stead."))
})
