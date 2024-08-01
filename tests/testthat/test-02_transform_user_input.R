test_that("source and unit year compatibility", {
  gdp <- tibble::tibble("iso3c" = "DEU", "year" = 2010, "value" = 100)

  expect_error(transform_user_input(gdp,
                                    unit_in = "constant 2010 LCU",
                                    unit_out = "constant 2100 LCU",
                                    source = "wb_wdi",
                                    use_USA_deflator_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL))
  expect_error(transform_user_input(gdp,
                                    unit_in = "constant 2100 LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    use_USA_deflator_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL))

})

test_that("unit and year availability compatibility", {
  gdp <- tibble::tibble("iso3c" = "DEU", "value" = 100)
  gdp2 <- magclass::new.magpie("USA", years = NULL, names = c("ssp1", "ssp2"), fill = 100)

  expect_error(transform_user_input(gdp,
                                    unit_in = "current LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    use_USA_deflator_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL),
               glue::glue("Invalid 'gdp' argument. 'gdp' does not have a 'year' column, required when \\
                          converting current values, and no other column could be identified in its stead."))
  expect_error(transform_user_input(gdp2,
                                    unit_in = "current LCU",
                                    unit_out = "constant 2010 LCU",
                                    source = "wb_wdi",
                                    use_USA_deflator_for_all = FALSE,
                                    with_regions = NULL,
                                    replace_NAs = NULL),
               glue::glue("Invalid 'gdp' argument. 'gdp' does not have a 'year' column, required when \\
                          converting current values, and no other column could be identified in its stead."))
})
