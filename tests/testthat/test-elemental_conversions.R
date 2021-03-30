test_that("current_LCU_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (current international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  gdp_conv <- current_LCU_2_current_IntPPP(gdp_in, "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_LCU_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP: linked series (current LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  gdp_conv <- current_IntPPP_2_current_LCU(gdp_in, "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_LCU_2_current_USMER", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  gdp_conv <- current_LCU_2_current_USMER(gdp_in, "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_USMER_2_current_LCU", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  gdp_conv <- current_USMER_2_current_LCU(gdp_in, "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  expect_equal(gdp_conv, gdp_out)
})
