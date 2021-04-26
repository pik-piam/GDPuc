test_that("current_LCU_2_constant_IntPPP_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  gdp_conv <- current_LCU_2_constant_IntPPP_base_y(gdp_in, 2017, "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_LCU_2_constant_USMER_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (constant 2010 US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  gdp_conv <- current_LCU_2_constant_USMER_base_y(gdp_in, 2010, "wb_wdi")%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant 2010 US$)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_IntPPP_2_current_USMER", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (current US$)`),
                  `GDP (current LCU)` == `GDP: linked series (current LCU)`) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  gdp_conv <- current_IntPPP_2_current_USMER(gdp_in, "wb_wdi")%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("current_USMER_2_current_IntPPP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (current international $)`),
                  `GDP (current LCU)` == `GDP: linked series (current LCU)`) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current US$)`)

  gdp_conv <- current_USMER_2_current_IntPPP(gdp_in, "wb_wdi")%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (current international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("constant_IntPPP_base_x_2_constant_USMER_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP (constant 2010 US$)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  gdp_conv <- constant_IntPPP_base_x_2_constant_USMER_base_y(gdp_in, 2017, 2010, "wb_wdi")%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant 2010 US$)`)

  expect_equal(gdp_conv, gdp_out, max_diffs = Inf)
})

test_that("constant_USMER_base_x_2_constant_IntPPP_base_y", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (constant 2010 US$)`)

  gdp_conv <- constant_USMER_base_x_2_constant_IntPPP_base_y(gdp_in, 2010, 2017, "wb_wdi")%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("constant_IntPPP_base_x_2_current_LCU", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP: linked series (current LCU)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  gdp_conv <- constant_IntPPP_base_x_2_current_LCU(gdp_in, 2017, "wb_wdi")%>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  expect_equal(gdp_conv, gdp_out)
})


test_that("constant_IntPPP_base_x_2_constant_LCU", {
  gdp_out_c <- wb_wdi %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`) %>%
    current_LCU_2_constant_LCU_base_y(2010, "wb_wdi", linked = TRUE)%>%
    dplyr::filter(!is.na(value))

  gdp_in <- wb_wdi %>%
    dplyr::right_join(gdp_out_c %>%
                        dplyr::select(-value),
                      by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  gdp_conv <- constant_IntPPP_base_x_2_constant_LCU_base_y(gdp_in, 2017, 2010, "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- gdp_out_c %>%
    dplyr::right_join(gdp_conv %>%
                        dplyr::select(-value),
                      by = c("iso3c", "year"))

  expect_equal(gdp_conv, gdp_out)
})
