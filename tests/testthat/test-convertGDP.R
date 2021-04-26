test_that("convertGDP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP: linked series (current LCU)`)

  gdp_conv <- convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP", "wb_wdi") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("convertGDP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select(iso3c, "y" = year, "value" = `GDP: linked series (current LCU)`)

  expect_warning(
    suppressMessages(
      convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP", "wb_wdi")
    )
  )

  gdp_conv <- suppressWarnings(suppressMessages(
    convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP", "wb_wdi") %>%
      dplyr::filter(!is.na(value))
  ))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year" = "y")) %>%
    dplyr::select( iso3c, "y" = year, "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("convertGDP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("r"=iso3c, year, "value" = `GDP: linked series (current LCU)`)

  expect_warning(
    suppressMessages(
      convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP", "wb_wdi")
    )
  )

  gdp_conv <- suppressWarnings(suppressMessages(
    convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP", "wb_wdi") %>%
      dplyr::filter(!is.na(value))
  ))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c" = "r", "year")) %>%
    dplyr::select("r" = iso3c, year, "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})
