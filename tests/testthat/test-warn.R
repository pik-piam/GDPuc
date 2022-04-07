# Currently 2 types of warnings in GDPuc: 1.NA generation and 2.inferring iso3c and year columns
test_that("convertGDP with option GDPuc.warn", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`),
                  iso3c == "USA") %>%
    dplyr::select("i" = iso3c, year, "value" = `GDP: linked series (current LCU)`) %>%
    dplyr::mutate(year = year + 10)

  expect_warning(expect_warning(withr::with_options(
    list(GDPuc.warn = TRUE),
    convertGDP(gdp_in, "current Int$PPP", "current LCU")
  )))
  expect_silent(withr::with_options(
    list(GDPuc.warn = FALSE),
    convertGDP(gdp_in, "current Int$PPP", "current LCU")
  ))
})
