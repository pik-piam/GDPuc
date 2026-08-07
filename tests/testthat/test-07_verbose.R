test_that("convertGDP with verbose = TRUE", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c == "USA") %>%
    dplyr::select("iso3c", "year", "value" = "GDP (constant LCU)")

  # 2 messages expected for this conversion
  expect_message(
    expect_message(
      convertGDP(gdp_in, "constant 2011 LCU", "constant 2010 LCU", verbose = TRUE),
      "Converting GDP with conversion factors from wb_wdi:"
    )
  )
})

test_that("convertGDP with option GDPuc.verbose = TRUE", {
  withr::local_options(list(GDPuc.verbose = TRUE))

  gdp_in <- wb_wdi %>%
    dplyr::filter(iso3c == "USA") %>%
    dplyr::select("iso3c", "year", "value" = "GDP (constant LCU)")

  # 3 messages expected for this conversion
  expect_message(
    expect_message(
      expect_message(
        convertGDP(gdp_in, "constant 2010 LCU", "constant 2014 Int$PPP"),
        "Converting GDP with conversion factors from wb_wdi:"
      )
    )
  )
})
