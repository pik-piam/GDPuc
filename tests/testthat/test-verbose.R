test_that("convertGDP with verbose = TRUE", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`),
                  iso3c == "USA") %>%
    dplyr::select(iso3c, year, "value" = `GDP: linked series (current LCU)`)

  expect_message(
    expect_message(
      expect_message(
        expect_message(
          convertGDP(gdp_in, "constant 2010 Int$PPP", "constant 2017 Int$PPP", verbose = TRUE),
          "Converting GDP with conversion factors from wb_wdi:"
        )
      )
    )
  )
})

test_that("convertGDP with option GDPuc.verbose = TRUE", {
  withr::local_options(list(GDPuc.verbose = TRUE))

  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`),
                  iso3c == "USA") %>%
    dplyr::select(iso3c, year, "value" = `GDP: linked series (current LCU)`)

  expect_message(
    expect_message(
      expect_message(
        expect_message(
          convertGDP(gdp_in, "constant 2010 Int$PPP", "constant 2017 Int$PPP"),
          "Converting GDP with conversion factors from wb_wdi:"
        )
      )
    )
  )
})
