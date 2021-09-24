test_that("convertGDP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("iso3c", "year", "value" = `GDP (current LCU)`)

  gdp_conv <- convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP") %>%
    dplyr::filter(!is.na(value))

  gdp_out <- wb_wdi %>%
    dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
    dplyr::select("iso3c", "year", "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv, gdp_out)
})

test_that("convertGDP different column names", {
  gdp_in1 <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("r"=iso3c, year, "value" = `GDP (current LCU)`)
  gdp_in1b <- dplyr::mutate(gdp_in1, r = "")

  gdp_in2 <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select(iso3c, "y" = year, "value" = `GDP (current LCU)`)
  gdp_in2b <- dplyr::mutate(gdp_in2, y = "")

  expect_warning(
      convertGDP(gdp_in1, "current LCU", "constant 2017 Int$PPP", wb_wdi)
    )
  expect_error(
      convertGDP(gdp_in1b, "current LCU", "constant 2017 Int$PPP", wb_wdi)
  )
  expect_warning(
    convertGDP(gdp_in2, "current LCU", "constant 2017 Int$PPP", wb_wdi)
  )
  expect_error(
    convertGDP(gdp_in2b, "current LCU", "constant 2017 Int$PPP", wb_wdi)
  )

  gdp_conv1 <- suppressWarnings(suppressMessages(
    convertGDP(gdp_in1, "current LCU", "constant 2017 Int$PPP", wb_wdi) %>%
      dplyr::filter(!is.na(value))
  ))
  gdp_conv2 <- suppressWarnings(suppressMessages(
    convertGDP(gdp_in2, "current LCU", "constant 2017 Int$PPP", wb_wdi) %>%
      dplyr::filter(!is.na(value))
  ))

  gdp_out1 <- wb_wdi %>%
    dplyr::right_join(gdp_conv1, by = c("iso3c" = "r", "year")) %>%
    dplyr::select("r" = iso3c, year, "value" = `GDP, PPP (constant 2017 international $)`)
  gdp_out2 <- wb_wdi %>%
    dplyr::right_join(gdp_conv2, by = c("iso3c", "year" = "y")) %>%
    dplyr::select( iso3c, "y" = year, "value" = `GDP, PPP (constant 2017 international $)`)

  expect_equal(gdp_conv1, gdp_out1)
  expect_equal(gdp_conv2, gdp_out2)
})



test_that("convertGDP magpie object", {
  gdp_in <- magclass::new.magpie("USA",
                                 years = c(2001, 2002),
                                 names = c("ssp1", "ssp2"),
                                 fill = 100)
  magclass::getSets(gdp_in)[1] <- c("r")

  gdp_conv <- convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP")

  expect_s4_class(gdp_conv, "magpie")
  expect_mapequal(magclass::getSets(gdp_in), magclass::getSets(gdp_conv))
})



test_that("convertGDP data.frame object", {
  gdp_in <- data.frame("iso3c" = "USA", "year" = c(2001, 2002), "value" = 100:101)
  gdp_conv <- convertGDP(gdp_in, "constant 2005 US$MER", "constant 2015 US$MER")

  expect_s3_class(gdp_conv, "data.frame", exact = TRUE)
})



test_that("convertGDP with verbose = TRUE", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`),
                  iso3c == "USA") %>%
    dplyr::select(iso3c, year, "value" = `GDP: linked series (current LCU)`)

  expect_message(
    expect_message(
      expect_message(
        expect_message(
          convertGDP(gdp_in, "constant 2010 Int$PPP", "constant 2017 Int$PPP", verbose = TRUE)
        )
      )
    )
  )

})



test_that("convertGDP unit_in == unit_out", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select(iso3c, year, "value" = `GDP: linked series (current LCU)`)

  expect_message(convertGDP(gdp_in, "current LCU", "current LCU", verbose = TRUE))

  gdp_conv <- convertGDP(gdp_in, "current LCU", "current LCU")

  expect_equal(gdp_conv, gdp_in)
})



test_that("convertGDP missing conversion factors", {
  gdp_1 <- tibble::tibble("iso3c" = "USA", "year" = 1010, "value" = 100)
  gdp_2 <- tibble::tibble("iso3c" = "JJJ", "year" = 2010, "value" = 100)
  gdp_3 <- tibble::tibble("iso3c" = "USA", "year" = c(1010,2010), "value" = 100)

  expect_error(convertGDP(gdp_1, "current LCU", "current Int$PPP"))
  expect_error(convertGDP(gdp_2, "current LCU", "current Int$PPP"))
  expect_warning(convertGDP(gdp_3, "current LCU", "current Int$PPP"))
})




test_that("convertGDP with regions", {
  gdp <- tibble::tibble("iso3c" = c("JPN", "EUR", "DEU"), "year" = 2010, "value" = 100)
  gdp2 <- tibble::tibble("region" = c("JPN", "EUR", "DEU"), "period" = 2010, "value" = 100)
  with_regions <- tibble::tibble("iso3c" = c("FRA", "ESP", "DEU"), "region" = "EUR")

  gdp_conv <- convertGDP(gdp,
                         unit_in = "constant 2015 Int$PPP",
                         unit_out = "constant 2017 Int$PPP",
                         with_regions = with_regions)

  gdp_conv2 <- suppressWarnings(suppressWarnings(
    convertGDP(gdp2,
               unit_in = "constant 2015 Int$PPP",
               unit_out = "constant 2017 Int$PPP",
               with_regions = with_regions)
  ))

  gdp_conv_b <- convertGDP(gdp,
                           unit_in = "constant 2017 Int$PPP",
                           unit_out = "constant 2017 Int$PPP",
                           with_regions = with_regions)

  gdp_conv3 <- convertGDP(gdp,
                          unit_in = "constant 2015 US$MER",
                          unit_out = "constant 2017 Int$PPP",
                          with_regions = with_regions)

  expect_equal(gdp$iso3c, gdp_conv$iso3c)
  expect_equal(gdp_conv$value, gdp_conv2$value)
  expect_true(all(gdp_conv3$value != gdp_conv$value))
})


test_that("convertGDP replace missing conversion factors", {
  # wb_wi does not have info for AIA, so AIA is used for testing here
  gdp <- tidyr::expand_grid("iso3c" = c("AIA", "FRA", "DEU", "USA", "EUR"), "year" = c(2010, 3010),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)
  gdp2 <- tidyr::expand_grid("iso3c" = c("AIA", "USA"), "year" = c(2010, 3010), "value" = 100)
  with_regions <- tibble::tibble("iso3c" = c("FRA", "ESP", "DEU", "BEL", "AIA"), "region" = "EUR")

  expect_warning(convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP"))
  expect_error(convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP", replace_NAs = FALSE))
  expect_error(convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP", replace_NAs = "regional_average"))

  gdp_conv <- suppressWarnings(
    convertGDP(gdp,
               unit_in = "constant 2005 Int$PPP",
               unit_out = "constant 2005 US$MER")
  )

  gdp_conv2 <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2005 US$MER",
                          replace_NAs = 0)

  gdp_conv3 <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2005 US$MER",
                          replace_NAs = 1)

  gdp_conv4 <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2005 US$MER",
                          with_regions = with_regions,
                          replace_NAs = "regional_average")

  gdp_conv5 <- convertGDP(gdp2,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2005 US$MER",
                          with_regions = with_regions,
                          replace_NAs = "regional_average")

  expect_true(any(is.na(gdp_conv$value)))
  expect_true(!any(is.na(gdp_conv2$value)))
  expect_true(!any(is.na(gdp_conv3$value)))
  expect_true(!any(is.na(gdp_conv4$value)))

  expect_equal(gdp$iso3c, gdp_conv2$iso3c)
  expect_equal(gdp$iso3c, gdp_conv3$iso3c)
  expect_equal(gdp$iso3c, gdp_conv4$iso3c)
  expect_equal(gdp2$iso3c, gdp_conv5$iso3c)
})
