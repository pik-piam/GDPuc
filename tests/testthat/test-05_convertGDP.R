test_that("convertGDP", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!iso3c %in% bad_countries,
                  !is.na(`GDP, PPP (constant 2017 international $)`)) %>%
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
    dplyr::filter(!iso3c %in% bad_countries,
                  !is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select("r" = iso3c, year, "value" = `GDP (current LCU)`)
  gdp_in1b <- dplyr::mutate(gdp_in1, r = "")

  gdp_in2 <- wb_wdi %>%
    dplyr::filter(!iso3c %in% bad_countries,
                  !is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select(iso3c, "y" = year, "value" = `GDP (current LCU)`)
  gdp_in2b <- dplyr::mutate(gdp_in2, y = "")

  expect_warning(
      convertGDP(gdp_in1, "current LCU", "constant 2017 Int$PPP", wb_wdi)
    )
  expect_no_warning(
    convertGDP(gdp_in1, "current LCU", "constant 2017 Int$PPP", wb_wdi, iso3c_column = "r")
  )
  expect_error(
      convertGDP(gdp_in1b, "current LCU", "constant 2017 Int$PPP", wb_wdi)
  )
  expect_warning(
    convertGDP(gdp_in2, "current LCU", "constant 2017 Int$PPP", wb_wdi)
  )
  expect_no_warning(
    convertGDP(gdp_in2, "current LCU", "constant 2017 Int$PPP", wb_wdi, year_column = "y")
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
  skip_if_not_installed("magclass")
  gdp_in <- magclass::new.magpie("USA",
                                 years = c(2001, 2002),
                                 names = c("ssp1", "ssp2"),
                                 fill = 100)
  gdp_in2 <- magclass::new.magpie("USA.FRA",
                                  years = c(2001, 2002),
                                  names = c("ssp1", "ssp2"),
                                  fill = 100)
  gdp_in3 <- magclass::new.magpie("USA",
                                  years = NULL,
                                  names = c("ssp1", "ssp2"),
                                  fill = 100)
  gdp_conv  <- convertGDP(gdp_in, "current LCU", "constant 2017 Int$PPP")
  gdp_conv2 <- convertGDP(gdp_in2, "current LCU", "constant 2017 Int$PPP")
  gdp_conv3 <- convertGDP(gdp_in3, "constant 2005 LCU", "constant 2017 Int$PPP")

  expect_s4_class(gdp_conv, "magpie")
  expect_s4_class(gdp_conv2, "magpie")
  expect_s4_class(gdp_conv3, "magpie")
  expect_mapequal(magclass::getSets(gdp_in),  magclass::getSets(gdp_conv))
  expect_mapequal(magclass::getSets(gdp_in2), magclass::getSets(gdp_conv2))
  expect_mapequal(magclass::getSets(gdp_in3), magclass::getSets(gdp_conv3))
})



test_that("convertGDP data.frame object", {
  gdp_in <- data.frame("iso3c" = "USA", "year" = c(2001, 2002), "value" = 100:101)
  gdp_conv <- convertGDP(gdp_in, "constant 2005 US$MER", "constant 2015 US$MER")

  expect_s3_class(gdp_conv, "data.frame", exact = TRUE)
})



test_that("convertGDP unit_in == unit_out", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!iso3c %in% bad_countries,
                  !is.na(`GDP, PPP (constant 2017 international $)`)) %>%
    dplyr::select(iso3c, year, "value" = `GDP: linked series (current LCU)`)

  expect_message(convertGDP(gdp_in, "current LCU", "current LCU", verbose = TRUE),
                 "No conversion: unit_in = unit_out.")

  gdp_conv <- convertGDP(gdp_in, "current LCU", "current LCU")

  expect_equal(gdp_conv, gdp_in)
})



test_that("convertGDP missing conversion factors", {
  gdp_1 <- tibble::tibble("iso3c" = "USA", "year" = 1010, "value" = 100)
  gdp_2 <- tibble::tibble("iso3c" = "JJJ", "year" = 2010, "value" = 100)
  gdp_3 <- tibble::tibble("iso3c" = "USA", "year" = c(1010,2010), "value" = 100)

  expect_error(convertGDP(gdp_1, "current LCU", "current Int$PPP"))
  expect_error(convertGDP(gdp_2, "current LCU", "current Int$PPP"))
  expect_warning(convertGDP(gdp_3, "current LCU", "current Int$PPP"),
                 "NAs have been generated for countries lacking conversion factors!")
})




test_that("convertGDP with regions, custom data-frame", {
  gdp <- tibble::tibble("iso3c" = c("JPN", "DEU", "ESP", "FRA", "EUR"), "value" = 100)
  with_regions <- tibble::tibble("iso3c" = c("DEU", "ESP", "FRA"), "region" = "EUR")

  gdp_conv <- convertGDP(gdp,
                         unit_in = "constant 2015 Int$PPP",
                         unit_out = "constant 2017 Int$PPP",
                         with_regions = with_regions)

  expect_equal(gdp$iso3c, gdp_conv$iso3c)
  expect_true(all(!is.na(gdp_conv$value)))

  shares <- wb_wdi %>%
    dplyr::select("iso3c", "year", "value" = "GDP, PPP (constant 2017 international $)") %>%
    dplyr::left_join(dplyr::rename(with_regions, "gdpuc_region" = "region"), by = "iso3c") %>%
    dplyr::filter(.data$year == 2015, !is.na(.data$gdpuc_region)) %>%
    convertGDP("constant 2017 Int$PPP", "constant 2015 Int$PPP", source = wb_wdi, verbose = FALSE) %>%
    dplyr::mutate(share = .data$value / sum(.data$value, na.rm = TRUE), .by = "gdpuc_region")

  gdp2 <- tibble::tibble("iso3c" = c("JPN", "DEU", "ESP", "FRA", "EUR"),
                         "value" = c(100, 100 * shares$share[1], 100 * shares$share[2], 100 * shares$share[3], 100))

  gdp_conv2 <- convertGDP(gdp2,
                          unit_in = "constant 2015 Int$PPP",
                          unit_out = "constant 2017 Int$PPP",
                          with_regions = with_regions)

  expect_equal(sum(gdp_conv2$value[2:4]), gdp_conv2$value[5])


  shares <- wb_wdi %>%
    dplyr::select("iso3c", "year", "value" = "GDP (constant 2015 US$)") %>%
    dplyr::left_join(dplyr::rename(with_regions, "gdpuc_region" = "region"), by = "iso3c") %>%
    dplyr::filter(.data$year == 2015, !is.na(.data$gdpuc_region)) %>%
    convertGDP("constant 2015 US$MER", "constant 2015 US$MER", source = wb_wdi, verbose = FALSE) %>%
    dplyr::mutate(share = .data$value / sum(.data$value, na.rm = TRUE), .by = "gdpuc_region")

  gdp2 <- tibble::tibble("iso3c" = c("JPN", "DEU", "ESP", "FRA", "EUR"),
                         "value" = c(100, 100 * shares$share[1], 100 * shares$share[2], 100 * shares$share[3], 100))

  gdp_conv2 <- convertGDP(gdp2,
                          unit_in = "constant 2015 US$MER",
                          unit_out = "constant 2017 US$MER",
                          with_regions = with_regions)

  expect_equal(sum(gdp_conv2$value[2:4]), gdp_conv2$value[5])
})

test_that("convertGDP with regions, madrat data-frame", {
  skip_if_not_installed("madrat")
  gdp <- tibble::tibble("iso3c" = c("JPN", "EUR", "DEU"), "value" = 100)
  expect_no_warning(convertGDP(gdp,
                               unit_in = "constant 2015 Int$PPP",
                               unit_out = "constant 2017 Int$PPP",
                               with_regions = "regionmappingH12.csv"))

})

test_that("convertGDP using US conversion factors", {
  skip_if_not_installed("zoo")

  gdp_1 <- tibble::tibble("iso3c" = "USA", "year" = 2010, "value" = 100)
  gdp_2 <- tibble::tibble("iso3c" = "DEU", "year" = 2010, "value" = 100)
  gdp_3 <- tibble::tibble("iso3c" = "JJJ", "year" = 2010, "value" = 100)

  gdp1_conv <- convertGDP(gdp_1,
                          unit_in = "constant 2015 LCU",
                          unit_out = "constant 2017 LCU")
  gdp2_conv <- convertGDP(gdp_2,
                          unit_in = "constant 2015 LCU",
                          unit_out = "constant 2017 LCU",
                          use_USA_cf_for_all = TRUE)
  gdp3_conv <- convertGDP(gdp_3,
                          unit_in = "constant 2015 LCU",
                          unit_out = "constant 2017 LCU",
                          replace_NAs = "with_USA")

  expect_equal(gdp1_conv, gdp2_conv %>% dplyr::mutate(iso3c = "USA"))
  expect_equal(gdp1_conv, gdp3_conv %>% dplyr::mutate(iso3c = "USA"))
})

test_that("convert euros", {
  gdp1_conv <- convertSingle(100, "USA",
                             unit_in = "constant 2015 LCU",
                             unit_out = "constant 2017 €")

  gdp2_conv <- convertSingle(100, "USA",
                             unit_in = "constant 2015 LCU",
                             unit_out = "constant 2017 EUR")

  gdp3_conv <- convertSingle(100, "DEU",
                             unit_in = "constant 2010 LCU",
                             unit_out = "constant 2017 €")

  gdp4_conv <- convertSingle(100, "DEU",
                             unit_in = "constant 2010 €",
                             unit_out = "constant 2017 LCU")

  expect_equal(gdp1_conv, gdp2_conv)
  expect_equal(gdp3_conv, gdp4_conv)
})
