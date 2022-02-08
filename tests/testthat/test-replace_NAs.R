test_that("convertGDP replace missing conversion factors", {
  # wb_wi does not have info for AIA, so AIA is used for testing here
  gdp <- tidyr::expand_grid("iso3c" = c("AIA", "FRA", "DEU", "USA", "EUR"), "year" = c(2010, 3010),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)
  gdp2 <- tidyr::expand_grid("iso3c" = c("AIA", "USA"), "year" = c(2010, 3010), "value" = 100)

  with_regions <- tibble::tibble("iso3c" = c("FRA", "ESP", "DEU", "BEL", "AIA"), "region" = "EUR") %>%
    dplyr::bind_rows(tibble::tibble("iso3c" = "USA", "region" = "USA"))

  expect_warning(convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP"),
                 "NAs have been generated for countries lacking conversion factors!")
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
                          replace_NAs = 1) %>%
    suppressWarnings()

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


test_that("convertGDP replace_NAs = linear", {
  # wb_wi does not have info for AIA, so AIA is used for testing here
  gdp <- tidyr::expand_grid("iso3c" = c("ABW", "DEU", "USA"),
                            "year" = c(2010, 2015, 2025),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)

  expect_warning(convertGDP(gdp,
                            unit_in = "constant 2005 Int$PPP",
                            unit_out = "constant 2019 US$MER"))

  gdp_conv <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2005 US$MER",
                          replace_NAs = "linear")

  expect_true(!any(is.na(gdp_conv$value)))
})


test_that("lin_int_ext", {
  x <- c(NA,NA,NA,NA,NA,NA,2,3,4,5,NA,7,8,NA,NA,NA,NA,NA,NA)
  expect_equal(lin_int_ext(x), -4:14)
})
