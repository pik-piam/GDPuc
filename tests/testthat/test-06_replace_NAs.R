skip_if_not_installed("zoo")

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

test_that("convertGDP replace_NAs = NA", {
  # wb_wi does not have info for AFG in 2022
  gdp <- tidyr::expand_grid("iso3c" = c("AFG", "DEU", "USA"),
                            "year" = c(2010, 2015, 2025),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)

  expect_warning(convertGDP(gdp,
                            unit_in = "constant 2005 Int$PPP",
                            unit_out = "constant 2022 US$MER"))

  expect_silent(convertGDP(gdp,
                           unit_in = "constant 2005 Int$PPP",
                           unit_out = "constant 2022 US$MER",
                           replace_NAs = NA))

  gdp_1 <- suppressWarnings(convertGDP(gdp,
                                       unit_in = "constant 2005 Int$PPP",
                                       unit_out = "constant 2022 US$MER"))

  gdp_2 <- convertGDP(gdp,
                      unit_in = "constant 2005 Int$PPP",
                      unit_out = "constant 2022 US$MER",
                      replace_NAs = NA)

  expect_equal(gdp_1, gdp_2)
})

test_that("convertGDP replace_NAs = 'no_conversion'", {
  # wb_wi does not have info for AFG in 2022
  gdp <- tidyr::expand_grid("iso3c" = c("AFG", "DEU", "USA"),
                            "year" = c(2010, 2015, 2025),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)

  expect_warning(convertGDP(gdp,
                            unit_in = "constant 2005 Int$PPP",
                            unit_out = "constant 2022 US$MER"))

  gdp_conv <- convertGDP(gdp,
                         unit_in = "constant 2005 Int$PPP",
                         unit_out = "constant 2022 US$MER",
                         replace_NAs = "no_conversion")

  expect_identical(gdp[1:6,], gdp_conv[1:6,])
})

test_that("convertGDP replace_NAs = linear", {
  # wb_wi does not have info for AFG in 2022
  gdp <- tidyr::expand_grid("iso3c" = c("AFG", "DEU", "USA"),
                            "year" = c(2010, 2015, 2025),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)

  expect_warning(convertGDP(gdp,
                            unit_in = "constant 2005 Int$PPP",
                            unit_out = "constant 2022 US$MER"))

  gdp_conv <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2022 US$MER",
                          replace_NAs = "linear")

  expect_true(!any(is.na(gdp_conv$value)))
})

test_that("convertGDP replace_NAs = with_USA", {
  # wb_wi does not have info for AIA at all, nor for AFG in 2022
  gdp <- tidyr::expand_grid("iso3c" = c("AIA", "AFG", "DEU", "USA"),
                            "year" = c(2010, 2015, 2025),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)

  expect_warning(convertGDP(gdp,
                            unit_in = "constant 2005 Int$PPP",
                            unit_out = "constant 2022 US$MER"))

  gdp_conv <- convertGDP(gdp,
                         unit_in = "constant 2005 Int$PPP",
                         unit_out = "constant 2022 US$MER",
                         replace_NAs = "with_USA",
                         return_cfs = TRUE)

  expect_true(!any(is.na(gdp_conv$result$value)))
  expect_true(!any(is.na(gdp_conv$cfs)))
  expect_identical(dplyr::filter(gdp_conv$cfs, .data$iso3c == "AIA") %>% dplyr::select(-"iso3c"),
                   dplyr::filter(gdp_conv$cfs, .data$iso3c == "USA") %>% dplyr::select(-"iso3c"))

  gdp_conv2 <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2022 US$MER",
                          replace_NAs = c("linear"),
                          return_cfs = TRUE)

  gdp_conv3 <- convertGDP(gdp,
                          unit_in = "constant 2005 Int$PPP",
                          unit_out = "constant 2022 US$MER",
                          replace_NAs = c("linear", "with_USA"),
                          return_cfs = TRUE)

  expect_true(any(is.na(gdp_conv2$result$value)))
  expect_true(!any(is.na(gdp_conv3$result$value)))
})

test_that("lin_int_ext", {
  x <- c(NA,NA,NA,NA,NA,NA,2,3,4,5,NA,7,8,NA,NA,NA,NA,NA,NA)
  expect_equal(lin_int_ext(x), -4:14)
})

test_that("convertGDP replace_NAs = c('linear', 'no_conversion')", {
  # wb_wi does not have info for ABW in 2019
  gdp <- tidyr::expand_grid("iso3c" = c("ABW", "DEU", "USA", "JJJ"),
                            "year" = c(2010, 2015, 2025),
                            "SSP" = c("SSP1", "SSP2"), "value" = 100)

  expect_warning(convertGDP(gdp,
                            unit_in = "constant 2005 Int$PPP",
                            unit_out = "constant 2019 US$MER"))

  gdp_conv <- convertGDP(gdp,
                         unit_in = "constant 2005 Int$PPP",
                         unit_out = "constant 2019 US$MER",
                         replace_NAs = c("linear", "no_conversion"))

  expect_true(!any(is.na(gdp_conv$value)))
  expect_identical(gdp[19:24,], gdp_conv[19:24,])

  gdp_conv <- convertGDP(gdp,
                         unit_in = "constant 2005 Int$PPP",
                         unit_out = "constant 2019 US$MER",
                         replace_NAs = c("linear", 0))
  expect_identical(dplyr::pull(gdp_conv[19:24, "value"]), rep(0, 6))
})

