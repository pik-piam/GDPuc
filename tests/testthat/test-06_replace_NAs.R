skip_if_not_installed("zoo")

test_that("convertGDP replace missing conversion factors", {
  # wb_wi does not have info for AIA, so AIA is used for testing here
  gdp <- tidyr::expand_grid(
    "iso3c" = c("AIA", "FRA", "DEU", "USA", "EUR"),
    "year" = c(2010, 3010),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )
  gdp2 <- tidyr::expand_grid("iso3c" = c("AIA", "USA"), "year" = c(2010, 3010), "value" = 100)

  with_regions <- tibble::tibble("iso3c" = c("FRA", "ESP", "DEU", "BEL", "AIA"), "region" = "EUR") %>%
    dplyr::bind_rows(tibble::tibble("iso3c" = "USA", "region" = "USA"))

  expect_warning(
    convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP"),
    "NAs have been generated for countries lacking conversion factors!"
  )
  expect_error(convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP", replace_NAs = FALSE))
  expect_error(convertGDP(gdp, "constant 2010 US$MER", "constant 2011 Int$PPP", replace_NAs = "regional_average"))

  gdp_conv <- suppressWarnings(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2005 US$MER"))

  gdp_conv2 <- convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2005 US$MER", replace_NAs = 0)

  gdp_conv4 <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2005 US$MER",
    with_regions = with_regions,
    replace_NAs = "regional_average"
  )

  gdp_conv5 <- convertGDP(
    gdp2,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2005 US$MER",
    with_regions = with_regions,
    replace_NAs = "regional_average"
  )

  expect_true(any(is.na(gdp_conv$value)))
  expect_true(!any(is.na(gdp_conv2$value)))
  expect_true(!any(is.na(gdp_conv4$value)))

  expect_equal(gdp$iso3c, gdp_conv2$iso3c)
  expect_equal(gdp$iso3c, gdp_conv4$iso3c)
  expect_equal(gdp2$iso3c, gdp_conv5$iso3c)
})

test_that("convertGDP replace_NAs = NA", {
  # wb_wi does not have info for AFG in 2025
  gdp <- tidyr::expand_grid(
    "iso3c" = c("AFG", "DEU", "USA"),
    "year" = c(2010, 2015, 2025),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )

  expect_warning(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER"))

  expect_silent(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER", replace_NAs = NA))

  gdp_1 <- suppressWarnings(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER"))

  gdp_2 <- convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER", replace_NAs = NA)

  expect_equal(gdp_1, gdp_2)
})

test_that("convertGDP replace_NAs = 'no_conversion'", {
  # wb_wi does not have info for AFG in 2025
  gdp <- tidyr::expand_grid(
    "iso3c" = c("AFG", "DEU", "USA"),
    "year" = c(2010, 2015, 2025),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )

  expect_warning(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER"))

  gdp_conv <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2025 US$MER",
    replace_NAs = "no_conversion"
  )

  expect_identical(gdp[1:6, ], gdp_conv[1:6, ])
})

test_that("convertGDP replace_NAs = linear", {
  # wb_wi does not have info for AFG in 2025
  gdp <- tidyr::expand_grid(
    "iso3c" = c("AFG", "DEU", "USA"),
    "year" = c(2010, 2015, 2025),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )

  expect_warning(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER"))

  gdp_conv <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2025 US$MER",
    replace_NAs = "linear"
  )

  expect_true(!any(is.na(gdp_conv$value)))
})

test_that("convertGDP replace_NAs = with_USA", {
  # wb_wi does not have info for AIA at all, nor for AFG in 2022
  gdp <- tidyr::expand_grid(
    "iso3c" = c("AIA", "AFG", "DEU", "USA"),
    "year" = c(2010, 2015, 2025),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )

  expect_warning(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2025 US$MER"))

  gdp_conv <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2025 US$MER",
    replace_NAs = "with_USA",
    return_cfs = TRUE
  )

  expect_true(!any(is.na(gdp_conv$result$value)))
  expect_true(!any(is.na(gdp_conv$cfs)))
  expect_identical(
    dplyr::filter(gdp_conv$cfs, .data$iso3c == "AIA") %>% dplyr::select(-"iso3c"),
    dplyr::filter(gdp_conv$cfs, .data$iso3c == "USA") %>% dplyr::select(-"iso3c")
  )

  gdp_conv2 <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2025 US$MER",
    replace_NAs = c("linear"),
    return_cfs = TRUE
  )

  gdp_conv3 <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2025 US$MER",
    replace_NAs = c("linear", "with_USA"),
    return_cfs = TRUE
  )

  expect_true(any(is.na(gdp_conv2$result$value)))
  expect_true(!any(is.na(gdp_conv3$result$value)))
})

test_that("convertGDP replace_NAs = with_USA, no NAs for MADRAT countries", {
  gdp <- tibble::tibble("iso3c" = madrat::toolGetMapping("regionmappingH12.csv")$CountryCode, value = 1) %>%
    convertGDP(unit_in = "constant 2005 US$MER", unit_out = "constant 2017 Int$PPP", replace_NAs = "with_USA")
  expect_true(!any(is.na(gdp$value)))
})

test_that("log_lin_int_ext", {
  # Geometric series with an interior gap, filled at constant growth up to 5 points from the data
  x <- c(rep(NA, 6), 2^(1:4), NA, 2^(6:7), rep(NA, 6))
  expect_equal(log_lin_int_ext(x), c(NA, 2^(-4:12), NA))
})

test_that("log_lin_int_ext uses OLS growth rate over the closest n_ext points", {
  # OLS slope over log values 0, 1, 3, 6, 10 is 2.5; the endpoint slope would be 4
  x <- exp(c(0, 1, 3, 6, 10))
  expect_equal(log_lin_int_ext(c(x, NA)), c(x, exp(12.5)))
  expect_equal(log_lin_int_ext(c(NA, rev(x))), c(exp(12.5), rev(x)))
})

test_that("log_lin_int_ext keeps observed values and needs n_ext points to extrapolate", {
  x <- c(NA, 1.1, 2.3, 3.7, 4.9, NA)
  expect_identical(log_lin_int_ext(x), x)
  x <- c(NA, 1.1, 2.3, 3.7, 4.9, 6.2, NA)
  expect_identical(log_lin_int_ext(x)[2:6], x[2:6])
})

test_that("log_lin_int_ext with max_ext = 0 changes nothing", {
  x <- c(1, 2, 4, 7, 11, NA)
  expect_equal(log_lin_int_ext(x, max_ext = 0), x)
  expect_equal(log_lin_int_ext(rev(x), max_ext = 0), rev(x))
})

test_that("log_lin_int_ext with trend = FALSE holds the closest value", {
  x <- c(NA, 1, 2, 4, 8, 16, NA, NA)
  expect_equal(log_lin_int_ext(x, trend = FALSE), c(1, 1, 2, 4, 8, 16, 16, 16))
})

test_that("fill_by_anchor holds the price level constant where MER exists", {
  ppp <- c(NA, 2, 4, NA, 8, 10, NA)
  mer <- c(5, 4, 8, 12, 16, 20, 100)
  # Price level 0.5 everywhere, so PPP follows the MER, also when the MER jumps
  expect_equal(fill_by_anchor(ppp, mer), c(2.5, 2, 4, 6, 8, 10, 50))
})

test_that("fill_by_anchor falls back to the PPP trend where MER is missing", {
  ppp <- c(NA, 2, 4, 8, 16, 32, NA)
  mer <- c(NA, 4, 8, 16, 32, 64, 1000)
  expect_equal(fill_by_anchor(ppp, mer), c(1, 2, 4, 8, 16, 32, 500))
})

test_that("fill_by_anchor extrapolates at most 5 years, independent of NAs elsewhere in MER (#55)", {
  # PPP data 1960-1970, MER constant where it exists
  ppp <- c(rep(2, 11), rep(NA, 10))
  # Within 5 years of the data, PPP follows MER at a constant price level, beyond that it stays NA
  expected <- c(rep(2, 16), rep(NA, 5))
  expect_equal(fill_by_anchor(ppp, rep(2, 21)), expected)
  # An NA in MER 1960, where PPP is observed, must not extend the fill beyond 5 years
  expect_equal(fill_by_anchor(ppp, replace(rep(2, 21), 1, NA)), expected)
})

test_that("log_lin_int_ext does not extrapolate conversion factors to non-positive values (#55)", {
  # Shape of the SYR PPP factor: data only for the last years, rising steeply
  x <- c(NA, NA, NA, NA, 133, 250, 400, 550, 700, 808)
  res <- log_lin_int_ext(x)
  expect_true(all(res > 0))
})

test_that("replace_NAs = c('linear', 'no_conversion') gives plausible factors for SYR (#55)", {
  conv <- function(year, unitIn, unitOut) {
    convertGDP(
      data.frame(iso3c = "SYR", year = year, value = 1),
      unitIn,
      unitOut,
      replace_NAs = c("linear", "no_conversion")
    )$value
  }
  # SYR PPP data starts in 2017. Bounds are loose plausibility limits, not target values.
  expect_gt(conv(2017, "constant 2017 Int$PPP", "constant 2005 Int$PPP"), 0)
  expect_true(all(conv(2012:2016, "current Int$PPP", "current LCU") > 0))
  res <- purrr::map_dbl(2012:2016, function(y) conv(2017, "constant 2017 Int$PPP", paste("constant", y, "Int$PPP")))
  expect_true(all(res > 0.1 & res < 10))
})

test_that("replace_NAs = c('linear', 'no_conversion') gives plausible PPP to MER factors for VEN (#55)", {
  # VEN PPP data ends in 2011, MER continues through the hyperinflation. Loose plausibility bound, not a target value.
  res <- convertGDP(
    data.frame(iso3c = "VEN", year = 2012:2016, value = 1),
    "current Int$PPP",
    "current US$MER",
    replace_NAs = c("linear", "no_conversion")
  )
  expect_true(all(res$value > 0.1))
})

test_that("convertGDP replace_NAs = c('linear', 'no_conversion')", {
  # wb_wi does not have info for ABW in 2019
  gdp <- tidyr::expand_grid(
    "iso3c" = c("ABW", "DEU", "USA", "JJJ"),
    "year" = c(2010, 2015, 2025),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )

  expect_warning(convertGDP(gdp, unit_in = "constant 2005 Int$PPP", unit_out = "constant 2019 US$MER"))

  gdp_conv <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2019 US$MER",
    replace_NAs = c("linear", "no_conversion")
  )

  expect_true(!any(is.na(gdp_conv$value)))
  expect_identical(gdp[19:24, ], gdp_conv[19:24, ])

  gdp_conv <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2019 US$MER",
    replace_NAs = c("linear", 0)
  )
  expect_identical(dplyr::pull(gdp_conv[19:24, "value"]), rep(0, 6))
})

test_that("convertGDP use_USA_cf_for_all = TRUE", {
  gdp <- tidyr::expand_grid(
    "iso3c" = c("AIA", "DEU", "USA", "AFG"),
    "year" = c(2010, 2015, 2025),
    "SSP" = c("SSP1", "SSP2"),
    "value" = 100
  )

  gdp_conv <- convertGDP(
    gdp,
    unit_in = "constant 2005 Int$PPP",
    unit_out = "constant 2019 US$MER",
    use_USA_cf_for_all = TRUE
  )

  expect_true(!any(is.na(gdp_conv)))
  expect_identical(
    dplyr::filter(gdp_conv, .data$iso3c == "AIA") %>% dplyr::select(-"iso3c"),
    dplyr::filter(gdp_conv, .data$iso3c == "USA") %>% dplyr::select(-"iso3c"),
    dplyr::filter(gdp_conv, .data$iso3c == "DEU") %>% dplyr::select(-"iso3c"),
    dplyr::filter(gdp_conv, .data$iso3c == "AFG") %>% dplyr::select(-"iso3c")
  )
})

test_that("replace_NAs = 'linear' fills deflators consistently with PPPs for SSD", {
  # SSD has PPPs for the 2016-2017 hyperinflation but no deflator, and no data before 2008. Filling the deflator
  # with its own trend gave a factor of ~8. Loose plausibility bounds, not target values.
  res <- convertGDP(
    data.frame(iso3c = "SSD", year = 2020, value = 1),
    "constant 2017 Int$PPP",
    "constant 2005 Int$PPP",
    replace_NAs = "linear"
  )
  expect_gt(res$value, 0.5)
  expect_lt(res$value, 2)
})

test_that("fill_by_anchor fills the deflator from PPP times US deflator", {
  # Deflator and PPP agree up to year 5, then the PPP jumps (hyperinflation) and the deflator is missing
  deflator <- c(1, 1.1, 1.2, 1.3, 1.4, NA, NA)
  ppp <- c(1, 1.1, 1.2, 1.3, 1.4, 14, 140)
  us <- c(1, 1, 1, 1, 1, 1, 1.1)
  res <- fill_by_anchor(deflator, ppp * us)
  expect_equal(res[1:5], deflator[1:5])
  expect_equal(res[6:7], c(14, 154))
  # Without PPPs, the deflator trend is used
  expect_equal(fill_by_anchor(deflator, rep(NA_real_, 7)), log_lin_int_ext(deflator))
})

test_that("fill_by_anchor fills x when x and anchor overlap little or not at all", {
  # Deflator in years 1-10, anchor only from year 8: 3 years of overlap are enough to hold the ratio
  x <- c(1.1^(0:9), rep(NA, 5))
  anchor <- c(rep(NA, 7), 1.1^(7:14))
  expect_equal(fill_by_anchor(x, anchor), 1.1^(0:14))
  # No overlap: the trend of x is used, also where the anchor exists
  expect_equal(fill_by_anchor(x, c(rep(NA, 10), 1.1^(10:14))), log_lin_int_ext(x))
})
