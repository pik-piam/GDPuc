test_that("toolConverGDP", {
  gdp <- tibble::tibble("iso3c" = "USA", "year" = 2010, "value" = 100)
  gdp_conv <- toolConvertGDP(gdp, "current LCU", "current Int$PPP")
  gdp_conv2 <- convertGDP(gdp, "current LCU", "current Int$PPP")
  expect_identical(gdp_conv, gdp_conv2)
})

test_that("convertCPI and toolConvertCPI", {
  gdp <- tibble::tibble("iso3c" = "DEU", "year" = 2010, "value" = 100)
  gdp_conv1 <- convertGDP(gdp, "constant 2015 LCU", "constant 2017 LCU", source = "wb_wdi_cpi")
  gdp_conv2 <- convertCPI(gdp, "constant 2015 LCU", "constant 2017 LCU")
  gdp_conv3 <- toolConvertCPI(gdp, "constant 2015 LCU", "constant 2017 LCU")
  expect_identical(gdp_conv1, gdp_conv2)
  expect_identical(gdp_conv1, gdp_conv3)
})


test_that("convertSingle and toolConvertSingle", {
  gdp <- tibble::tibble("iso3c" = "DEU", year = 2010, "value" = 100)
  gdp_conv1 <- convertGDP(gdp, "constant 2015 LCU", "constant 2017 LCU")
  gdp_conv2 <- convertSingle(100, "DEU", unit_in =  "constant 2015 LCU", unit_out =  "constant 2017 LCU")
  gdp_conv3 <- toolConvertSingle(100, "DEU", year = 1000,
                                 unit_in =  "constant 2015 LCU",
                                 unit_out =  "constant 2017 LCU")
  gdp_conv4 <- toolConvertSingle(100, "DEU", 2010,
                                 unit_in = "constant 2015 LCU",
                                 unit_out = "constant 2017 LCU",
                                 return_cfs = TRUE)
  expect_identical(gdp_conv1$value, gdp_conv2)
  expect_identical(gdp_conv1$value, gdp_conv3)
  expect_identical(gdp_conv1, gdp_conv4$result)
})

