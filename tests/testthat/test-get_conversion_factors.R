test_that("get_conversion_factors", {
  gdp_in <- wb_wdi %>%
    dplyr::filter(!is.na(`GDP, PPP (constant 2017 international $)`),
                  iso3c %in% c("ABW", "USA")) %>%
    dplyr::select(iso3c, year, "value" = `GDP: linked series (current LCU)`)

  myresult <- convertGDP(gdp_in, "constant 2010 Int$PPP", "constant 2015 LCU", return_cfs = FALSE)
  mylist <- convertGDP(gdp_in, "constant 2010 Int$PPP", "constant 2015 LCU", return_cfs = TRUE)

  expect_type(mylist, "list")
  expect_length(mylist, 2)
  expect_named(mylist, c("result", "cfs"))

  expect_identical(myresult, mylist$result)

  expect_s3_class(mylist$cfs, "tbl")
  expect_lte(length(mylist$cfs), 4)
  expect_gte(length(mylist$cfs), 2)
  expect_true(colnames(mylist$cfs)[1] == "iso3c")
  expect_true(mylist$cfs %>%
                dplyr::filter(iso3c == "USA") %>%
                dplyr::pull("2010 PPP conversion factors in (LCU per international $)") == 1)
})



test_that("get_conversion_factors with verbose", {
  gdp <- tibble::tibble("iso3c" = c("JPN", "FRA", "DEU"), "year" = 2010, "value" = 100)

  myresult <- convertGDP(gdp,
                         unit_in = "constant 2015 Int$PPP",
                         unit_out = "constant 2017 Int$PPP",
                         verbose = TRUE) %>%
    suppressMessages()

  mylist <- convertGDP(gdp,
                       unit_in = "constant 2015 Int$PPP",
                       unit_out = "constant 2017 Int$PPP",
                       verbose = TRUE,
                       return_cfs = TRUE) %>%
    suppressMessages()

  expect_identical(myresult, mylist$result)
  expect_identical(mylist$cfs$iso3c, c("DEU", "FRA", "JPN"))
})


test_that("get_conversion_factors with regions", {
  gdp <- tibble::tibble("iso3c" = c("JPN", "EUR", "DEU"), "year" = 2010, "value" = 100)
  with_regions <- tibble::tibble("iso3c" = c("FRA", "ESP", "DEU"), "region" = "EUR")

  myresult <- convertGDP(gdp,
                       unit_in = "constant 2015 Int$PPP",
                       unit_out = "constant 2017 Int$PPP",
                       with_regions = with_regions)
  mylist <- convertGDP(gdp,
                       unit_in = "constant 2015 Int$PPP",
                       unit_out = "constant 2017 Int$PPP",
                       with_regions = with_regions,
                       return_cfs = TRUE)

  expect_identical(myresult, mylist$result)
  expect_identical(mylist$cfs$iso3c, c("DEU", "ESP", "FRA", "JPN"))
})
