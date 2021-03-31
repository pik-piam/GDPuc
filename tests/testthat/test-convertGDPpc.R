# test_that("convertGDPpc works", {
#   gdp_in <- imf_weo %>%
#     dplyr::filter(!is.na(`GDPpc, PPP (constant 2017 international $)`)) %>%
#     dplyr::mutate(value = `GDP (current LCU)` / `Population, total`) %>%
#     dplyr::select(iso3c, year, value)
#
#   gdp_conv <- convertGDPpc(gdp_in, "current LCU", "constant 2017 Int$PPP", "imf_weo") %>%
#     dplyr::filter(!is.na(value))
#
#   gdp_out <- imf_weo %>%
#     dplyr::right_join(gdp_conv, by = c("iso3c", "year")) %>%
#     dplyr::select(iso3c, year, "value" = `GDPpc, PPP (constant 2017 international $)`)
#
#   expect_equal(gdp_conv, gdp_out, max_diffs = 1000)
# })
