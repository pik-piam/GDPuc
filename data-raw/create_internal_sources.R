# Download data from WDI and save as internal package data
# !! Don't forget to update the "date" section in print_source_info
library(magrittr)
rlang::check_installed(c("usethis"), reason = "to create the R/sysdata.rda file.")

# my_vars <- c(
#   "GDP (constant LCU)",
#   "GDP (current LCU)",
#   "GDP: linked series (current LCU)",
#   "GDP (constant 2015 US$)",
#   "GDP (current US$)",
#   "GDP, PPP (constant 2021 international $)",
#   "PPP conversion factor, GDP (LCU per international $)",
#   "GDP, PPP (current international $)",
#   "Population, total",
#   "GDP deflator (base year varies by country)",
#   "GDP deflator: linked series (base year varies by country)",
#   "DEC alternative conversion factor (LCU per US$)",
#   "Consumer price index (2010 = 100)"
# )

my_data <- readr::read_csv("~/Downloads/wdi_GDPuc_15Jul2026.csv", na = "..", show_col_types = FALSE) %>%
  tidyr::pivot_longer(tidyselect::starts_with(c("19", "20")),
                      names_to = "year",
                      names_transform = ~as.integer(substr(.x, 1, 4))) %>%
  dplyr::select("iso3c" = "Country Code", "year", "name" = "Series Name", "value") %>%
  dplyr::arrange(.data$iso3c, .data$name, .data$year)

wb_wdi <- my_data %>%
  tidyr::pivot_wider(names_from = name) %>%
  dplyr::mutate(`GDP deflator: linked series` = `GDP deflator: linked series (base year varies by country)` / 100,
                `GDP deflator` = `GDP deflator (base year varies by country)` / 100,
                `CPI` = `Consumer price index (2010 = 100)` / 100,
                `MER (LCU per US$)` = `DEC alternative conversion factor (LCU per US$)`)

wb_wdi_linked <- wb_wdi %>%
  dplyr::select("iso3c",
                "year",
                "GDP deflator" = "GDP deflator: linked series",
                "PPP conversion factor, GDP (LCU per international $)",
                "MER (LCU per US$)")

wb_wdi_cpi <- wb_wdi %>%
  dplyr::select("iso3c",
                "year",
                "GDP deflator" = "CPI",
                "PPP conversion factor, GDP (LCU per international $)",
                "MER (LCU per US$)")

usethis::use_data(wb_wdi, wb_wdi_linked, wb_wdi_cpi, internal = TRUE, overwrite = TRUE)
