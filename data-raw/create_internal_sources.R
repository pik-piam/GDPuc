# Download data from WDI and save as internal package data
# !! Don't forget to update the "date" section in print_source_info
library(magrittr)
rlang::check_installed(c("WDI", "stringr", "usethis"), reason = "in order to access the WDI database.")

my_vars <- c(
  "GDP (constant LCU)",
  "GDP (current LCU)",
  "GDP: linked series (current LCU)",
  "GDP (constant 2015 US$)",
  "GDP (current US$)",
  "GDP, PPP (constant 2017 international $)",
  "PPP conversion factor, GDP (LCU per international $)",
  "GDP, PPP (current international $)",
  "Population, total",
  "GDP deflator (base year varies by country)",
  "GDP deflator: linked series (base year varies by country)",
  "DEC alternative conversion factor (LCU per US$)",
  "Consumer price index (2010 = 100)"
)

my_info <- WDI::WDIsearch(
  paste0("^",
         paste0(stringr::str_replace_all(my_vars, c("\\$" = "\\\\$",
                                                    "\\(" = "\\\\(",
                                                    "\\)" = "\\\\)",
                                                    "\\%" = "\\\\%")),
                collapse = "$|^"),
         "$"),
  field = "name",
  short = FALSE
) %>%
  tibble::as_tibble()

# Get codes of only countries (excluding regional aggregates)
wb_country_codes  <- WDI::WDIcache()$country %>%
  dplyr::pull(iso3c) %>%
  countrycode::countrycode("wb", "country.name") %>%
  `[`(!is.na(.)) %>%
  countrycode::countrycode("country.name", "wb")


# Download data, remove aggregates and do some pivoting and renaming
my_data <- purrr::map2(my_info$indicator,
                       my_info$name,
                       ~ WDI::WDI(indicator = .x, extra = TRUE) %>%
                         tibble::as_tibble() %>%
                         dplyr::filter(iso3c %in% wb_country_codes) %>%
                         #dplyr::filter(!is.na(region) & region != "Aggregates") %>%
                         dplyr::arrange(iso3c, year) %>%
                         dplyr::select(iso3c, year, tidyselect::contains(.x)) %>%
                         tidyr::pivot_longer(cols = tidyselect::contains(.x), names_to = "id") %>%
                         dplyr::mutate(name = .y) %>%
                         dplyr::select(iso3c, year, id, name, value)) %>%
  purrr::list_rbind()

wb_wdi <- my_data %>%
  dplyr::select(-id) %>%
  tidyr::pivot_wider(names_from = name) %>%
  dplyr::mutate(`GDP deflator: linked series` = `GDP deflator: linked series (base year varies by country)` / 100,
                `GDP deflator` = `GDP deflator (base year varies by country)` / 100,
                `CPI` = `Consumer price index (2010 = 100)` / 100,
                `MER (LCU per US$)` = `DEC alternative conversion factor (LCU per US$)`)

wb_wdi_linked <- wb_wdi %>%
  dplyr::select(iso3c,
                year,
                `GDP deflator` = `GDP deflator: linked series`,
                `PPP conversion factor, GDP (LCU per international $)`,
                `MER (LCU per US$)`)

wb_wdi_cpi <- wb_wdi %>%
  dplyr::select(iso3c,
                year,
                `GDP deflator` = `CPI`,
                `PPP conversion factor, GDP (LCU per international $)`,
                `MER (LCU per US$)`)

# For now, IMF is removed due to copyright issues
#usethis::use_data(imf_weo, wb_wdi, wb_wdi_linked, internal = TRUE, overwrite = TRUE)
usethis::use_data(wb_wdi, wb_wdi_linked, wb_wdi_cpi, internal = TRUE, overwrite = TRUE)




#####################################################################
#####################################################################
#####################################################################
#
##### IMF DATA
#
# Path to the IMF WEO report from April 2021, downloaded as .xls file
# imf_2021_file <- "../../R_projects/gdp_pop_trends/data/source/IMF/WEOApr2021all.xls"
#
# imf_weo <- readr::read_tsv(imf_2021_file) %>%
#   dplyr::rename("iso3c" = ISO) %>%
#   dplyr::filter(`WEO Subject Code` %in% c(
#     "NGDP_R",
#     "NGDP",
#     "NGDPD",
#     "PPPGDP",
#     "NGDPRPPPPC",
#     "NGDP_D",
#     "PPPEX",
#     "LP"
#   )) %>%
#   dplyr::select(iso3c, `WEO Subject Code`, tidyselect::starts_with(c("1", "2"))) %>%
#   dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with(c("1", "2")),
#                               ~ stringr::str_remove_all(.x, ",") %>%
#                                 as.double())) %>%
#   tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")),
#                       names_to = "year") %>%
#   dplyr::mutate(year = as.double(year)) %>%
#   tidyr::pivot_wider(names_from = `WEO Subject Code`) %>%
#   dplyr::rename(
#     "GDP (constant LCU)" = NGDP_R,
#     "GDP (current LCU)" = NGDP,
#     "GDP (current US$)" = NGDPD,
#     "GDP, PPP (current international $)" = PPPGDP,
#     "GDPpc, PPP (constant 2017 international $)" = NGDPRPPPPC,
#     "GDP deflator" = NGDP_D,
#     "PPP conversion factor, GDP (LCU per international $)" = PPPEX,
#     "Population, total" = LP
#   ) %>%
#   dplyr::mutate(
#     `GDP (constant LCU)` = `GDP (constant LCU)` * 1e+9,
#     `GDP (current LCU)` = `GDP (current LCU)` * 1e+9,
#     `GDP (current US$)` = `GDP (current US$)` * 1e+9,
#     `GDP deflator` = `GDP deflator` / 100,
#     `GDP, PPP (current international $)` = `GDP, PPP (current international $)` * 1e+9,
#     `MER (LCU per US$)` = `GDP (current LCU)` / `GDP (current US$)`,
#     `Population, total` = `Population, total` * 1e+6
#   )
