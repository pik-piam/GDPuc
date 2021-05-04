imf_2020_file <- "../../R_projects/gdp_pop_trends/data/source/IMF/WEOOct2020all.xlsx"

imf_weo <- readxl::read_xlsx(
  imf_2020_file,
  sheet = 1,
  range = "A1:BD8776",
  col_types = "text"
) %>%
  dplyr::filter(`WEO Subject Code` %in% c(
    "NGDP_R",
    "NGDP",
    "NGDPD",
    "PPPGDP",
    "NGDPRPPPPC",
    "NGDP_D",
    "PPPEX",
    "LP")) %>%
  dplyr::rename("iso3c" = ISO) %>%
  dplyr::select(iso3c, `WEO Subject Code`, tidyselect::starts_with(c("1", "2"))) %>%
  dplyr::mutate(dplyr::across(.cols = tidyselect::starts_with(c("1", "2")),
                              ~ stringr::str_remove_all(.x, ",") %>%
                                as.double())) %>%
  tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")),
                      names_to = "year") %>%
  dplyr::mutate(year = as.double(year)) %>%
  tidyr::pivot_wider(names_from = `WEO Subject Code`) %>%
  dplyr::rename(
    "GDP (constant LCU)" = NGDP_R,
    "GDP (current LCU)" = NGDP,
    "GDP (current US$)" = NGDPD,
    "GDP, PPP (current international $)" = PPPGDP,
    "GDPpc, PPP (constant 2017 international $)" = NGDPRPPPPC,
    "GDP deflator" = NGDP_D,
    "PPP conversion factor, GDP (LCU per international $)" = PPPEX,
    "Population, total" = LP
  ) %>%
  dplyr::mutate(
    `GDP (constant LCU)` = `GDP (constant LCU)` * 1e+9,
    `GDP (current LCU)` = `GDP (current LCU)` * 1e+9,
    `GDP (current US$)` = `GDP (current US$)` * 1e+9,
    `GDP deflator` = `GDP deflator` / 100,
    `GDP, PPP (current international $)` = `GDP, PPP (current international $)` * 1e+9,
    `MER (LCU per US$)` = `GDP (current LCU)` / `GDP (current US$)`,
    `Population, total` = `Population, total` * 1e+6
  )


wdi_file <- "../../R_projects/gdp_pop_trends/data/source/WB/0280a7cc-4b5d-4768-bfc8-52104dc79791_Data.csv"

wb_wdi <- readr::read_csv(wdi_file, col_types = readr::cols(
  .default = readr::col_double(),
  `Country Name` = readr::col_character(),
  `Country Code` = readr::col_character(),
  `Series Name` = readr::col_character(),
  `Series Code` = readr::col_character()
)) %>%
  tidyr::pivot_longer(cols = starts_with(c("1", "2")), names_to = "year") %>%
  dplyr::select("iso3c" = `Country Code`,
                year,
                "variable" = `Series Name`,
                value) %>%
  tidyr::pivot_wider(names_from = variable) %>%
  dplyr::mutate(year = as.integer(substr(year, 1, 4)),
                `GDP deflator: linked series` = `GDP deflator: linked series (base year varies by country)` / 100,
                `GDP deflator` = `GDP deflator (base year varies by country)` / 100,
                `MER (LCU per US$)` = `DEC alternative conversion factor (LCU per US$)`)

wb_wdi_linked <- wb_wdi %>%
  dplyr::select(iso3c,
                year,
                "GDP deflator" = `GDP deflator: linked series`,
                `PPP conversion factor, GDP (LCU per international $)`,
                `MER (LCU per US$)`)

usethis::use_data(imf_weo, wb_wdi, wb_wdi_linked, internal = TRUE, overwrite = TRUE)
