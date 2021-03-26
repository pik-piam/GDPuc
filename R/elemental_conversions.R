#' Convert from current LCU to constant LCU base 2010
#' @inheritParams convertGDP
current_LCU_2_constant_LCU_base_2010 <- function(gdp, source) {
  gdp_deflator <- eval(rlang::sym(source)) %>%
    dplyr::select(iso3c, year, `GDP deflator`)

  gdp_deflator_at_2010 <- gdp_deflator %>%
    dplyr::filter(year == 2010) %>%
    dplyr::select(iso3c, "GDP deflator in 2010" = `GDP deflator`)

  gdp_deflator_base_2010 <- gdp_deflator  %>%
    dplyr::left_join(gdp_deflator_at_2010, by = "iso3c") %>%
    dplyr::mutate(`GDP deflator` = `GDP deflator` / `GDP deflator in 2010`,
                  .keep = "unused")

  gdp %>%
    dplyr::left_join(gdp_deflator_base_2010, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value / `GDP deflator`, .keep = "unused")
}


#' Convert from current LCU to current Int$PPP
#' @inheritParams convertGDP
current_LCU_2_current_IntPPP <- function(gdp, source) {
  PPP <-  eval(rlang::sym(source)) %>%
    dplyr::select(iso3c, year, `PPP conversion factor, GDP (LCU per international $)`)

  gdp %>%
    dplyr::left_join(PPP, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value / `PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}


#' Convert from current Int$PPP to current LCU
#' @inheritParams convertGDP
current_IntPPP_2_current_LCU <- function(gdp, source) {
  PPP <-  eval(rlang::sym(source)) %>%
    dplyr::select(iso3c, year, `PPP conversion factor, GDP (LCU per international $)`)

  gdp %>%
    dplyr::left_join(PPP, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value * `PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}


#' Convert from current LCU to current US$MER
#' @inheritParams convertGDP
current_LCU_2_current_USMER <- function(gdp, source) {
  MER <-  eval(rlang::sym(source)) %>%
    dplyr::mutate(`MER (LCU per US$)` = `GDP (current LCU)` / `GDP (current US$)`) %>%
    dplyr::select(iso3c, year, `MER (LCU per US$)`)

  gdp %>%
    dplyr::left_join(MER, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value / `MER (LCU per US$)`,
                  .keep = "unused")
}


#' Convert from current US$MER to current LCU
#' @inheritParams convertGDP
current_USMER_2_current_LCU <- function(gdp, source) {
  MER <-  eval(rlang::sym(source)) %>%
    dplyr::mutate(`MER (LCU per US$)` = `GDP (current LCU)` / `GDP (current US$)`) %>%
    dplyr::select(iso3c, year, `MER (LCU per US$)`)

  gdp %>%
    dplyr::left_join(MER, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value * `MER (LCU per US$)`,
                  .keep = "unused")
}


#' Convert from constant LCU to constant Int$PPP
#' @param base A double, the base year of incoming and outgoing constant GDP
#' @inheritParams convertGDP
constant_LCU_2_constant_IntPPP <- function(gdp, base, source) {
  PPP_base <-  eval(rlang::sym(source)) %>%
    dplyr::filter(year == base) %>%
    dplyr::select(iso3c, `PPP conversion factor, GDP (LCU per international $)`)

  gdp %>%
    dplyr::left_join(PPP_base, by = "iso3c") %>%
    dplyr::mutate(value = value / `PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}


#' Convert from constant Int$PPP to constant LCU
#' @inheritParams constant_LCU_2_constant_IntPPP
constant_IntPPP_2_constant_LCU <- function(gdp, base, source) {
  PPP_base <- eval(rlang::sym(source)) %>%
    dplyr::filter(year == base) %>%
    dplyr::select(iso3c, `PPP conversion factor, GDP (LCU per international $)`)

  gdp %>%
    dplyr::left_join(PPP_base, by = "iso3c") %>%
    dplyr::mutate(value = value * `PPP conversion factor, GDP (LCU per international $)`,
                  .keep = "unused")
}


#' Convert from constant LCU to constant US$MER
#' @param base A double, the base year of incoming and outgoing constant GDP
#' @inheritParams convertGDP
constant_LCU_2_constant_USMER <- function(gdp, base, source) {
  MER_base <-  eval(rlang::sym(source)) %>%
    dplyr::mutate(`MER (LCU per US$)` = `current LCU` / `current US$`) %>%
    dplyr::filter(year == base) %>%
    dplyr::select(iso3c, `MER (LCU per US$)`)

  gdp %>%
    dplyr::left_join(MER_base, by = "iso3c") %>%
    dplyr::mutate(value = value / `MER (LCU per US$)`,
                  .keep = "unused")
}


#' Convert from constant US$MER to constant LCU
#' @inheritParams constant_LCU_2_constant_IntPPP
constant_USMER_2_constant_LCU <- function(gdp, base, source) {
  MER_base <-  eval(rlang::sym(source)) %>%
    dplyr::mutate(`MER (LCU per US$)` = `current LCU` / `current US$`) %>%
    dplyr::filter(year == base) %>%
    dplyr::select(iso3c, `MER (LCU per US$)`)

  gdp %>%
    dplyr::left_join(MER_base, by = "iso3c") %>%
    dplyr::mutate(value = value * `MER (LCU per US$)`,
                  .keep = "unused")
}


#' Convert constant LCU series from one base year to another
#' @param base_x A double, base year of incoming constant gdp series
#' @param base_y A double, base year of outgoing constant gdp series
#' @inheritParams convertGDP
constant_LCU_base_x_2_constant_LCU_base_y <- function(gdp, base_x, base_y, source) {
  gdps_LCU <- eval(rlang::sym(source)) %>%
    dplyr::select(iso3c, year, `GDP deflator`)

  def_at_x <- gdps_LCU %>%
    dplyr::filter(year == base_x) %>%
    dplyr::select(iso3c, "def" = `GDP deflator`)

  def_base_x <- gdps_LCU  %>%
    dplyr::left_join(def_at_x, by = "iso3c") %>%
    dplyr::mutate(`GDP deflator` = `GDP deflator` / def,
                  .keep = "unused")

  def_base_x_at_y <- def_base_x %>%
    dplyr::filter(year == base_y) %>%
    dplyr::select(iso3c, "def" = `GDP deflator`)

  gdp %>%
    dplyr::left_join(def_base_x_at_y, by = "iso3c") %>%
    dplyr::mutate(value = value * def, .keep = "unused")
}
