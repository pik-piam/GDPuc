#' Convert from current Int$PPP to current US$MER
#' @inheritParams convertGDP
current_IntPPP_2_current_USMER <- function(gdp, source) {
  gdp %>%
    current_IntPPP_2_current_LCU(source) %>%
    current_LCU_2_current_USMER(source)
}


#' Convert from current US$MER to current Int$PPP
#' @inheritParams convertGDP
current_USMER_2_current_IntPPP <- function(gdp, source) {
  gdp %>%
    current_USMER_2_current_LCU(source) %>%
    current_LCU_2_current_IntPPP(source)
}


#' Convert from current LCU to constant LCU base y
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
current_LCU_2_constant_LCU_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_LCU_2_constant_LCU_base_2010(source) %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x = 2010, base_y, source)
}


#' Convert from current LCU to constant Int$PPP base y
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
current_LCU_2_constant_IntPPP_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_LCU_2_constant_LCU_base_y(base_y, source) %>%
    constant_LCU_2_constant_IntPPP(base_y, source)
}


#' Convert from current Int$PPP to constant Int$PPP base y
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
current_IntPPP_2_constant_IntPPP_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_IntPPP_2_current_LCU(source) %>%
    current_LCU_2_constant_LCU_base_y(base_y, source) %>%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}


#' Convert from current LCU to constant US$MER base y
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
current_LCU_2_constant_USMER_base_y <- function(gdp, base_y, source) {
  gdp %>%
    current_LCU_2_constant_LCU_base_y(source, base_y) %>%
    constant_LCU_2_constant_USMER(base = base_y, source)
}


#' Convert constant Int$PPP series from one base year to another
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
constant_IntPPP_base_x_2_constant_IntPPP_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_IntPPP_2_constant_LCU(base = base_x, source) %>%
    constant_LCU_base_x_2_constant_IntPPP_base_y(base_x, base_y, source)
}


#' Convert from constant LCU in one base year to constant Int$PPP of another
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
constant_LCU_base_x_2_constant_IntPPP_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}


#' Convert from constant LCU in one base year to constant US$MER of another
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
constant_LCU_base_x_2_constant_USMER_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>%
    constant_LCU_2_constant_USMER(base = base_y, source)
}


#' Convert from constant Int$PPP in one base year to constant LCU of another
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
constant_IntPPP_base_x_2_constant_LCU_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_IntPPP_2_constant_LCU(base = base_x, source) %>%
    constant_LCU_base_x_2_constant_LCU_base_y(basex, base_y, source)
}


#' Convert from constant Int$PPP to constant US$MER
#' @inheritParams constant_LCU_2_constant_IntPPP
constant_IntPPP_2_constant_USMER <- function(gdp, base, source) {
  gdp %>%
    constant_IntPPP_2_constant_LCU(base, source) %>%
    constant_LCU_2_constant_USMER(base, source)
}


#' Convert from constant US$MER to constant Int$PPP
#' @inheritParams constant_LCU_2_constant_IntPPP
constant_USMER_2_constant_IntPPP <- function(gdp, base, source) {
  gdp %>%
    constant_USMER_2_constant_LCU(base, source) %>%
    constant_LCU_2_constant_IntPPP(base, source)
}


#' Convert from constant Int$PPP in one base year to constant US$MER of another
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
constant_IntPPP_base_x_2_constant_USMER_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_IntPPP_2_constant_LCU(base = base_x, source) %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>%
    constant_LCU_2_constant_USMER(base = base_y, source)
}


#' Convert from constant US$MER in one base year to constant Int$PPP of another
#' @inheritParams constant_LCU_base_x_2_constant_LCU_base_y
constant_USMER_base_x_2_constant_IntPPP_base_y <- function(gdp, base_x, base_y, source) {
  gdp %>%
    constant_USMER_2_constant_LCU(base = base_x, source) %>%
    constant_LCU_base_x_2_constant_LCU_base_y(base_x, base_y, source) %>%
    constant_LCU_2_constant_IntPPP(base = base_y, source)
}
